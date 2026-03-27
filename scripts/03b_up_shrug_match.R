# Match UP Election Data to SHRUG via LGD Block Panchayat Hierarchy
#
# Matching strategy:
# 1. Filter out municipality/urban entries before matching
# 2. Match election blocks to LGD blocks via comprehensive crosswalk
# 3. GP name matching within block: exact first, then fuzzy (JW distance < 0.20)
# 4. Detect and handle ties, one-to-many, many-to-one issues
# 5. Link matched GPs to SHRUG via LGD code
#
# Uses comprehensive crosswalks from:
# - scripts/01e_up_create_district_xwalk.R
# - scripts/01f_up_create_block_xwalk.R

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== UP SHRUG-LGD Block Matching ===")

# ============================================================================
# STEP 1: Load crosswalks and data
# ============================================================================

block_xwalk <- read_csv(here("data/crosswalks/active/up_block_xwalk.csv"), show_col_types = FALSE)
message("Block crosswalk entries: ", nrow(block_xwalk))

lgd_gp <- read_csv(here("data/lgd/processed/lgd_up_block_gp.csv"), show_col_types = FALSE) %>%
    mutate(gp_name_std = normalize_string(gp_name))
message("LGD GPs: ", nrow(lgd_gp))

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
message("Election GPs (raw): ", nrow(up_05_10))

# ============================================================================
# STEP 2: Filter out municipality/urban entries
# ============================================================================

message("\n=== FILTERING MUNICIPALITY/URBAN ENTRIES ===")

urban_patterns <- c(
    "NAGAR PALIKA",
    "NAGAR PANCHAYAT",
    "MUNICIPAL",
    "NAGARPALIKA",
    "NAGARPANCHAYAT",
    "\\bWARD\\s*NO\\b",
    "\\bWARD\\s*[0-9]+\\b"
)

up_05_10 <- up_05_10 %>%
    mutate(
        gp_upper = toupper(gp_name_eng_2010),
        is_urban = grepl(paste(urban_patterns, collapse = "|"), gp_upper, ignore.case = TRUE)
    )

urban_excluded <- up_05_10 %>%
    filter(is_urban) %>%
    select(
        district_name_eng_2010,
        block_name_eng_2010,
        gp_name_eng_2010,
        key_2010
    ) %>%
    distinct()

message("Urban/municipality entries excluded: ", nrow(urban_excluded))
write_csv(urban_excluded, here("data/crosswalks/audit/03b_up_urban_excluded.csv"))
message("Exported: data/crosswalks/audit/03b_up_urban_excluded.csv")

up_05_10 <- up_05_10 %>%
    filter(!is_urban) %>%
    select(-gp_upper, -is_urban)

message("Election GPs after filtering: ", nrow(up_05_10))

# ============================================================================
# STEP 3: Join election data to block crosswalk
# ============================================================================

up_with_block <- up_05_10 %>%
    left_join(
        block_xwalk %>% select(elex_district, elex_block, lgd_block_code, lgd_block_name, match_type),
        by = c("district_name_eng_2010" = "elex_district",
               "block_name_eng_2010" = "elex_block")
    ) %>%
    rename(block_match_type = match_type)

message("Election rows with block code: ", sum(!is.na(up_with_block$lgd_block_code)),
    "of", nrow(up_with_block), "\n")

unmatched_blocks <- up_with_block %>%
    filter(is.na(lgd_block_code)) %>%
    select(district_name_eng_2010, block_name_eng_2010) %>%
    distinct()

if (nrow(unmatched_blocks) > 0) {
    message("\nWARNING: Unmatched blocks (may be urban wards):")
    print(unmatched_blocks)
}

# ============================================================================
# STEP 4: Match GP names within block (exact + fuzzy)
# ============================================================================

up_gps <- up_with_block %>%
    mutate(
        elex_gp_std = normalize_string(gp_name_eng_2010),
        elex_key = paste(lgd_block_code, elex_gp_std)
    ) %>%
    filter(!is.na(lgd_block_code), !is.na(gp_name_eng_2010))

lgd_gps <- lgd_gp %>%
    mutate(lgd_key = paste(block_code, gp_name_std))

message("\nElection GPs to match: ", nrow(up_gps))

# Exact match first
exact_matches <- up_gps %>%
    inner_join(
        lgd_gps %>% select(gp_code, gp_name, block_code, lgd_key),
        by = c("elex_key" = "lgd_key"),
        relationship = "many-to-many"
    )

message("Exact GP matches: ", nrow(exact_matches))

# Unmatched GPs for fuzzy matching
unmatched <- up_gps %>%
    anti_join(exact_matches, by = c("key_2010", "gp_name_eng_2010"))

message("Unmatched GPs for fuzzy: ", nrow(unmatched))

# Fuzzy matching (JW distance < 0.20)
FUZZY_THRESHOLD <- 0.20

fuzzy_results <- list()
unique_blocks <- unique(unmatched$lgd_block_code)
message("Processing ", length(unique_blocks), " blocks for fuzzy matching (threshold = ", FUZZY_THRESHOLD, ")...")

pb <- txtProgressBar(min = 0, max = length(unique_blocks), style = 3)
for (i in seq_along(unique_blocks)) {
    blk <- unique_blocks[i]
    block_elex <- unmatched %>% filter(lgd_block_code == blk)
    block_lgd <- lgd_gps %>% filter(block_code == blk)

    for (j in 1:nrow(block_elex)) {
        result <- fuzzy_match_within_block(
            block_elex[j, ], block_lgd,
            threshold = FUZZY_THRESHOLD,
            id_col = "key_2010",
            gp_col = "gp_name_eng_2010"
        )
        if (!is.null(result)) {
            fuzzy_results[[length(fuzzy_results) + 1]] <- result
        }
    }
    setTxtProgressBar(pb, i)
}
close(pb)

fuzzy_df <- bind_rows(fuzzy_results)
message("Fuzzy GP matches: ", nrow(fuzzy_df))

# ============================================================================
# STEP 5: Export tie-resolved matches for audit
# ============================================================================

tie_resolved <- fuzzy_df %>%
    filter(match_confidence == "tie_resolved") %>%
    left_join(
        up_gps %>% select(key_2010, district_name_eng_2010, block_name_eng_2010),
        by = "key_2010"
    ) %>%
    select(
        district = district_name_eng_2010,
        block = block_name_eng_2010,
        elex_gp = gp_name_eng_2010,
        lgd_gp = lgd_gp_name,
        lgd_gp_code,
        match_distance,
        tie_count
    )

message("\nTie-resolved matches: ", nrow(tie_resolved))
if (nrow(tie_resolved) > 0) {
    write_csv(tie_resolved, here("data/crosswalks/audit/03b_up_tie_resolved.csv"))
    message("Exported: data/crosswalks/audit/03b_up_tie_resolved.csv")
}

numeric_mismatch <- fuzzy_df %>%
    filter(match_confidence == "numeric_mismatch") %>%
    left_join(
        up_gps %>% select(key_2010, district_name_eng_2010, block_name_eng_2010),
        by = "key_2010"
    ) %>%
    select(
        district = district_name_eng_2010,
        block = block_name_eng_2010,
        elex_gp = gp_name_eng_2010,
        lgd_gp = lgd_gp_name,
        lgd_gp_code,
        match_distance
    )

message("Numeric mismatch matches: ", nrow(numeric_mismatch))
if (nrow(numeric_mismatch) > 0) {
    write_csv(numeric_mismatch, here("data/crosswalks/audit/03b_up_numeric_mismatch.csv"))
    message("Exported: data/crosswalks/audit/03b_up_numeric_mismatch.csv")
}

# ============================================================================
# STEP 6: Combine matches
# ============================================================================

all_matches <- bind_rows(
    exact_matches %>%
        transmute(
            key_2010 = key_2010,
            gp_name_eng_2010 = gp_name_eng_2010,
            lgd_gp_code = gp_code,
            lgd_gp_name = gp_name,
            lgd_block_code = lgd_block_code,
            lgd_block_name = lgd_block_name,
            gp_match_type = "exact",
            match_distance = 0,
            match_confidence = "unique"
        ),
    fuzzy_df %>%
        filter(!match_confidence %in% c("tie_resolved", "numeric_mismatch")) %>%
        left_join(
            up_gps %>% select(key_2010, lgd_block_code, lgd_block_name) %>% distinct(),
            by = "key_2010"
        ) %>%
        transmute(
            key_2010 = key_2010,
            gp_name_eng_2010 = gp_name_eng_2010,
            lgd_gp_code = lgd_gp_code,
            lgd_gp_name = lgd_gp_name,
            lgd_block_code = lgd_block_code,
            lgd_block_name = lgd_block_name,
            gp_match_type = "fuzzy",
            match_distance = match_distance,
            match_confidence = match_confidence
        )
)

# ============================================================================
# STEP 7: Check for one-to-many and many-to-one issues
# ============================================================================

message("\n=== DEDUPLICATION CHECKS ===")

# One-to-many: election GP matches multiple LGD GPs
one_to_many <- all_matches %>%
    group_by(key_2010, gp_name_eng_2010) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    left_join(
        up_gps %>% select(key_2010, district_name_eng_2010, block_name_eng_2010) %>% distinct(),
        by = "key_2010"
    )

message("One-to-many matches (election GP -> multiple LGD GPs): ", n_distinct(one_to_many$key_2010))

if (nrow(one_to_many) > 0) {
    one_to_many_export <- one_to_many %>%
        select(
            district = district_name_eng_2010,
            block = block_name_eng_2010,
            elex_gp = gp_name_eng_2010,
            lgd_gp = lgd_gp_name,
            lgd_gp_code,
            match_distance,
            gp_match_type
        ) %>%
        arrange(district, block, elex_gp, match_distance)

    write_csv(one_to_many_export, here("data/crosswalks/audit/03b_up_one_to_many_errors.csv"))
    message("Exported: data/crosswalks/audit/03b_up_one_to_many_errors.csv")

    all_matches <- all_matches %>%
        group_by(key_2010, gp_name_eng_2010) %>%
        slice_min(match_distance, n = 1, with_ties = FALSE) %>%
        ungroup()

    message("Resolved one-to-many by keeping best match. New total: ", nrow(all_matches))
}

# Many-to-one: multiple election GPs match same LGD GP
many_to_one <- all_matches %>%
    group_by(lgd_gp_code) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    left_join(
        up_gps %>% select(key_2010, district_name_eng_2010, block_name_eng_2010) %>% distinct(),
        by = "key_2010"
    )

message("Many-to-one matches (multiple election GPs -> same LGD GP): ", n_distinct(many_to_one$lgd_gp_code))

if (nrow(many_to_one) > 0) {
    many_to_one_export <- many_to_one %>%
        select(
            district = district_name_eng_2010,
            block = block_name_eng_2010,
            elex_gp = gp_name_eng_2010,
            lgd_gp = lgd_gp_name,
            lgd_gp_code,
            match_distance,
            gp_match_type,
            key_2010
        ) %>%
        arrange(lgd_gp_code, match_distance)

    write_csv(many_to_one_export, here("data/crosswalks/audit/03b_up_many_to_one_audit.csv"))
    message("Exported: data/crosswalks/audit/03b_up_many_to_one_audit.csv")

    all_matches <- all_matches %>%
        left_join(
            up_gps %>% select(key_2010, district_name_eng_2010, block_name_eng_2010) %>% distinct(),
            by = "key_2010"
        ) %>%
        group_by(district_name_eng_2010, block_name_eng_2010, lgd_gp_code) %>%
        slice_min(match_distance, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(-district_name_eng_2010, -block_name_eng_2010)

    message("Resolved many-to-one within district/block by keeping best match. New total: ", nrow(all_matches))
}

# ============================================================================
# STEP 8: Join back to election data and link to SHRUG
# ============================================================================

up_matched <- up_05_10 %>%
    left_join(
        up_with_block %>%
            select(key_2010, lgd_block_code, lgd_block_name, block_match_type) %>%
            distinct(),
        by = "key_2010"
    ) %>%
    left_join(
        all_matches %>% select(key_2010, lgd_gp_code, lgd_gp_name, gp_match_type, match_distance, match_confidence),
        by = "key_2010"
    )

message("\n=== LGD GP MATCH SUMMARY ===")
message("Total election GPs: ", nrow(up_05_10))
message("Matched to LGD GP: ", sum(!is.na(up_matched$lgd_gp_code)))
message("LGD match rate: ", round(100 * sum(!is.na(up_matched$lgd_gp_code)) / nrow(up_05_10), 1), "%")

message("\nBy GP match type:")
print(table(up_matched$gp_match_type, useNA = "ifany"))

message("\nBy block match type:")
print(table(up_matched$block_match_type, useNA = "ifany"))

# ============================================================================
# STEP 9: Link to SHRUG via LGD code
# ============================================================================

# NOTE: Multiple SHRUG villages may map to one LGD GP code. We keep the first match.
# SHRUG covariates reflect one village per GP, not GP-level aggregates.
# For robustness, consider: group_by(LGD_code) %>% summarize(across(starts_with("pc01"), mean))
shrug_lgd <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
                      show_col_types = FALSE) %>%
    filter(tolower(state_name) == "uttar pradesh") %>%
    select(shrid2, LGD_code, local_body_name) %>%
    filter(!is.na(LGD_code)) %>%
    group_by(LGD_code) %>%
    slice(1) %>%
    ungroup()

message("\nSHRUG GPs for UP: ", n_distinct(shrug_lgd$LGD_code))

up_shrug <- up_matched %>%
    left_join(
        shrug_lgd %>% select(shrid2, LGD_code),
        by = c("lgd_gp_code" = "LGD_code")
    )

message("\n=== SHRUG MATCH SUMMARY ===")
message("Matched to SHRUG: ", sum(!is.na(up_shrug$shrid2)))
message("SHRUG match rate: ", round(100 * sum(!is.na(up_shrug$shrid2)) / nrow(up_05_10), 1), "%")

# ============================================================================
# STEP 10: Export unmatched GPs for audit
# ============================================================================

message("\n=== EXPORTING UNMATCHED GPs ===")

lgd_but_no_shrug <- up_shrug %>%
    filter(!is.na(lgd_gp_code), is.na(shrid2)) %>%
    select(
        district_name_eng_2010,
        block_name_eng_2010,
        gp_name_eng_2010,
        lgd_block_code,
        lgd_block_name,
        lgd_gp_code,
        lgd_gp_name,
        gp_match_type
    ) %>%
    distinct()

message("GPs matched to LGD but not SHRUG: ", nrow(lgd_but_no_shrug))

no_lgd_match <- up_shrug %>%
    filter(is.na(lgd_gp_code), !is.na(gp_name_eng_2010)) %>%
    select(
        district_name_eng_2010,
        block_name_eng_2010,
        gp_name_eng_2010,
        lgd_block_code,
        lgd_block_name
    ) %>%
    distinct()

message("GPs not matched to LGD: ", nrow(no_lgd_match))

write_csv(lgd_but_no_shrug, here("data/crosswalks/audit/03b_up_gp_lgd_no_shrug.csv"))
write_csv(no_lgd_match, here("data/crosswalks/audit/03b_up_gp_unmatched.csv"))
message("Exported unmatched GPs for review.")

# ============================================================================
# STEP 11: Load SHRUG covariates and aggregate to LGD GP level
# ============================================================================

message("\n=== LOADING SHRUG COVARIATES ===")

shrug_pca <- read_csv(here("data/shrug/shrug-pca01-csv/pc01_pca_clean_shrid.csv.zip"), show_col_types = FALSE)
shrug_vd <- read_csv(here("data/shrug/shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"), show_col_types = FALSE)

message("SHRUG PCA rows: ", nrow(shrug_pca))
message("SHRUG VD rows: ", nrow(shrug_vd))

shrug_lgd_full <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"), show_col_types = FALSE) %>%
    filter(tolower(state_name) == "uttar pradesh")

sum_or_na <- function(x) {
    if (all(is.na(x))) return(NA_real_)
    sum(x, na.rm = TRUE)
}

shrug_covars <- shrug_lgd_full %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2") %>%
    filter(!is.na(LGD_code)) %>%
    group_by(LGD_code) %>%
    summarize(
        shrid2 = first(shrid2),
        n_villages = n(),
        across(starts_with("pc01_pca_"), sum_or_na),
        across(starts_with("pc01_vd_"), sum_or_na),
        .groups = "drop"
    )

message("SHRUG covariates aggregated to ", n_distinct(shrug_covars$LGD_code), " LGD GPs")

# ============================================================================
# STEP 12: Build LGD mapping from 05-10 panel for propagation to other panels
# ============================================================================

message("\n=== BUILDING LGD MAPPING FOR PANEL PROPAGATION ===")

up_mapping <- up_shrug %>%
    filter(!is.na(lgd_gp_code)) %>%
    distinct(match_key, .keep_all = TRUE) %>%
    select(
        match_key,
        lgd_gp_code,
        lgd_gp_name,
        lgd_block_code,
        lgd_block_name,
        block_match_type,
        gp_match_type,
        match_distance,
        match_confidence
    )

message("Unique GPs with LGD match: ", nrow(up_mapping))

# ============================================================================
# STEP 13: Process ALL panels and add SHRUG covariates
# ============================================================================

message("\n=== PROCESSING ALL PANELS ===")

process_panel <- function(panel_path, output_path) {
    panel_name <- basename(panel_path)
    message("\nProcessing: ", panel_name)

    panel <- read_parquet(panel_path)
    message("  Input rows: ", nrow(panel))

    panel_with_lgd <- panel %>%
        left_join(up_mapping, by = "match_key")

    panel_final <- panel_with_lgd %>%
        left_join(
            shrug_covars,
            by = c("lgd_gp_code" = "LGD_code")
        )

    write_parquet(panel_final, output_path)

    message("  Rows with LGD match: ", sum(!is.na(panel_final$lgd_gp_code)))
    message("  Rows with SHRUG data: ", sum(!is.na(panel_final$shrid2)))
    message("  Saved: ", output_path)

    return(panel_final)
}

up_05_10_final <- process_panel(
    here("data/up/up_05_10.parquet"),
    here("data/up/shrug_gp_up_05_10_block.parquet")
)

up_10_15_final <- process_panel(
    here("data/up/up_10_15.parquet"),
    here("data/up/shrug_gp_up_10_15_block.parquet")
)

up_15_21_final <- process_panel(
    here("data/up/up_15_21.parquet"),
    here("data/up/shrug_gp_up_15_21_block.parquet")
)

up_05_21_final <- process_panel(
    here("data/up/up_05_21.parquet"),
    here("data/up/shrug_gp_up_05_21_block.parquet")
)

# ============================================================================
# STEP 14: Report by district (using 05-10 panel)
# ============================================================================

message("\n=== MATCH RATE BY DISTRICT (05-10 panel) ===")

by_dist <- up_05_10_final %>%
    group_by(district_name_eng_2010) %>%
    summarize(
        total = n(),
        lgd_matched = sum(!is.na(lgd_gp_code)),
        lgd_rate = round(100 * lgd_matched / total, 1),
        shrug_matched = sum(!is.na(shrid2)),
        shrug_rate = round(100 * shrug_matched / total, 1),
        .groups = "drop"
    ) %>%
    arrange(shrug_rate)

message("\nLowest SHRUG match rates:")
print(head(by_dist, 15))

message("\nHighest SHRUG match rates:")
print(tail(by_dist, 10))

# ============================================================================
# STEP 15: Verification summary
# ============================================================================

message("\n=== VERIFICATION SUMMARY ===")
message("Block crosswalk entries: ", nrow(block_xwalk))
message("Block match types:")
print(table(block_xwalk$match_type))
message("\nUrban entries excluded: ", nrow(urban_excluded))

message("\n05-10 Panel:")
message("  Total GPs: ", nrow(up_05_10_final))
message("  Matched to LGD: ", sum(!is.na(up_05_10_final$lgd_gp_code)))
message("  Match rate: ", round(100 * sum(!is.na(up_05_10_final$lgd_gp_code)) / nrow(up_05_10_final), 1), "%")
message("  Matched to SHRUG: ", sum(!is.na(up_05_10_final$shrid2)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(up_05_10_final$shrid2)) / nrow(up_05_10_final), 1), "%")

message("\n10-15 Panel:")
message("  Total GPs: ", nrow(up_10_15_final))
message("  Matched to LGD: ", sum(!is.na(up_10_15_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(up_10_15_final$shrid2)) / nrow(up_10_15_final), 1), "%")

message("\n15-21 Panel:")
message("  Total GPs: ", nrow(up_15_21_final))
message("  Matched to LGD: ", sum(!is.na(up_15_21_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(up_15_21_final$shrid2)) / nrow(up_15_21_final), 1), "%")

message("\n05-21 Panel:")
message("  Total GPs: ", nrow(up_05_21_final))
message("  Matched to LGD: ", sum(!is.na(up_05_21_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(up_05_21_final$shrid2)) / nrow(up_05_21_final), 1), "%")

message("\nSHRUG covariates included: ", sum(grepl("^pc01_", names(up_05_10_final))), " variables")

message("\nNote: SHRUG match rate is limited by SHRUG-LGD crosswalk coverage.")

message("\n=== Done ===")
