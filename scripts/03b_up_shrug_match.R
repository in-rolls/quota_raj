# Match UP Election Data to SHRUG via LGD Block Panchayat Hierarchy
#
# Matching strategy:
# 1. Filter out municipality/urban entries before matching
# 2. Match election blocks to LGD blocks via comprehensive crosswalk
# 3. GP name matching within block: exact first, then two-pass fuzzy
#    - Strict pass: JW distance < 0.20
#    - Loose pass: JW distance < 0.30 for remaining unmatched
# 4. Detect and handle ties, one-to-many, many-to-one issues
# 5. Link matched GPs to SHRUG via LGD code
#
# Uses comprehensive crosswalks from:
# - scripts/01f_up_create_district_xwalk.R
# - scripts/01g_up_create_block_xwalk.R

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
write_csv(urban_excluded, here("data/crosswalks/audit/up_urban_excluded.csv"))
message("Exported: data/crosswalks/audit/up_urban_excluded.csv")

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
# STEP 4: Match GP names within block (two-pass fuzzy matching)
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

# Two-pass fuzzy matching
run_fuzzy_pass <- function(unmatched_data, lgd_data, threshold, pass_name) {
    fuzzy_results <- list()
    unique_blocks <- unique(unmatched_data$lgd_block_code)
    message("Processing", length(unique_blocks), "blocks for", pass_name, "fuzzy matching (threshold =", threshold, ")...")

    pb <- txtProgressBar(min = 0, max = length(unique_blocks), style = 3)
    for (i in seq_along(unique_blocks)) {
        blk <- unique_blocks[i]
        block_elex <- unmatched_data %>% filter(lgd_block_code == blk)
        block_lgd <- lgd_data %>% filter(block_code == blk)

        for (j in 1:nrow(block_elex)) {
            result <- fuzzy_match_within_block(
                block_elex[j, ], block_lgd,
                threshold = threshold,
                id_col = "key_2010",
                gp_col = "gp_name_eng_2010"
            )
            if (!is.null(result)) {
                result$match_quality <- pass_name
                fuzzy_results[[length(fuzzy_results) + 1]] <- result
            }
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    bind_rows(fuzzy_results)
}

# Pass 1: Strict matching (threshold = 0.20)
fuzzy_strict <- run_fuzzy_pass(unmatched, lgd_gps, threshold = 0.20, pass_name = "strict")
message("Strict fuzzy matches: ", nrow(fuzzy_strict))

# Pass 2: Loose matching for remaining unmatched (threshold = 0.30)
still_unmatched <- unmatched %>%
    anti_join(fuzzy_strict, by = "key_2010")

fuzzy_loose <- run_fuzzy_pass(still_unmatched, lgd_gps, threshold = 0.30, pass_name = "loose")
message("Loose fuzzy matches: ", nrow(fuzzy_loose))

fuzzy_df <- bind_rows(fuzzy_strict, fuzzy_loose)
message("Total fuzzy GP matches: ", nrow(fuzzy_df))

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
        tie_count,
        match_quality
    )

message("\nTie-resolved matches: ", nrow(tie_resolved))
if (nrow(tie_resolved) > 0) {
    write_csv(tie_resolved, here("data/crosswalks/audit/up_tie_resolved.csv"))
    message("Exported: data/crosswalks/audit/up_tie_resolved.csv")
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
            match_quality = "exact",
            match_distance = 0,
            match_confidence = "unique"
        ),
    fuzzy_df %>%
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
            match_quality = match_quality,
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
            match_quality
        ) %>%
        arrange(district, block, elex_gp, match_distance)

    write_csv(one_to_many_export, here("data/crosswalks/audit/up_one_to_many_errors.csv"))
    message("Exported: data/crosswalks/audit/up_one_to_many_errors.csv")

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
            match_quality,
            key_2010
        ) %>%
        arrange(lgd_gp_code, match_distance)

    write_csv(many_to_one_export, here("data/crosswalks/audit/up_many_to_one_audit.csv"))
    message("Exported: data/crosswalks/audit/up_many_to_one_audit.csv")
    message("NOTE: Many-to-one may be valid (same GP in multiple election rounds). Review manually.")
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
        all_matches %>% select(key_2010, lgd_gp_code, lgd_gp_name, gp_match_type, match_quality, match_confidence),
        by = "key_2010"
    )

message("\n=== LGD GP MATCH SUMMARY ===")
message("Total election GPs: ", nrow(up_05_10))
message("Matched to LGD GP: ", sum(!is.na(up_matched$lgd_gp_code)))
message("LGD match rate: ", round(100 * sum(!is.na(up_matched$lgd_gp_code)) / nrow(up_05_10), 1), "%")

message("\nBy GP match type:")
print(table(up_matched$gp_match_type, useNA = "ifany"))

message("\nBy match quality:")
print(table(up_matched$match_quality, useNA = "ifany"))

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

write_csv(lgd_but_no_shrug, here("data/crosswalks/audit/up_gp_lgd_no_shrug.csv"))
write_csv(no_lgd_match, here("data/crosswalks/audit/up_gp_unmatched.csv"))
message("Exported unmatched GPs for review.")

# ============================================================================
# STEP 11: Save output
# ============================================================================

write_parquet(up_shrug, here("data/up/shrug_lgd_up_elex_05_10_block.parquet"))
message("\nSaved: data/up/shrug_lgd_up_elex_05_10_block.parquet")

# ============================================================================
# STEP 12: Report by district
# ============================================================================

message("\n=== MATCH RATE BY DISTRICT ===")

by_dist <- up_shrug %>%
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
# STEP 13: Summary verification
# ============================================================================

message("\n=== VERIFICATION SUMMARY ===")
message("Block crosswalk entries: ", nrow(block_xwalk))
message("Block match types:")
print(table(block_xwalk$match_type))
message("\nUrban entries excluded: ", nrow(urban_excluded))
message("Election GPs with block match: ", sum(!is.na(up_with_block$lgd_block_code)),
    "/", nrow(up_05_10), "\n")
message("Election GPs with LGD GP match: ", sum(!is.na(up_matched$lgd_gp_code)),
    "/", nrow(up_05_10), "\n")
message("Election GPs with SHRUG match: ", sum(!is.na(up_shrug$shrid2)),
    "/", nrow(up_05_10), "\n")

message("\nNote: SHRUG match rate is limited by SHRUG-LGD crosswalk coverage.")
message("See tabs/up_shrug_coverage_audit.md for full analysis.")

message("\n=== Done ===")
