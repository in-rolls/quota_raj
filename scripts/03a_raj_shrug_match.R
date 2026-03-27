# Match Rajasthan Election Data to SHRUG via LGD Block Panchayat Hierarchy
#
# Matching strategy:
# 1. Filter out municipality/urban entries before matching
# 2. EXACT district + samiti matching via manual crosswalk (100% coverage)
# 3. GP name matching within block: exact first, then two-pass fuzzy
#    - Strict pass: JW distance < 0.20
#    - Loose pass: JW distance < 0.30 for remaining unmatched
# 4. Detect and handle ties, one-to-many, many-to-one issues
# 5. Link matched GPs to SHRUG via LGD code

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(fuzzyjoin)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== RAJ SHRUG-LGD Block Matching ===")

# ============================================================================
# STEP 1: Load crosswalks and data
# ============================================================================

block_xwalk <- read_csv(here("data/crosswalks/active/raj_samiti_xwalk.csv"), show_col_types = FALSE)

lgd_gp <- read_csv(here("data/lgd/processed/lgd_raj_block_gp.csv"), show_col_types = FALSE) %>%
    mutate(gp_name_std = normalize_string(gp_name))

elex <- read_parquet(here("data/raj/raj_05_10.parquet")) %>%
    rename(
        dist_name_new_2010 = district_std_2010,
        samiti_name_new_2010 = samiti_std_2010,
        gp_new_2010 = gp_std_2010,
        sl_no_2010 = match_key
    )

message("Election GPs (raw): ", nrow(elex))
message("LGD GPs: ", n_distinct(lgd_gp$gp_code))
message("Block crosswalk entries: ", nrow(block_xwalk))

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
    "\\bWARD\\s*[0-9]+\\b",
    "NAGAR PARISHAD"
)

elex <- elex %>%
    mutate(
        gp_upper = toupper(gp_new_2010),
        is_urban = grepl(paste(urban_patterns, collapse = "|"), gp_upper, ignore.case = TRUE)
    )

urban_excluded <- elex %>%
    filter(is_urban) %>%
    select(
        dist_name_new_2010,
        samiti_name_new_2010,
        gp_new_2010,
        sl_no_2010
    ) %>%
    distinct()

message("Urban/municipality entries excluded: ", nrow(urban_excluded))
write_csv(urban_excluded, here("data/crosswalks/audit/03a_raj_urban_excluded.csv"))
message("Exported: data/crosswalks/audit/03a_raj_urban_excluded.csv")

elex <- elex %>%
    filter(!is_urban) %>%
    select(-gp_upper, -is_urban)

message("Election GPs after filtering: ", nrow(elex))

# ============================================================================
# STEP 3: Join election data to block crosswalk (EXACT matching only)
# ============================================================================

elex_samitis <- elex %>%
    select(dist_name_new_2010, samiti_name_new_2010) %>%
    distinct() %>%
    filter(!is.na(samiti_name_new_2010)) %>%
    mutate(
        dist_lower = tolower(dist_name_new_2010),
        samiti_lower = tolower(samiti_name_new_2010)
    )

missing_samitis <- elex_samitis %>%
    anti_join(
        block_xwalk %>% mutate(
            elex_district_lower = tolower(elex_district),
            elex_samiti_lower = tolower(elex_samiti)
        ),
        by = c("dist_lower" = "elex_district_lower",
               "samiti_lower" = "elex_samiti_lower")
    ) %>%
    select(dist_name_new_2010, samiti_name_new_2010)

if (nrow(missing_samitis) > 0) {
    message("WARNING: Missing samitis in crosswalk:")
    print(missing_samitis)
}

message("Block crosswalk coverage check complete")

elex_with_block <- elex %>%
    mutate(
        dist_name_lower = tolower(dist_name_new_2010),
        samiti_name_lower = tolower(samiti_name_new_2010)
    ) %>%
    left_join(
        block_xwalk %>%
            mutate(
                elex_district_lower = tolower(elex_district),
                elex_samiti_lower = tolower(elex_samiti)
            ) %>%
            select(elex_district_lower, elex_samiti_lower, lgd_block_code, lgd_block_name),
        by = c("dist_name_lower" = "elex_district_lower",
               "samiti_name_lower" = "elex_samiti_lower")
    ) %>%
    select(-dist_name_lower, -samiti_name_lower)

message("Election rows with block code: ", sum(!is.na(elex_with_block$lgd_block_code)),
    "of", nrow(elex_with_block), "\n")

# ============================================================================
# STEP 4: Match GP names within block (two-pass fuzzy matching)
# ============================================================================

elex_gps <- elex_with_block %>%
    mutate(
        elex_gp_std = normalize_string(gp_new_2010),
        elex_key = paste(lgd_block_code, elex_gp_std)
    ) %>%
    filter(!is.na(lgd_block_code), !is.na(gp_new_2010))

lgd_gps <- lgd_gp %>%
    mutate(
        lgd_key = paste(block_code, gp_name_std)
    )

message("\nElection GPs to match: ", nrow(elex_gps))

# Exact match first
exact_matches <- elex_gps %>%
    inner_join(
        lgd_gps %>% select(gp_code, gp_name, block_code, block_name, zila_name, lgd_key),
        by = c("elex_key" = "lgd_key")
    )

message("Exact GP matches: ", nrow(exact_matches))

# Unmatched GPs for fuzzy matching
unmatched <- elex_gps %>%
    anti_join(exact_matches, by = c("sl_no_2010", "gp_new_2010"))

message("Unmatched GPs for fuzzy: ", nrow(unmatched))

# Two-pass fuzzy matching
run_fuzzy_pass <- function(unmatched_data, lgd_data, threshold, pass_name) {
    fuzzy_results <- list()
    unique_blocks <- unique(unmatched_data$lgd_block_code)
    message("Processing", length(unique_blocks), "blocks for", pass_name, "fuzzy matching (threshold =", threshold, ")...")

    for (blk in unique_blocks) {
        block_elex <- unmatched_data %>% filter(lgd_block_code == blk)
        block_lgd <- lgd_data %>% filter(block_code == blk)

        for (i in 1:nrow(block_elex)) {
            result <- fuzzy_match_within_block(
                block_elex[i, ], block_lgd,
                threshold = threshold,
                id_col = "sl_no_2010",
                gp_col = "gp_new_2010"
            )
            if (!is.null(result)) {
                result$match_quality <- pass_name
                fuzzy_results[[length(fuzzy_results) + 1]] <- result
            }
        }
    }
    bind_rows(fuzzy_results)
}

# Pass 1: Strict matching (threshold = 0.20)
fuzzy_strict <- run_fuzzy_pass(unmatched, lgd_gps, threshold = 0.20, pass_name = "strict")
message("Strict fuzzy matches: ", nrow(fuzzy_strict))

# Pass 2: Loose matching for remaining unmatched (threshold = 0.30)
still_unmatched <- unmatched %>%
    anti_join(fuzzy_strict, by = "sl_no_2010")

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
        elex_gps %>% select(sl_no_2010, dist_name_new_2010, samiti_name_new_2010) %>% distinct(),
        by = "sl_no_2010"
    ) %>%
    select(
        district = dist_name_new_2010,
        samiti = samiti_name_new_2010,
        elex_gp = gp_new_2010,
        lgd_gp = lgd_gp_name,
        lgd_gp_code,
        match_distance,
        tie_count,
        match_quality
    )

message("\nTie-resolved matches: ", nrow(tie_resolved))
if (nrow(tie_resolved) > 0) {
    write_csv(tie_resolved, here("data/crosswalks/audit/03a_raj_tie_resolved.csv"))
    message("Exported: data/crosswalks/audit/03a_raj_tie_resolved.csv")
}

numeric_mismatch <- fuzzy_df %>%
    filter(match_confidence == "numeric_mismatch") %>%
    left_join(
        elex_gps %>% select(sl_no_2010, dist_name_new_2010, samiti_name_new_2010) %>% distinct(),
        by = "sl_no_2010"
    ) %>%
    select(
        district = dist_name_new_2010,
        samiti = samiti_name_new_2010,
        elex_gp = gp_new_2010,
        lgd_gp = lgd_gp_name,
        lgd_gp_code,
        match_distance,
        match_quality
    )

message("Numeric mismatch matches: ", nrow(numeric_mismatch))
if (nrow(numeric_mismatch) > 0) {
    write_csv(numeric_mismatch, here("data/crosswalks/audit/03a_raj_numeric_mismatch.csv"))
    message("Exported: data/crosswalks/audit/03a_raj_numeric_mismatch.csv")
}

# ============================================================================
# STEP 6: Combine matches
# ============================================================================

all_matches <- bind_rows(
    exact_matches %>%
        transmute(
            sl_no_2010 = sl_no_2010,
            gp_new_2010 = gp_new_2010,
            lgd_gp_code = gp_code,
            lgd_gp_name = gp_name,
            lgd_block_code = lgd_block_code,
            lgd_block_name = block_name,
            lgd_district = zila_name,
            match_type = "exact",
            match_quality = "exact",
            match_distance = 0,
            match_confidence = "unique"
        ),
    fuzzy_df %>%
        filter(!match_confidence %in% c("tie_resolved", "numeric_mismatch")) %>%
        left_join(
            elex_gps %>% select(sl_no_2010, lgd_block_code, lgd_block_name),
            by = "sl_no_2010"
        ) %>%
        left_join(
            lgd_gps %>% select(gp_code, zila_name) %>% distinct(),
            by = c("lgd_gp_code" = "gp_code")
        ) %>%
        transmute(
            sl_no_2010 = sl_no_2010,
            gp_new_2010 = gp_new_2010,
            lgd_gp_code = lgd_gp_code,
            lgd_gp_name = lgd_gp_name,
            lgd_block_code = lgd_block_code,
            lgd_block_name = lgd_block_name,
            lgd_district = zila_name,
            match_type = "fuzzy",
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
    group_by(sl_no_2010, gp_new_2010) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    left_join(
        elex_gps %>% select(sl_no_2010, dist_name_new_2010, samiti_name_new_2010) %>% distinct(),
        by = "sl_no_2010"
    )

message("One-to-many matches (election GP -> multiple LGD GPs): ", n_distinct(one_to_many$sl_no_2010))

if (nrow(one_to_many) > 0) {
    one_to_many_export <- one_to_many %>%
        select(
            district = dist_name_new_2010,
            samiti = samiti_name_new_2010,
            elex_gp = gp_new_2010,
            lgd_gp = lgd_gp_name,
            lgd_gp_code,
            match_distance,
            match_quality
        ) %>%
        arrange(district, samiti, elex_gp, match_distance)

    write_csv(one_to_many_export, here("data/crosswalks/audit/03a_raj_one_to_many_errors.csv"))
    message("Exported: data/crosswalks/audit/03a_raj_one_to_many_errors.csv")

    all_matches <- all_matches %>%
        group_by(sl_no_2010, gp_new_2010) %>%
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
        elex_gps %>% select(sl_no_2010, dist_name_new_2010, samiti_name_new_2010) %>% distinct(),
        by = "sl_no_2010"
    )

message("Many-to-one matches (multiple election GPs -> same LGD GP): ", n_distinct(many_to_one$lgd_gp_code))

if (nrow(many_to_one) > 0) {
    many_to_one_export <- many_to_one %>%
        select(
            district = dist_name_new_2010,
            samiti = samiti_name_new_2010,
            elex_gp = gp_new_2010,
            lgd_gp = lgd_gp_name,
            lgd_gp_code,
            match_distance,
            match_quality,
            sl_no_2010
        ) %>%
        arrange(lgd_gp_code, match_distance)

    write_csv(many_to_one_export, here("data/crosswalks/audit/03a_raj_many_to_one_audit.csv"))
    message("Exported: data/crosswalks/audit/03a_raj_many_to_one_audit.csv")
    message("NOTE: Many-to-one may be valid (same GP in multiple election rounds). Review manually.")
}

# ============================================================================
# STEP 8: Join back to election data and link to SHRUG
# ============================================================================

elex_matched <- elex %>%
    left_join(all_matches, by = c("sl_no_2010", "gp_new_2010"))

message("\n=== LGD GP MATCH SUMMARY ===")
message("Total election GPs: ", nrow(elex))
message("Matched to LGD GP: ", sum(!is.na(elex_matched$lgd_gp_code)))
message("Match rate: ", round(100 * sum(!is.na(elex_matched$lgd_gp_code)) / nrow(elex), 1), "%")

message("\nBy match type:")
print(table(elex_matched$match_type, useNA = "ifany"))

message("\nBy match quality:")
print(table(elex_matched$match_quality, useNA = "ifany"))

# ============================================================================
# STEP 9: Link to SHRUG via LGD code
# ============================================================================

# Aggregate SHRUG covariates across villages within each LGD GP
shrug_lgd <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"), show_col_types = FALSE) %>%
    filter(state_name == "rajasthan") %>%
    group_by(LGD_code) %>%
    summarize(
        shrid2 = first(shrid2),
        local_body_name = first(local_body_name),
        n_villages = n(),
        across(starts_with("pc01"), ~mean(.x, na.rm = TRUE)),
        .groups = "drop"
    )

message("\nSHRUG GPs for Rajasthan: ", n_distinct(shrug_lgd$LGD_code))

elex_shrug <- elex_matched %>%
    left_join(
        shrug_lgd %>%
            filter(!is.na(LGD_code)) %>%
            select(shrid2, LGD_code),
        by = c("lgd_gp_code" = "LGD_code")
    )

message("\n=== SHRUG MATCH SUMMARY ===")
message("Matched to SHRUG: ", sum(!is.na(elex_shrug$shrid2)))
message("SHRUG match rate: ", round(100 * sum(!is.na(elex_shrug$shrid2)) / nrow(elex), 1), "%")

# ============================================================================
# STEP 10: Export unmatched GPs for audit
# ============================================================================

message("\n=== EXPORTING UNMATCHED GPs ===")

lgd_but_no_shrug <- elex_shrug %>%
    filter(!is.na(lgd_gp_code), is.na(shrid2)) %>%
    select(
        dist_name_new_2010,
        samiti_name_new_2010,
        gp_new_2010,
        lgd_block_code,
        lgd_block_name,
        lgd_gp_code,
        lgd_gp_name,
        match_type
    ) %>%
    distinct()

message("GPs matched to LGD but not SHRUG: ", nrow(lgd_but_no_shrug))

unmatched_final <- elex_shrug %>%
    filter(is.na(lgd_gp_code)) %>%
    select(dist_name_new_2010, samiti_name_new_2010, gp_new_2010) %>%
    distinct()

message("GPs not matched to LGD: ", nrow(unmatched_final))

if (nrow(lgd_but_no_shrug) > 0) {
    write_csv(lgd_but_no_shrug, here("data/crosswalks/audit/03a_raj_gp_lgd_no_shrug.csv"))
}

if (nrow(unmatched_final) > 0) {
    write_csv(unmatched_final, here("data/crosswalks/audit/03a_raj_gp_unmatched.csv"))
}

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
    filter(state_name == "rajasthan")

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

raj_mapping <- elex_shrug %>%
    filter(!is.na(lgd_gp_code)) %>%
    distinct(match_key = sl_no_2010, .keep_all = TRUE) %>%
    select(
        match_key,
        lgd_gp_code,
        lgd_gp_name,
        lgd_block_code,
        lgd_block_name,
        lgd_district,
        match_type,
        match_quality,
        match_distance,
        match_confidence
    )

message("Unique GPs with LGD match: ", nrow(raj_mapping))

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
        left_join(raj_mapping, by = "match_key")

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

raj_05_10_final <- process_panel(
    here("data/raj/raj_05_10.parquet"),
    here("data/raj/shrug_gp_raj_05_10_block.parquet")
)

raj_10_15_final <- process_panel(
    here("data/raj/raj_10_15.parquet"),
    here("data/raj/shrug_gp_raj_10_15_block.parquet")
)

raj_15_20_final <- process_panel(
    here("data/raj/raj_15_20.parquet"),
    here("data/raj/shrug_gp_raj_15_20_block.parquet")
)

raj_05_20_final <- process_panel(
    here("data/raj/raj_05_20.parquet"),
    here("data/raj/shrug_gp_raj_05_20_block.parquet")
)

# ============================================================================
# STEP 14: Report by district (using 05-10 panel)
# ============================================================================

message("\n=== MATCH RATE BY DISTRICT (05-10 panel) ===")
by_dist <- raj_05_10_final %>%
    group_by(district_std_2010) %>%
    summarize(
        total = n(),
        lgd_matched = sum(!is.na(lgd_gp_code)),
        lgd_rate = round(100 * lgd_matched / total, 1),
        shrug_matched = sum(!is.na(shrid2)),
        shrug_rate = round(100 * shrug_matched / total, 1),
        .groups = "drop"
    ) %>%
    arrange(lgd_rate)
print(by_dist)

# ============================================================================
# STEP 15: Verification summary
# ============================================================================

message("\n=== VERIFICATION SUMMARY ===")
message("Urban entries excluded: ", nrow(urban_excluded))

message("\n05-10 Panel:")
message("  Total GPs: ", nrow(raj_05_10_final))
message("  Matched to LGD: ", sum(!is.na(raj_05_10_final$lgd_gp_code)))
message("  Match rate: ", round(100 * sum(!is.na(raj_05_10_final$lgd_gp_code)) / nrow(raj_05_10_final), 1), "%")
message("  Matched to SHRUG: ", sum(!is.na(raj_05_10_final$shrid2)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(raj_05_10_final$shrid2)) / nrow(raj_05_10_final), 1), "%")

message("\n10-15 Panel:")
message("  Total GPs: ", nrow(raj_10_15_final))
message("  Matched to LGD: ", sum(!is.na(raj_10_15_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(raj_10_15_final$shrid2)) / nrow(raj_10_15_final), 1), "%")

message("\n15-20 Panel:")
message("  Total GPs: ", nrow(raj_15_20_final))
message("  Matched to LGD: ", sum(!is.na(raj_15_20_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(raj_15_20_final$shrid2)) / nrow(raj_15_20_final), 1), "%")

message("\n05-20 Panel:")
message("  Total GPs: ", nrow(raj_05_20_final))
message("  Matched to LGD: ", sum(!is.na(raj_05_20_final$lgd_gp_code)))
message("  SHRUG match rate: ", round(100 * sum(!is.na(raj_05_20_final$shrid2)) / nrow(raj_05_20_final), 1), "%")

message("\nSHRUG covariates included: ", sum(grepl("^pc01_", names(raj_05_10_final))), " variables")

message("\n=== Done ===")
