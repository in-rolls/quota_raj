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

cat("=== RAJ SHRUG-LGD Block Matching ===\n")

# ============================================================================
# STEP 1: Load crosswalks and data
# ============================================================================

block_xwalk <- read_csv("data/crosswalks/raj_samiti_xwalk.csv", show_col_types = FALSE)

lgd_gp <- read_csv("data/crosswalks/lgd_raj_block_gp.csv", show_col_types = FALSE) %>%
    mutate(gp_name_std = normalize_string(gp_name))

elex <- read_parquet("data/raj/elex_raj_05_10.parquet")

cat("Election GPs (raw):", nrow(elex), "\n")
cat("LGD GPs:", n_distinct(lgd_gp$gp_code), "\n")
cat("Block crosswalk entries:", nrow(block_xwalk), "\n")

# ============================================================================
# STEP 2: Filter out municipality/urban entries
# ============================================================================

cat("\n=== FILTERING MUNICIPALITY/URBAN ENTRIES ===\n")

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

cat("Urban/municipality entries excluded:", nrow(urban_excluded), "\n")
write_csv(urban_excluded, "data/crosswalks/raj_urban_excluded.csv")
cat("Exported: data/crosswalks/raj_urban_excluded.csv\n")

elex <- elex %>%
    filter(!is_urban) %>%
    select(-gp_upper, -is_urban)

cat("Election GPs after filtering:", nrow(elex), "\n")

# ============================================================================
# STEP 3: Join election data to block crosswalk (EXACT matching only)
# ============================================================================

elex_samitis <- elex %>%
    select(dist_name_new_2010, samiti_name_new_2010) %>%
    distinct() %>%
    filter(!is.na(samiti_name_new_2010))

missing_samitis <- elex_samitis %>%
    anti_join(block_xwalk, by = c("dist_name_new_2010" = "elex_district",
                                   "samiti_name_new_2010" = "elex_samiti"))

if (nrow(missing_samitis) > 0) {
    cat("WARNING: Missing samitis in crosswalk:\n")
    print(missing_samitis)
}

cat("Block crosswalk coverage check complete\n")

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

cat("Election rows with block code:", sum(!is.na(elex_with_block$lgd_block_code)),
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

cat("\nElection GPs to match:", nrow(elex_gps), "\n")

# Exact match first
exact_matches <- elex_gps %>%
    inner_join(
        lgd_gps %>% select(gp_code, gp_name, block_code, block_name, zila_name, lgd_key),
        by = c("elex_key" = "lgd_key")
    )

cat("Exact GP matches:", nrow(exact_matches), "\n")

# Unmatched GPs for fuzzy matching
unmatched <- elex_gps %>%
    anti_join(exact_matches, by = c("sl_no_2010", "gp_new_2010"))

cat("Unmatched GPs for fuzzy:", nrow(unmatched), "\n")

# Two-pass fuzzy matching
run_fuzzy_pass <- function(unmatched_data, lgd_data, threshold, pass_name) {
    fuzzy_results <- list()
    unique_blocks <- unique(unmatched_data$lgd_block_code)
    cat("Processing", length(unique_blocks), "blocks for", pass_name, "fuzzy matching (threshold =", threshold, ")...\n")

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
cat("Strict fuzzy matches:", nrow(fuzzy_strict), "\n")

# Pass 2: Loose matching for remaining unmatched (threshold = 0.30)
still_unmatched <- unmatched %>%
    anti_join(fuzzy_strict, by = "sl_no_2010")

fuzzy_loose <- run_fuzzy_pass(still_unmatched, lgd_gps, threshold = 0.30, pass_name = "loose")
cat("Loose fuzzy matches:", nrow(fuzzy_loose), "\n")

fuzzy_df <- bind_rows(fuzzy_strict, fuzzy_loose)
cat("Total fuzzy GP matches:", nrow(fuzzy_df), "\n")

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

cat("\nTie-resolved matches:", nrow(tie_resolved), "\n")
if (nrow(tie_resolved) > 0) {
    write_csv(tie_resolved, "data/crosswalks/raj_tie_resolved.csv")
    cat("Exported: data/crosswalks/raj_tie_resolved.csv\n")
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

cat("\n=== DEDUPLICATION CHECKS ===\n")

# One-to-many: election GP matches multiple LGD GPs
one_to_many <- all_matches %>%
    group_by(sl_no_2010, gp_new_2010) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    left_join(
        elex_gps %>% select(sl_no_2010, dist_name_new_2010, samiti_name_new_2010) %>% distinct(),
        by = "sl_no_2010"
    )

cat("One-to-many matches (election GP -> multiple LGD GPs):", n_distinct(one_to_many$sl_no_2010), "\n")

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

    write_csv(one_to_many_export, "data/crosswalks/raj_one_to_many_errors.csv")
    cat("Exported: data/crosswalks/raj_one_to_many_errors.csv\n")

    all_matches <- all_matches %>%
        group_by(sl_no_2010, gp_new_2010) %>%
        slice_min(match_distance, n = 1, with_ties = FALSE) %>%
        ungroup()

    cat("Resolved one-to-many by keeping best match. New total:", nrow(all_matches), "\n")
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

cat("Many-to-one matches (multiple election GPs -> same LGD GP):", n_distinct(many_to_one$lgd_gp_code), "\n")

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

    write_csv(many_to_one_export, "data/crosswalks/raj_many_to_one_audit.csv")
    cat("Exported: data/crosswalks/raj_many_to_one_audit.csv\n")
    cat("NOTE: Many-to-one may be valid (same GP in multiple election rounds). Review manually.\n")
}

# ============================================================================
# STEP 8: Join back to election data and link to SHRUG
# ============================================================================

elex_matched <- elex %>%
    left_join(all_matches, by = c("sl_no_2010", "gp_new_2010"))

cat("\n=== LGD GP MATCH SUMMARY ===\n")
cat("Total election GPs:", nrow(elex), "\n")
cat("Matched to LGD GP:", sum(!is.na(elex_matched$lgd_gp_code)), "\n")
cat("Match rate:", round(100 * sum(!is.na(elex_matched$lgd_gp_code)) / nrow(elex), 1), "%\n")

cat("\nBy match type:\n")
print(table(elex_matched$match_type, useNA = "ifany"))

cat("\nBy match quality:\n")
print(table(elex_matched$match_quality, useNA = "ifany"))

# ============================================================================
# STEP 9: Link to SHRUG via LGD code
# ============================================================================

# Aggregate SHRUG covariates across villages within each LGD GP
shrug_lgd <- read_csv("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv", show_col_types = FALSE) %>%
    filter(state_name == "rajasthan") %>%
    group_by(LGD_code) %>%
    summarize(
        shrid2 = first(shrid2),
        local_body_name = first(local_body_name),
        n_villages = n(),
        across(starts_with("pc01"), ~mean(.x, na.rm = TRUE)),
        .groups = "drop"
    )

cat("\nSHRUG GPs for Rajasthan:", n_distinct(shrug_lgd$LGD_code), "\n")

elex_shrug <- elex_matched %>%
    left_join(
        shrug_lgd %>%
            filter(!is.na(LGD_code)) %>%
            select(shrid2, LGD_code),
        by = c("lgd_gp_code" = "LGD_code")
    )

cat("\n=== SHRUG MATCH SUMMARY ===\n")
cat("Matched to SHRUG:", sum(!is.na(elex_shrug$shrid2)), "\n")
cat("SHRUG match rate:", round(100 * sum(!is.na(elex_shrug$shrid2)) / nrow(elex), 1), "%\n")

# ============================================================================
# STEP 10: Export unmatched GPs for audit
# ============================================================================

cat("\n=== EXPORTING UNMATCHED GPs ===\n")

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

cat("GPs matched to LGD but not SHRUG:", nrow(lgd_but_no_shrug), "\n")

unmatched_final <- elex_shrug %>%
    filter(is.na(lgd_gp_code)) %>%
    select(dist_name_new_2010, samiti_name_new_2010, gp_new_2010) %>%
    distinct()

cat("GPs not matched to LGD:", nrow(unmatched_final), "\n")

if (nrow(lgd_but_no_shrug) > 0) {
    write_csv(lgd_but_no_shrug, "data/crosswalks/raj_gp_lgd_no_shrug.csv")
}

if (nrow(unmatched_final) > 0) {
    write_csv(unmatched_final, "data/crosswalks/raj_gp_unmatched.csv")
}

cat("Exported unmatched GPs for review.\n")

# ============================================================================
# STEP 11: Save output
# ============================================================================

write_parquet(elex_shrug, "data/raj/shrug_lgd_raj_elex_05_10_block.parquet")
cat("\nSaved: data/raj/shrug_lgd_raj_elex_05_10_block.parquet\n")

# ============================================================================
# STEP 12: Report by district
# ============================================================================

cat("\n=== MATCH RATE BY DISTRICT ===\n")
by_dist <- elex_shrug %>%
    group_by(dist_name_new_2010) %>%
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
# STEP 13: Verification summary
# ============================================================================

cat("\n=== VERIFICATION SUMMARY ===\n")
cat("Urban entries excluded:", nrow(urban_excluded), "\n")
cat("Total GPs:", nrow(elex_shrug), "\n")
cat("Matched to LGD:", sum(!is.na(elex_shrug$lgd_gp_code)), "\n")
cat("Match rate:", round(100 * sum(!is.na(elex_shrug$lgd_gp_code)) / nrow(elex_shrug), 1), "%\n")
cat("Matched to SHRUG:", sum(!is.na(elex_shrug$shrid2)), "\n")
cat("SHRUG match rate:", round(100 * sum(!is.na(elex_shrug$shrid2)) / nrow(elex_shrug), 1), "%\n")

cat("\n=== Done ===\n")
