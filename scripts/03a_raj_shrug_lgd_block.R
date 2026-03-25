# Match Election Data to SHRUG via LGD Block Panchayat Hierarchy
#
# This script matches election GPs to SHRUG data using the official
# LGD Block Panchayat -> GP hierarchy (not Census tehsil/subdistrict).
#
# Matching strategy:
# 1. EXACT district + samiti matching via manual crosswalk (100% coverage)
# 2. GP name matching within block: exact first, then fuzzy (JW distance < 0.3)
# 3. Link matched GPs to SHRUG via LGD code

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(fuzzyjoin)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# ============================================================================
# STEP 1: Load crosswalks and data
# ============================================================================

# Block crosswalk (election samiti -> LGD block)
# Using the new comprehensive crosswalk that covers all 4 election years
block_xwalk <- read_csv("data/crosswalks/raj_samiti_xwalk.csv")

# LGD GP hierarchy (GP -> Block -> District)
lgd_gp <- read_csv("data/crosswalks/lgd_raj_block_gp.csv") %>%
    mutate(gp_name_std = normalize_string(gp_name))

# Election data
elex <- read_parquet("data/raj/elex_raj_05_10.parquet")

cat("Election GPs:", nrow(elex), "\n")
cat("LGD GPs:", n_distinct(lgd_gp$gp_code), "\n")
cat("Block crosswalk entries:", nrow(block_xwalk), "\n")

# ============================================================================
# STEP 2: Join election data to block crosswalk (EXACT matching only)
# ============================================================================

# Verify crosswalk has 100% coverage
elex_samitis <- elex %>%
    select(dist_name_new_2010, samiti_name_new_2010) %>%
    distinct() %>%
    filter(!is.na(samiti_name_new_2010))

missing_samitis <- elex_samitis %>%
    anti_join(block_xwalk, by = c("dist_name_new_2010" = "elex_district",
                                   "samiti_name_new_2010" = "elex_samiti"))

if (nrow(missing_samitis) > 0) {
    cat("ERROR: Missing samitis in crosswalk:\n")
    print(missing_samitis)
    stop("Block crosswalk must have 100% coverage!")
}

cat("Block crosswalk verified: 100% samiti coverage\n")

elex_with_block <- elex %>%
    left_join(
        block_xwalk %>% select(elex_district, elex_samiti, lgd_block_code, lgd_block_name),
        by = c("dist_name_new_2010" = "elex_district",
               "samiti_name_new_2010" = "elex_samiti")
    )

cat("Election rows with block code:", sum(!is.na(elex_with_block$lgd_block_code)),
    "of", nrow(elex_with_block), "\n")

# ============================================================================
# STEP 3: Match GP names within district + block
# ============================================================================

# Prepare election GPs for matching
elex_gps <- elex_with_block %>%
    mutate(
        elex_gp_std = normalize_string(gp_new_2010),
        elex_key = paste(lgd_block_code, elex_gp_std)
    ) %>%
    filter(!is.na(lgd_block_code), !is.na(gp_new_2010))

# Prepare LGD GPs for matching
lgd_gps <- lgd_gp %>%
    mutate(
        lgd_key = paste(block_code, gp_name_std)
    )

cat("Election GPs to match:", nrow(elex_gps), "\n")

# Exact match first (by block + GP name)
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

# Run fuzzy matching (using shared fuzzy_match_within_block from 00_utils.R)
fuzzy_results <- list()
unique_blocks <- unique(unmatched$lgd_block_code)

for (blk in unique_blocks) {
    block_elex <- unmatched %>% filter(lgd_block_code == blk)
    block_lgd <- lgd_gps %>% filter(block_code == blk)

    for (i in 1:nrow(block_elex)) {
        result <- fuzzy_match_within_block(
            block_elex[i, ], block_lgd,
            threshold = 0.30,
            id_col = "sl_no_2010",
            gp_col = "gp_new_2010"
        )
        if (!is.null(result)) {
            fuzzy_results[[length(fuzzy_results) + 1]] <- result
        }
    }
}

fuzzy_df <- bind_rows(fuzzy_results)
cat("Fuzzy GP matches:", nrow(fuzzy_df), "\n")

# ============================================================================
# STEP 4: Combine matches
# ============================================================================

# Combine exact and fuzzy
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
            match_type = "exact"
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
            match_type = "fuzzy"
        )
)

# ============================================================================
# STEP 5: Join back to election data and link to SHRUG
# ============================================================================

elex_matched <- elex %>%
    left_join(all_matches, by = c("sl_no_2010", "gp_new_2010"))

cat("\nMatch summary:\n")
cat("Total election GPs:", nrow(elex), "\n")
cat("Matched to LGD GP:", sum(!is.na(elex_matched$lgd_gp_code)), "\n")
cat("Match rate:", round(100 * sum(!is.na(elex_matched$lgd_gp_code)) / nrow(elex), 1), "%\n")

# Match type breakdown
cat("\nBy match type:\n")
print(table(elex_matched$match_type, useNA = "ifany"))

# ============================================================================
# STEP 6: Link to SHRUG via LGD code
# ============================================================================

# Load SHRUG-LGD module data
# Note: SHRUG has multiple shrids per GP (multiple villages per GP)
# We keep the first shrid per LGD code for this match
shrug_lgd <- read_csv("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv") %>%
    filter(state_name == "rajasthan") %>%
    select(shrid2, LGD_code, local_body_name) %>%
    group_by(LGD_code) %>%
    slice(1) %>%  # Keep one shrid per GP
    ungroup()

cat("\nSHRUG GPs for Rajasthan:", n_distinct(shrug_lgd$LGD_code), "\n")

# Join election data to SHRUG (only where we have lgd_gp_code)
elex_shrug <- elex_matched %>%
    left_join(
        shrug_lgd %>%
            filter(!is.na(LGD_code)) %>%
            select(shrid2, LGD_code),
        by = c("lgd_gp_code" = "LGD_code")
    )

cat("Matched to SHRUG:", sum(!is.na(elex_shrug$shrid2)), "\n")
cat("SHRUG match rate:", round(100 * sum(!is.na(elex_shrug$shrid2)) / nrow(elex), 1), "%\n")

# ============================================================================
# STEP 7: Save output
# ============================================================================

write_parquet(elex_shrug, "data/raj/shrug_lgd_raj_elex_05_10_block.parquet")
cat("\nSaved: data/raj/shrug_lgd_raj_elex_05_10_block.parquet\n")

# Report unmatched for review
unmatched_final <- elex_shrug %>%
    filter(is.na(lgd_gp_code)) %>%
    select(dist_name_new_2010, samiti_name_new_2010, gp_new_2010) %>%
    distinct()

if (nrow(unmatched_final) > 0) {
    cat("\nUnmatched GPs (sample):\n")
    print(head(unmatched_final, 20))
    write_csv(unmatched_final, "data/crosswalks/raj_gp_unmatched.csv")
}

# ============================================================================
# STEP 8: Verification - compare with previous approach
# ============================================================================

cat("\n=== VERIFICATION ===\n")
cat("New approach (Block Panchayat hierarchy):\n")
cat("  Total GPs:", nrow(elex_shrug), "\n")
cat("  Matched to LGD:", sum(!is.na(elex_shrug$lgd_gp_code)), "\n")
cat("  Match rate:", round(100 * sum(!is.na(elex_shrug$lgd_gp_code)) / nrow(elex_shrug), 1), "%\n")
cat("  Matched to SHRUG:", sum(!is.na(elex_shrug$shrid2)), "\n")
cat("  SHRUG match rate:", round(100 * sum(!is.na(elex_shrug$shrid2)) / nrow(elex_shrug), 1), "%\n")

# By district
cat("\nMatch rate by district:\n")
by_dist <- elex_shrug %>%
    group_by(dist_name_new_2010) %>%
    summarize(
        total = n(),
        matched = sum(!is.na(lgd_gp_code)),
        rate = round(100 * matched / total, 1)
    ) %>%
    arrange(rate)
print(by_dist)
