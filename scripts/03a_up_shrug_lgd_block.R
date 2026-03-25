# Match UP Election Data to SHRUG via LGD Block Panchayat Hierarchy
#
# Matching strategy:
# 1. Match election blocks to LGD blocks via comprehensive crosswalk
# 2. GP name matching within block: exact first, then fuzzy (JW distance < 0.3)
# 3. Link matched GPs to SHRUG via LGD code
#
# Uses new comprehensive crosswalks from:
# - scripts/01c_up_create_district_crosswalk.R
# - scripts/01e_up_create_block_crosswalk.R

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== UP SHRUG-LGD Block Matching ===\n")

# ============================================================================
# STEP 1: Load crosswalks and data
# ============================================================================

block_xwalk <- read_csv(here("data/crosswalks/up_block_xwalk.csv"), show_col_types = FALSE)
cat("Block crosswalk entries:", nrow(block_xwalk), "\n")

lgd_gp <- read_csv(here("data/crosswalks/lgd_up_block_gp.csv"), show_col_types = FALSE) %>%
    mutate(gp_name_std = normalize_string(gp_name))
cat("LGD GPs:", nrow(lgd_gp), "\n")

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
cat("Election GPs:", nrow(up_05_10), "\n")

# ============================================================================
# STEP 2: Join election data to block crosswalk
# ============================================================================

up_with_block <- up_05_10 %>%
    left_join(
        block_xwalk %>% select(elex_district, elex_block, lgd_block_code, lgd_block_name, match_type),
        by = c("district_name_eng_2010" = "elex_district",
               "block_name_eng_2010" = "elex_block")
    ) %>%
    rename(block_match_type = match_type)

cat("Election rows with block code:", sum(!is.na(up_with_block$lgd_block_code)),
    "of", nrow(up_with_block), "\n")

# Report unmatched blocks
unmatched_blocks <- up_with_block %>%
    filter(is.na(lgd_block_code)) %>%
    select(district_name_eng_2010, block_name_eng_2010) %>%
    distinct()

if (nrow(unmatched_blocks) > 0) {
    cat("\nWARNING: Unmatched blocks (may be urban wards):\n")
    print(unmatched_blocks)
}

# ============================================================================
# STEP 3: Match GP names within block
# ============================================================================

up_gps <- up_with_block %>%
    mutate(
        elex_gp_std = normalize_string(gp_name_eng_2010),
        elex_key = paste(lgd_block_code, elex_gp_std)
    ) %>%
    filter(!is.na(lgd_block_code), !is.na(gp_name_eng_2010))

lgd_gps <- lgd_gp %>%
    mutate(lgd_key = paste(block_code, gp_name_std))

cat("Election GPs to match:", nrow(up_gps), "\n")

# Exact match first
exact_matches <- up_gps %>%
    inner_join(
        lgd_gps %>% select(gp_code, gp_name, block_code, lgd_key),
        by = c("elex_key" = "lgd_key")
    )

cat("Exact GP matches:", nrow(exact_matches), "\n")

# Unmatched GPs for fuzzy matching
unmatched <- up_gps %>%
    anti_join(exact_matches, by = c("key_2010", "gp_name_eng_2010"))

cat("Unmatched GPs for fuzzy:", nrow(unmatched), "\n")

# Run fuzzy matching (using shared fuzzy_match_within_block from 00_utils.R)
fuzzy_results <- list()
unique_blocks <- unique(unmatched$lgd_block_code)
cat("Processing", length(unique_blocks), "blocks for fuzzy matching...\n")

pb <- txtProgressBar(min = 0, max = length(unique_blocks), style = 3)
for (i in seq_along(unique_blocks)) {
    blk <- unique_blocks[i]
    block_elex <- unmatched %>% filter(lgd_block_code == blk)
    block_lgd <- lgd_gps %>% filter(block_code == blk)

    for (j in 1:nrow(block_elex)) {
        result <- fuzzy_match_within_block(
            block_elex[j, ], block_lgd,
            threshold = 0.30,
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
cat("Fuzzy GP matches:", nrow(fuzzy_df), "\n")

# ============================================================================
# STEP 4: Combine matches
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
            gp_match_type = "exact"
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
            gp_match_type = "fuzzy"
        )
)

# ============================================================================
# STEP 5: Join back to election data and link to SHRUG
# ============================================================================

up_matched <- up_05_10 %>%
    left_join(
        up_with_block %>%
            select(key_2010, lgd_block_code, lgd_block_name, block_match_type) %>%
            distinct(),
        by = "key_2010"
    ) %>%
    left_join(
        all_matches %>% select(key_2010, lgd_gp_code, lgd_gp_name, gp_match_type),
        by = "key_2010"
    )

cat("\n=== LGD GP MATCH SUMMARY ===\n")
cat("Total election GPs:", nrow(up_05_10), "\n")
cat("Matched to LGD GP:", sum(!is.na(up_matched$lgd_gp_code)), "\n")
cat("LGD match rate:", round(100 * sum(!is.na(up_matched$lgd_gp_code)) / nrow(up_05_10), 1), "%\n")

cat("\nBy GP match type:\n")
print(table(up_matched$gp_match_type, useNA = "ifany"))

cat("\nBy block match type:\n")
print(table(up_matched$block_match_type, useNA = "ifany"))

# ============================================================================
# STEP 6: Link to SHRUG via LGD code
# ============================================================================

shrug_lgd <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
                      show_col_types = FALSE) %>%
    filter(tolower(state_name) == "uttar pradesh") %>%
    select(shrid2, LGD_code, local_body_name) %>%
    filter(!is.na(LGD_code)) %>%
    group_by(LGD_code) %>%
    slice(1) %>%
    ungroup()

cat("\nSHRUG GPs for UP:", n_distinct(shrug_lgd$LGD_code), "\n")

up_shrug <- up_matched %>%
    left_join(
        shrug_lgd %>% select(shrid2, LGD_code),
        by = c("lgd_gp_code" = "LGD_code")
    )

cat("\n=== SHRUG MATCH SUMMARY ===\n")
cat("Matched to SHRUG:", sum(!is.na(up_shrug$shrid2)), "\n")
cat("SHRUG match rate:", round(100 * sum(!is.na(up_shrug$shrid2)) / nrow(up_05_10), 1), "%\n")

# ============================================================================
# STEP 7: Export unmatched GPs for audit
# ============================================================================

cat("\n=== EXPORTING UNMATCHED GPs ===\n")

# GPs that matched to LGD but not to SHRUG
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

cat("GPs matched to LGD but not SHRUG:", nrow(lgd_but_no_shrug), "\n")

# GPs that didn't match to LGD at all
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

cat("GPs not matched to LGD:", nrow(no_lgd_match), "\n")

# Save unmatched for review
write_csv(lgd_but_no_shrug, here("data/crosswalks/up_gp_lgd_no_shrug.csv"))
write_csv(no_lgd_match, here("data/crosswalks/up_gp_unmatched.csv"))
cat("Exported unmatched GPs for review.\n")

# ============================================================================
# STEP 8: Save output
# ============================================================================

write_parquet(up_shrug, here("data/up/shrug_lgd_up_elex_05_10_block.parquet"))
cat("\nSaved: data/up/shrug_lgd_up_elex_05_10_block.parquet\n")

# ============================================================================
# STEP 9: Report by district
# ============================================================================

cat("\n=== MATCH RATE BY DISTRICT ===\n")

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

cat("\nLowest SHRUG match rates:\n")
print(head(by_dist, 15))

cat("\nHighest SHRUG match rates:\n")
print(tail(by_dist, 10))

# ============================================================================
# STEP 10: Summary verification
# ============================================================================

cat("\n=== VERIFICATION SUMMARY ===\n")
cat("Block crosswalk entries:", nrow(block_xwalk), "\n")
cat("Block match types:\n")
print(table(block_xwalk$match_type))
cat("\nElection GPs with block match:", sum(!is.na(up_with_block$lgd_block_code)),
    "/", nrow(up_05_10), "\n")
cat("Election GPs with LGD GP match:", sum(!is.na(up_matched$lgd_gp_code)),
    "/", nrow(up_05_10), "\n")
cat("Election GPs with SHRUG match:", sum(!is.na(up_shrug$shrid2)),
    "/", nrow(up_05_10), "\n")

cat("\nNote: SHRUG match rate is limited by SHRUG-LGD crosswalk coverage.\n")
cat("See tabs/up_shrug_coverage_audit.md for full analysis.\n")

cat("\n=== Done ===\n")
