# 03c_shrug_all_panels_block.R
# Create SHRUG-linked panels using LGD Block Panchayat matching (88% match rate)
#
# Strategy:
#   - Use new block-matched data as base: data/raj/shrug_lgd_raj_elex_05_10_block.parquet
#   - Extract GP→SHRUG mapping using match_key (district_std_YEAR_samiti_std_YEAR_gp_std_YEAR)
#   - Propagate shrid2 to later panels (10-15, 15-20) via match_key
#
# Input:
#   - data/raj/shrug_lgd_raj_elex_05_10_block.parquet (new block matching)
#   - data/raj/raj_10_15.parquet
#   - data/raj/raj_15_20.parquet
#   - data/raj/raj_05_20.parquet (4-way panel)
#
# Output:
#   - data/raj/shrug_gp_raj_05_10_block.parquet
#   - data/raj/shrug_gp_raj_10_15_block.parquet
#   - data/raj/shrug_gp_raj_15_20_block.parquet
#   - data/raj/shrug_gp_raj_05_20_block.parquet (long-term panel)

library(tidyverse)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Creating SHRUG-Linked Panels from Block Matching ===\n")

# =============================================================================
# Load base data, crosswalks, and SHRUG census data
# =============================================================================

cat("\nLoading SHRUG Census data...\n")
shrug_pca <- read_csv(here("data/shrug/shrug-pca01-csv/pc01_pca_clean_shrid.csv.zip"),
                       show_col_types = FALSE)
shrug_vd <- read_csv(here("data/shrug/shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"),
                      show_col_types = FALSE)

# Load crosswalks to standardize names
cat("Loading crosswalks...\n")
crosswalk_district <- read_csv(here("data/raj/crosswalk_district.csv"), show_col_types = FALSE)
crosswalk_samiti <- read_csv(here("data/raj/crosswalk_samiti.csv"), show_col_types = FALSE)

# Load block-matched 05-10 data
cat("Loading block-matched 05-10 data...\n")
raj_block_05_10 <- read_parquet(here("data/raj/shrug_lgd_raj_elex_05_10_block.parquet"))
cat("Block-matched 05-10 rows:", nrow(raj_block_05_10), "\n")

# =============================================================================
# Create SHRUG mapping from block-matched data
# =============================================================================

cat("\n--- Creating SHRUG Mapping ---\n")

# Apply crosswalks to standardize district/samiti names in block-matched data
# This ensures the mapping key matches how panels create their match_key
raj_block_05_10 <- raj_block_05_10 %>%
    left_join(crosswalk_district, by = c("dist_name_new_2010" = "district_raw")) %>%
    mutate(district_std_2010 = ifelse(is.na(district_std), dist_name_new_2010, district_std)) %>%
    select(-district_std) %>%
    left_join(
        crosswalk_samiti %>% select(district_std, samiti_raw, samiti_std),
        by = c("district_std_2010" = "district_std", "samiti_name_new_2010" = "samiti_raw")
    ) %>%
    mutate(
        samiti_std_2010 = ifelse(is.na(samiti_std), samiti_name_new_2010, samiti_std),
        gp_std_2010 = normalize_string(gp_new_2010)
    ) %>%
    select(-samiti_std)

# Create match_key using standardized columns to match panel keys
raj_shrug_mapping <- raj_block_05_10 %>%
    filter(!is.na(shrid2)) %>%
    mutate(
        match_key_2010 = paste(
            tolower(trimws(district_std_2010)),
            tolower(trimws(samiti_std_2010)),
            tolower(trimws(gp_std_2010)),
            sep = "_"
        )
    ) %>%
    select(
        match_key_2010,
        shrid2,
        lgd_gp_code, lgd_gp_name,
        lgd_block_code, lgd_block_name,
        lgd_district, match_type
    ) %>%
    distinct(match_key_2010, .keep_all = TRUE)

cat("SHRUG mapping rows (unique match_key_2010):", nrow(raj_shrug_mapping), "\n")

# =============================================================================
# PANEL 1: Rajasthan 05-10 SHRUG Panel
# =============================================================================

cat("\n--- Rajasthan 05-10 SHRUG Panel ---\n")

raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet"))

# Create match key from standardized names
raj_05_10 <- raj_05_10 %>%
    mutate(
        match_key_2010 = paste(
            tolower(trimws(district_std_2010)),
            tolower(trimws(samiti_std_2010)),
            tolower(trimws(gp_std_2010)),
            sep = "_"
        )
    )

# Join SHRUG data
raj_05_10_shrug <- raj_05_10 %>%
    left_join(raj_shrug_mapping, by = "match_key_2010")

# Join census data
raj_05_10_shrug <- raj_05_10_shrug %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(raj_05_10_shrug$shrid2))
cat("Raj 05-10: total =", nrow(raj_05_10), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(raj_05_10), 1), "%)\n")

write_parquet(raj_05_10_shrug, here("data/raj/shrug_gp_raj_05_10_block.parquet"))
cat("Saved: data/raj/shrug_gp_raj_05_10_block.parquet\n")

# =============================================================================
# PANEL 2: Rajasthan 10-15 SHRUG Panel
# =============================================================================

cat("\n--- Rajasthan 10-15 SHRUG Panel ---\n")

raj_10_15 <- read_parquet(here("data/raj/raj_10_15.parquet"))

# Create match key from 2010 standardized names (to link to SHRUG mapping)
raj_10_15 <- raj_10_15 %>%
    mutate(
        match_key_2010 = paste(
            tolower(trimws(district_std_2010)),
            tolower(trimws(samiti_std_2010)),
            tolower(trimws(gp_std_2010)),
            sep = "_"
        )
    )

raj_10_15_shrug <- raj_10_15 %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(raj_10_15_shrug$shrid2))
cat("Raj 10-15: total =", nrow(raj_10_15), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(raj_10_15), 1), "%)\n")

write_parquet(raj_10_15_shrug, here("data/raj/shrug_gp_raj_10_15_block.parquet"))
cat("Saved: data/raj/shrug_gp_raj_10_15_block.parquet\n")

# =============================================================================
# PANEL 3: Rajasthan 15-20 SHRUG Panel
# =============================================================================

cat("\n--- Rajasthan 15-20 SHRUG Panel ---\n")

raj_15_20 <- read_parquet(here("data/raj/raj_15_20.parquet"))

# For 15-20 panel, we need to chain through 10-15 to get 2010 match key
# First, create mapping from 2015 key to 2010 key via 10-15 panel
raj_10_15_key_map <- raj_10_15 %>%
    mutate(
        match_key_2015 = paste(
            tolower(trimws(district_std_2015)),
            tolower(trimws(samiti_std_2015)),
            tolower(trimws(gp_std_2015)),
            sep = "_"
        )
    ) %>%
    select(match_key_2015, match_key_2010) %>%
    distinct(match_key_2015, .keep_all = TRUE)

raj_15_20 <- raj_15_20 %>%
    mutate(
        match_key_2015 = paste(
            tolower(trimws(district_std_2015)),
            tolower(trimws(samiti_std_2015)),
            tolower(trimws(gp_std_2015)),
            sep = "_"
        )
    )

raj_15_20_shrug <- raj_15_20 %>%
    left_join(raj_10_15_key_map, by = "match_key_2015") %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(raj_15_20_shrug$shrid2))
cat("Raj 15-20: total =", nrow(raj_15_20), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(raj_15_20), 1), "%)\n")

write_parquet(raj_15_20_shrug, here("data/raj/shrug_gp_raj_15_20_block.parquet"))
cat("Saved: data/raj/shrug_gp_raj_15_20_block.parquet\n")

# =============================================================================
# PANEL 4: Rajasthan 05-20 Long-Term Panel
# =============================================================================

cat("\n--- Rajasthan 05-20 Long-Term Panel ---\n")

raj_05_20 <- read_parquet(here("data/raj/raj_05_20.parquet"))

# The 4-way panel uses various key structures; we need to create match_key_2010
# Check available columns
cat("Available columns in raj_05_20:\n")
cat(paste(names(raj_05_20), collapse = ", "), "\n")

# Create match key from 2010 names (should be present in 4-way panel)
if ("district_std_2010" %in% names(raj_05_20)) {
    raj_05_20 <- raj_05_20 %>%
        mutate(
            match_key_2010 = paste(
                tolower(trimws(district_std_2010)),
                tolower(trimws(samiti_std_2010)),
                tolower(trimws(gp_std_2010)),
                sep = "_"
            )
        )
} else if ("dist_name_new_2010" %in% names(raj_05_20)) {
    raj_05_20 <- raj_05_20 %>%
        mutate(
            match_key_2010 = paste(
                tolower(trimws(dist_name_new_2010)),
                tolower(trimws(samiti_name_new_2010)),
                tolower(trimws(gp_new_2010)),
                sep = "_"
            )
        )
} else {
    # Try to extract from key_2010 if available
    cat("WARNING: Could not find 2010 name columns in raj_05_20. Checking for key_2010...\n")
    if ("key_2010" %in% names(raj_05_20)) {
        raj_05_20 <- raj_05_20 %>%
            mutate(match_key_2010 = tolower(key_2010))
    }
}

raj_05_20_shrug <- raj_05_20 %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(raj_05_20_shrug$shrid2))
cat("Raj 05-20: total =", nrow(raj_05_20), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(raj_05_20), 1), "%)\n")

write_parquet(raj_05_20_shrug, here("data/raj/shrug_gp_raj_05_20_block.parquet"))
cat("Saved: data/raj/shrug_gp_raj_05_20_block.parquet\n")

# =============================================================================
# UTTAR PRADESH: Create SHRUG-linked panels
# =============================================================================

cat("\n\n========================================\n")
cat("=== UTTAR PRADESH SHRUG Panels ===\n")
cat("========================================\n")

# Load block-matched UP 05-10 data
up_block_05_10 <- read_parquet(here("data/up/shrug_lgd_up_elex_05_10_block.parquet"))
cat("UP block-matched 05-10 rows:", nrow(up_block_05_10), "\n")

# Create SHRUG mapping from block-matched data
up_shrug_mapping <- up_block_05_10 %>%
    filter(!is.na(shrid2)) %>%
    select(
        key_2010,
        shrid2,
        lgd_gp_code, lgd_gp_name,
        lgd_block_code, lgd_block_name,
        gp_match_type
    ) %>%
    rename(match_type = gp_match_type) %>%
    distinct(key_2010, .keep_all = TRUE)

cat("UP SHRUG mapping rows (unique key_2010):", nrow(up_shrug_mapping), "\n")

# =============================================================================
# UP PANEL 1: 05-10 SHRUG Panel
# =============================================================================

cat("\n--- UP 05-10 SHRUG Panel ---\n")

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

up_05_10_shrug <- up_05_10 %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(up_05_10_shrug$shrid2))
cat("UP 05-10: total =", nrow(up_05_10), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(up_05_10), 1), "%)\n")

write_parquet(up_05_10_shrug, here("data/up/shrug_gp_up_05_10_block.parquet"))
cat("Saved: data/up/shrug_gp_up_05_10_block.parquet\n")

# =============================================================================
# UP PANEL 2: 10-15 SHRUG Panel
# =============================================================================

cat("\n--- UP 10-15 SHRUG Panel ---\n")

up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))

# Create mapping from 2015 key to 2010 key
up_05_10_key_map <- up_05_10 %>%
    filter(!is.na(key_2010)) %>%
    select(key_2010, district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010) %>%
    distinct()

# Join via eng_key pattern matching
up_10_15_shrug <- up_10_15 %>%
    left_join(
        up_shrug_mapping %>%
            left_join(up_05_10_key_map, by = "key_2010") %>%
            select(key_2010, shrid2, lgd_gp_code, lgd_gp_name, lgd_block_code, lgd_block_name,
                   district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010),
        by = c("district_name_eng_2010", "block_name_eng_2010", "gp_name_eng_2010")
    ) %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(up_10_15_shrug$shrid2))
cat("UP 10-15: total =", nrow(up_10_15), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(up_10_15), 1), "%)\n")

write_parquet(up_10_15_shrug, here("data/up/shrug_gp_up_10_15_block.parquet"))
cat("Saved: data/up/shrug_gp_up_10_15_block.parquet\n")

# =============================================================================
# UP PANEL 3: 15-21 SHRUG Panel
# =============================================================================

cat("\n--- UP 15-21 SHRUG Panel ---\n")

up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

# Chain via 10-15 panel: 15-21 -> 10-15 (2015 names) -> SHRUG mapping
up_10_15_mapping <- up_10_15 %>%
    filter(!is.na(key_2010)) %>%
    select(district_name_eng_2015, block_name_eng_2015, gp_name_eng_2015, key_2010) %>%
    distinct()

up_15_21_shrug <- up_15_21 %>%
    left_join(up_10_15_mapping, by = c("district_name_eng_2015", "block_name_eng_2015", "gp_name_eng_2015")) %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(up_15_21_shrug$shrid2))
cat("UP 15-21: total =", nrow(up_15_21), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(up_15_21), 1), "%)\n")

write_parquet(up_15_21_shrug, here("data/up/shrug_gp_up_15_21_block.parquet"))
cat("Saved: data/up/shrug_gp_up_15_21_block.parquet\n")

# =============================================================================
# UP PANEL 4: 05-21 Long-Term Panel
# =============================================================================

cat("\n--- UP 05-21 Long-Term Panel ---\n")

up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

# The 4-way panel should have key_2010
up_05_21_shrug <- up_05_21 %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

n_matched <- sum(!is.na(up_05_21_shrug$shrid2))
cat("UP 05-21: total =", nrow(up_05_21), ", SHRUG matched =", n_matched,
    "(", round(100 * n_matched / nrow(up_05_21), 1), "%)\n")

write_parquet(up_05_21_shrug, here("data/up/shrug_gp_up_05_21_block.parquet"))
cat("Saved: data/up/shrug_gp_up_05_21_block.parquet\n")

# =============================================================================
# Summary
# =============================================================================

cat("\n=== Summary ===\n")
cat("Created SHRUG-matched panel files using block matching:\n\n")
cat("Rajasthan:\n")
cat("  - data/raj/shrug_gp_raj_05_10_block.parquet\n")
cat("  - data/raj/shrug_gp_raj_10_15_block.parquet\n")
cat("  - data/raj/shrug_gp_raj_15_20_block.parquet\n")
cat("  - data/raj/shrug_gp_raj_05_20_block.parquet (long-term)\n")
cat("\nUttar Pradesh:\n")
cat("  - data/up/shrug_gp_up_05_10_block.parquet\n")
cat("  - data/up/shrug_gp_up_10_15_block.parquet\n")
cat("  - data/up/shrug_gp_up_15_21_block.parquet\n")
cat("  - data/up/shrug_gp_up_05_21_block.parquet (long-term)\n")

cat("\n=== Done ===\n")
