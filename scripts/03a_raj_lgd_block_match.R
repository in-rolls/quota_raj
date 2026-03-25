# Match Election Samitis to LGD Block Panchayats
# Using official Block Panchayat -> GP hierarchy from LGD

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# ============================================================================
# STEP 1: Load LGD Block Panchayat data
# ============================================================================

lgd_gp <- read_csv("data/crosswalks/lgd_raj_block_gp.csv")

# Get unique blocks from LGD
lgd_blocks <- lgd_gp %>%
    select(block_code, block_name, zila_code, zila_name) %>%
    distinct() %>%
    mutate(
        lgd_dist_std = normalize_string_strict(zila_name),
        lgd_block_std = normalize_string_strict(block_name)
    )

cat("LGD blocks:", nrow(lgd_blocks), "\n")

# ============================================================================
# STEP 2: Load election samitis
# ============================================================================

elex <- read_parquet("data/raj/elex_raj_05_10.parquet")

elex_samitis <- elex %>%
    select(dist_name_new_2010, samiti_name_new_2010) %>%
    distinct() %>%
    filter(!is.na(samiti_name_new_2010)) %>%
    mutate(
        elex_dist_std = normalize_string_strict(dist_name_new_2010),
        elex_samiti_std = normalize_string_strict(samiti_name_new_2010)
    )

cat("Election samitis:", nrow(elex_samitis), "\n")

# ============================================================================
# STEP 3: Match election samitis to LGD blocks
# ============================================================================

# Complete manual crosswalk for all non-exact samiti matches
# These are verified spelling variations of the same administrative unit
manual_samiti_xwalk <- tribble(
    ~elex_district, ~elex_samiti, ~lgd_district, ~lgd_block_name, ~lgd_block_code,
    # Spelling variations (same samiti)
    "ajmer", "ARAI", "Ajmer", "Arain", 1149,
    "ajmer", "KISHANGARH", "Ajmer", "Kishangarh (Silora)", 1153,
    "ajmer", "MASUDA", "Ajmer", "Masooda", 1154,
    "ajmer", "PEESANGAN", "Ajmer", "Pisangan", 1155,
    "alwar", "RAINI", "Alwar", "Reni", 1167,
    "bharatpur", "NAGAR", "Bharatpur", "Nagar Pahari", 1199,
    "bikaner", "DUNGARGARH", "Bikaner", "Sri Dungargarh", 1218,
    "chittorgarh", "CHHITORGARH", "Chittorgarh", "Chittorgarh", 1230,
    "dholpur", "DHOLPUR", "Dholpur", "Dhaulpur", 1250,
    "dungarpur", "BICHHIWARA", "Dungarpur", "Bichiwara", 1253,
    "jhalawar", "PIRAWA", "Jhalawar", "Pirawa (Sunel)", 1295,
    "karauli", "NADOTI", "Karauli", "Nadauti", 1315,
    "nagaur", "MUNDAWA", "Nagaur", "Mundwa", 1330,
    "nagaur", "RIYAN", "Nagaur", "Riyan Badi", 1333,
    "pali", "MARWAR JUN", "Pali", "Kharchi(Mar.Junction)", 1337,
    "sawaimadhopur", "GANGAPUR", "Sawai Madhopur", "Gangapur City", 1353,
    "sikar", "DHOND", "Sikar", "Dhod", 1357,
    "tonk", "NIWAI", "Tonk", "Newai", 1371,
    "udaipur", "BHINDAR", "Udaipur", "Bhinder", 1376
)

# Exact match first
exact_match <- elex_samitis %>%
    inner_join(lgd_blocks, by = c("elex_dist_std" = "lgd_dist_std",
                                   "elex_samiti_std" = "lgd_block_std"))

cat("Exact matches:", nrow(exact_match), "\n")

# Unmatched election samitis
unmatched_elex <- elex_samitis %>%
    anti_join(exact_match, by = c("dist_name_new_2010", "samiti_name_new_2010"))

cat("Unmatched election samitis:", nrow(unmatched_elex), "\n")

# Check all unmatched are in manual crosswalk
unmatched_not_in_manual <- unmatched_elex %>%
    mutate(dist_name_lower = tolower(dist_name_new_2010)) %>%
    anti_join(manual_samiti_xwalk, by = c("dist_name_lower" = "elex_district",
                                           "samiti_name_new_2010" = "elex_samiti"))

if (nrow(unmatched_not_in_manual) > 0) {
    cat("\nERROR: Unmatched samitis not in manual crosswalk:\n")
    print(unmatched_not_in_manual)
    stop("All samitis must be matched. Add missing entries to manual_samiti_xwalk.")
}

cat("All unmatched samitis covered by manual crosswalk.\n")

# ============================================================================
# STEP 4: Create final crosswalk (exact + manual only, NO fuzzy)
# ============================================================================

block_xwalk <- bind_rows(
    exact_match %>%
        transmute(
            elex_district = dist_name_new_2010,
            elex_samiti = samiti_name_new_2010,
            lgd_district = zila_name,
            lgd_block_name = block_name,
            lgd_block_code = block_code,
            match_type = "exact"
        ),
    manual_samiti_xwalk %>%
        mutate(match_type = "manual")
)

cat("\nTotal matched samitis:", nrow(block_xwalk), "out of", nrow(elex_samitis), "\n")

if (nrow(block_xwalk) != nrow(elex_samitis)) {
    stop("ERROR: Not all samitis matched!")
}

cat("Match type breakdown:\n")
print(table(block_xwalk$match_type))

# Save crosswalk
write_csv(block_xwalk, "data/crosswalks/raj_block_xwalk.csv")
cat("\nSaved: data/crosswalks/raj_block_xwalk.csv\n")
