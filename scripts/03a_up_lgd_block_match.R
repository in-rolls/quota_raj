# Match UP Election Blocks to LGD Block Panchayats (Kshetra Panchayats)
# Using official Block Panchayat -> GP hierarchy from LGD

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== UP Block Crosswalk Creation ===\n")

# ============================================================================
# District name mapping (election -> LGD)
# Some districts were renamed or have spelling variations
# ============================================================================

district_mapping <- tribble(
    ~elex_district, ~lgd_district,
    "Muzzafarnagar", "Muzaffarnagar",
    "Bijnaur", "Bijnor",
    "Jyotiba Phule Nagar", "Amroha",
    "Muradabad", "Moradabad",
    "Shahjahnapur", "Shahjahanpur",
    "Itawah", "Etawah",
    "Jhanshi", "Jhansi",
    "Allahabad", "Prayagraj",
    "Lakhimpur Khiri", "Kheri",
    "Faizabad", "Ayodhya",
    "Baliya", "Ballia",
    "Bagpat", "Baghpat",
    "Kushinagar", "Kushi Nagar"
)

# ============================================================================
# STEP 1: Load LGD Block Panchayat data
# ============================================================================

lgd_blocks <- read_csv(here("data/crosswalks/lgd_up_blocks.csv"), show_col_types = FALSE)

cat("LGD blocks:", nrow(lgd_blocks), "\n")
cat("LGD districts:", n_distinct(lgd_blocks$zp_name), "\n")

# Normalize for matching
lgd_blocks <- lgd_blocks %>%
    mutate(
        lgd_dist_std = normalize_string(zp_name),
        lgd_block_std = normalize_string(block_name)
    )

# ============================================================================
# STEP 2: Define manual crosswalk for cross-district mappings
# (Blocks that moved to newly created districts)
# ============================================================================

lgd_block_lookup <- lgd_blocks %>% select(block_code, block_name, zp_name)

manual_block_xwalk <- tribble(
    ~elex_district, ~elex_block, ~lgd_district, ~lgd_block_name,
    # =========================================================================
    # CROSS-DISTRICT MAPPINGS (administrative reorganization)
    # =========================================================================
    # Muzzafarnagar blocks now in Shamli district (created 2011)
    "Muzzafarnagar", "Kairana", "Shamli", "Kairana",
    "Muzzafarnagar", "Shamli", "Shamli", "Shamli",
    "Muzzafarnagar", "Un", "Shamli", "Un",
    "Muzzafarnagar", "Kandla", "Shamli", "Kandhla",
    # Moradabad blocks now in Sambhal district (created 2011)
    "Muradabad", "Sambhal", "Sambhal", "Sambhal",
    "Muradabad", "Bahjoi", "Sambhal", "Bahjoi",
    "Muradabad", "Panwasa", "Sambhal", "Panwasa",
    "Muradabad", "Asamoli", "Sambhal", "Asmauli",
    # Ghaziabad blocks now in Hapur district (created 2011)
    "Ghaziabad", "Dhaulana", "Hapur", "Dhaulana",
    "Ghaziabad", "Garhmukteshwar", "Hapur", "Garh Mukteshwar",
    "Ghaziabad", "Simbhavli", "Hapur", "Simbhawali",
    # Budaun blocks now in Sambhal district
    "Budaun", "Junawai", "Sambhal", "Junawai",
    "Budaun", "Gunnaur", "Sambhal", "Gunnaur",
    # =========================================================================
    # SPELLING VARIATIONS (verified manual matches)
    # =========================================================================
    # Saharanpur district
    "Saharanpur", "Sadholi Kadim", "Saharanpur", "Sadauli Qadeem",
    # Bulandshahr district
    "Bulandshahr", "B. B. Nagar", "Bulandshahr", "Bhawan Bahadur Nagar",
    # Bijnor district (election data uses old spelling "Bijnaur")
    "Bijnaur", "Mau Pur Devmal", "Bijnor", "Mohammedpur Deomal",
    "Bijnaur", "Syohara", "Bijnor", "Budhanpur Seohara",
    # Moradabad district
    "Muradabad", "Dingarpur (Kundarki)", "Moradabad", "Kundarki",
    # Rampur district
    "Rampur", "Chamaraua", "Rampur", "Chamraon",
    # Bareilly district
    "Bareilly", "Vithri", "Bareilly", "Bithiri Chainpur",
    # Shahjahanpur district
    "Shahjahnapur", "Katra - Khudaganj", "Shahjahanpur", "Khudaganj Katra",
    # Budaun district (remaining in Budaun)
    "Budaun", "Myau", "Budaun", "Mion",
    # Etawah district
    "Itawah", "Saifai", "Etawah", "Sefai",
    # Pratapgarh district
    "Pratapgarh", "Sadar", "Pratapgarh", "Pratapgarh (Sadar)",
    # Unnao district
    "Unnao", "Miyoganj", "Unnao", "Mianganj",
    "Unnao", "Si. Sirosi", "Unnao", "Sikandarpur Sarausi",
    # Kheri district (election data uses "Lakhimpur Khiri")
    "Lakhimpur Khiri", "Gola (Kumbhi)", "Kheri", "Kumbhigola",
    # Sultanpur district
    "Sultanpur", "Pratappur Kemaicha", "Sultanpur", "P.P.Kamaicha",
    # Sant Kabeer Nagar district
    "Sant Kabeer Nagar", "Hesarbazar", "Sant Kabeer Nagar", "Hainsar Bazar",
    # Gorakhpur district
    "Gorakhpur", "J. O. Kauriya", "Gorakhpur", "Jangal Kaudia",
    # Maharajganj district
    "Maharajganj", "Ferenda", "Maharajganj", "Pharenda",
    # Jaunpur district
    "Jaunpur", "Buxa", "Jaunpur", "Baksha",
    # Mirzapur district
    "Mirzapur", "Seeti", "Mirzapur", "Shikhar",
    # Mainpuri district
    "Mainpuri", "Ahir", "Mainpuri", "Karhal",
    # Gautam Buddha Nagar district
    "Gautam Buddha Nagar", "Dankaur", "Gautam Buddha Nagar", "Dadri",
    # Muzaffarnagar district (Sadar block)
    "Muzzafarnagar", "Sadar", "Muzaffarnagar", "Muzaffarnagar"
) %>%
    left_join(lgd_block_lookup, by = c("lgd_district" = "zp_name", "lgd_block_name" = "block_name")) %>%
    rename(lgd_block_code = block_code)

cat("Manual crosswalk entries:", nrow(manual_block_xwalk), "\n")

# ============================================================================
# STEP 3: Load election blocks
# ============================================================================

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

elex_blocks <- up_05_10 %>%
    select(district_name_eng_2010, block_name_eng_2010) %>%
    distinct() %>%
    filter(!is.na(block_name_eng_2010)) %>%
    left_join(district_mapping, by = c("district_name_eng_2010" = "elex_district")) %>%
    mutate(
        district_for_match = coalesce(lgd_district, district_name_eng_2010),
        elex_dist_std = normalize_string(district_for_match),
        elex_block_std = normalize_string(block_name_eng_2010)
    )

cat("Election blocks:", nrow(elex_blocks), "\n")
cat("Election districts:", n_distinct(elex_blocks$district_name_eng_2010), "\n")

# ============================================================================
# STEP 4: Exact match first
# ============================================================================

exact_match <- elex_blocks %>%
    inner_join(lgd_blocks, by = c("elex_dist_std" = "lgd_dist_std",
                                   "elex_block_std" = "lgd_block_std"))

cat("Exact matches:", nrow(exact_match), "\n")

# Unmatched election blocks (excluding manual crosswalk entries)
unmatched_elex <- elex_blocks %>%
    anti_join(exact_match, by = c("district_name_eng_2010", "block_name_eng_2010")) %>%
    anti_join(manual_block_xwalk, by = c("district_name_eng_2010" = "elex_district",
                                          "block_name_eng_2010" = "elex_block"))

cat("Unmatched election blocks (excl. manual):", nrow(unmatched_elex), "\n")

# ============================================================================
# STEP 5: Fuzzy match within district for unmatched
# ============================================================================

fuzzy_results <- list()

for (i in 1:nrow(unmatched_elex)) {
    elex_row <- unmatched_elex[i, ]

    lgd_same_dist <- lgd_blocks %>%
        filter(lgd_dist_std == elex_row$elex_dist_std)

    if (nrow(lgd_same_dist) == 0) {
        next
    }

    distances <- stringdist(elex_row$elex_block_std,
                            lgd_same_dist$lgd_block_std,
                            method = "jw")

    best_idx <- which.min(distances)
    best_dist <- distances[best_idx]

    if (best_dist <= 0.25) {
        fuzzy_results[[length(fuzzy_results) + 1]] <- tibble(
            district_name_eng_2010 = elex_row$district_name_eng_2010,
            block_name_eng_2010 = elex_row$block_name_eng_2010,
            block_code = lgd_same_dist$block_code[best_idx],
            block_name = lgd_same_dist$block_name[best_idx],
            zp_code = lgd_same_dist$zp_code[best_idx],
            zp_name = lgd_same_dist$zp_name[best_idx],
            match_dist = best_dist
        )
    }
}

fuzzy_df <- bind_rows(fuzzy_results)
cat("Fuzzy matches:", nrow(fuzzy_df), "\n")

# ============================================================================
# STEP 6: Check still unmatched
# ============================================================================

still_unmatched <- unmatched_elex %>%
    anti_join(fuzzy_df, by = c("district_name_eng_2010", "block_name_eng_2010"))

cat("Still unmatched after fuzzy:", nrow(still_unmatched), "\n")

# Separate urban wards (Wardpur/Vardpur entries) from actual unmatched blocks
# These are urban ward subdivisions in Siddharth Nagar, not rural block panchayats
urban_wards <- still_unmatched %>%
    filter(str_detect(block_name_eng_2010, "(?i)wardpur|vardpur|ward\\s*no"))

actual_unmatched <- still_unmatched %>%
    filter(!str_detect(block_name_eng_2010, "(?i)wardpur|vardpur|ward\\s*no"))

cat("Urban wards (not block panchayats):", nrow(urban_wards), "\n")
cat("Actual unmatched blocks:", nrow(actual_unmatched), "\n")

if (nrow(actual_unmatched) > 0) {
    cat("\nActual unmatched blocks:\n")
    print(actual_unmatched %>% select(district_name_eng_2010, block_name_eng_2010))
}

# ============================================================================
# STEP 7: Combine all matches
# ============================================================================

block_xwalk <- bind_rows(
    exact_match %>%
        transmute(
            elex_district = district_name_eng_2010,
            elex_block = block_name_eng_2010,
            lgd_district = zp_name,
            lgd_block_name = block_name,
            lgd_block_code = block_code,
            match_type = "exact"
        ),
    fuzzy_df %>%
        transmute(
            elex_district = district_name_eng_2010,
            elex_block = block_name_eng_2010,
            lgd_district = zp_name,
            lgd_block_name = block_name,
            lgd_block_code = block_code,
            match_type = "fuzzy"
        ),
    manual_block_xwalk %>%
        transmute(
            elex_district = elex_district,
            elex_block = elex_block,
            lgd_district = lgd_district,
            lgd_block_name = lgd_block_name,
            lgd_block_code = lgd_block_code,
            match_type = "manual"
        )
)

cat("\nTotal matched blocks:", nrow(block_xwalk), "out of", nrow(elex_blocks), "\n")
cat("Match rate:", round(100 * nrow(block_xwalk) / nrow(elex_blocks), 1), "%\n")

cat("\nMatch type breakdown:\n")
print(table(block_xwalk$match_type))

# Save crosswalk
write_csv(block_xwalk, here("data/crosswalks/up_block_xwalk.csv"))
cat("\nSaved: data/crosswalks/up_block_xwalk.csv\n")

# Save unmatched for review (separately for urban wards vs actual blocks)
if (nrow(actual_unmatched) > 0) {
    write_csv(actual_unmatched, here("data/crosswalks/up_blocks_unmatched.csv"))
    cat("Saved actual unmatched blocks: data/crosswalks/up_blocks_unmatched.csv\n")
}

if (nrow(urban_wards) > 0) {
    write_csv(urban_wards, here("data/crosswalks/up_urban_wards_excluded.csv"))
    cat("Saved urban wards (excluded): data/crosswalks/up_urban_wards_excluded.csv\n")
}

# Summary stats
cat("\n=== SUMMARY ===\n")
cat("Total election blocks:", nrow(elex_blocks), "\n")
cat("Matched blocks:", nrow(block_xwalk), "\n")
cat("Match rate (excluding urban wards):",
    round(100 * nrow(block_xwalk) / (nrow(elex_blocks) - nrow(urban_wards)), 1), "%\n")
cat("Urban wards excluded:", nrow(urban_wards), "\n")
cat("Actual unmatched blocks:", nrow(actual_unmatched), "\n")

cat("\n=== Done ===\n")
