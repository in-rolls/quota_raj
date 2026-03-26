# 01e_up_create_district_xwalk.R
# Create district crosswalk between UP election data and LGD
# Links election district names to LGD district names for block/GP-level matching
#
# Following Rajasthan's 01b_raj_create_district_xwalk.R pattern

library(here)
library(dplyr)
library(readr)
library(tidyr)
library(arrow)

source(here("scripts/00_utils.R"))

message("=== Creating UP District Crosswalk ===")

# =============================================================================
# STEP 1: Load LGD districts
# =============================================================================

lgd_blocks <- read_csv(here("data/lgd/processed/lgd_up_blocks.csv"),
                       show_col_types = FALSE)

lgd_districts <- lgd_blocks %>%
    distinct(zp_code, zp_name) %>%
    arrange(zp_name)

message("LGD districts (", nrow(lgd_districts), "):\n", sep = "")
message(lgd_districts$zp_name, sep = ", ")
message("")

# =============================================================================
# STEP 2: Load election districts from UP election data
# =============================================================================

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

districts_2005 <- unique(up_05_10$district_name_eng_2005)
districts_2010 <- unique(up_05_10$district_name_eng_2010)

message("Election districts by year:")
message("  2005: ", length(districts_2005[!is.na(districts_2005)]), "unique districts")
message("  2010: ", length(districts_2010[!is.na(districts_2010)]), "unique districts\n")

all_elex_districts <- unique(c(districts_2005, districts_2010))
all_elex_districts <- all_elex_districts[!is.na(all_elex_districts)]
message("Total unique election district names: ", length(all_elex_districts), "\n")

# =============================================================================
# STEP 3: Manual district mapping
# =============================================================================
# Match types:
#   exact     - Direct name match (just case normalization)
#   spelling  - Minor spelling variant
#   renamed   - District was renamed (e.g., Allahabad → Prayagraj)
#   new_dist  - Blocks moved to newly created district
#   merged    - District merged or consolidated

district_mapping <- tribble(
    ~elex_district, ~lgd_district, ~lgd_zp_code, ~match_type,
    # =========================================================================
    # EXACT MATCHES (just case normalization)
    # =========================================================================
    "Agra", "Agra", 101, "exact",
    "Ambedkar Nagar", "Ambedkar Nagar", 103, "exact",
    "Auraiya", "Auraiya", 106, "exact",
    "Bahraich", "Bahraich", 108, "exact",
    "Balrampur", "Balrampur", 110, "exact",
    "Banda", "Banda", 111, "exact",
    "Barabanki", "Barabanki", 112, "exact",
    "Bareilly", "Bareilly", 113, "exact",
    "Basti", "Basti", 114, "exact",
    "Budaun", "Budaun", 118, "exact",
    "Bulandshahr", "Bulandshahr", 125, "exact",
    "Chandauli", "Chandauli", 126, "exact",
    "Chitrakoot", "Chitrakoot", 127, "exact",
    "Fatehpur", "Fatehpur", 130, "exact",
    "Firozabad", "Firozabad", 131, "exact",
    "Gautam Buddha Nagar", "Gautam Buddha Nagar", 132, "exact",
    "Ghaziabad", "Ghaziabad", 133, "exact",
    "Ghazipur", "Ghazipur", 134, "exact",
    "Gonda", "Gonda", 135, "exact",
    "Gorakhpur", "Gorakhpur", 136, "exact",
    "Hamirpur", "Hamirpur", 137, "exact",
    "Hardoi", "Hardoi", 139, "exact",
    "Hathras", "Hathras", 140, "exact",
    "Jalaun", "Jalaun", 141, "exact",
    "Jaunpur", "Jaunpur", 142, "exact",
    "Kannauj", "Kannauj", 144, "exact",
    "Kanpur Nagar", "Kanpur Nagar", 146, "exact",
    "Kaushambi", "Kaushambi", 148, "exact",
    "Lalitpur", "Lalitpur", 151, "exact",
    "Lucknow", "Lucknow", 152, "exact",
    "Maharajganj", "Maharajganj", 153, "exact",
    "Mahoba", "Mahoba", 154, "exact",
    "Mainpuri", "Mainpuri", 155, "exact",
    "Mau", "Mau", 157, "exact",
    "Meerut", "Meerut", 160, "exact",
    "Mirzapur", "Mirzapur", 161, "exact",
    "Pilibhit", "Pilibhit", 162, "exact",
    "Pratapgarh", "Pratapgarh", 163, "exact",
    "Rampur", "Rampur", 167, "exact",
    "Saharanpur", "Saharanpur", 168, "exact",
    "Sant Kabeer Nagar", "Sant Kabeer Nagar", 169, "exact",
    "Sant Ravidas Nagar", "Sant Ravidas Nagar", 170, "exact",
    "Shravasti", "Shravasti", 173, "exact",
    "Siddharth Nagar", "Siddharth Nagar", 174, "exact",
    "Sitapur", "Sitapur", 175, "exact",
    "Sonbhadra", "Sonbhadra", 176, "exact",
    "Sultanpur", "Sultanpur", 177, "exact",
    "Unnao", "Unnao", 178, "exact",
    "Varanasi", "Varanasi", 179, "exact",

    # =========================================================================
    # SPELLING VARIATIONS
    # =========================================================================
    "Bagpat", "Baghpat", 107, "spelling",
    "Baliya", "Ballia", 109, "spelling",
    "Bijnaur", "Bijnor", 123, "spelling",
    "Itawah", "Etawah", 129, "spelling",
    "Jhanshi", "Jhansi", 143, "spelling",
    "Muradabad", "Moradabad", 162, "spelling",
    "Muzzafarnagar", "Muzaffarnagar", 166, "spelling",
    "Rae bareli", "Rae Bareli", 165, "spelling",
    "Shahjahnapur", "Shahjahanpur", 171, "spelling",

    # =========================================================================
    # RENAMED DISTRICTS (names changed after 2010)
    # =========================================================================
    "Allahabad", "Prayagraj", 164, "renamed",
    "Faizabad", "Ayodhya", 105, "renamed",
    "Jyotiba Phule Nagar", "Amroha", 104, "renamed",

    # =========================================================================
    # DISTRICTS WITH BLOCKS MOVED TO NEW DISTRICTS (created after 2010)
    # New districts: Shamli (2011), Sambhal (2011), Hapur (2011), Amethi (2010)
    # These are handled at the block level, not district level
    # =========================================================================
    "Kushinagar", "Kushi Nagar", 150, "spelling",
    "Lakhimpur Khiri", "Kheri", 149, "renamed"
)

# Refresh hard-coded district codes against current LGD extracts.
district_mapping <- district_mapping %>%
    left_join(
        lgd_districts %>% select(lgd_district = zp_name, lgd_zp_code_current = zp_code),
        by = "lgd_district"
    ) %>%
    mutate(
        lgd_zp_code_legacy = lgd_zp_code,
        lgd_zp_code = as.integer(lgd_zp_code_current)
    ) %>%
    select(-lgd_zp_code_current)

missing_code <- district_mapping %>% filter(is.na(lgd_zp_code))
if (nrow(missing_code) > 0) {
    message("ERROR: Missing LGD district code after name join:")
    print(missing_code %>% select(elex_district, lgd_district, match_type))
    stop("Could not refresh LGD district codes from current LGD extracts")
}

code_changes <- district_mapping %>%
    filter(!is.na(lgd_zp_code_legacy), lgd_zp_code_legacy != lgd_zp_code)

if (nrow(code_changes) > 0) {
    message("Updated ", nrow(code_changes), " district code mappings to current LGD zp_code values.")
}

# =============================================================================
# STEP 4: Validation
# =============================================================================

message("=== VALIDATION ===")

# Check all election districts are mapped
unmapped <- setdiff(all_elex_districts, district_mapping$elex_district)
if (length(unmapped) > 0) {
    message("ERROR: Unmapped election districts:")
    message(unmapped, sep = "")
    stop("All election districts must be mapped")
} else {
    message("All election districts mapped.")
}

# Check all mappings point to valid LGD districts
invalid_lgd <- district_mapping %>%
    filter(!lgd_district %in% lgd_districts$zp_name) %>%
    pull(lgd_district) %>%
    unique()

if (length(invalid_lgd) > 0) {
    message("ERROR: Invalid LGD district mappings:")
    message(invalid_lgd, sep = "")
    stop("All mappings must point to valid LGD districts")
} else {
    message("All mappings point to valid LGD districts.")
}

# Check which LGD districts are covered
mapped_lgd <- district_mapping %>%
    pull(lgd_district) %>%
    unique()

unmapped_lgd <- setdiff(lgd_districts$zp_name, mapped_lgd)
if (length(unmapped_lgd) > 0) {
    message("\nNote: LGD districts not in 2005-2010 election data:")
    message(unmapped_lgd, sep = "")
    message("\n(These include new districts created after 2010 and districts not in sample)")
}

# =============================================================================
# STEP 5: Create final crosswalk
# =============================================================================

final_xwalk <- district_mapping %>%
    select(elex_district, lgd_district, lgd_zp_code, match_type)

message("\n=== SUMMARY ===")
message("Crosswalk entries: ", nrow(final_xwalk))
message("Unique election districts: ", n_distinct(final_xwalk$elex_district))
message("Unique LGD districts mapped: ", n_distinct(final_xwalk$lgd_district))

# =============================================================================
# STEP 6: Save output
# =============================================================================

output_dir <- here("data/crosswalks/active")
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

write_csv(final_xwalk, here("data/crosswalks/active/up_district_xwalk.csv"))
message("\nCrosswalk saved to: data/crosswalks/active/up_district_xwalk.csv")

# Print mapping summary by match type
message("\n=== MAPPING TYPES ===")
final_xwalk %>%
    count(match_type) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

message("\n=== Done ===")
