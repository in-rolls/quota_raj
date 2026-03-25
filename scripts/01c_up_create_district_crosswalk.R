# 01c_up_create_district_crosswalk.R
# Create district crosswalk between UP election data and LGD
# Links election district names to LGD district names for block/GP-level matching
#
# Following Rajasthan's 01c_raj_create_district_crosswalk.R pattern

library(here)
library(dplyr)
library(readr)
library(tidyr)
library(arrow)

source(here("scripts/00_utils.R"))

cat("=== Creating UP District Crosswalk ===\n\n")

# =============================================================================
# STEP 1: Load LGD districts
# =============================================================================

lgd_blocks <- read_csv(here("data/crosswalks/lgd_up_blocks.csv"),
                       show_col_types = FALSE)

lgd_districts <- lgd_blocks %>%
    distinct(zp_code, zp_name) %>%
    arrange(zp_name)

cat("LGD districts (", nrow(lgd_districts), "):\n", sep = "")
cat(lgd_districts$zp_name, sep = ", ")
cat("\n\n")

# =============================================================================
# STEP 2: Load election districts from UP election data
# =============================================================================

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

districts_2005 <- unique(up_05_10$district_name_eng_2005)
districts_2010 <- unique(up_05_10$district_name_eng_2010)

cat("Election districts by year:\n")
cat("  2005:", length(districts_2005[!is.na(districts_2005)]), "unique districts\n")
cat("  2010:", length(districts_2010[!is.na(districts_2010)]), "unique districts\n\n")

all_elex_districts <- unique(c(districts_2005, districts_2010))
all_elex_districts <- all_elex_districts[!is.na(all_elex_districts)]
cat("Total unique election district names:", length(all_elex_districts), "\n\n")

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

# =============================================================================
# STEP 4: Validation
# =============================================================================

cat("=== VALIDATION ===\n\n")

# Check all election districts are mapped
unmapped <- setdiff(all_elex_districts, district_mapping$elex_district)
if (length(unmapped) > 0) {
    cat("ERROR: Unmapped election districts:\n")
    cat(unmapped, sep = "\n")
    stop("All election districts must be mapped")
} else {
    cat("All election districts mapped.\n")
}

# Check all mappings point to valid LGD districts
invalid_lgd <- district_mapping %>%
    filter(!lgd_district %in% lgd_districts$zp_name) %>%
    pull(lgd_district) %>%
    unique()

if (length(invalid_lgd) > 0) {
    cat("ERROR: Invalid LGD district mappings:\n")
    cat(invalid_lgd, sep = "\n")
    stop("All mappings must point to valid LGD districts")
} else {
    cat("All mappings point to valid LGD districts.\n")
}

# Check which LGD districts are covered
mapped_lgd <- district_mapping %>%
    pull(lgd_district) %>%
    unique()

unmapped_lgd <- setdiff(lgd_districts$zp_name, mapped_lgd)
if (length(unmapped_lgd) > 0) {
    cat("\nNote: LGD districts not in 2005-2010 election data:\n")
    cat(unmapped_lgd, sep = "\n")
    cat("\n(These include new districts created after 2010 and districts not in sample)\n")
}

# =============================================================================
# STEP 5: Create final crosswalk
# =============================================================================

final_xwalk <- district_mapping %>%
    select(elex_district, lgd_district, lgd_zp_code, match_type)

cat("\n=== SUMMARY ===\n")
cat("Crosswalk entries:", nrow(final_xwalk), "\n")
cat("Unique election districts:", n_distinct(final_xwalk$elex_district), "\n")
cat("Unique LGD districts mapped:", n_distinct(final_xwalk$lgd_district), "\n")

# =============================================================================
# STEP 6: Save output
# =============================================================================

output_dir <- here("data/crosswalks")
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

write_csv(final_xwalk, here("data/crosswalks/up_district_xwalk.csv"))
cat("\nCrosswalk saved to: data/crosswalks/up_district_xwalk.csv\n")

# Print mapping summary by match type
cat("\n=== MAPPING TYPES ===\n")
final_xwalk %>%
    count(match_type) %>%
    arrange(desc(n)) %>%
    print(n = Inf)

cat("\n=== Done ===\n")
