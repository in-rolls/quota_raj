# 01c_raj_create_district_crosswalk.R
# Create district crosswalk between election data and SHRUG
# Links election district names to SHRUG district names for GP-level fuzzy matching

library(here)
library(dplyr)
library(readr)
library(tidyr)

# =============================================================================
# LOAD SHRUG DISTRICTS
# =============================================================================

shrug <- read_csv(
    here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
    show_col_types = FALSE
)

shrug_districts <- shrug %>%
    filter(state_name == "rajasthan") %>%
    distinct(district_name) %>%
    pull(district_name) %>%
    sort()

cat("SHRUG districts (", length(shrug_districts), "):\n", sep = "")
cat(shrug_districts, sep = ", ")
cat("\n\n")

# =============================================================================
# LOAD ELECTION DISTRICTS FROM EACH YEAR
# =============================================================================

elex_2005 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2005.csv"),
    show_col_types = FALSE
)
districts_2005 <- unique(elex_2005$dist_name)

elex_2010 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2010.csv"),
    show_col_types = FALSE
)
districts_2010 <- unique(elex_2010$dist_name)

elex_2015 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2015_manual_sex.csv"),
    show_col_types = FALSE
)
districts_2015 <- unique(elex_2015$dist_name)

elex_2020 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2020.csv"),
    show_col_types = FALSE
)
districts_2020 <- unique(elex_2020$district_2020)

cat("Election districts by year:\n")
cat("  2005:", length(districts_2005), "unique districts\n")
cat("  2010:", length(districts_2010), "unique districts\n")
cat("  2015:", length(districts_2015), "unique districts\n")
cat("  2020:", length(districts_2020), "unique districts\n\n")

# Combine all election districts
all_elex_districts <- unique(c(districts_2005, districts_2010, districts_2015, districts_2020))
cat("Total unique election district names across all years:", length(all_elex_districts), "\n\n")

# =============================================================================
# MANUAL DISTRICT MAPPING
# =============================================================================

district_mapping <- tribble(
    ~elex_district_raw, ~shrug_district, ~note,
    # Standard matches (case normalization only)
    "AJMER", "ajmer", "exact",
    "Ajmer", "ajmer", "exact",
    "ALWAR", "alwar", "exact",
    "Alwar", "alwar", "exact",
    "BANSWARA", "banswara", "exact",
    "Banswara", "banswara", "exact",
    "BARAN", "baran", "exact",
    "Baran", "baran", "exact",
    "BARMER", "barmer", "exact",
    "Barmer", "barmer", "exact",
    "BHARATPUR", "bharatpur", "exact",
    "Bharatpur", "bharatpur", "exact",
    "BHILWARA", "bhilwara", "exact",
    "Bhilwara", "bhilwara", "exact",
    "BIKANER", "bikaner", "exact",
    "Bikaner", "bikaner", "exact",
    "BUNDI", "bundi", "exact",
    "Bundi", "bundi", "exact",
    "CHURU", "churu", "exact",
    "Churu", "churu", "exact",
    "DAUSA", "dausa", "exact",
    "Dausa", "dausa", "exact",
    "GANGANAGAR", "ganganagar", "exact",
    "Ganganagar", "ganganagar", "exact",
    "HANUMANGARH", "hanumangarh", "exact",
    "Hanumangarh", "hanumangarh", "exact",
    "JAIPUR", "jaipur", "exact",
    "Jaipur", "jaipur", "exact",
    "JAISALMER", "jaisalmer", "exact",
    "Jaisalmer", "jaisalmer", "exact",
    "JHALAWAR", "jhalawar", "exact",
    "Jhalawar", "jhalawar", "exact",
    "JODHPUR", "jodhpur", "exact",
    "Jodhpur", "jodhpur", "exact",
    "KARAULI", "karauli", "exact",
    "Karauli", "karauli", "exact",
    "KOTA", "kota", "exact",
    "Kota", "kota", "exact",
    "NAGAUR", "nagaur", "exact",
    "Nagaur", "nagaur", "exact",
    "PALI", "pali", "exact",
    "Pali", "pali", "exact",
    "RAJSAMAND", "rajsamand", "exact",
    "Rajsamand", "rajsamand", "exact",
    "SIKAR", "sikar", "exact",
    "Sikar", "sikar", "exact",
    "SIROHI", "sirohi", "exact",
    "Sirohi", "sirohi", "exact",
    "TONK", "tonk", "exact",
    "Tonk", "tonk", "exact",
    "UDAIPUR", "udaipur", "exact",
    "Udaipur", "udaipur", "exact",
    "PRATAPGARH", "pratapgarh", "exact",
    "Pratapgarh", "pratapgarh", "exact",
    "DUNGARPUR", "dungarpur", "exact",
    "Dungarpur", "dungarpur", "exact",

    # Spelling differences (SHRUG uses different spelling)
    "CHITTORGARH", "chittaurgarh", "spelling: o vs au",
    "Chittorgarh", "chittaurgarh", "spelling: o vs au",
    "DHOLPUR", "dhaulpur", "spelling: o vs au",
    "Dholpur", "dhaulpur", "spelling: o vs au",
    "JALORE", "jalor", "spelling: e suffix",
    "Jalore", "jalor", "spelling: e suffix",
    "JHUNJHUNU", "jhunjhunun", "spelling: n suffix",
    "Jhunjhunu", "jhunjhunun", "spelling: n suffix",

    # Abbreviations for Sawai Madhopur
    "S. MADHOPUR", "sawai madhopur", "abbreviation",
    "S.MADHOPUR", "sawai madhopur", "abbreviation",
    "S. Madhopur", "sawai madhopur", "abbreviation",

    # OCR errors (spaces inserted due to scanning issues)
    "BANSW ARA", "banswara", "OCR space",
    "P ALI", "pali", "OCR space",
    "KOT A", "kota", "OCR space",
    "PRA T APGARH", "pratapgarh", "OCR space",
    "BHARA TPUR", "bharatpur", "OCR space",

    # Typos
    "ANUMANGAR", "hanumangarh", "typo: missing H prefix",
    "DUNGERPUR", "dungarpur", "typo: e vs a",

    # Data entry error (sex value in district column)
    "MALE", NA_character_, "data error: sex value in district column"
)

# =============================================================================
# VALIDATION
# =============================================================================

cat("=== VALIDATION ===\n\n")

# Check all election districts are mapped
unmapped <- setdiff(all_elex_districts, district_mapping$elex_district_raw)
if (length(unmapped) > 0) {
    cat("ERROR: Unmapped election districts:\n")
    cat(unmapped, sep = "\n")
    stop("All election districts must be mapped")
} else {
    cat("All election districts mapped.\n")
}

# Check all mappings point to valid SHRUG districts (excluding NA for data errors)
invalid_shrug <- district_mapping %>%
    filter(!is.na(shrug_district)) %>%
    filter(!shrug_district %in% shrug_districts) %>%
    pull(shrug_district) %>%
    unique()

if (length(invalid_shrug) > 0) {
    cat("ERROR: Invalid SHRUG district mappings:\n")
    cat(invalid_shrug, sep = "\n")
    stop("All mappings must point to valid SHRUG districts")
} else {
    cat("All mappings point to valid SHRUG districts.\n")
}

# Check which SHRUG districts are covered
mapped_shrug <- district_mapping %>%
    filter(!is.na(shrug_district)) %>%
    pull(shrug_district) %>%
    unique()

unmapped_shrug <- setdiff(shrug_districts, mapped_shrug)
if (length(unmapped_shrug) > 0) {
    cat("\nWARNING: SHRUG districts not present in election data:\n")
    cat(unmapped_shrug, sep = "\n")
} else {
    cat("All SHRUG districts are mapped from election data.\n")
}

# =============================================================================
# CREATE FINAL CROSSWALK
# =============================================================================

final_xwalk <- district_mapping %>%
    filter(!is.na(shrug_district)) %>%
    select(elex_district_raw, shrug_district, note)

cat("\n=== SUMMARY ===\n")
cat("Crosswalk entries:", nrow(final_xwalk), "\n")
cat("Unique election district variants:", n_distinct(final_xwalk$elex_district_raw), "\n")
cat("Unique SHRUG districts mapped:", n_distinct(final_xwalk$shrug_district), "\n")

# =============================================================================
# SAVE OUTPUT
# =============================================================================

output_dir <- here("data/crosswalks")
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

write_csv(final_xwalk, here("data/crosswalks/raj_district_xwalk.csv"))
cat("\nCrosswalk saved to: data/crosswalks/raj_district_xwalk.csv\n")

# Print mapping summary by note type
cat("\n=== MAPPING TYPES ===\n")
final_xwalk %>%
    count(note) %>%
    arrange(desc(n)) %>%
    print(n = Inf)
