# 02a_raj_create_2way_panels.R
# Create separate 2-way match files for Rajasthan
# Output: sp_2010_2015.csv, sp_2015_2020.csv

library(readr)
library(dplyr)
library(stringi)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Creating Rajasthan 2-Way Match Files ===\n")

# =============================================================================
# STEP 1: Create sp_2010_2015.csv
# =============================================================================
cat("\n--- Matching 2010 to 2015 ---\n")

s2010 <- read_csv(here("data/raj/source/sarpanch_election_data/sarpanch_2010.csv"),
                  show_col_types = FALSE)
s2015 <- read_csv(here("data/raj/source/sarpanch_election_data/sarpanch_2015.csv"),
                  show_col_types = FALSE)

cat("2010 file:", nrow(s2010), "rows\n")
cat("2015 file:", nrow(s2015), "rows\n")

s2010 <- s2010 %>%
    mutate(key = normalize_string(paste(dist_name_new, samiti_name_new, gp_new)))

s2015 <- s2015 %>%
    mutate(key = normalize_string(paste(dist_name_new, samiti_name_new, gp_new)))

sp_10_15 <- inner_join(
    s2010 %>% select(sl_no, dist_name, samiti_name, gp, reservation, name, sex,
                     category, gp_new, dist_name_new, samiti_name_new, key),
    s2015 %>% select(sl_no, dist_name, samiti_name, gp, reservation, name, sex,
                     category, gp_new, dist_name_new, samiti_name_new, key),
    by = "key",
    suffix = c("_2010", "_2015")
)

cat("Matched 2010-2015:", nrow(sp_10_15), "rows\n")

# Rename columns to match expected format
sp_10_15 <- sp_10_15 %>%
    rename(
        key_2010 = key,
        reservation_2010 = reservation_2010,
        reservation_2015 = reservation_2015,
        sex_2010 = sex_2010,
        sex_2015 = sex_2015
    ) %>%
    mutate(key_2015 = key_2010)

write_csv(sp_10_15, here("data/raj/source/sarpanch_election_data/sp_2010_2015.csv"))
cat("Saved: data/raj/source/sarpanch_election_data/sp_2010_2015.csv\n")

# =============================================================================
# STEP 2: Create sp_2015_2020.csv
# =============================================================================
cat("\n--- Matching 2015 to 2020 ---\n")

s2020 <- read_csv(here("data/raj/source/sarpanch_election_data/sarpanch_2020_clean.csv"),
                  show_col_types = FALSE)

cat("2020 file:", nrow(s2020), "rows\n")

# 2020 has different column names
s2020 <- s2020 %>%
    mutate(
        ps_clean = gsub(" PANCHAYAT SAMITI$", "", PanchayatSamiti, ignore.case = TRUE),
        key = normalize_string(paste(District, ps_clean, NameOfGramPanchyat))
    )

# Reset s2015 key (already computed above)
sp_15_20 <- inner_join(
    s2015 %>% select(sl_no, dist_name, samiti_name, gp, reservation, name, sex,
                     category, gp_new, dist_name_new, samiti_name_new, key),
    s2020 %>% select(District, PanchayatSamiti, NameOfGramPanchyat,
                     CategoryOfGramPanchyat, ps_clean, key),
    by = "key"
)

cat("Matched 2015-2020:", nrow(sp_15_20), "rows\n")

# Rename columns to match expected format
sp_15_20 <- sp_15_20 %>%
    rename(
        key_2015 = key,
        sl_no_2015 = sl_no,
        dist_name_2015 = dist_name,
        samiti_name_2015 = samiti_name,
        gp_2015 = gp,
        reservation_2015 = reservation,
        name_2015 = name,
        sex_2015 = sex,
        category_2015 = category,
        gp_new_2015 = gp_new,
        dist_name_new_2015 = dist_name_new,
        samiti_name_new_2015 = samiti_name_new,
        district_2020 = District,
        ps_2020 = ps_clean,
        gp_2020 = NameOfGramPanchyat,
        reservation_2020 = CategoryOfGramPanchyat
    ) %>%
    mutate(key_2020 = key_2015) %>%
    select(-PanchayatSamiti)

write_csv(sp_15_20, here("data/raj/source/sarpanch_election_data/sp_2015_2020.csv"))
cat("Saved: data/raj/source/sarpanch_election_data/sp_2015_2020.csv\n")

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Summary ===\n")
cat("sp_2005_2010.csv: (existing)\n")
cat("sp_2010_2015.csv:", nrow(sp_10_15), "rows\n")
cat("sp_2015_2020.csv:", nrow(sp_15_20), "rows\n")
cat("=== Done ===\n")
