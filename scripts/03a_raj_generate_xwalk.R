# Generate initial subdistrict crosswalk using fuzzy matching
# This creates a draft that needs manual review

library(arrow)
library(dplyr)
library(stringi)
library(stringdist)
library(readr)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# Load district crosswalk
dist_xwalk <- read_csv("data/crosswalks/raj_district_xwalk.csv", show_col_types = FALSE)

# Load election data
elex <- read_parquet("data/raj/elex_raj_05_10.parquet")

# Get unique district-samiti combinations from election data
elex_samitis <- elex %>%
    select(dist_name_new_2010, samiti_name_new_2010) %>%
    distinct() %>%
    mutate(
        elex_district = tolower(dist_name_new_2010),
        elex_samiti = samiti_name_new_2010
    ) %>%
    select(elex_district, elex_samiti) %>%
    arrange(elex_district, elex_samiti)

# Load SHRUG data
shrug <- read_csv("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv",
                  show_col_types = FALSE)

# Get unique district-subdistrict combinations from SHRUG
shrug_subdists <- shrug %>%
    filter(state_name == "rajasthan", local_body_type == "Gram Panchayat") %>%
    select(district_name, subdistrict_name) %>%
    distinct() %>%
    mutate(
        shrug_district = tolower(district_name),
        shrug_subdistrict = subdistrict_name
    ) %>%
    select(shrug_district, shrug_subdistrict) %>%
    arrange(shrug_district, shrug_subdistrict)

cat("Election samitis:", nrow(elex_samitis), "\n")
cat("SHRUG subdistricts:", nrow(shrug_subdists), "\n")

# Join district crosswalk to election samitis
elex_samitis <- elex_samitis %>%
    left_join(dist_xwalk, by = "elex_district")

# For each election samiti, find best matching SHRUG subdistrict in same district
xwalk <- elex_samitis %>%
    rowwise() %>%
    mutate(
        # Get candidate SHRUG subdistricts in same district
        candidates = list(shrug_subdists %>%
            filter(shrug_district == shrug_district) %>%
            pull(shrug_subdistrict)),
        # Normalize samiti name for matching
        samiti_norm = normalize_string(elex_samiti),
        # Find best match
        best_match = {
            cands <- shrug_subdists %>%
                filter(shrug_district == .data$shrug_district) %>%
                pull(shrug_subdistrict)
            if (length(cands) == 0) {
                NA_character_
            } else {
                cands_norm <- sapply(cands, normalize_string)
                dists <- stringdist(samiti_norm, cands_norm, method = "jw")
                best_idx <- which.min(dists)
                cands[best_idx]
            }
        },
        match_dist = {
            cands <- shrug_subdists %>%
                filter(shrug_district == .data$shrug_district) %>%
                pull(shrug_subdistrict)
            if (length(cands) == 0) {
                NA_real_
            } else {
                cands_norm <- sapply(cands, normalize_string)
                min(stringdist(samiti_norm, cands_norm, method = "jw"))
            }
        }
    ) %>%
    ungroup() %>%
    select(elex_district, elex_samiti, shrug_district, shrug_subdistrict = best_match, match_dist)

# Sort by match distance (worst matches first for easier review)
xwalk <- xwalk %>%
    arrange(desc(match_dist))

cat("\nMatch quality summary:\n")
cat("Perfect matches (dist=0):", sum(xwalk$match_dist == 0, na.rm = TRUE), "\n")
cat("Good matches (dist<0.1):", sum(xwalk$match_dist < 0.1, na.rm = TRUE), "\n")
cat("Fair matches (dist<0.2):", sum(xwalk$match_dist < 0.2, na.rm = TRUE), "\n")
cat("Poor matches (dist>=0.2):", sum(xwalk$match_dist >= 0.2, na.rm = TRUE), "\n")
cat("No match:", sum(is.na(xwalk$match_dist)), "\n")

# Show worst matches for review
cat("\nWorst matches (need manual review):\n")
xwalk %>%
    filter(match_dist >= 0.15) %>%
    print(n = 50)

# Save initial crosswalk (remove match_dist column for final version)
xwalk_final <- xwalk %>%
    select(-match_dist)

write_csv(xwalk_final, "data/crosswalks/raj_subdistrict_xwalk.csv")
cat("\nSaved initial crosswalk to data/crosswalks/raj_subdistrict_xwalk.csv\n")
cat("Please review and manually correct any mismatches.\n")
