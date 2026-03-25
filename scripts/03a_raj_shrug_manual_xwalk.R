# Match Rajasthan election data to SHRUG GP module using manual crosswalks
#
# Approach:
# 1. Use district crosswalk to harmonize district names
# 2. Fuzzy match GP names within each district
# 3. Drop ambiguous matches (multiple GPs with same best match distance)

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

cat("Election data:", nrow(elex), "rows\n")
cat("Unique GPs in election data:", n_distinct(paste(elex$dist_name_new_2010, elex$gp_new_2010)), "\n")

# Normalize and join district crosswalk
elex <- elex %>%
    mutate(elex_district = tolower(dist_name_new_2010)) %>%
    left_join(dist_xwalk, by = "elex_district")

# Check for unmatched districts
unmatched_dists <- elex %>%
    filter(is.na(shrug_district)) %>%
    distinct(elex_district)
if (nrow(unmatched_dists) > 0) {
    cat("WARNING: Unmatched election districts:\n")
    print(unmatched_dists)
}

# Load SHRUG GP data
shrug <- read_csv("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv",
                  show_col_types = FALSE)

# Filter to Rajasthan GPs only
shrug_raj <- shrug %>%
    filter(state_name == "rajasthan", local_body_type == "Gram Panchayat") %>%
    mutate(
        shrug_district = district_name,
        shrug_gp = local_body_name,
        shrug_gp_norm = normalize_string(local_body_name)
    ) %>%
    select(shrug_district, subdistrict_name, shrug_gp, shrug_gp_norm, LGD_code,
           pc11_shrid_id, shrid2, village_name)

cat("\nSHRUG GPs:", n_distinct(shrug_raj$LGD_code), "unique LGD codes\n")
cat("SHRUG villages:", nrow(shrug_raj), "rows\n")

# Aggregate SHRUG data to GP level (one row per GP)
shrug_gp <- shrug_raj %>%
    group_by(shrug_district, shrug_gp, shrug_gp_norm, LGD_code) %>%
    summarize(
        subdistrict_name = first(subdistrict_name),
        n_villages = n(),
        shrid_list = paste(unique(pc11_shrid_id), collapse = ";"),
        .groups = "drop"
    )

cat("SHRUG GPs (aggregated):", nrow(shrug_gp), "rows\n\n")

# Normalize election GP names
elex <- elex %>%
    mutate(elex_gp_norm = normalize_string(gp_new_2010))

# Function to find best GP match within a district
find_best_match <- function(elex_gp_norm, shrug_district, shrug_gp_df) {
    candidates <- shrug_gp_df %>%
        filter(shrug_district == !!shrug_district)

    if (nrow(candidates) == 0) {
        return(tibble(
            matched_gp = NA_character_,
            matched_LGD_code = NA_integer_,
            match_dist = NA_real_,
            n_ties = NA_integer_
        ))
    }

    # Calculate distances
    dists <- stringdist(elex_gp_norm, candidates$shrug_gp_norm, method = "jw")
    min_dist <- min(dists)

    # Find all candidates with the minimum distance
    best_matches <- candidates[dists == min_dist, ]

    tibble(
        matched_gp = best_matches$shrug_gp[1],
        matched_LGD_code = best_matches$LGD_code[1],
        matched_subdistrict = best_matches$subdistrict_name[1],
        match_dist = min_dist,
        n_ties = nrow(best_matches)
    )
}

# Apply matching to each election GP
cat("Matching GPs...\n")

# Create unique election GP list for matching
elex_unique <- elex %>%
    select(elex_district, shrug_district, dist_name_new_2010, samiti_name_new_2010,
           gp_new_2010, elex_gp_norm) %>%
    distinct()

cat("Unique election district-GP combinations:", nrow(elex_unique), "\n")

# Match each election GP
matches <- elex_unique %>%
    rowwise() %>%
    mutate(
        match_result = list(find_best_match(elex_gp_norm, shrug_district, shrug_gp))
    ) %>%
    unnest(match_result) %>%
    ungroup()

# Summary statistics
cat("\n=== Match Quality Summary ===\n")
cat("Total election GPs:", nrow(matches), "\n")
cat("Exact matches (dist=0):", sum(matches$match_dist == 0, na.rm = TRUE), "\n")
cat("Very good matches (dist<=0.05):", sum(matches$match_dist <= 0.05, na.rm = TRUE), "\n")
cat("Good matches (dist<=0.10):", sum(matches$match_dist <= 0.10, na.rm = TRUE), "\n")
cat("Fair matches (dist<=0.15):", sum(matches$match_dist <= 0.15, na.rm = TRUE), "\n")
cat("Matches with ties:", sum(matches$n_ties > 1, na.rm = TRUE), "\n")
cat("No match:", sum(is.na(matches$match_dist)), "\n")

# Apply distance threshold and remove ties
MAX_DIST <- 0.15  # Maximum Jaro-Winkler distance to accept

matches_clean <- matches %>%
    filter(!is.na(match_dist), match_dist <= MAX_DIST, n_ties == 1)

cat("\n=== After Filtering (dist<=", MAX_DIST, ", no ties) ===\n")
cat("Matched GPs:", nrow(matches_clean), "\n")
cat("Match rate:", round(100 * nrow(matches_clean) / nrow(elex_unique), 1), "%\n")

# Join back to full election data
elex_matched <- elex %>%
    left_join(
        matches_clean %>%
            select(dist_name_new_2010, samiti_name_new_2010, gp_new_2010,
                   matched_gp, matched_LGD_code, matched_subdistrict, match_dist),
        by = c("dist_name_new_2010", "samiti_name_new_2010", "gp_new_2010")
    )

cat("\nElection rows with matched GP:", sum(!is.na(elex_matched$matched_LGD_code)),
    "of", nrow(elex_matched), "\n")

# Match rate by district
cat("\n=== Match Rate by District ===\n")
district_stats <- elex_matched %>%
    group_by(dist_name_new_2010) %>%
    summarize(
        total = n(),
        matched = sum(!is.na(matched_LGD_code)),
        match_rate = round(100 * matched / total, 1),
        .groups = "drop"
    ) %>%
    arrange(match_rate)

print(district_stats, n = 35)

# Join SHRUG GP-level data
elex_final <- elex_matched %>%
    left_join(
        shrug_gp %>% select(LGD_code, n_villages, shrid_list),
        by = c("matched_LGD_code" = "LGD_code")
    )

# Save output
write_parquet(elex_final, "data/raj/shrug_manual_xwalk_raj_05_10.parquet")
cat("\nSaved matched data to data/raj/shrug_manual_xwalk_raj_05_10.parquet\n")

# Save match diagnostics
write_csv(matches, "data/crosswalks/raj_gp_match_diagnostics.csv")
cat("Saved match diagnostics to data/crosswalks/raj_gp_match_diagnostics.csv\n")

# Show some example matches
cat("\n=== Sample Exact Matches ===\n")
matches %>%
    filter(match_dist == 0) %>%
    select(dist_name_new_2010, gp_new_2010, matched_gp, match_dist) %>%
    head(10) %>%
    print()

cat("\n=== Sample Fuzzy Matches (0 < dist <= 0.10) ===\n")
matches %>%
    filter(match_dist > 0, match_dist <= 0.10, n_ties == 1) %>%
    select(dist_name_new_2010, gp_new_2010, matched_gp, match_dist) %>%
    head(10) %>%
    print()

cat("\n=== Sample Poor Matches (dist > 0.15) - DROPPED ===\n")
matches %>%
    filter(match_dist > 0.15) %>%
    select(dist_name_new_2010, gp_new_2010, matched_gp, match_dist) %>%
    head(10) %>%
    print()
