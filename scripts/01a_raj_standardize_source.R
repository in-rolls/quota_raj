# 01a_raj_standardize_source.R
# Standardize 4 raw Rajasthan election source files
# Output: data/raj/source_YYYY_std.parquet for each year

library(readr)
library(dplyr)
library(arrow)
library(stringi)
library(here)

source(here("scripts/00_config.R"))

cat("=== Standardizing Rajasthan Source Files ===\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

standardize_reservation <- function(res) {
    res_upper <- toupper(trimws(res))

    female_reserved <- as.integer(grepl("W|WOMAN", res_upper))

    caste_category <- case_when(
        grepl("^GEN|GENERAL", res_upper) ~ "GEN",
        grepl("^OBC|OTHER BACKWARD", res_upper) ~ "OBC",
        grepl("^SC[^H]|^SC$|SCHEDULED CASTE", res_upper) ~ "SC",
        grepl("^ST|SCHEDULED TRIBE", res_upper) ~ "ST",
        TRUE ~ NA_character_
    )

    list(female_reserved = female_reserved, caste_category = caste_category)
}

standardize_sex <- function(sex, category = NULL, infer_from_category = FALSE) {
    if (!is.null(sex) && !all(is.na(sex) | sex == "")) {
        sex_upper <- toupper(trimws(sex))
        return(as.integer(sex_upper %in% c("F", "FEMALE", "1")))
    }
    if (infer_from_category && !is.null(category)) {
        return(as.integer(grepl("WOMAN|W$|\\(W\\)", toupper(category))))
    }
    NA_integer_
}

normalize_gp_name <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
    x <- stri_trans_tolower(x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    x <- gsub("[[:punct:]]", "", x)
    x
}

# =============================================================================
# 2005 SOURCE
# =============================================================================
cat("\n--- Processing 2005 source ---\n")

raw_2005 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2005.csv"),
    show_col_types = FALSE
)
cat("Read", nrow(raw_2005), "rows from 2005 source\n")

res_2005 <- mapply(function(r) standardize_reservation(r), raw_2005$reservation, SIMPLIFY = FALSE)

std_2005 <- raw_2005 %>%
    mutate(
        year = 2005L,
        district_raw = toupper(trimws(dist_name)),
        samiti_raw = toupper(trimws(samiti_name)),
        gp_raw = toupper(trimws(gp)),
        gp_std = normalize_gp_name(gp_new),
        winner_name = name,
        winner_female = standardize_sex(sex),
        female_reserved = sapply(res_2005, function(x) x$female_reserved),
        caste_category = sapply(res_2005, function(x) x$caste_category),
        reservation_raw = reservation
    ) %>%
    select(
        year, district_raw, samiti_raw, gp_raw, gp_std,
        winner_name, winner_female, female_reserved, caste_category, reservation_raw
    )

cat("  District raw values:", n_distinct(std_2005$district_raw), "\n")
cat("  Samiti raw values:", n_distinct(std_2005$samiti_raw), "\n")
cat("  Female reserved:", sum(std_2005$female_reserved, na.rm = TRUE), "/", nrow(std_2005), "\n")

write_parquet(std_2005, here("data/raj/source_2005_std.parquet"))
cat("  Saved: data/raj/source_2005_std.parquet\n")

# =============================================================================
# 2010 SOURCE
# =============================================================================
cat("\n--- Processing 2010 source ---\n")

raw_2010 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2010.csv"),
    show_col_types = FALSE
)
cat("Read", nrow(raw_2010), "rows from 2010 source\n")

res_2010 <- mapply(function(r) standardize_reservation(r), raw_2010$reservation, SIMPLIFY = FALSE)

std_2010 <- raw_2010 %>%
    mutate(
        year = 2010L,
        district_raw = toupper(trimws(dist_name)),
        samiti_raw = toupper(trimws(samiti_name)),
        gp_raw = toupper(trimws(gp)),
        gp_std = normalize_gp_name(gp_new),
        winner_name = name,
        winner_female = standardize_sex(sex),
        female_reserved = sapply(res_2010, function(x) x$female_reserved),
        caste_category = sapply(res_2010, function(x) x$caste_category),
        reservation_raw = reservation
    ) %>%
    select(
        year, district_raw, samiti_raw, gp_raw, gp_std,
        winner_name, winner_female, female_reserved, caste_category, reservation_raw
    )

cat("  District raw values:", n_distinct(std_2010$district_raw), "\n")
cat("  Samiti raw values:", n_distinct(std_2010$samiti_raw), "\n")
cat("  Female reserved:", sum(std_2010$female_reserved, na.rm = TRUE), "/", nrow(std_2010), "\n")

write_parquet(std_2010, here("data/raj/source_2010_std.parquet"))
cat("  Saved: data/raj/source_2010_std.parquet\n")

# =============================================================================
# 2015 SOURCE
# =============================================================================
cat("\n--- Processing 2015 source ---\n")

raw_2015 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2015_manual_sex.csv"),
    show_col_types = FALSE
)
cat("Read", nrow(raw_2015), "rows from 2015 source\n")

res_2015 <- mapply(function(r) standardize_reservation(r), raw_2015$reservation, SIMPLIFY = FALSE)

std_2015 <- raw_2015 %>%
    mutate(
        year = 2015L,
        district_raw = toupper(trimws(dist_name)),
        samiti_raw = toupper(trimws(samiti_name)),
        gp_raw = toupper(trimws(gp)),
        gp_std = normalize_gp_name(gp_new),
        winner_name = name,
        winner_female = standardize_sex(sex_manual),
        female_reserved = sapply(res_2015, function(x) x$female_reserved),
        caste_category = sapply(res_2015, function(x) x$caste_category),
        reservation_raw = reservation
    ) %>%
    select(
        year, district_raw, samiti_raw, gp_raw, gp_std,
        winner_name, winner_female, female_reserved, caste_category, reservation_raw
    )

cat("  District raw values:", n_distinct(std_2015$district_raw), "\n")
cat("  Samiti raw values:", n_distinct(std_2015$samiti_raw), "\n")
cat("  Female reserved:", sum(std_2015$female_reserved, na.rm = TRUE), "/", nrow(std_2015), "\n")
cat("  Winner female inferred:", sum(std_2015$winner_female == 1, na.rm = TRUE), "\n")
cat("  Winner male inferred:", sum(std_2015$winner_female == 0, na.rm = TRUE), "\n")
cat("  Winner sex unknown:", sum(is.na(std_2015$winner_female)), "\n")

write_parquet(std_2015, here("data/raj/source_2015_std.parquet"))
cat("  Saved: data/raj/source_2015_std.parquet\n")

# =============================================================================
# 2020 SOURCE
# =============================================================================
cat("\n--- Processing 2020 source ---\n")

raw_2020 <- read_csv(
    here("data/raj/source/sarpanch_election_data/sarpanch_2020.csv"),
    show_col_types = FALSE
)
cat("Read", nrow(raw_2020), "rows from 2020 source\n")

res_2020 <- mapply(function(r) standardize_reservation(r), raw_2020$reservation_2020, SIMPLIFY = FALSE)

std_2020 <- raw_2020 %>%
    mutate(
        year = 2020L,
        district_raw = toupper(trimws(district_2020)),
        samiti_raw = toupper(trimws(ps_2020)),
        gp_raw = toupper(trimws(gp_2020)),
        gp_std = normalize_gp_name(gp_2020),
        winner_name = NA_character_,
        winner_female = NA_integer_,
        female_reserved = sapply(res_2020, function(x) x$female_reserved),
        caste_category = sapply(res_2020, function(x) x$caste_category),
        reservation_raw = reservation_2020
    ) %>%
    select(
        year, district_raw, samiti_raw, gp_raw, gp_std,
        winner_name, winner_female, female_reserved, caste_category, reservation_raw
    )

cat("  District raw values:", n_distinct(std_2020$district_raw), "\n")
cat("  Samiti raw values:", n_distinct(std_2020$samiti_raw), "\n")
cat("  Female reserved:", sum(std_2020$female_reserved, na.rm = TRUE), "/", nrow(std_2020), "\n")

write_parquet(std_2020, here("data/raj/source_2020_std.parquet"))
cat("  Saved: data/raj/source_2020_std.parquet\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== Summary ===\n")
cat("source_2005_std.parquet:", nrow(std_2005), "rows\n")
cat("source_2010_std.parquet:", nrow(std_2010), "rows\n")
cat("source_2015_std.parquet:", nrow(std_2015), "rows\n")
cat("source_2020_std.parquet:", nrow(std_2020), "rows\n")
cat("=== Standardization Complete ===\n")
