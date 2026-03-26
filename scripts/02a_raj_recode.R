# 02a_raj_recode.R
# Create Rajasthan 2-way panels from standardized sources with crosswalks
# Input: source_YYYY_std.parquet, data/crosswalks/active/raj_district_xwalk.csv, data/crosswalks/active/raj_samiti_std.csv
# Output: raj_05_10.parquet, raj_10_15.parquet, raj_15_20.parquet, raj_05_20.parquet

library(readr)
library(dplyr)
library(arrow)
library(stringi)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Creating Rajasthan Panels ===")

join_diag <- list()
ambiguous_exports <- list()

# =============================================================================
# Load Crosswalks
# =============================================================================
message("\n--- Loading Crosswalks ---")

crosswalk_district <- read_csv(here("data/crosswalks/active/raj_district_xwalk.csv"), show_col_types = FALSE) %>%
    select(elex_district_raw, shrug_district) %>%
    rename(district_raw = elex_district_raw, district_std = shrug_district) %>%
    mutate(district_std = toupper(district_std))
crosswalk_samiti <- read_csv(here("data/crosswalks/active/raj_samiti_std.csv"), show_col_types = FALSE)

message("District crosswalk: ", nrow(crosswalk_district), "mappings")
message("Samiti crosswalk: ", nrow(crosswalk_samiti), "mappings")

# =============================================================================
# Load Standardized Sources
# =============================================================================
message("\n--- Loading Standardized Sources ---")

src_2005 <- read_parquet(here("data/raj/source_2005_std.parquet"))
src_2010 <- read_parquet(here("data/raj/source_2010_std.parquet"))
src_2015 <- read_parquet(here("data/raj/source_2015_std.parquet"))
src_2020 <- read_parquet(here("data/raj/source_2020_std.parquet"))

message("2005: ", nrow(src_2005), "rows")
message("2010: ", nrow(src_2010), "rows")
message("2015: ", nrow(src_2015), "rows")
message("2020: ", nrow(src_2020), "rows")

# =============================================================================
# Apply Crosswalks
# =============================================================================
message("\n--- Applying Crosswalks ---")

apply_crosswalks <- function(df, year_label) {
    df <- df %>%
        left_join(crosswalk_district, by = "district_raw") %>%
        mutate(district_std = ifelse(is.na(district_std), district_raw, district_std))

    df <- df %>%
        left_join(
            crosswalk_samiti %>% select(district_std, samiti_raw, samiti_std),
            by = c("district_std", "samiti_raw")
        ) %>%
        mutate(samiti_std = ifelse(is.na(samiti_std), samiti_raw, samiti_std))

    df <- df %>%
        mutate(match_key = make_match_key(district_std, samiti_std, gp_std))

    n_missing_district <- sum(is.na(df$district_std))
    n_missing_samiti <- sum(is.na(df$samiti_std))
    if (n_missing_district > 0) message("  WARNING: ", year_label, "missing district_std:", n_missing_district)
    if (n_missing_samiti > 0) message("  WARNING: ", year_label, "missing samiti_std:", n_missing_samiti)

    df
}

src_2005 <- apply_crosswalks(src_2005, "2005")
src_2010 <- apply_crosswalks(src_2010, "2010")
src_2015 <- apply_crosswalks(src_2015, "2015")
src_2020 <- apply_crosswalks(src_2020, "2020")

# =============================================================================
# Load 2020 Winner Sex Data
# =============================================================================
message("\n--- Loading 2020 Winner Sex Data ---")

cand_2020 <- read_csv(
    here("data/raj/source/sarpanch_election_data/background/ContestingSarpanch_2020.csv"),
    show_col_types = FALSE
)
winner_2020 <- read_csv(
    here("data/raj/source/sarpanch_election_data/background/WinnerSarpanch_2020.csv"),
    show_col_types = FALSE
)

winner_sex_2020 <- winner_2020 %>%
    left_join(
        cand_2020 %>% select(District, PanchayatSamiti, NameOfGramPanchayat, NameOfContestingCandidate, Gender),
        by = c("District", "PanchayatSamiti",
               "NameOfGramPanchyat" = "NameOfGramPanchayat",
               "WinnerCandidateName" = "NameOfContestingCandidate"),
        relationship = "many-to-many"
    ) %>%
    mutate(
        winner_sex_from_cand = ifelse(Gender == "F", 1L, 0L),
        ps_clean = gsub(" PANCHAYAT SAMITI$", "", PanchayatSamiti, ignore.case = TRUE)
    ) %>%
    filter(!is.na(winner_sex_from_cand)) %>%
    left_join(crosswalk_district, by = c("District" = "district_raw")) %>%
    mutate(district_std = ifelse(is.na(district_std), District, district_std)) %>%
    left_join(
        crosswalk_samiti %>% select(district_std, samiti_raw, samiti_std),
        by = c("district_std", "ps_clean" = "samiti_raw")
    ) %>%
    mutate(
        samiti_std = ifelse(is.na(samiti_std), ps_clean, samiti_std),
        gp_std = normalize_string(NameOfGramPanchyat),
        match_key = make_match_key(district_std, samiti_std, gp_std)
    ) %>%
    select(match_key, winner_sex_from_cand) %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup()

message("Winner sex data for 2020: ", nrow(winner_sex_2020), " GPs")

# =============================================================================
# Helper: Prepare Source for Panel Join
# =============================================================================
prepare_source <- function(df, year) {
    suffix <- paste0("_", year)
    df %>%
        select(
            match_key,
            district_std, samiti_std, gp_std,
            female_reserved, caste_category, winner_female, winner_name
        ) %>%
        rename_with(~ paste0(., suffix), -match_key)
}

# Prepare sources
p_2005 <- prepare_source(src_2005, 2005)
p_2010 <- prepare_source(src_2010, 2010)
p_2015 <- prepare_source(src_2015, 2015)
p_2020 <- prepare_source(src_2020, 2020)

# NOTE: create_panel helper removed - panels are created inline for clarity

# =============================================================================
# Panel 1: 2005-2010
# =============================================================================
message("\n--- Creating 2005-2010 Panel ---")
message("  Input: 2005 =", nrow(p_2005), ", 2010 =", nrow(p_2010))

raj_05_10_raw <- p_2005 %>% inner_join(p_2010, by = "match_key", relationship = "many-to-many")
n_after_join <- nrow(raj_05_10_raw)
message("  After inner_join: ", n_after_join)

ambiguous_05_10 <- raj_05_10_raw %>%
    count(match_key, name = "n_pairs") %>%
    filter(n_pairs > 1) %>%
    arrange(desc(n_pairs))

raj_05_10 <- raj_05_10_raw %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(
        treat_2005 = female_reserved_2005,
        treat_2010 = female_reserved_2010,
        case = paste0(treat_2005, treat_2010),
        female_winner_2005 = winner_female_2005,
        female_winner_2010 = winner_female_2010,
        dist_samiti_2010 = paste0(tolower(district_std_2010), "_", tolower(samiti_std_2010)),
        obc_2005 = as.integer(caste_category_2005 == "OBC"),
        sc_2005 = as.integer(caste_category_2005 == "SC"),
        st_2005 = as.integer(caste_category_2005 == "ST"),
        obc_2010 = as.integer(caste_category_2010 == "OBC"),
        sc_2010 = as.integer(caste_category_2010 == "SC"),
        st_2010 = as.integer(caste_category_2010 == "ST")
    )

n_duplicates <- n_after_join - nrow(raj_05_10)
message("  Duplicates dropped: ", n_duplicates)
message("  Final panel N: ", nrow(raj_05_10))
write_parquet(raj_05_10, here("data/raj/raj_05_10.parquet"))

join_diag[[length(join_diag) + 1]] <- tibble(
    panel = "raj_05_10",
    join_rows = n_after_join,
    final_rows = nrow(raj_05_10),
    ambiguous_keys = nrow(ambiguous_05_10),
    dropped_rows = n_duplicates
)
ambiguous_exports[[length(ambiguous_exports) + 1]] <- ambiguous_05_10 %>%
    mutate(panel = "raj_05_10")

# =============================================================================
# Panel 2: 2010-2015
# =============================================================================
message("\n--- Creating 2010-2015 Panel ---")
message("  Input: 2010 =", nrow(p_2010), ", 2015 =", nrow(p_2015))

raj_10_15_raw <- p_2010 %>% inner_join(p_2015, by = "match_key", relationship = "many-to-many")
n_after_join <- nrow(raj_10_15_raw)
message("  After inner_join: ", n_after_join)

ambiguous_10_15 <- raj_10_15_raw %>%
    count(match_key, name = "n_pairs") %>%
    filter(n_pairs > 1) %>%
    arrange(desc(n_pairs))

raj_10_15 <- raj_10_15_raw %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(
        treat_2010 = female_reserved_2010,
        treat_2015 = female_reserved_2015,
        case = paste0(treat_2010, treat_2015),
        female_winner_2010 = winner_female_2010,
        female_winner_2015 = winner_female_2015,
        dist_samiti_2015 = paste0(tolower(district_std_2015), "_", tolower(samiti_std_2015)),
        obc_2010 = as.integer(caste_category_2010 == "OBC"),
        sc_2010 = as.integer(caste_category_2010 == "SC"),
        st_2010 = as.integer(caste_category_2010 == "ST"),
        obc_2015 = as.integer(caste_category_2015 == "OBC"),
        sc_2015 = as.integer(caste_category_2015 == "SC"),
        st_2015 = as.integer(caste_category_2015 == "ST")
    )

n_duplicates <- n_after_join - nrow(raj_10_15)
message("  Duplicates dropped: ", n_duplicates)
message("  Final panel N: ", nrow(raj_10_15))
write_parquet(raj_10_15, here("data/raj/raj_10_15.parquet"))

join_diag[[length(join_diag) + 1]] <- tibble(
    panel = "raj_10_15",
    join_rows = n_after_join,
    final_rows = nrow(raj_10_15),
    ambiguous_keys = nrow(ambiguous_10_15),
    dropped_rows = n_duplicates
)
ambiguous_exports[[length(ambiguous_exports) + 1]] <- ambiguous_10_15 %>%
    mutate(panel = "raj_10_15")

# =============================================================================
# Panel 3: 2015-2020
# =============================================================================
message("\n--- Creating 2015-2020 Panel ---")
message("  Input: 2015 =", nrow(p_2015), ", 2020 =", nrow(p_2020))

raj_15_20_raw <- p_2015 %>% inner_join(p_2020, by = "match_key", relationship = "many-to-many")
n_after_join <- nrow(raj_15_20_raw)
message("  After inner_join: ", n_after_join)

ambiguous_15_20 <- raj_15_20_raw %>%
    count(match_key, name = "n_pairs") %>%
    filter(n_pairs > 1) %>%
    arrange(desc(n_pairs))

raj_15_20 <- raj_15_20_raw %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(match_key_2020 = make_match_key(district_std_2020, samiti_std_2020, gp_std_2020)) %>%
    left_join(winner_sex_2020, by = c("match_key_2020" = "match_key")) %>%
    mutate(
        treat_2015 = female_reserved_2015,
        treat_2020 = female_reserved_2020,
        case = paste0(treat_2015, treat_2020),
        female_winner_2015 = winner_female_2015,
        female_winner_2020 = coalesce(winner_sex_from_cand, NA_integer_),
        dist_samiti_2020 = paste0(tolower(district_std_2020), "_", tolower(samiti_std_2020)),
        obc_2015 = as.integer(caste_category_2015 == "OBC"),
        sc_2015 = as.integer(caste_category_2015 == "SC"),
        st_2015 = as.integer(caste_category_2015 == "ST"),
        obc_2020 = as.integer(caste_category_2020 == "OBC"),
        sc_2020 = as.integer(caste_category_2020 == "SC"),
        st_2020 = as.integer(caste_category_2020 == "ST")
    )

n_duplicates <- n_after_join - nrow(raj_15_20)
message("  Duplicates dropped: ", n_duplicates)
message("  Final panel N: ", nrow(raj_15_20))
n_winner_sex_matched <- sum(!is.na(raj_15_20$female_winner_2020))
message("  Winner sex 2020 matched: ", n_winner_sex_matched, "/", nrow(raj_15_20),
    "(", round(100 * n_winner_sex_matched / nrow(raj_15_20), 1), "%)\n")
write_parquet(raj_15_20, here("data/raj/raj_15_20.parquet"))

join_diag[[length(join_diag) + 1]] <- tibble(
    panel = "raj_15_20",
    join_rows = n_after_join,
    final_rows = nrow(raj_15_20),
    ambiguous_keys = nrow(ambiguous_15_20),
    dropped_rows = n_duplicates
)
ambiguous_exports[[length(ambiguous_exports) + 1]] <- ambiguous_15_20 %>%
    mutate(panel = "raj_15_20")

# =============================================================================
# Panel 4: Full 4-way Panel (2005-2020)
# =============================================================================
message("\n--- Creating 4-way Panel (2005-2020) ---")
message("  Input: 2005 =", nrow(p_2005), ", 2010 =", nrow(p_2010),
    ", 2015 =", nrow(p_2015), ", 2020 =", nrow(p_2020), "\n")

raj_05_20_step1 <- p_2005 %>% inner_join(p_2010, by = "match_key", relationship = "many-to-many")
message("  After 2005-2010 join: ", nrow(raj_05_20_step1))

raj_05_20_step2 <- raj_05_20_step1 %>% inner_join(p_2015, by = "match_key", relationship = "many-to-many")
message("  After adding 2015: ", nrow(raj_05_20_step2))

raj_05_20_step3 <- raj_05_20_step2 %>% inner_join(p_2020, by = "match_key", relationship = "many-to-many")
message("  After adding 2020: ", nrow(raj_05_20_step3))

raj_05_20 <- raj_05_20_step3 %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(match_key_2020 = make_match_key(district_std_2020, samiti_std_2020, gp_std_2020)) %>%
    left_join(winner_sex_2020, by = c("match_key_2020" = "match_key")) %>%
    mutate(
        treat_2005 = female_reserved_2005,
        treat_2010 = female_reserved_2010,
        treat_2015 = female_reserved_2015,
        treat_2020 = female_reserved_2020,
        female_winner_2005 = winner_female_2005,
        female_winner_2010 = winner_female_2010,
        female_winner_2015 = winner_female_2015,
        female_winner_2020 = coalesce(winner_sex_from_cand, NA_integer_),
        never_treated = as.integer(treat_2005 == 0 & treat_2010 == 0 & treat_2015 == 0),
        always_treated = as.integer(treat_2005 == 1 & treat_2010 == 1 & treat_2015 == 1),
        count_treated = treat_2005 + treat_2010 + treat_2015,
        dist_samiti_2020 = paste0(tolower(district_std_2020), "_", tolower(samiti_std_2020)),
        dist_samiti_2015 = paste0(tolower(district_std_2015), "_", tolower(samiti_std_2015)),
        dist_samiti_2010 = paste0(tolower(district_std_2010), "_", tolower(samiti_std_2010)),
        obc_2005 = as.integer(caste_category_2005 == "OBC"),
        sc_2005 = as.integer(caste_category_2005 == "SC"),
        st_2005 = as.integer(caste_category_2005 == "ST"),
        obc_2010 = as.integer(caste_category_2010 == "OBC"),
        sc_2010 = as.integer(caste_category_2010 == "SC"),
        st_2010 = as.integer(caste_category_2010 == "ST"),
        obc_2015 = as.integer(caste_category_2015 == "OBC"),
        sc_2015 = as.integer(caste_category_2015 == "SC"),
        st_2015 = as.integer(caste_category_2015 == "ST"),
        obc_2020 = as.integer(caste_category_2020 == "OBC"),
        sc_2020 = as.integer(caste_category_2020 == "SC"),
        st_2020 = as.integer(caste_category_2020 == "ST")
    )

n_duplicates <- nrow(raj_05_20_step3) - nrow(raj_05_20)
message("  Duplicates dropped: ", n_duplicates)
message("  Final panel N: ", nrow(raj_05_20))
n_winner_sex_matched <- sum(!is.na(raj_05_20$female_winner_2020))
message("  Winner sex 2020 matched: ", n_winner_sex_matched, "/", nrow(raj_05_20),
    "(", round(100 * n_winner_sex_matched / nrow(raj_05_20), 1), "%)\n")
write_parquet(raj_05_20, here("data/raj/raj_05_20.parquet"))

ambiguous_05_20 <- raj_05_20_step3 %>%
    count(match_key, name = "n_pairs") %>%
    filter(n_pairs > 1) %>%
    arrange(desc(n_pairs))

join_diag[[length(join_diag) + 1]] <- tibble(
    panel = "raj_05_20",
    join_rows = nrow(raj_05_20_step3),
    final_rows = nrow(raj_05_20),
    ambiguous_keys = nrow(ambiguous_05_20),
    dropped_rows = n_duplicates
)
ambiguous_exports[[length(ambiguous_exports) + 1]] <- ambiguous_05_20 %>%
    mutate(panel = "raj_05_20")

dir.create(here("data/crosswalks/audit"), showWarnings = FALSE, recursive = TRUE)
join_diag_df <- bind_rows(join_diag)
ambiguous_df <- bind_rows(ambiguous_exports)

write_csv(join_diag_df, here("data/crosswalks/audit/02a_raj_panel_join_diagnostics.csv"))
write_csv(ambiguous_df, here("data/crosswalks/audit/02a_raj_panel_ambiguous_keys.csv"))
message("Saved: data/crosswalks/audit/02a_raj_panel_join_diagnostics.csv")
message("Saved: data/crosswalks/audit/02a_raj_panel_ambiguous_keys.csv")

# =============================================================================
# Summary
# =============================================================================
message("\n=== Summary ===")
message("raj_05_10.parquet: ", nrow(raj_05_10), " GPs")
message("raj_10_15.parquet: ", nrow(raj_10_15), " GPs")
message("raj_15_20.parquet: ", nrow(raj_15_20), " GPs")
message("raj_05_20.parquet: ", nrow(raj_05_20), "GPs (4-way)")
message("=== Rajasthan Panels Complete ===")
