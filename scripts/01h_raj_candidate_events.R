library(arrow)
library(dplyr)
library(readr)
library(here)
source(here("scripts/00_utils.R"))

phases <- c("JAN-MAR 2020", "SEP-OCT 2020")
districts <- read_csv(here("data/crosswalks/active/raj_district_xwalk.csv"), show_col_types = FALSE) %>%
  transmute(district_raw = elex_district_raw, district_std = toupper(shrug_district))
samitis <- read_csv(here("data/crosswalks/active/raj_samiti_std.csv"), show_col_types = FALSE)

add_event_keys <- function(data, gp_column) {
  data %>%
    mutate(
      district_raw = toupper(trimws(district)),
      samiti_raw = toupper(trimws(sub(" PANCHAYAT SAMITI$", "", panchayat_samiti,
        ignore.case = TRUE
      ))),
      gp_std = normalize_string(.data[[gp_column]])
    ) %>%
    left_join(districts, by = "district_raw", relationship = "many-to-one") %>%
    mutate(district_std = coalesce(district_std, district_raw)) %>%
    left_join(samitis, by = c("district_std", "samiti_raw"), relationship = "many-to-one") %>%
    mutate(
      samiti_std = coalesce(samiti_std, samiti_raw),
      match_key = make_match_key(district_std, samiti_std, gp_std),
      event_key = if_else(is.na(match_key), NA_character_,
        paste(election_type, election_duration, match_key, sep = "|")
      )
    )
}
read_events <- function(filename, row_column) {
  read_csv(here("data/raj/source/sarpanch_election_data/background", filename),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(!!row_column := row_number()) %>%
    filter(election_type == "General Election", election_duration %in% phases) %>%
    distinct(across(-any_of(c(row_column, "sr_no"))), .keep_all = TRUE)
}
primary_reservation <- read_parquet(here("data/raj/source_2020_std.parquet")) %>%
  left_join(districts, by = "district_raw", relationship = "many-to-one") %>%
  mutate(district_std = coalesce(district_std, district_raw)) %>%
  left_join(samitis, by = c("district_std", "samiti_raw"), relationship = "many-to-one") %>%
  mutate(
    samiti_std = coalesce(samiti_std, samiti_raw),
    match_key = make_match_key(district_std, samiti_std, gp_std)
  ) %>%
  group_by(match_key) %>%
  filter(!is.na(match_key), n() == 1L) %>%
  ungroup() %>%
  transmute(match_key,
    primary_female_reserved = female_reserved,
    primary_caste_category = caste_category, primary_reservation_raw = reservation_raw
  )
add_primary_reservation <- function(data) {
  data %>%
    left_join(primary_reservation,
      by = "match_key",
      relationship = "many-to-one", na_matches = "never"
    ) %>%
    mutate(primary_reservation_available = !is.na(primary_female_reserved))
}
candidates <- read_events("ContestingSarpanch_2020.csv", "candidate_source_row") %>%
  add_event_keys("name_of_gram_panchayat") %>%
  add_primary_reservation() %>%
  group_by(match_key) %>%
  mutate(gp_event_unique = !is.na(match_key) & n_distinct(event_key) == 1L) %>%
  ungroup() %>%
  # Candidate identities use serial and parent/spouse name; winner lookup has only candidate name.
  group_by(
    event_key, contesting_candidate_serial_no, name_of_contesting_candidate,
    father_husband_of_contesting_candidate
  ) %>%
  mutate(candidate_key_unique = !is.na(event_key) & n() == 1L) %>%
  ungroup() %>%
  group_by(event_key, name_of_contesting_candidate) %>%
  mutate(
    candidate_name_unique = !is.na(event_key) & n() == 1L,
    candidate_female = case_when(
      toupper(trimws(gender)) == "F" ~ 1L,
      toupper(trimws(gender)) %in% c("M", "O") ~ 0L
    )
  ) %>%
  ungroup()
sex_lookup <- candidates %>%
  filter(candidate_name_unique, candidate_key_unique) %>%
  select(event_key, name = name_of_contesting_candidate, candidate_female)
reservation_lookup <- candidates %>%
  group_by(event_key) %>%
  summarise(
    candidate_reservation_raw = if (n_distinct(category_of_gram_panchayat,
      na.rm = TRUE
    ) == 1L) {
      first(na.omit(category_of_gram_panchayat))
    } else {
      NA_character_
    },
    candidate_reservation_unique = n_distinct(category_of_gram_panchayat,
      na.rm = TRUE
    ) == 1L,
    n_candidate_records = n(),
    n_ambiguous_candidate_records = sum(!candidate_key_unique),
    .groups = "drop"
  )
winners <- read_events("WinnerSarpanch_2020.csv", "winner_source_row") %>%
  add_event_keys("name_of_gram_panchyat") %>%
  add_primary_reservation() %>%
  group_by(event_key) %>%
  mutate(winner_key_unique = !is.na(event_key) & n() == 1L) %>%
  ungroup() %>%
  group_by(match_key) %>%
  mutate(gp_event_unique = !is.na(match_key) & n_distinct(event_key) == 1L) %>%
  ungroup() %>%
  left_join(sex_lookup,
    by = c("event_key", "winner_candidate_name" = "name"),
    relationship = "many-to-one", na_matches = "never"
  ) %>%
  rename(female_winner_2020 = candidate_female) %>%
  left_join(sex_lookup,
    by = c("event_key", "runnerup_candidate_name" = "name"),
    relationship = "many-to-one", na_matches = "never"
  ) %>%
  rename(female_runnerup_2020 = candidate_female) %>%
  left_join(reservation_lookup, by = "event_key", relationship = "many-to-one")
write_parquet(select(candidates, -any_of(c("mobile_no", "email_address"))), here("data/raj/candidates_2020_events.parquet"))
write_parquet(select(winners, -any_of("mobile_no")), here("data/raj/winners_2020_events.parquet"))
dir.create(here("data/crosswalks/audit"), recursive = TRUE, showWarnings = FALSE)
write_csv(
  candidates %>% count(candidate_key_unique, candidate_name_unique, gp_event_unique,
    observed_gender = !is.na(candidate_female)
  ),
  here("data/crosswalks/audit/01h_candidate_event_coverage.csv")
)
write_csv(
  winners %>% count(winner_key_unique, gp_event_unique,
    observed_winner_gender = !is.na(female_winner_2020),
    observed_runnerup_gender = !is.na(female_runnerup_2020)
  ),
  here("data/crosswalks/audit/01h_winner_event_coverage.csv")
)
message("Canonical 2020 records: ", nrow(candidates), " candidates, ", nrow(winners), " winners")
