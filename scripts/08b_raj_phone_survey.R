# 07d_raj_phone_survey.R
# Samples phone numbers for survey (data prep)

library(dplyr)
library(here)
library(readr)
library(writexl)

source(here("scripts/00_config.R"))

# Helper function for stratified sampling with top-up
sample_stratified <- function(data, group_vars, target_n, seed1, seed2) {
    n_groups <- data %>%
        distinct(across(all_of(group_vars))) %>%
        nrow()
    samples_per_group <- ceiling(target_n / n_groups)

    set.seed(seed1)
    sampled <- data %>%
        group_by(across(all_of(group_vars))) %>%
        sample_n(min(samples_per_group, n()), replace = FALSE) %>%
        ungroup()

    remaining <- target_n - nrow(sampled)
    if (remaining > 0) {
        set.seed(seed2)
        additional <- data %>%
            filter(!MobileNo %in% sampled$MobileNo) %>%
            sample_n(remaining, replace = FALSE)
        sampled <- bind_rows(sampled, additional)
    }
    sampled
}

# Load and prepare data
file_path <- here("data/raj/source/sarpanch_election_data/background/WinnerSarpanch_2020.csv")

raj_sarpanch <- read_csv(file_path) %>%
    filter(ElectionType == "General Election") %>%
    mutate(
        treat_2020 = if_else(
            CategoryOfGramPanchyat %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"),
            1L, 0L
        )
    )

# Quota Seats --------------------------------------------------------------

treated_units_df <- raj_sarpanch %>%
    filter(treat_2020 == 1, MobileNo != 0)

sampled_mobile_nos <- sample_stratified(
    treated_units_df,
    c("District", "CategoryOfGramPanchyat"),
    target_n = 500,
    seed1 = 12091986,
    seed2 = 12101986
)

write_xlsx(sampled_mobile_nos, here("data/raj/source/sarpanch_election_data/background/sampled_mobile_nos.xlsx"))

# Non-Quota Seats ----------------------------------------------------------

open_units_df <- raj_sarpanch %>%
    filter(treat_2020 == 0, MobileNo != 0)

sampled_mobile_nos_open <- sample_stratified(
    open_units_df,
    c("District", "CategoryOfGramPanchyat"),
    target_n = 500,
    seed1 = 12091986,
    seed2 = 12101986
)

write_xlsx(sampled_mobile_nos_open, here("data/raj/source/sarpanch_election_data/background/sampled_mobile_nos_open.xlsx"))
