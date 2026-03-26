# 07b_candidate_quality.R
# Candidate quality: t-tests comparing quota vs open candidates
# Consolidated script for both Rajasthan and UP

library(tidyverse)
library(broom)
library(psych)
library(haven)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Candidate Quality Analysis ===")

# =============================================================================
# RAJASTHAN
# =============================================================================

message("\n--- Rajasthan 2020 ---")

contestants <- read_csv(here("data/raj/source/sarpanch_election_data/background/ContestingSarpanch_2020.csv"),
                         show_col_types = FALSE)
names(contestants) <- tolower(names(contestants))
contestants <- contestants %>%
    mutate(across(where(is.character), tolower)) %>%
    filter(electiontype != "by election") %>%
    mutate(
        treat = ifelse(categoryofgrampanchayat %in%
            c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
        win_assets = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE),
        grad_status = ifelse(educationstatus %in%
            c("postgraduate", "graduate", "professional graduate", "professional post graduate"), 1, 0),
        total_children = as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995),
        age = as.integer(age),
        unemployed = ifelse(contestingcandidateoccupation == "unemployed", 1, 0),
        log_assets = log(win_assets + 1)
    )

winners_ref <- read_csv(here("data/raj/source/sarpanch_election_data/background/WinnerSarpanch_2020.csv"),
                         show_col_types = FALSE)
names(winners_ref) <- tolower(names(winners_ref))
winners_ref <- winners_ref %>%
    mutate(across(where(is.character), tolower)) %>%
    filter(electiontype != "by election")

contestants <- contestants %>%
    mutate(is_winner = ifelse(
        paste(key, nameofcontestingcandidate, sep = "|||") %in%
            paste(winners_ref$key_2020, winners_ref$winnercandidatename, sep = "|||"), 1, 0
    ))

raj_vars <- c("age", "total_children", "grad_status", "unemployed", "log_assets")
raj_labels <- c("Age", "Total Children", "Graduation Status", "Unemployed", "Assets (log)")

winners <- contestants %>% filter(is_winner == 1)
winner_results <- run_t_tests(winners, raj_vars, raj_labels)
cand_results <- run_t_tests(contestants, raj_vars, raj_labels)

make_balance_table(
    dfs = list(winner_results, cand_results),
    group_names = c("Winners", "All Candidates"),
    notes = paste0(
        "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
        "T-tests comparing characteristics in open vs quota seats. ",
        "Data from Rajasthan 2020 panchayat elections. Assets winsorized at 10\\%."
    ),
    out = here("tabs", "cand_characteristics_combined.tex")
)
message("Saved: tabs/cand_characteristics_combined.tex")

raj_member_reply <- read_csv(here("data/raj/source/phone_survey_response/member_answered_phone.csv"),
                              show_col_types = FALSE)
names(raj_member_reply) <- tolower(names(raj_member_reply))
raj_member_reply <- raj_member_reply %>% mutate(across(where(is.character), tolower))

winners$key_clean <- trimws(winners$key)
raj_member_reply$key_clean <- trimws(raj_member_reply$key)

respondents <- winners %>% filter(key_clean %in% raj_member_reply$key_clean)
mean_values_df <- make_mean_table(respondents, raj_vars, raj_labels)

custom_stargazer(mean_values_df,
    summary = FALSE, rownames = FALSE,
    colnames = c("Variable", "Mean"),
    title = "Characteristics of Representatives Who Answered Phone Calls",
    label = "tab:phone_reply_char",
    notes = paste0("Characteristics of representatives in quota seats who answered our phone calls. N = ", nrow(respondents), "."),
    out = here("tabs", "mean_values_respondents.tex")
)
message("Saved: tabs/mean_values_respondents.tex")

# =============================================================================
# UTTAR PRADESH
# =============================================================================

message("\n--- Uttar Pradesh ---")

jw <- read_dta(here("data/up/weaver_data_2.dta"))

jw <- jw %>%
    mutate(year = case_when(
        election == -1 ~ 2010,
        election == 0 ~ 2015,
        election == 1 ~ 2020
    ))

up_vars <- c("winner_age", "winner_education", "winner_total_assets_asinh")
up_labels <- c("Age", "Education", "Assets (asinh)")

results_2010 <- run_t_tests(filter(jw, year == 2010), up_vars, up_labels, treat_var = "reservation_female")
results_2015 <- run_t_tests(filter(jw, year == 2015), up_vars, up_labels, treat_var = "reservation_female")
results_2020 <- run_t_tests(filter(jw, year == 2020), up_vars, up_labels, treat_var = "reservation_female")

make_balance_table(
    dfs = list(results_2010, results_2015, results_2020),
    group_names = c("2010", "2015", "2020"),
    notes = paste0(
        "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
        "Data from \\citet{weaver_data}. ",
        "T-tests comparing winner characteristics in open vs quota seats. ",
        "Education is ordinal (1=Primary to 9=Doctorate). ",
        "Assets (asinh transformation) available only for 2020."
    ),
    out = here("tabs", "up_cand_characteristics.tex")
)
message("Saved: tabs/up_cand_characteristics.tex")

message("\n=== Done ===")
