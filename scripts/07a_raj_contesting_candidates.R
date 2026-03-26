# 07a_raj_contesting_candidates.R
# Candidacy effects: Do more women contest after quotas?
# Outputs: consolidated candidacy effects table (raj_candidacy_combined.tex)

library(readr)
library(dplyr)
library(stringr)
library(here)
library(fixest)
library(arrow)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# =============================================================================
# LOAD AND PREPARE CANDIDATE DATA
# =============================================================================

contest <- read_csv("data/raj/source/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
colnames(contest) <- tolower(colnames(contest))

contest <- contest %>%
    mutate(across(where(is.character), tolower))

contest <- contest %>%
    mutate(women_contestants_bin = ifelse((contest$gender == "f"), 1, 0))
contest$women_contestants_bin <- as.integer(contest$women_contestants_bin)

contest$panchayatsamiti <- gsub("panchayat samiti", "", as.character(contest$panchayatsamiti))

contest <- contest %>%
    mutate(match_key_2020 = make_match_key(district, panchayatsamiti, nameofgrampanchayat))

contest <- contest %>%
    group_by(match_key_2020) %>%
    mutate(
        prop_women = mean(women_contestants_bin),
        women_present = ifelse(sum(women_contestants_bin, na.rm = TRUE) > 0, 1, 0)
    ) %>%
    ungroup() %>%
    group_by(match_key_2020) %>%
    mutate(
        num_women = sum(women_contestants_bin, na.rm = TRUE),
        total_contestants = n()
    ) %>%
    ungroup()

# Load panel data with standardized treatment variables
raj_panch <- read_parquet(here("data/raj/raj_05_20.parquet"))

contest_prop <- contest %>%
    select(match_key_2020, prop_women, women_present, num_women, total_contestants) %>%
    distinct()
raj_panch <- raj_panch %>% left_join(contest_prop, by = "match_key_2020")

# =============================================================================
# LOAD AND PREPARE VOTE SHARE DATA
# =============================================================================

winner_sarpanch <- readr::read_csv("data/raj/source/sarpanch_election_data/background/WinnerSarpanch_2020.csv")
winner_sarpanch <- winner_sarpanch %>%
    rename_with(tolower) %>%
    mutate(across(where(is.character), tolower)) %>%
    filter(electiontype != "by election")

contest_sarpanch <- readr::read_csv("data/raj/source/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
contest_sarpanch <- contest_sarpanch %>%
    rename_with(tolower) %>%
    mutate(across(where(is.character), tolower)) %>%
    filter(electiontype != "by election")

get_gender <- contest_sarpanch %>%
    select(nameofcontestingcandidate, gender) %>%
    distinct(nameofcontestingcandidate, .keep_all = TRUE) %>%
    rename(sex = gender)

winner_sarpanch <- winner_sarpanch %>%
    left_join(get_gender,
        by = c("runnerupcandidatename" = "nameofcontestingcandidate")
    ) %>%
    rename(runnerup_gender = sex)

winner_sarpanch <- winner_sarpanch %>%
    left_join(get_gender,
        by = c("winnercandidatename" = "nameofcontestingcandidate")
    ) %>%
    rename(winner_gender = sex)

winner_sarpanch <- winner_sarpanch %>%
    mutate(
        votesecurebywinner = as.numeric(gsub("[^0-9]", "", votesecurebywinner)),
        votesecurebyrunnerup = as.numeric(gsub("[^0-9]", "", votesecurebyrunnerup)),
        totalvalidvotes = as.numeric(gsub("[^0-9]", "", totalvalidvotes))
    )

winner_sarpanch <- winner_sarpanch %>%
    mutate(
        fem_vote_share = case_when(
            winner_gender == "f" & runnerup_gender == "f" ~
                (votesecurebywinner + votesecurebyrunnerup) / totalvalidvotes,
            winner_gender == "f" & runnerup_gender != "f" ~
                votesecurebywinner / totalvalidvotes,
            winner_gender != "f" & runnerup_gender == "f" ~
                votesecurebyrunnerup / totalvalidvotes,
            TRUE ~ 0
        )
    )

winner_sarpanch$panchayatsamiti <- gsub("panchayat samiti", "", as.character(winner_sarpanch$panchayatsamiti))

winner_sarpanch <- winner_sarpanch %>%
    mutate(match_key_2020 = make_match_key(district, panchayatsamiti, nameofgrampanchyat))

vote_share_analysis <- winner_sarpanch %>% select(match_key_2020, fem_vote_share)
raj_panch <- raj_panch %>% left_join(vote_share_analysis, by = "match_key_2020")

# =============================================================================
# ESTIMATE ALL 8 MODELS FOR CONSOLIDATED TABLE
# =============================================================================

open_seats <- filter(raj_panch, treat_2020 == 0)

# Proportion of women candidates
m_prop <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015, data = open_seats)
m_prop_fe <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020, data = open_seats)

# Number of women candidates
m_num <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015, data = open_seats)
m_num_fe <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020, data = open_seats)

# At least one woman runs
m_oneW <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015, data = open_seats)
m_oneW_fe <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020, data = open_seats)

# Women's vote share
m_vote <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015, data = open_seats)
m_vote_fe <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020, data = open_seats)

# =============================================================================
# OUTPUT CONSOLIDATED TABLE
# =============================================================================

candidacy_dict <- c(
    "treat_2005" = "$\\text{Quota}_{2005}$",
    "treat_2010" = "$\\text{Quota}_{2010}$",
    "treat_2015" = "$\\text{Quota}_{2015}$",
    "dist_samiti_2020" = "(District, Samiti)",
    "prop_women" = "Prop. Women",
    "num_women" = "Num. Women",
    "I(women_present)" = "$\\geq$1 Woman",
    "fem_vote_share" = "Vote Share"
)

models_combined <- list(m_prop, m_prop_fe, m_num, m_num_fe, m_oneW, m_oneW_fe, m_vote, m_vote_fe)

aer_etable(
    models_combined,
    file = here("tabs", "raj_candidacy_combined.tex"),
    title = "Candidacy Effects in Open Seats (2020, Rajasthan)",
    label = "tab:candidacy_combined",
    headers = list("Prop. Women" = 2, "Num. Women" = 2, "$\\geq$1 Woman" = 2, "Vote Share" = 2),
    notes = paste0(
        "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
        "The sample is restricted to GPs where the seat was open (not reserved for women) in 2020. ",
        "Columns [i]-[ii]: proportion of women among all candidates; ",
        "Columns [iii]-[iv]: number of women candidates; ",
        "Columns [v]-[vi]: whether at least one woman ran; ",
        "Columns [vii]-[viii]: women's vote share among winner and runner-up. ",
        "Odd-numbered columns show estimates without fixed effects; even-numbered columns include (District, Samiti) FE. ",
        "Heteroskedasticity-robust standard errors."
    ),
    placement = "htbp",
    dict = candidacy_dict,
    cmidrules = list(after = 1, rules = c("2-3", "4-5", "6-7", "8-9"))
)

message("Saved consolidated candidacy table to tabs/raj_candidacy_combined.tex")
