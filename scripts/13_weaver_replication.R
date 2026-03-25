# 13_weaver_replication.R
# Weaver Data Replication for UP 2010-2020
# Outputs: tabs/weaver_short_term.tex, tabs/weaver_long_term.tex

library(arrow)
library(dplyr)
library(fixest)
library(haven)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

# Load Weaver data
jw <- read_parquet(here("data/up/jeff_wide.parquet")) %>%
    mutate(
        across(where(is.labelled), as.numeric),
        district_block_2015 = paste(district_code_ele15, block_code_ele15, sep = "_"),
        district_block_2020 = paste(district_code_ele20, block_code_ele20, sep = "_")
    )

cat("=== Weaver Data Replication ===\n")
cat("Total GPs:", nrow(jw), "\n")
cat("Variables:", ncol(jw), "\n")
cat("Unique districts:", length(unique(jw$district_code_ele15)), "\n")
cat("Unique blocks:", length(unique(jw$block_code_ele15)), "\n\n")

# =============================================================================
# 1. SHORT-TERM EFFECTS
# =============================================================================

cat("--- Short-Term Effects ---\n")

# Filter data for each outcome year
jw_open_2015 <- jw %>% filter(reservation_female_2015 == 0)
jw_open_2020 <- jw %>% filter(reservation_female_2020 == 0)

# 2010 -> 2015: No FE
m_10_15 <- feols(winner_female_2015 ~ reservation_female_2010,
                 data = jw_open_2015, vcov = "hetero")

# 2010 -> 2015: With FE
m_10_15_fe <- feols(winner_female_2015 ~ reservation_female_2010 | district_block_2015,
                    data = jw_open_2015, vcov = "hetero")

# 2010 -> 2015: Pre-treatment covariates + FE
m_10_15_cov <- feols(winner_female_2015 ~ reservation_female_2010 +
                     turnout_2010 + total_candidates_2010 + avg_age_2010 | district_block_2015,
                     data = jw_open_2015, vcov = "hetero")

# 2015 -> 2020: No FE
m_15_20 <- feols(winner_female_2020 ~ reservation_female_2015,
                 data = jw_open_2020, vcov = "hetero")

# 2015 -> 2020: With FE (use 2020 admin codes)
m_15_20_fe <- feols(winner_female_2020 ~ reservation_female_2015 | district_block_2020,
                    data = jw_open_2020, vcov = "hetero")

# 2015 -> 2020: Pre-treatment covariates + FE
m_15_20_cov <- feols(winner_female_2020 ~ reservation_female_2015 +
                     turnout_2015 + total_candidates_2015 + avg_age_2015 | district_block_2020,
                     data = jw_open_2020, vcov = "hetero")

cat("2010->2015: No FE =", round(coef(m_10_15)["reservation_female_2010"], 4),
    ", FE =", round(coef(m_10_15_fe)["reservation_female_2010"], 4),
    ", Cov+FE =", round(coef(m_10_15_cov)["reservation_female_2010"], 4), "\n")
cat("2015->2020: No FE =", round(coef(m_15_20)["reservation_female_2015"], 4),
    ", FE =", round(coef(m_15_20_fe)["reservation_female_2015"], 4),
    ", Cov+FE =", round(coef(m_15_20_cov)["reservation_female_2015"], 4), "\n\n")

models_short_term <- list(m_10_15, m_10_15_fe, m_10_15_cov,
                          m_15_20, m_15_20_fe, m_15_20_cov)

aer_etable(models_short_term,
       title = "Short-Term Effects: Weaver UP Replication",
       label = "tab:weaver_short",
       notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in the subsequent election. Columns [i]-[iii] show effects of 2010 quota on 2015 outcomes; columns [iv]-[vi] show effects of 2015 quota on 2020 outcomes. Models [i] and [iv] use heteroskedasticity-robust SE. Models [ii] and [v] add (District, Samiti) FE. Models [iii] and [vi] add pre-treatment covariates (turnout, candidates, avg age from treatment year) with FE.",
       placement = "htbp",
       headers = c("2015", "2015 (FE)", "2015 (Cov)", "2020", "2020 (FE)", "2020 (Cov)"),
       dict = c(
           "winner_female_2015" = "Woman Elected",
           "winner_female_2020" = "Woman Elected",
           "reservation_female_2010" = "$\\text{Quota}_{t-1}$",
           "reservation_female_2015" = "$\\text{Quota}_{t-1}$",
           "turnout_2010" = "Turnout$_{t-1}$",
           "turnout_2015" = "Turnout$_{t-1}$",
           "total_candidates_2010" = "Candidates$_{t-1}$",
           "total_candidates_2015" = "Candidates$_{t-1}$",
           "avg_age_2010" = "Avg Age$_{t-1}$",
           "avg_age_2015" = "Avg Age$_{t-1}$",
           "district_block_2015" = "(District, Samiti)",
           "district_block_2020" = "(District, Samiti)"
       ),
       file = here("tabs", "weaver_short_term.tex"))

cat("Saved: tabs/weaver_short_term.tex\n\n")

# =============================================================================
# 2. LONG-TERM EFFECTS
# =============================================================================

cat("--- Long-Term Effects ---\n")

# Full interaction model: No FE
m_long <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015,
                data = jw_open_2020, vcov = "hetero")

# Full interaction model: With FE (use 2020 admin codes)
m_long_fe <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015 | district_block_2020,
                   data = jw_open_2020, vcov = "hetero")

# Full interaction model: Pre-treatment covariates + FE
m_long_cov <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015 +
                    turnout_2015 + total_candidates_2015 + avg_age_2015 | district_block_2020,
                    data = jw_open_2020, vcov = "hetero")

cat("Long-term interaction model:\n")
cat("  No FE: N =", nobs(m_long), "\n")
cat("  FE: N =", nobs(m_long_fe), "\n")
cat("  Cov+FE: N =", nobs(m_long_cov), "\n\n")

models_long_term <- list(m_long, m_long_fe, m_long_cov)

aer_etable(models_long_term,
       title = "Long-Term Effects: Weaver UP Replication",
       label = "tab:weaver_long",
       notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in 2020. All models include 2010 quota, 2015 quota, and their interaction. Model [i] uses heteroskedasticity-robust SE. Model [ii] adds (District, Samiti) FE. Model [iii] adds pre-treatment covariates (2015 turnout, candidates, avg age) with FE.",
       placement = "htbp",
       headers = c("No FE", "FE", "Cov+FE"),
       dict = c(
           "winner_female_2020" = "Woman Elected",
           "reservation_female_2010" = "$\\text{Quota}_{2010}$",
           "reservation_female_2015" = "$\\text{Quota}_{2015}$",
           "reservation_female_2010:reservation_female_2015" = "$\\text{Quota}_{2010} \\times \\text{Quota}_{2015}$",
           "turnout_2015" = "Turnout$_{2015}$",
           "total_candidates_2015" = "Candidates$_{2015}$",
           "avg_age_2015" = "Avg Age$_{2015}$",
           "district_block_2020" = "(District, Samiti)"
       ),
       file = here("tabs", "weaver_long_term.tex"))

cat("Saved: tabs/weaver_long_term.tex\n\n")

cat("=== Weaver Replication Complete ===\n")
