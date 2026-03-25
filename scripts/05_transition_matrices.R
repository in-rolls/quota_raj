# 05_transition_matrices.R
# Transition matrices + chi-squared tests for quota randomization
# Consolidated script for both Rajasthan and UP

library(readr)
library(arrow)
library(tibble)
library(tidyverse)
library(kableExtra)
library(here)
library(fixest)
library(broom)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Transition Matrices and Chi-Squared Tests ===\n")

# =============================================================================
# RAJASTHAN
# =============================================================================

cat("\n--- Rajasthan ---\n")

raj_panch <- read_parquet(here("data/raj/raj_05_20.parquet"))

raj_trans_matrices <- list(
    `2005-2010` = make_transition_matrix(raj_panch, female_res_2005, female_res_2010),
    `2010-2015` = make_transition_matrix(raj_panch, female_res_2010, female_res_2015),
    `2015-2020` = make_transition_matrix(raj_panch, female_res_2015, female_res_2020),
    `2005-2015` = make_transition_matrix(raj_panch, female_res_2005, female_res_2015),
    `2005-2020` = make_transition_matrix(raj_panch, female_res_2005, female_res_2020),
    `2010-2020` = make_transition_matrix(raj_panch, female_res_2010, female_res_2020)
)

raj_chi_results <- purrr::imap_dfr(raj_trans_matrices, function(mat, name) {
    test <- chisq.test(mat)
    broom::tidy(test) %>%
        mutate(Comparison = name)
}) %>%
    select(Comparison, statistic, parameter, p.value)

cat("Rajasthan Chi-Squared Results:\n")
print(raj_chi_results)

raj_chi_results %>%
    knitr::kable(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Comparison", "Chi-Squared", "DF", "P-Value"),
        caption = "Are Reservations Predictable Over Time (Rajasthan)",
        label = "chi_square_reserved_over_time"
    ) %>%
    kableExtra::kable_styling(latex_options = c("hold_position")) %>%
    save_kable(here("tabs", "reserved_or_not_chi_sq_raj.tex"))

cat("Saved: tabs/reserved_or_not_chi_sq_raj.tex\n")

raj_panch <- read_parquet(here("data/raj/raj_05_20.parquet"))

raj_models <- list(
    feols(treat_2010 ~ treat_2005, data = raj_panch),
    feols(treat_2015 ~ treat_2010, data = raj_panch),
    feols(treat_2020 ~ treat_2015, data = raj_panch),
    feols(treat_2020 ~ treat_2005 + treat_2010 + treat_2015, data = raj_panch)
)

aer_etable(raj_models,
    file = here("tabs", "raj_treatment_reg.tex"),
    dict = DICT_RAJ_TREAT
)
cat("Saved: tabs/raj_treatment_reg.tex\n")

# =============================================================================
# UTTAR PRADESH
# =============================================================================

cat("\n--- Uttar Pradesh ---\n")

up_all <- read_parquet(here("data/up/up_05_21.parquet"))

up_trans_matrices <- list(
    `2005-2010` = make_transition_matrix(up_all, treat_2005, treat_2010),
    `2010-2015` = make_transition_matrix(up_all, treat_2010, treat_2015),
    `2015-2021` = make_transition_matrix(up_all, treat_2015, treat_2021),
    `2005-2015` = make_transition_matrix(up_all, treat_2005, treat_2015),
    `2005-2021` = make_transition_matrix(up_all, treat_2005, treat_2021),
    `2010-2021` = make_transition_matrix(up_all, treat_2010, treat_2021)
)

up_chi_results <- purrr::imap_dfr(up_trans_matrices, function(mat, name) {
    test <- chisq.test(mat)
    broom::tidy(test) %>%
        mutate(Comparison = name)
}) %>%
    select(Comparison, statistic, parameter, p.value)

cat("UP Chi-Squared Results:\n")
print(up_chi_results)

up_chi_results %>%
    knitr::kable(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Comparison", "Chi-Squared Statistic", "Degrees of Freedom", "P-Value"),
        caption = "Chi-Squared Tests for Treatment Rotation in UP",
        label = "tab:up_chi_squared_results"
    ) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), font_size = 8) %>%
    save_kable(here("tabs", "up_chi_squared_results.tex"))

cat("Saved: tabs/up_chi_squared_results.tex\n")

up_models <- list(
    feols(treat_2010 ~ treat_2005, data = up_all),
    feols(treat_2015 ~ treat_2010, data = up_all),
    feols(treat_2021 ~ treat_2015, data = up_all),
    feols(treat_2021 ~ treat_2005 + treat_2010 + treat_2015, data = up_all)
)

aer_etable(up_models,
    file = here("tabs", "up_treatment_reg.tex"),
    dict = DICT_UP_TREAT
)
cat("Saved: tabs/up_treatment_reg.tex\n")

cat("\n=== Done ===\n")
