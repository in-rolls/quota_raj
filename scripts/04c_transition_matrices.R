# 04c_transition_matrices.R
# Transition matrices + chi-squared tests for quota randomization
# Consolidated script for both Rajasthan and UP

library(readr)
library(arrow)
library(tidyverse)
library(kableExtra)
library(here)
library(fixest)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Transition Matrices and Chi-Squared Tests ===")

# =============================================================================
# RAJASTHAN
# =============================================================================

message("\n--- Rajasthan ---")

raj_panch <- read_parquet(here("data/raj/raj_05_20.parquet"))

raj_trans_matrices <- list(
    `2005-2010` = make_transition_matrix(raj_panch, treat_2005, treat_2010),
    `2010-2015` = make_transition_matrix(raj_panch, treat_2010, treat_2015),
    `2015-2020` = make_transition_matrix(raj_panch, treat_2015, treat_2020),
    `2005-2015` = make_transition_matrix(raj_panch, treat_2005, treat_2015),
    `2005-2020` = make_transition_matrix(raj_panch, treat_2005, treat_2020),
    `2010-2020` = make_transition_matrix(raj_panch, treat_2010, treat_2020)
)

raj_chi_results <- purrr::imap_dfr(raj_trans_matrices, function(mat, name) {
    test <- chisq.test(mat)
    broom::tidy(test) %>%
        mutate(Comparison = name)
}) %>%
    select(Comparison, statistic, p.value)

message("Rajasthan Chi-Squared Results:")
print(raj_chi_results)

raj_chi_results %>%
    mutate(
        statistic = round(statistic, 2),
        p.value = round(p.value, 2)
    ) %>%
    knitr::kable(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Comparison", "Chi-Squared", "P-Value"),
        caption = "Chi-Squared Tests for Treatment Rotation (Rajasthan)",
        label = "chi_sq_raj"
    ) %>%
    kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) %>%
    save_kable(here("tabs", "reserved_or_not_chi_sq_raj.tex"))

message("Saved: tabs/reserved_or_not_chi_sq_raj.tex")

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
message("Saved: tabs/raj_treatment_reg.tex")

# =============================================================================
# UTTAR PRADESH
# =============================================================================

message("\n--- Uttar Pradesh ---")

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
    select(Comparison, statistic, p.value)

message("UP Chi-Squared Results:")
print(up_chi_results)

up_chi_results %>%
    mutate(
        statistic = round(statistic, 2),
        p.value = round(p.value, 2)
    ) %>%
    knitr::kable(
        format = "latex",
        booktabs = TRUE,
        col.names = c("Comparison", "Chi-Squared", "P-Value"),
        caption = "Chi-Squared Tests for Treatment Rotation (UP)",
        label = "chi_sq_up"
    ) %>%
    kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8) %>%
    save_kable(here("tabs", "up_chi_squared_results.tex"))

message("Saved: tabs/up_chi_squared_results.tex")

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
message("Saved: tabs/up_treatment_reg.tex")

message("\n=== Done ===")
