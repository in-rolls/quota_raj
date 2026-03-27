# 04g_balance_fe.R
# Balance tests conditional on district fixed effects
#
# Output:
#   - tabs/balance_fe_raj.tex
#   - tabs/balance_fe_up.tex

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(arrow)
library(fixest)
library(kableExtra)
library(here)

source(here("scripts/00_config.R"))

message("=== Balance Tests with District Fixed Effects ===")

balance_vars <- c(
    "pc01_vd_t_p" = "Population",
    "pc01_vd_t_f" = "Female Pop.",
    "pc01_vd_sc_f" = "SC (Female)",
    "pc01_vd_st_f" = "ST (Female)",
    "pc01_vd_medi_fac" = "Medical fac.",
    "pc01_vd_m_home" = "Maternity homes",
    "pc01_vd_mcw_cntr" = "FW Centres",
    "pc01_vd_p_sch" = "Primary sch.",
    "pc01_vd_m_sch" = "Middle sch.",
    "pc01_vd_handpump" = "Handpumps",
    "pc01_vd_tap" = "Tap water",
    "pc01_vd_well" = "Wells",
    "pc01_vd_bank_fac" = "Banking",
    "pc01_vd_power_supl" = "Power supply",
    "pc01_vd_app_mr" = "Mud road"
)

# =============================================================================
# Helper: Run FE balance test for one panel
# =============================================================================

run_fe_balance <- function(data, treat_var, fe_var) {
    avail_vars <- intersect(names(balance_vars), names(data))

    data_clean <- data %>%
        filter(!is.na(.data[[treat_var]]), !is.na(.data[[fe_var]])) %>%
        drop_na(all_of(avail_vars))

    # Run regression for each covariate: covariate ~ treatment | district
    # Using IID SEs (no clustering - this is a balance test, not causal inference)
    results <- map_dfr(avail_vars, function(v) {
        fml <- as.formula(paste0(v, " ~ ", treat_var, " | ", fe_var))
        mod <- feols(fml, data = data_clean, vcov = "iid", warn = FALSE, notes = FALSE)
        tidy(mod) %>%
            filter(term == treat_var) %>%
            mutate(variable = v)
    })

    # Joint Wald test with IID SEs
    fml_joint <- as.formula(paste0(treat_var, " ~ ", paste(avail_vars, collapse = " + "), " | ", fe_var))
    mod_joint <- feols(fml_joint, data = data_clean, vcov = "iid", warn = FALSE, notes = FALSE)
    joint_p <- tryCatch({
        wald(mod_joint, keep = avail_vars)$p
    }, error = function(e) NA_real_)

    list(
        results = results %>%
            mutate(label = balance_vars[variable]) %>%
            select(label, estimate, std.error, p.value),
        joint_p = joint_p,
        n = nrow(data_clean)
    )
}

# =============================================================================
# Rajasthan
# =============================================================================

message("\n=== RAJASTHAN ===")

raj_0510 <- run_fe_balance(
    read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")),
    "treat_2005", "district_std_2010"
)
raj_1015 <- run_fe_balance(
    read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")),
    "treat_2010", "district_std_2015"
)
raj_1520 <- run_fe_balance(
    read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")),
    "treat_2015", "district_std_2020"
)

# Combine into wide format
raj_table <- raj_0510$results %>%
    rename(coef_0510 = estimate, se_0510 = std.error, p_0510 = p.value) %>%
    left_join(
        raj_1015$results %>%
            rename(coef_1015 = estimate, se_1015 = std.error, p_1015 = p.value),
        by = "label"
    ) %>%
    left_join(
        raj_1520$results %>%
            rename(coef_1520 = estimate, se_1520 = std.error, p_1520 = p.value),
        by = "label"
    )

# Add footer row
raj_footer <- tibble(
    label = c("N", "Joint p (Wald)"),
    coef_0510 = c(raj_0510$n, raj_0510$joint_p),
    se_0510 = NA, p_0510 = NA,
    coef_1015 = c(raj_1015$n, raj_1015$joint_p),
    se_1015 = NA, p_1015 = NA,
    coef_1520 = c(raj_1520$n, raj_1520$joint_p),
    se_1520 = NA, p_1520 = NA
)

raj_out <- bind_rows(raj_table, raj_footer) %>%
    mutate(across(where(is.numeric), ~round(., 2)))

kable(raj_out,
      format = "latex",
      booktabs = TRUE,
      col.names = c("Variable", rep(c("Coef", "SE", "p"), 3)),
      caption = "Rajasthan: Balance Tests with District Fixed Effects",
      label = "balance_fe_raj",
      align = c("l", rep("r", 9)),
      escape = FALSE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,
                       "2005→2010" = 3,
                       "2010→2015" = 3,
                       "2015→2020" = 3)) %>%
    footnote(general = "Each row: coefficient on treatment from regression of covariate on treatment with district FE.") %>%
    save_kable(here("tabs/balance_fe_raj.tex"))

message("Created: tabs/balance_fe_raj.tex")

# =============================================================================
# Uttar Pradesh
# =============================================================================

message("\n=== UTTAR PRADESH ===")

up_0510 <- run_fe_balance(
    read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")),
    "treat_2005", "district_name_eng_2010"
)
up_1015 <- run_fe_balance(
    read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")),
    "treat_2010", "district_name_eng_2015"
)
up_1521 <- run_fe_balance(
    read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")),
    "treat_2015", "district_name_eng_2021"
)

up_table <- up_0510$results %>%
    rename(coef_0510 = estimate, se_0510 = std.error, p_0510 = p.value) %>%
    left_join(
        up_1015$results %>%
            rename(coef_1015 = estimate, se_1015 = std.error, p_1015 = p.value),
        by = "label"
    ) %>%
    left_join(
        up_1521$results %>%
            rename(coef_1521 = estimate, se_1521 = std.error, p_1521 = p.value),
        by = "label"
    )

up_footer <- tibble(
    label = c("N", "Joint p (Wald)"),
    coef_0510 = c(up_0510$n, up_0510$joint_p),
    se_0510 = NA, p_0510 = NA,
    coef_1015 = c(up_1015$n, up_1015$joint_p),
    se_1015 = NA, p_1015 = NA,
    coef_1521 = c(up_1521$n, up_1521$joint_p),
    se_1521 = NA, p_1521 = NA
)

up_out <- bind_rows(up_table, up_footer) %>%
    mutate(across(where(is.numeric), ~round(., 2)))

kable(up_out,
      format = "latex",
      booktabs = TRUE,
      col.names = c("Variable", rep(c("Coef", "SE", "p"), 3)),
      caption = "Uttar Pradesh: Balance Tests with District Fixed Effects",
      label = "balance_fe_up",
      align = c("l", rep("r", 9)),
      escape = FALSE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,
                       "2005→2010" = 3,
                       "2010→2015" = 3,
                       "2015→2021" = 3)) %>%
    footnote(general = "Each row: coefficient on treatment from regression of covariate on treatment with district FE.") %>%
    save_kable(here("tabs/balance_fe_up.tex"))

message("Created: tabs/balance_fe_up.tex")

message("\n=== FE Balance Tests Complete ===")
