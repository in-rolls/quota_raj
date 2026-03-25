# 07e_het_effects.R
# Consolidated heterogeneous effects table for ALL panels
# All three interactions (near_town, high_infra, has_power) in single models
# Uses new SHRUG-matched data from block matching (02c_shrug_all_panels_block.R)
# Output: tabs/het_effects_combined.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Consolidated Heterogeneous Effects Analysis (All Panels) ===\n")

# =============================================================================
# Helper function: add heterogeneity variables
# =============================================================================

add_het_vars <- function(df) {
    df %>%
        mutate(
            near_town = ifelse(pc01_vd_dist_town <= median(pc01_vd_dist_town, na.rm = TRUE), 1, 0),
            infra_index = rowSums(across(c(pc01_vd_edu_fac, pc01_vd_medi_fac,
                                           pc01_vd_power_supl, pc01_vd_bank_fac),
                                         ~as.numeric(. == 1)), na.rm = TRUE),
            high_infra = ifelse(infra_index > median(infra_index, na.rm = TRUE), 1, 0),
            has_power = as.numeric(pc01_vd_power_supl == 1)
        )
}

# =============================================================================
# RAJASTHAN: Load and prepare new SHRUG-matched data (block matching)
# =============================================================================

cat("\n--- Rajasthan ---\n")

raj_shrug_05_10 <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")) %>%
    add_het_vars()

raj_shrug_10_15 <- read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")) %>%
    add_het_vars()

raj_shrug_15_20 <- read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")) %>%
    add_het_vars()

cat("Raj 05-10 rows:", nrow(raj_shrug_05_10), "\n")
cat("Raj 10-15 rows:", nrow(raj_shrug_10_15), "\n")
cat("Raj 15-20 rows:", nrow(raj_shrug_15_20), "\n")

# =============================================================================
# UTTAR PRADESH: Load and prepare SHRUG-matched data for all panels
# =============================================================================

cat("\n--- Uttar Pradesh ---\n")

up_shrug_05_10 <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")) %>%
    mutate(woman_elected_2010 = as.integer(sex_2010 == "महिला")) %>%
    add_het_vars()

up_shrug_10_15 <- read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")) %>%
    mutate(female_winner_2015 = as.integer(sex_2015 == "महिला")) %>%
    add_het_vars()

up_shrug_15_21 <- read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")) %>%
    mutate(female_winner_2021 = as.integer(sex_2021 == "महिला")) %>%
    add_het_vars()

cat("UP 05-10 GPs:", nrow(up_shrug_05_10), "\n")
cat("UP 10-15 rows:", nrow(up_shrug_10_15), "\n")
cat("UP 15-21 rows:", nrow(up_shrug_15_21), "\n")

# =============================================================================
# Filter to open seats and complete cases
# =============================================================================

raj_05_10_open <- raj_shrug_05_10 %>%
    filter(female_res_2010 == 0) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

raj_10_15_open <- raj_shrug_10_15 %>%
    filter(female_res_2015 == 0) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

raj_15_20_open <- raj_shrug_15_20 %>%
    filter(female_res_2020 == 0 & !is.na(female_winner_2020)) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

up_05_10_open <- up_shrug_05_10 %>%
    filter(treat_2010 == 0) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

up_10_15_open <- up_shrug_10_15 %>%
    filter(treat_2015 == 0) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

up_15_21_open <- up_shrug_15_21 %>%
    filter(treat_2021 == 0) %>%
    filter(!is.na(near_town) & !is.na(high_infra) & !is.na(has_power))

cat("\nRaj 05-10 open seats with all het vars:", nrow(raj_05_10_open), "\n")
cat("Raj 10-15 open seats with all het vars:", nrow(raj_10_15_open), "\n")
cat("Raj 15-20 open seats with all het vars:", nrow(raj_15_20_open), "\n")
cat("UP 05-10 open seats with all het vars:", nrow(up_05_10_open), "\n")
cat("UP 10-15 open seats with all het vars:", nrow(up_10_15_open), "\n")
cat("UP 15-21 open seats with all het vars:", nrow(up_15_21_open), "\n")

# =============================================================================
# Run combined interaction models for ALL panels
# =============================================================================

cat("\n=== Running consolidated models ===\n")

models <- list()

# Rajasthan 05-10: No FE
models[[1]] <- feols(
    female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * has_power,
    data = raj_05_10_open
)

# Rajasthan 05-10: With FE
models[[2]] <- feols(
    female_winner_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * has_power | dist_samiti_2010,
    data = raj_05_10_open
)

# Rajasthan 10-15: No FE
models[[3]] <- feols(
    female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * has_power,
    data = raj_10_15_open
)

# Rajasthan 10-15: With FE
models[[4]] <- feols(
    female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * has_power | dist_samiti_2015,
    data = raj_10_15_open
)

# Rajasthan 15-20: No FE
models[[5]] <- feols(
    female_winner_2020 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * has_power,
    data = raj_15_20_open
)

# Rajasthan 15-20: With FE
models[[6]] <- feols(
    female_winner_2020 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * has_power | dist_samiti_2020,
    data = raj_15_20_open
)

# UP 05-10: No FE
models[[7]] <- feols(
    woman_elected_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * has_power,
    data = up_05_10_open
)

# UP 05-10: With FE
models[[8]] <- feols(
    woman_elected_2010 ~ treat_2005 * near_town + treat_2005 * high_infra + treat_2005 * has_power | dist_block_2010,
    data = up_05_10_open
)

# UP 10-15: No FE
models[[9]] <- feols(
    female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * has_power,
    data = up_10_15_open
)

# UP 10-15: With FE
models[[10]] <- feols(
    female_winner_2015 ~ treat_2010 * near_town + treat_2010 * high_infra + treat_2010 * has_power | dist_block_2015,
    data = up_10_15_open
)

# UP 15-21: No FE
models[[11]] <- feols(
    female_winner_2021 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * has_power,
    data = up_15_21_open
)

# UP 15-21: With FE
models[[12]] <- feols(
    female_winner_2021 ~ treat_2015 * near_town + treat_2015 * high_infra + treat_2015 * has_power | dist_block_2021,
    data = up_15_21_open
)

# =============================================================================
# Output table
# =============================================================================

dict_combined <- c(
    "female_winner_2010" = "Woman Elected",
    "female_winner_2015" = "Woman Elected",
    "female_winner_2020" = "Woman Elected",
    "female_winner_2021" = "Woman Elected",
    "woman_elected_2010" = "Woman Elected",
    "treat_2005" = "$\\text{Quota}_{t-1}$",
    "treat_2010" = "$\\text{Quota}_{t-1}$",
    "treat_2015" = "$\\text{Quota}_{t-1}$",
    "near_town" = "Near Town",
    "high_infra" = "High Infrastructure",
    "has_power" = "Has Power",
    "treat_2005:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
    "treat_2005:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
    "treat_2005:has_power" = "$\\text{Quota}_{t-1} \\times$ Has Power",
    "treat_2010:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
    "treat_2010:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
    "treat_2010:has_power" = "$\\text{Quota}_{t-1} \\times$ Has Power",
    "treat_2015:near_town" = "$\\text{Quota}_{t-1} \\times$ Near Town",
    "treat_2015:high_infra" = "$\\text{Quota}_{t-1} \\times$ High Infra",
    "treat_2015:has_power" = "$\\text{Quota}_{t-1} \\times$ Has Power",
    "(Intercept)" = "Intercept",
    "dist_samiti_2010" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_block_2010" = "(District, Samiti)",
    "dist_block_2015" = "(District, Samiti)",
    "dist_block_2021" = "(District, Samiti)"
)

aer_etable(models,
    file = here("tabs", "het_effects_combined.tex"),
    headers = list(
        c("Rajasthan" = 6, "Uttar Pradesh" = 6),
        c("05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$20" = 2,
          "05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$21" = 2),
        c("No FE", "FE", "No FE", "FE", "No FE", "FE",
          "No FE", "FE", "No FE", "FE", "No FE", "FE")
    ),
    cmidrules = list(after = 1, rules = c("2-7", "8-13")),
    colsep = list(after = 6, space = "1em"),
    keep = c("%treat_", "%near_town", "%high_infra", "%has_power"),
    notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: woman elected in open seat. Near Town = 1 if distance to nearest town $\\leq$ median. High Infrastructure = 1 if sum of (education facility, medical facility, power supply, bank facility) $>$ median. Has Power = 1 if village has power supply. All variables from Census 2001 Village Directory via SHRUG (LGD Block Panchayat matching). Heteroskedasticity-robust standard errors.",
    dict = dict_combined)

cat("\nCreated: tabs/het_effects_combined.tex\n")
cat("\n=== Consolidated Heterogeneous Effects Analysis Complete ===\n")
