# 07c_placebo.R
# Combined placebo tests for all 2-way panels
# Tests: Does FUTURE treatment predict PAST outcomes? (Should be null)
# Model: female_winner_YEAR1 ~ treat_YEAR2 (in open seats at YEAR1)
# Output: tabs/placebo_combined.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Combined Placebo Tests ===\n")

# =============================================================================
# Load all panels
# =============================================================================

raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet"))
raj_10_15 <- read_parquet(here("data/raj/raj_10_15.parquet"))
raj_15_20 <- read_parquet(here("data/raj/raj_15_20.parquet"))

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

cat("\nLoaded panels:\n")
cat("Raj 05-10:", nrow(raj_05_10), "GPs\n")
cat("Raj 10-15:", nrow(raj_10_15), "GPs\n")
cat("Raj 15-20:", nrow(raj_15_20), "GPs\n")
cat("UP 05-10:", nrow(up_05_10), "GPs\n")
cat("UP 10-15:", nrow(up_10_15), "GPs\n")
cat("UP 15-21:", nrow(up_15_21), "GPs\n")

# =============================================================================
# Create female_winner variables for UP (Hindi: महिला = Female)
# =============================================================================

up_05_10 <- up_05_10 %>%
    mutate(
        female_winner_2005 = as.integer(cand_sex_fin_2005 == "महिला"),
        female_winner_2010 = as.integer(cand_sex_fin_2010 == "महिला")
    )

up_10_15 <- up_10_15 %>%
    mutate(
        female_winner_2010 = as.integer(sex_2010 == "महिला"),
        female_winner_2015 = as.integer(sex_2015 == "महिला")
    )

up_15_21 <- up_15_21 %>%
    mutate(
        female_winner_2015 = as.integer(sex_2015 == "महिला"),
        female_winner_2021 = as.integer(sex_2021 == "महिला")
    )

# =============================================================================
# Rajasthan 2005 → 2010 Placebo: female_winner_2005 ~ treat_2010
# (Open seats in 2005, test if 2010 treatment predicts 2005 outcome)
# =============================================================================
cat("\n--- Rajasthan 2005 → 2010 Placebo ---\n")

raj_05_10_placebo <- filter(raj_05_10, female_res_2005 == 0)
cat("Open seats in 2005:", nrow(raj_05_10_placebo), "\n")

m_raj_0510_nofe <- feols(female_winner_2005 ~ treat_2010, data = raj_05_10_placebo)
m_raj_0510_fe <- feols(female_winner_2005 ~ treat_2010 | dist_samiti_2010, data = raj_05_10_placebo)

cat("No FE coef:", round(coef(m_raj_0510_nofe)["treat_2010"], 4),
    "p =", round(pvalue(m_raj_0510_nofe)["treat_2010"], 4), "\n")
cat("FE coef:", round(coef(m_raj_0510_fe)["treat_2010"], 4),
    "p =", round(pvalue(m_raj_0510_fe)["treat_2010"], 4), "\n")

# =============================================================================
# Rajasthan 2010 → 2015 Placebo: female_winner_2010 ~ treat_2015
# =============================================================================
cat("\n--- Rajasthan 2010 → 2015 Placebo ---\n")

raj_10_15_placebo <- filter(raj_10_15, female_res_2010 == 0)
cat("Open seats in 2010:", nrow(raj_10_15_placebo), "\n")

m_raj_1015_nofe <- feols(female_winner_2010 ~ treat_2015, data = raj_10_15_placebo)
m_raj_1015_fe <- feols(female_winner_2010 ~ treat_2015 | dist_samiti_2015, data = raj_10_15_placebo)

cat("No FE coef:", round(coef(m_raj_1015_nofe)["treat_2015"], 4),
    "p =", round(pvalue(m_raj_1015_nofe)["treat_2015"], 4), "\n")

# =============================================================================
# Rajasthan 2015 → 2020 Placebo: female_winner_2015 ~ treat_2020
# =============================================================================
cat("\n--- Rajasthan 2015 → 2020 Placebo ---\n")

raj_15_20_placebo <- filter(raj_15_20, female_res_2015 == 0)
cat("Open seats in 2015:", nrow(raj_15_20_placebo), "\n")

m_raj_1520_nofe <- feols(female_winner_2015 ~ treat_2020, data = raj_15_20_placebo)
m_raj_1520_fe <- feols(female_winner_2015 ~ treat_2020 | dist_samiti_2020, data = raj_15_20_placebo)

cat("No FE coef:", round(coef(m_raj_1520_nofe)["treat_2020"], 4),
    "p =", round(pvalue(m_raj_1520_nofe)["treat_2020"], 4), "\n")

# =============================================================================
# UP 2005 → 2010 Placebo: female_winner_2005 ~ treat_2010
# =============================================================================
cat("\n--- UP 2005 → 2010 Placebo ---\n")

up_05_10_placebo <- filter(up_05_10, treat_2005 == 0)
cat("Open seats in 2005:", nrow(up_05_10_placebo), "\n")

m_up_0510_nofe <- feols(female_winner_2005 ~ treat_2010, data = up_05_10_placebo)
m_up_0510_fe <- feols(female_winner_2005 ~ treat_2010 | dist_block_2010, data = up_05_10_placebo)

cat("No FE coef:", round(coef(m_up_0510_nofe)["treat_2010"], 4),
    "p =", round(pvalue(m_up_0510_nofe)["treat_2010"], 4), "\n")

# =============================================================================
# UP 2010 → 2015 Placebo: female_winner_2010 ~ treat_2015
# =============================================================================
cat("\n--- UP 2010 → 2015 Placebo ---\n")

up_10_15_placebo <- filter(up_10_15, treat_2010 == 0)
cat("Open seats in 2010:", nrow(up_10_15_placebo), "\n")

m_up_1015_nofe <- feols(female_winner_2010 ~ treat_2015, data = up_10_15_placebo)
m_up_1015_fe <- feols(female_winner_2010 ~ treat_2015 | dist_block_2015, data = up_10_15_placebo)

cat("No FE coef:", round(coef(m_up_1015_nofe)["treat_2015"], 4),
    "p =", round(pvalue(m_up_1015_nofe)["treat_2015"], 4), "\n")

# =============================================================================
# UP 2015 → 2021 Placebo: female_winner_2015 ~ treat_2021
# =============================================================================
cat("\n--- UP 2015 → 2021 Placebo ---\n")

up_15_21_placebo <- filter(up_15_21, treat_2015 == 0)
cat("Open seats in 2015:", nrow(up_15_21_placebo), "\n")

m_up_1521_nofe <- feols(female_winner_2015 ~ treat_2021, data = up_15_21_placebo)
m_up_1521_fe <- feols(female_winner_2015 ~ treat_2021 | dist_block_2021, data = up_15_21_placebo)

cat("No FE coef:", round(coef(m_up_1521_nofe)["treat_2021"], 4),
    "p =", round(pvalue(m_up_1521_nofe)["treat_2021"], 4), "\n")

# =============================================================================
# Combined Output Table: All panels, NO FE and FE (12 columns)
# =============================================================================
cat("\n=== Generating output table ===\n")

all_models <- list(
    m_raj_0510_nofe, m_raj_0510_fe,
    m_raj_1015_nofe, m_raj_1015_fe,
    m_raj_1520_nofe, m_raj_1520_fe,
    m_up_0510_nofe, m_up_0510_fe,
    m_up_1015_nofe, m_up_1015_fe,
    m_up_1521_nofe, m_up_1521_fe
)

dict_placebo <- c(
    "female_winner_2005" = "Woman Elected$_{t-1}$",
    "female_winner_2010" = "Woman Elected$_{t-1}$",
    "female_winner_2015" = "Woman Elected$_{t-1}$",
    "treat_2010" = "$\\text{Quota}_{t}$",
    "treat_2015" = "$\\text{Quota}_{t}$",
    "treat_2020" = "$\\text{Quota}_{t}$",
    "treat_2021" = "$\\text{Quota}_{t}$",
    "(Intercept)" = "Intercept",
    "dist_samiti_2010" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_block_2010" = "(District, Samiti)",
    "dist_block_2015" = "(District, Samiti)",
    "dist_block_2021" = "(District, Samiti)"
)

aer_etable(all_models,
    file = here("tabs", "placebo_combined.tex"),
    headers = list(
        c("Rajasthan" = 6, "Uttar Pradesh" = 6),
        c("05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$20" = 2,
          "05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$21" = 2),
        c("No FE", "FE", "No FE", "FE", "No FE", "FE",
          "No FE", "FE", "No FE", "FE", "No FE", "FE")
    ),
    cmidrules = list(after = 1, rules = c("2-7", "8-13")),
    colsep = list(after = 6, space = "1em"),
    keep = c("%treat_"),
    notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Placebo test: FUTURE quota status predicting PAST outcomes. Outcome: woman elected in open seat at $t-1$. Treatment: quota status at $t$. Sample restricted to GPs where seat was open in the outcome election. Null effects support random assignment of quota. Heteroskedasticity-robust standard errors.",
    dict = dict_placebo)

cat("Created: tabs/placebo_combined.tex\n")

cat("\n=== Combined Placebo Tests Complete ===\n")
