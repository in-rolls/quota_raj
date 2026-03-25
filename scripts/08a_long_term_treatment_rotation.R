# 08a_long_term_treatment_rotation.R
# Long-term treatment rotation analysis: Tests rotation across all transitions
# Uses 4-way panels to validate rotation mandate over longer periods
# Includes: 05→10, 10→15, 15→20 (Raj) and 05→10, 10→15, 15→21 (UP)
# Output: tabs/long_term_treatment_rotation.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Long-Term Treatment Rotation Analysis ===\n")

# =============================================================================
# Load 4-way panels (contains GPs present in all 4 election years)
# =============================================================================

raj_05_20 <- read_parquet(here("data/raj/raj_05_20.parquet"))
up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

cat("\nLoaded 4-way panels:\n")
cat("Raj 05-20 (4-way):", nrow(raj_05_20), "GPs\n")
cat("UP 05-21 (4-way):", nrow(up_05_21), "GPs\n")

# =============================================================================
# Rajasthan 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
cat("\n--- Rajasthan 2005 → 2010 (4-way panel) ---\n")

m_raj_0510_nofe <- feols(female_res_2010 ~ treat_2005, data = raj_05_20)
m_raj_0510_fe <- feols(female_res_2010 ~ treat_2005 | dist_samiti_2010, data = raj_05_20)

cat("No FE coef:", round(coef(m_raj_0510_nofe)["treat_2005"], 4), "\n")
cat("FE coef:", round(coef(m_raj_0510_fe)["treat_2005"], 4), "\n")

# =============================================================================
# Rajasthan 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
cat("\n--- Rajasthan 2010 → 2015 (4-way panel) ---\n")

m_raj_1015_nofe <- feols(female_res_2015 ~ treat_2010, data = raj_05_20)
m_raj_1015_fe <- feols(female_res_2015 ~ treat_2010 | dist_samiti_2015, data = raj_05_20)

cat("No FE coef:", round(coef(m_raj_1015_nofe)["treat_2010"], 4), "\n")
cat("FE coef:", round(coef(m_raj_1015_fe)["treat_2010"], 4), "\n")

# =============================================================================
# UP 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
cat("\n--- UP 2005 → 2010 (4-way panel) ---\n")

m_up_0510_nofe <- feols(treat_2010 ~ treat_2005, data = up_05_21)
m_up_0510_fe <- feols(treat_2010 ~ treat_2005 | dist_block_2010, data = up_05_21)

cat("No FE coef:", round(coef(m_up_0510_nofe)["treat_2005"], 4), "\n")
cat("FE coef:", round(coef(m_up_0510_fe)["treat_2005"], 4), "\n")

# =============================================================================
# UP 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
cat("\n--- UP 2010 → 2015 (4-way panel) ---\n")

m_up_1015_nofe <- feols(treat_2015 ~ treat_2010, data = up_05_21)
m_up_1015_fe <- feols(treat_2015 ~ treat_2010 | dist_block_2015, data = up_05_21)

cat("No FE coef:", round(coef(m_up_1015_nofe)["treat_2010"], 4), "\n")
cat("FE coef:", round(coef(m_up_1015_fe)["treat_2010"], 4), "\n")

# =============================================================================
# Rajasthan 2015 → 2020: treat_2020 ~ treat_2015
# =============================================================================
cat("\n--- Rajasthan 2015 → 2020 (4-way panel) ---\n")

m_raj_1520_nofe <- feols(female_res_2020 ~ treat_2015, data = raj_05_20)
m_raj_1520_fe <- feols(female_res_2020 ~ treat_2015 | dist_samiti_2020, data = raj_05_20)

cat("No FE coef:", round(coef(m_raj_1520_nofe)["treat_2015"], 4), "\n")
cat("FE coef:", round(coef(m_raj_1520_fe)["treat_2015"], 4), "\n")

# =============================================================================
# UP 2015 → 2021: treat_2021 ~ treat_2015
# =============================================================================
cat("\n--- UP 2015 → 2021 (4-way panel) ---\n")

m_up_1521_nofe <- feols(treat_2021 ~ treat_2015, data = up_05_21)
m_up_1521_fe <- feols(treat_2021 ~ treat_2015 | dist_block_2021, data = up_05_21)

cat("No FE coef:", round(coef(m_up_1521_nofe)["treat_2015"], 4), "\n")
cat("FE coef:", round(coef(m_up_1521_fe)["treat_2015"], 4), "\n")

# =============================================================================
# Combined Output Table: 12 columns (3 transitions × 2 states × 2 FE specs)
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

dict_rotation <- c(
    "female_res_2010" = "Quota$_t$",
    "female_res_2015" = "Quota$_t$",
    "female_res_2020" = "Quota$_t$",
    "treat_2010" = "Quota$_t$",
    "treat_2015" = "Quota$_t$",
    "treat_2021" = "Quota$_t$",
    "treat_2005" = "$\\text{Quota}_{t-1}$",
    "treat_2010" = "$\\text{Quota}_{t-1}$",
    "treat_2015" = "$\\text{Quota}_{t-1}$",
    "(Intercept)" = "Intercept",
    "dist_samiti_2010" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_block_2010" = "(District, Samiti)",
    "dist_block_2015" = "(District, Samiti)",
    "dist_block_2021" = "(District, Samiti)"
)

notes_text <- paste0(
    "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
    "Outcome: quota status in election $t$. Treatment: quota status in election $t-1$. ",
    "Sample includes GPs present in all four election years (4-way panel). ",
    "A negative coefficient indicates rotation away from previously reserved GPs, consistent with the rotation mandate. ",
    "Heteroskedasticity-robust standard errors."
)

aer_etable(all_models,
    file = here("tabs", "long_term_treatment_rotation.tex"),
    headers = list(
        c("Rajasthan" = 6, "Uttar Pradesh" = 6),
        c("05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$20" = 2,
          "05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$21" = 2),
        c("No FE", "FE", "No FE", "FE", "No FE", "FE",
          "No FE", "FE", "No FE", "FE", "No FE", "FE")
    ),
    cmidrules = list(after = 1, rules = c("2-7", "8-13")),
    colsep = list(after = 6, space = "1em"),
    keep = c("(Intercept)", "%treat_"),
    notes = notes_text,
    dict = dict_rotation)

cat("Created: tabs/long_term_treatment_rotation.tex\n")

cat("\n=== Long-Term Treatment Rotation Analysis Complete ===\n")
