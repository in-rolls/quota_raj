# 05c_short_term_treatment_rotation.R
# Treatment rotation analysis: Does treatment in YEAR1 predict treatment in YEAR2?
# Shows rotation pattern (negative coefficient = proper rotation away from treated GPs)
# Output: tabs/treatment_rotation.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Treatment Rotation Analysis ===")

# =============================================================================
# Load all panels (FULL sample - all GPs, not just open seats)
# =============================================================================

raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet"))
raj_10_15 <- read_parquet(here("data/raj/raj_10_15.parquet"))
raj_15_20 <- read_parquet(here("data/raj/raj_15_20.parquet"))

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

message("\nLoaded panels (FULL sample):")
message("Raj 05-10: ", nrow(raj_05_10), " GPs")
message("Raj 10-15: ", nrow(raj_10_15), " GPs")
message("Raj 15-20: ", nrow(raj_15_20), " GPs")
message("UP 05-10: ", nrow(up_05_10), " GPs")
message("UP 10-15: ", nrow(up_10_15), " GPs")
message("UP 15-21: ", nrow(up_15_21), " GPs")

# =============================================================================
# Rajasthan 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
message("\n--- Rajasthan 2005 → 2010 ---")

m_raj_0510_nofe <- feols(treat_2010 ~ treat_2005, data = raj_05_10)
m_raj_0510_fe <- feols(treat_2010 ~ treat_2005 | dist_samiti_2010, data = raj_05_10, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_raj_0510_nofe)["treat_2005"], 4))
message("FE coef: ", round(coef(m_raj_0510_fe)["treat_2005"], 4))

# =============================================================================
# Rajasthan 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
message("\n--- Rajasthan 2010 → 2015 ---")

m_raj_1015_nofe <- feols(treat_2015 ~ treat_2010, data = raj_10_15)
m_raj_1015_fe <- feols(treat_2015 ~ treat_2010 | dist_samiti_2015, data = raj_10_15, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_raj_1015_nofe)["treat_2010"], 4))
message("FE coef: ", round(coef(m_raj_1015_fe)["treat_2010"], 4))

# =============================================================================
# Rajasthan 2015 → 2020: treat_2020 ~ treat_2015
# =============================================================================
message("\n--- Rajasthan 2015 → 2020 ---")

m_raj_1520_nofe <- feols(treat_2020 ~ treat_2015, data = raj_15_20)
m_raj_1520_fe <- feols(treat_2020 ~ treat_2015 | dist_samiti_2020, data = raj_15_20, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_raj_1520_nofe)["treat_2015"], 4))
message("FE coef: ", round(coef(m_raj_1520_fe)["treat_2015"], 4))

# =============================================================================
# UP 2005 → 2010: treat_2010 ~ treat_2005
# =============================================================================
message("\n--- UP 2005 → 2010 ---")

m_up_0510_nofe <- feols(treat_2010 ~ treat_2005, data = up_05_10)
m_up_0510_fe <- feols(treat_2010 ~ treat_2005 | dist_block_2010, data = up_05_10, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_up_0510_nofe)["treat_2005"], 4))
message("FE coef: ", round(coef(m_up_0510_fe)["treat_2005"], 4))

# =============================================================================
# UP 2010 → 2015: treat_2015 ~ treat_2010
# =============================================================================
message("\n--- UP 2010 → 2015 ---")

m_up_1015_nofe <- feols(treat_2015 ~ treat_2010, data = up_10_15)
m_up_1015_fe <- feols(treat_2015 ~ treat_2010 | dist_block_2015, data = up_10_15, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_up_1015_nofe)["treat_2010"], 4))
message("FE coef: ", round(coef(m_up_1015_fe)["treat_2010"], 4))

# =============================================================================
# UP 2015 → 2021: treat_2021 ~ treat_2015
# =============================================================================
message("\n--- UP 2015 → 2021 ---")

m_up_1521_nofe <- feols(treat_2021 ~ treat_2015, data = up_15_21)
m_up_1521_fe <- feols(treat_2021 ~ treat_2015 | dist_block_2021, data = up_15_21, fixef.rm = "singleton")

message("No FE coef: ", round(coef(m_up_1521_nofe)["treat_2015"], 4))
message("FE coef: ", round(coef(m_up_1521_fe)["treat_2015"], 4))

# =============================================================================
# Combined Output Table: All panels, NO FE and FE (12 columns)
# =============================================================================
message("\n=== Generating output table ===")

all_models <- list(
    m_raj_0510_nofe, m_raj_0510_fe,
    m_raj_1015_nofe, m_raj_1015_fe,
    m_raj_1520_nofe, m_raj_1520_fe,
    m_up_0510_nofe, m_up_0510_fe,
    m_up_1015_nofe, m_up_1015_fe,
    m_up_1521_nofe, m_up_1521_fe
)

dict_rotation <- c(
    "treat_2010" = "Quota$_t$",
    "treat_2015" = "Quota$_t$",
    "treat_2020" = "Quota$_t$",
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

aer_etable(all_models,
    file = here("tabs", "treatment_rotation.tex"),
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
    notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: quota status in election $t$. Treatment: quota status in election $t-1$. Sample includes all GPs (not restricted to open seats). A negative coefficient indicates rotation away from previously reserved GPs, consistent with the rotation mandate. Heteroskedasticity-robust standard errors.",
    dict = dict_rotation)

message("Created: tabs/treatment_rotation.tex")

message("\n=== Treatment Rotation Analysis Complete ===")
