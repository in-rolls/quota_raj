# 05e_short_term_covariates.R
# Short-term effects with SHRUG census covariates for ALL panels
# Adds controls: log(population), SC share, literacy share
# Uses new SHRUG-matched data from block matching (02c_shrug_all_panels_block.R)
# Output: tabs/short_term_shrug_covariates.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Short-Term Effects with SHRUG Covariates (All Panels) ===")

# =============================================================================
# Helper function: add SHRUG covariates
# =============================================================================

add_shrug_covariates <- function(df) {
    df %>%
        mutate(
            log_pop = log(pc01_pca_tot_p + 1),
            sc_share = ifelse(pc01_pca_tot_p > 0, pc01_pca_p_sc / pc01_pca_tot_p, NA),
            lit_share = ifelse(pc01_pca_tot_p > 0, pc01_pca_p_lit / pc01_pca_tot_p, NA)
        )
}

# =============================================================================
# RAJASTHAN: Load new SHRUG-matched data (block matching)
# =============================================================================

message("\n--- Rajasthan ---")

raj_shrug_05_10 <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet")) %>%
    add_shrug_covariates()

raj_shrug_10_15 <- read_parquet(here("data/raj/shrug_gp_raj_10_15_block.parquet")) %>%
    add_shrug_covariates()

raj_shrug_15_20 <- read_parquet(here("data/raj/shrug_gp_raj_15_20_block.parquet")) %>%
    add_shrug_covariates()

message("Raj 05-10 rows: ", nrow(raj_shrug_05_10))
message("Raj 10-15 rows: ", nrow(raj_shrug_10_15))
message("Raj 15-20 rows: ", nrow(raj_shrug_15_20))

# =============================================================================
# UTTAR PRADESH: Load SHRUG-matched data
# =============================================================================

message("\n--- Uttar Pradesh ---")

up_shrug_05_10 <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet")) %>%
    add_shrug_covariates()

up_shrug_10_15 <- read_parquet(here("data/up/shrug_gp_up_10_15_block.parquet")) %>%
    add_shrug_covariates()

up_shrug_15_21 <- read_parquet(here("data/up/shrug_gp_up_15_21_block.parquet")) %>%
    add_shrug_covariates()

message("UP 05-10 GPs: ", nrow(up_shrug_05_10))
message("UP 10-15 rows: ", nrow(up_shrug_10_15))
message("UP 15-21 rows: ", nrow(up_shrug_15_21))

# =============================================================================
# Rajasthan 2005 → 2010 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2005 → 2010 ---")

raj_05_10_open <- raj_shrug_05_10 %>%
    filter(treat_2010 == 0) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(raj_05_10_open))

m_raj_0510_nofe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share,
                         data = raj_05_10_open)
m_raj_0510_fe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share | dist_samiti_2010,
                       data = raj_05_10_open)

# =============================================================================
# Rajasthan 2010 → 2015 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2010 → 2015 ---")

raj_10_15_open <- raj_shrug_10_15 %>%
    filter(treat_2015 == 0) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(raj_10_15_open))

m_raj_1015_nofe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share,
                         data = raj_10_15_open)
m_raj_1015_fe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share | dist_samiti_2015,
                       data = raj_10_15_open)

# =============================================================================
# Rajasthan 2015 → 2020 with SHRUG covariates
# =============================================================================
message("\n--- Rajasthan 2015 → 2020 ---")

raj_15_20_open <- raj_shrug_15_20 %>%
    filter(treat_2020 == 0 & !is.na(female_winner_2020)) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(raj_15_20_open))

m_raj_1520_nofe <- feols(female_winner_2020 ~ treat_2015 + log_pop + sc_share + lit_share,
                         data = raj_15_20_open)
m_raj_1520_fe <- feols(female_winner_2020 ~ treat_2015 + log_pop + sc_share + lit_share | dist_samiti_2020,
                       data = raj_15_20_open)

# =============================================================================
# UP 2005 → 2010 with SHRUG covariates
# =============================================================================
message("\n--- UP 2005 → 2010 ---")

up_05_10_open <- up_shrug_05_10 %>%
    filter(treat_2010 == 0) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(up_05_10_open))

m_up_0510_nofe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share,
                        data = up_05_10_open)
m_up_0510_fe <- feols(female_winner_2010 ~ treat_2005 + log_pop + sc_share + lit_share | dist_block_2010,
                      data = up_05_10_open)

# =============================================================================
# UP 2010 → 2015 with SHRUG covariates
# =============================================================================
message("\n--- UP 2010 → 2015 ---")

up_10_15_open <- up_shrug_10_15 %>%
    filter(treat_2015 == 0) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(up_10_15_open))

m_up_1015_nofe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share,
                        data = up_10_15_open)
m_up_1015_fe <- feols(female_winner_2015 ~ treat_2010 + log_pop + sc_share + lit_share | dist_block_2015,
                      data = up_10_15_open)

# =============================================================================
# UP 2015 → 2021 with SHRUG covariates
# =============================================================================
message("\n--- UP 2015 → 2021 ---")

up_15_21_open <- up_shrug_15_21 %>%
    filter(treat_2021 == 0) %>%
    filter(!is.na(log_pop) & !is.na(sc_share) & !is.na(lit_share))

message("Open seats with covariates: ", nrow(up_15_21_open))

m_up_1521_nofe <- feols(female_winner_2021 ~ treat_2015 + log_pop + sc_share + lit_share,
                        data = up_15_21_open)
m_up_1521_fe <- feols(female_winner_2021 ~ treat_2015 + log_pop + sc_share + lit_share | dist_block_2021,
                      data = up_15_21_open)

# =============================================================================
# Combined Output Table
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

dict_shrug <- c(
    "female_winner_2010" = "Woman Elected",
    "female_winner_2015" = "Woman Elected",
    "female_winner_2020" = "Woman Elected",
    "female_winner_2021" = "Woman Elected",
    "treat_2005" = "$\\text{Quota}_{t-1}$",
    "treat_2010" = "$\\text{Quota}_{t-1}$",
    "treat_2015" = "$\\text{Quota}_{t-1}$",
    "log_pop" = "Log(Population)",
    "sc_share" = "SC Share",
    "lit_share" = "Literacy Share",
    "(Intercept)" = "Intercept",
    "dist_samiti_2010" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_block_2010" = "(District, Samiti)",
    "dist_block_2015" = "(District, Samiti)",
    "dist_block_2021" = "(District, Samiti)"
)

aer_etable(all_models,
    file = here("tabs", "short_term_shrug_covariates.tex"),
    headers = list(
        c("Rajasthan" = 6, "Uttar Pradesh" = 6),
        c("05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$20" = 2,
          "05$\\rightarrow$10" = 2, "10$\\rightarrow$15" = 2, "15$\\rightarrow$21" = 2),
        c("No FE", "FE", "No FE", "FE", "No FE", "FE",
          "No FE", "FE", "No FE", "FE", "No FE", "FE")
    ),
    cmidrules = list(after = 1, rules = c("2-7", "8-13")),
    colsep = list(after = 6, space = "1em"),
    keep = c("(Intercept)", "%treat_", "%log_pop", "%sc_share", "%lit_share"),
    notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. Outcome: woman elected in open seat. Census covariates from 2001 Census via SHRUG (LGD Block Panchayat matching). Population, SC share, and literacy computed at GP level. Sample restricted to GPs with SHRUG match and non-missing covariates. Heteroskedasticity-robust standard errors.",
    dict = dict_shrug)

message("Created: tabs/short_term_shrug_covariates.tex")

message("\n=== Short-Term with SHRUG Covariates Complete ===")
