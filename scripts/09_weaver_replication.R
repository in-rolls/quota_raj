# 07f_weaver_replication.R
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
        district_block_2020 = paste(subdistrictcode_lgd21, blockname_lgd21, sep = "_")
    )

message("=== Weaver Data Replication ===")
message("Total GPs: ", nrow(jw))
message("Variables: ", ncol(jw))
message("Unique districts: ", length(unique(jw$district_code_ele15)))
message("Unique blocks: ", length(unique(jw$block_code_ele15)), "\n")

# =============================================================================
# 1. SHORT-TERM EFFECTS
# =============================================================================

message("--- Short-Term Effects ---")

# Filter data for each outcome year
jw_open_2015 <- jw %>% filter(reservation_female_2015 == 0)
jw_open_2020 <- jw %>% filter(reservation_female_2020 == 0)

# 2010 -> 2015: No FE
m_10_15 <- feols(winner_female_2015 ~ reservation_female_2010,
                 data = jw_open_2015, vcov = "hetero")

# 2010 -> 2015: With FE
m_10_15_fe <- feols(winner_female_2015 ~ reservation_female_2010 | district_block_2015,
                    data = jw_open_2015, vcov = "hetero")

# 2015 -> 2020: No FE
m_15_20 <- feols(winner_female_2020 ~ reservation_female_2015,
                 data = jw_open_2020, vcov = "hetero")

# 2015 -> 2020: With FE (use 2020 admin codes)
m_15_20_fe <- feols(winner_female_2020 ~ reservation_female_2015 | district_block_2020,
                    data = jw_open_2020, vcov = "hetero")

message("2010->2015: No FE =", round(coef(m_10_15)["reservation_female_2010"], 4),
    ", FE =", round(coef(m_10_15_fe)["reservation_female_2010"], 4), "\n")
message("2015->2020: No FE =", round(coef(m_15_20)["reservation_female_2015"], 4),
    ", FE =", round(coef(m_15_20_fe)["reservation_female_2015"], 4), "\n\n")

models_short_term <- list(m_10_15, m_10_15_fe, m_15_20, m_15_20_fe)

aer_etable(models_short_term,
       title = "Short-Term Effects: Weaver UP Replication",
       label = "tab:weaver_short",
       notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in the subsequent election. Columns [i]-[ii] show effects of 2010 quota on 2015 outcomes; columns [iii]-[iv] show effects of 2015 quota on 2020 outcomes. Models [i] and [iii] use heteroskedasticity-robust SE. Models [ii] and [iv] add (District, Samiti) FE.",
       placement = "htbp",
       headers = c("2015", "2015 (FE)", "2020", "2020 (FE)"),
       dict = c(
           "winner_female_2015" = "Woman Elected",
           "winner_female_2020" = "Woman Elected",
           "reservation_female_2010" = "$\\text{Quota}_{t-1}$",
           "reservation_female_2015" = "$\\text{Quota}_{t-1}$",
           "district_block_2015" = "(District, Samiti)",
           "district_block_2020" = "(District, Samiti)"
       ),
       file = here("tabs", "weaver_short_term.tex"))

message("Saved: tabs/weaver_short_term.tex")

# =============================================================================
# 2. LONG-TERM EFFECTS
# =============================================================================

message("--- Long-Term Effects ---")

# Full interaction model: No FE
m_long <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015,
                data = jw_open_2020, vcov = "hetero")

# Full interaction model: With FE (use 2020 admin codes)
m_long_fe <- feols(winner_female_2020 ~ reservation_female_2010 * reservation_female_2015 | district_block_2020,
                   data = jw_open_2020, vcov = "hetero")

message("Long-term interaction model:")
message("  No FE: N =", nobs(m_long))
message("  FE: N =", nobs(m_long_fe), "\n")

models_long_term <- list(m_long, m_long_fe)

aer_etable(models_long_term,
       title = "Long-Term Effects: Weaver UP Replication",
       label = "tab:weaver_long",
       notes = "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. The dependent variable is whether a woman was elected in an open seat in 2020. All models include 2010 quota, 2015 quota, and their interaction. Model [i] uses heteroskedasticity-robust SE. Model [ii] adds (District, Samiti) FE.",
       placement = "htbp",
       headers = c("No FE", "FE"),
       dict = c(
           "winner_female_2020" = "Woman Elected",
           "reservation_female_2010" = "$\\text{Quota}_{2010}$",
           "reservation_female_2015" = "$\\text{Quota}_{2015}$",
           "reservation_female_2010:reservation_female_2015" = "$\\text{Quota}_{2010} \\times \\text{Quota}_{2015}$",
           "district_block_2020" = "(District, Samiti)"
       ),
       file = here("tabs", "weaver_long_term.tex"))

message("Saved: tabs/weaver_long_term.tex")

message("=== Weaver Replication Complete ===")
