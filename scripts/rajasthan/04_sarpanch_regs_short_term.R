# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")

# Regressions -------------------------------------------------------------

# Main Tables -----------------------------------------------------------

m_05_10_psfe <- feols((sex_2010 =="F") ~ treat_2005 | I(paste0(dist_name_2010, samiti_name_2010)),  data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10_psfe)

m_10_15_psfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | I(paste0(dist_name_2015, samiti_name_2015)), data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_psfe)

m_15_20_psfe <- feols((sex_2020 == "F") ~ treat_2015 | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_psfe)

models_short_term_list <- list(m_05_10_psfe, m_10_15_psfe, m_15_20_psfe)

etable(models_short_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       digits = 3,
       interaction.combine = " $\times $ ",
       file = "tables/raj_main_short_term_models.tex", 
       dict = c('sex_2010 == "F"' = "2010 Open Seat W Rep", 
                'sex_2020 == "F"' = "2020 Open Seat W Rep", 
                'sex_manual_2015 == "F"' = "2015 Open Seat W Rep",
                "treat_2010" = "Quota Treatment in 2010", 
                "treat_2005" = "Quota Treatment in 2005", 
                "treat_2015" = "Quota Treatment in 2015", 
                "I(paste0(district_2020, ps_2020))" = "(District, Panchayat Samiti)",
                "I(paste0(dist_name_2015, samiti_name_2015))" = "(District, Panchayat Samiti)",
                "I(paste0(dist_name_2010, samiti_name_2010))" = "(District, Panchayat Samiti)"),
       se.row = FALSE, 
       replace = TRUE)
