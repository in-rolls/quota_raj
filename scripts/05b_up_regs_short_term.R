# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)
library(here)

# Load data
up <- read_parquet(here("data/up/up_all_fuzzy_recoded.parquet"))
jw <- read_parquet(here("data/up/jeff_wide.parquet"))

m_05_10_psfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 |  I(paste0(district_name_eng_2010, block_name_eng_2010)), data = filter(up_all, treat_2010 == 0))
summary(m_05_10_psfe)

m_10_15_psfe <- feols((sex_2015 == "महिला") ~ treat_2010 |  I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_10_15_psfe)

models_short_term_list <- list(m_05_10_psfe, m_10_15_psfe)


etable(models_short_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times $ ",
       digits = 3,
       file = "tables/up_main_short_term_models.tex", 
       dict = c('cand_sex_fin_2010 =="महिला"' = "2010 rep is a woman in an open seat in UP", 
                'sex_2015 =="महिला"' = "2015 rep is a woman in an open seat in UP", 
                "treat_2010" = "Quota Treatment in 2010", 
                "treat_2005" = "Quota Treatment in 2005", 
                'I(paste0(district_name_eng_2010, block_name_eng_2010))' = "(District, Panchayat Block)",
                'I(paste0(district_name_eng_2015, block_name_eng_2015))' = "(District, Panchayat Block)"),
       se.row = FALSE, 
       replace = TRUE)

