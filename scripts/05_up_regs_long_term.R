# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)

load("data/up/up_all_recoded.RData")

# Long Term Interaction ---------------------------------------------------


# 2005 * 2010  interaction

m_long_term <- feols((sex_2015 == "महिला") ~   treat_2010 * treat_2005,  data = filter(up_all, treat_2015 == 0))
summary(m_long_term)

m_long_term_gpfe <- feols((sex_2015 == "महिला") ~  treat_2010 * treat_2005  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_longterm_interaction.tex", 
       digits =3,
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in 2005, 10 or 15",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)
library(broom)
# load("data/up/up_all_fuzzy_recoded.RData")
