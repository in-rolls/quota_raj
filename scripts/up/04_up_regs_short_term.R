# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)

# Short Run Effects of Quotas (05-->10) -----------------------------------

load("data/up/up_all_recoded.RData")

# summary(lm((cand_sex_fin_2010 =="महिला") ~ treat_2005, data = subset(up_all, treat_2010 == 0)))

m_05_10 <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005, data = filter(up_all, treat_2010 == 0))
summary(m_05_10)

m_05_10_dfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | district_name_eng_2010,  data = filter(up_all, treat_2010 == 0) )
summary(m_05_10_dfe)

m_05_10_psfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | district_name_eng_2010+ block_name_eng_2010, data = filter(up_all, treat_2010 == 0))
summary(m_05_10_psfe)

m_05_10_gpfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | I(paste0(district_name_eng_2010, block_name_eng_2010)), data = filter(up_all, treat_2010 == 0))
summary(m_05_10_gpfe)
# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe, m_05_10_gpfe)

etable(models_05_10_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_models_05_10.tex", 
       dict = c('cand_sex_fin_2010 =="महिला"' = "2010 rep is a woman in an open seat in UP", 
                "treat_2005" = "Quota Treatment in 2005", 
                "district_name_eng_2010" = "District (2010)",
                "block_name_eng_2010" = "Panchayat Block (2010)",
                "gp_name_eng_2010" = "Gram Panchayat (2010)", 
       "I(paste0(district_name_eng_2010, block_name_eng_2010))" = "District - Block"),
   se.row= FALSE,        replace = TRUE)

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((sex_2015 =="महिला") ~ treat_2010, data = filter(up_all, treat_2015 == 0))
summary(m_10_15)

m_10_15_dfe <- feols((sex_2015 == "महिला") ~ treat_2010 | district_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_dfe)

m_10_15_psfe <- feols((sex_2015 == "महिला") ~ treat_2010 | district_name_eng_2015 + block_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_psfe)

m_10_15_gpfe <- feols((sex_2015 =="महिला") ~ treat_2010 | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_10_15_gpfe)

models_10_15_list <- list(m_10_15, m_10_15_dfe, m_10_15_psfe,m_10_15_gpfe)

etable(models_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_models_10_15.tex", 
       dict = c( 'sex_2015 =="महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota Treatment in 2010", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "District - Block"),#  notes = "Robust standard errors clustered at gram panchayat level",
   se.row= FALSE,        replace = TRUE)




# Main Tables Tables -----------------------------------------------------------

m_05_10_psfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 |  I(paste0(district_name_eng_2010, block_name_eng_2010)), data = filter(up_all, treat_2010 == 0))
summary(m_05_10_psfe)

m_10_15_psfe <- feols((sex_2015 == "महिला") ~ treat_2010 |  I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_10_15_psfe)

models_short_term_list <- list(m_05_10_psfe, m_10_15_psfe)


etable(models_short_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_main_short_term_models.tex", 
       dict = c('cand_sex_fin_2010 =="महिला"' = "2010 rep is a woman in an open seat in UP", 
                'sex_2015 =="महिला"' = "2015 rep is a woman in an open seat in UP", 
                "treat_2010" = "Quota Treatment in 2010", 
                "treat_2005" = "Quota Treatment in 2005", 
                'I(paste0(district_name_eng_2010, block_name_eng_2010))' = "(District, Panchayat Block)",
                'I(paste0(district_name_eng_2015, block_name_eng_2015))' = "(District, Panchayat Block)"),
       se.row = FALSE, 
       replace = TRUE)

