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

summary(lm((cand_sex_fin_2010 == "TRUE") ~ treat_2005, data = subset(up_all, treat_2010 == 0)))

m_05_10 <- feols((female_cand_2010 =="TRUE") ~ treat_2005, data = filter(up_all, treat_2010 == 0))
summary(m_05_10)

m_05_10_dfe <- feols((female_cand_2010 =="TRUE") ~ treat_2005 | district_name_eng_2010,  data = filter(up_all, treat_2010 == 0) )
summary(m_05_10_dfe)

m_05_10_psfe <- feols((female_cand_2010 =="TRUE") ~ treat_2005 | district_name_eng_2010 + block_name_eng_2010, data = filter(up_all, treat_2010 == 0))
summary(m_05_10_psfe)

# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe)

etable(models_05_10_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_models_05_10.tex", 
       dict = c('female_cand_2010 =="TRUE"' = "2010 rep is a woman in an open seat in UP", 
                "treat_2005" = "Quota Treatment in 2005", 
                "district_name_eng_2010" = "District (2010)",
                "block_name_eng_2010" = "Panchayat Block (2010)",
                "gp_name_eng_2010" = "Gram Panchayat (2010)"), 
       replace = TRUE)

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((female_cand_2015 =="TRUE") ~ treat_2010, data = filter(up_all, treat_2015 == 0))
summary(m_10_15)

m_10_15_dfe <- feols((female_cand_2015 =="TRUE") ~ treat_2010 | district_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_dfe)

m_10_15_psfe <- feols((female_cand_2015 =="TRUE") ~ treat_2010 | district_name_eng_2015 + block_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_psfe)

models_10_15_list <- list(m_10_15, m_10_15_dfe, m_10_15_psfe)

etable(models_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_models_10_15.tex", 
       dict = c( 'female_cand_2015 =="TRUE"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota Treatment in 2010", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)



# 2015 ---> 2021 ----------------------------------------------------------

m_15_21 <- feols((female_cand_2021 =="TRUE") ~ treat_2015, data = filter(up_all, treat_2021 == 0))
summary(m_15_21)

m_15_21_dfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 | district_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_15_21_dfe)

m_15_21_psfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 | district_name_eng_2021 + block_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_15_21_psfe)

models_15_21_list <- list(m_15_21, m_15_21_dfe, m_15_21_psfe)


etable(models_15_21_list, 
       # vcov = list(NULL, ~gp_name_eng_2021, ~gp_name_eng_2021),
       # se = c("standard", "cluster","cluster"),
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       tex = TRUE, 
       file = "tables/up_models_15_21.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2015" = "Quota Treatment in 2015", 
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"),
       replace = TRUE)

