# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)


# Long Term ---------------------------------------------------------------

# Load data ---------------------------------------------------------------
load("data/up/up_all_recoded.RData")


# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(up_all, treat_2021 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021, vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_longterm_interaction.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)





# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((female_cand_2021 =="TRUE") ~ always_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((female_cand_2021 =="TRUE") ~ always_treated  | district_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((female_cand_2021 =="TRUE") ~ always_treated  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((female_cand_2021 =="TRUE") ~ always_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021, vcov = ~gp_name_eng_2021,, data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_gpfe)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_always.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_always_never <- feols((female_cand_2021 =="TRUE") ~ never_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_always_never)

m_always_never_dfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021, vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_always_never_dfe)

m_always_never_psfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_always_never_psfe)

m_always_never_gpfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_always_never_gpfe)


# TeX
m_always_never_list <- list(m_always_never, m_always_never_dfe, m_always_never_psfe, m_always_never_gpfe)

etable(m_always_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated (No quota in 2005, 10, & 15)",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((female_cand_2021 =="TRUE") ~ sometimes_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((female_cand_2021 =="TRUE") ~ sometimes_treated  | district_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((female_cand_2021 =="TRUE") ~ sometimes_treated  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((female_cand_2021 =="TRUE") ~ sometimes_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_sometimes.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)




# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((female_cand_2021 =="TRUE") ~ once , data = filter(up_all, treat_2021 == 0))
summary(m_once)

m_once_dfe <- feols((female_cand_2021 =="TRUE") ~ once  | district_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((female_cand_2021 =="TRUE") ~ once  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((female_cand_2021 =="TRUE") ~ once  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_low_intensity.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in either 2005, 2010 or 2010",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((female_cand_2021 =="TRUE") ~ twice , data = filter(up_all, treat_2021 == 0))
summary(m_twice)

m_twice_dfe <- feols((female_cand_2021 =="TRUE") ~ twice  | district_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_twice_dfe)

m_twice_psfe <- feols((female_cand_2021 =="TRUE") ~ twice  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_twice_psfe)

m_twice_gpfe <- feols((female_cand_2021 =="TRUE") ~ twice  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_twice_gpfe)

# TeX
m_twice_list <- list(m_twice, m_twice_dfe, m_twice_psfe, m_twice_gpfe)

etable(m_twice_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_med_intensity.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice" = "Treated twice in 2005, 2010 and 2010",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

# never treated

m_no_treat <- feols((female_cand_2021 =="TRUE") ~ never_treated , data = filter(up_all, treat_2021 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((female_cand_2021 =="TRUE") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'female_cand_2021 =="TRUE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Samiti (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)