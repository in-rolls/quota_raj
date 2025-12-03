# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)



# Short Run Effects of Quotas (05-->10) -----------------------------------

load("data/up/up_all_fuzzy_recoded.RData")

# Long Term ---------------------------------------------------------------

summary(lm((sex_2021 =="महिला") ~ always_treated, data = subset(up_all, treat_2021 == 0)))
summary(lm((sex_2021 =="महिला") ~ as.factor(count_treated), data = subset(up_all, treat_2021 == 0)))
summary(lm((sex_2021 =="महिला") ~ as.factor(treat_all), data = subset(up_all, treat_2021 == 0)))
summary(lm((sex_2021 =="महिला") ~ treat_2005*treat_2010*treat_2015, data = subset(up_all, treat_2021 == 0)))

summary(lm((sex_2021 =="महिला") ~ always_treated, data = subset(up_all, treat_2021 == 0)))
# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2021 == "महिला") ~ always_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2021 == "महिला") ~ always_treated  | district_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2021 == "महिला") ~ always_treated  | I(paste0(district_name_eng_2021, block_name_eng_2021)), data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2021 == "महिला") ~ always_treated + key, data = filter(up_all, treat_2021 == 0))
summary(m_always_lt_gpfe)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_always.tex", 
       dict = c( 'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_always_never <- feols((sex_2021 == "महिला") ~ never_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_always_never)

m_always_never_dfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021,  data = filter(up_all, treat_2021 == 0))
summary(m_always_never_dfe)

m_always_never_psfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_always_never_psfe)

m_always_never_gpfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_always_never_gpfe)


# TeX
m_always_never_list <- list(m_always_never, m_always_never_dfe, m_always_never_psfe, m_always_never_gpfe)

etable(m_always_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_long_term_always.tex", 
       dict = c( 'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated (No quota in 2005, 10, & 15)",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2021 == "महिला") ~ sometimes_treated,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2021 == "महिला") ~ sometimes_treated  | district_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2021 == "महिला") ~ sometimes_treated  | district_name_eng_2021 + block_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2021 == "महिला") ~ sometimes_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_long_term_sometimes.tex", 
       dict = c(
            'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
            "treat_2010" = "Quota in 2010", 
            "treat_2005" = "Quota in 2005",
            "treat_2015" = "Quota in 2015",
            "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
            "district_name_eng_2021" = "District (2021)",
            "block_name_eng_2021" = "Panchayat Block (2021)",
            "gp_name_eng_2021" = "Gram Panchayat (2021)"),
       replace = TRUE)


# 2005 * 2010 * 2015 full triple interaction

m_long_term <- feols((sex_2021 == "महिला") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(up_all, treat_2021 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((sex_2021 == "महिला") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((sex_2021 == "महिला") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((sex_2021 == "महिला") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_longterm_interaction.tex", 
       dict = c( 'female_cand_2021 =="FALSE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "district_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)


m_long_term_gpfe <- feols((sex_2021 == "महिला") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_long_term_gpfe)



# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2021 == "महिला") ~ once , data = filter(up_all, treat_2021 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2021 == "महिला") ~ once  | district_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2021 == "महिला") ~ once  | district_name_eng_2021 + block_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2021 == "महिला") ~ once  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_low_intensity.tex", 
       dict = c( 'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in 2005, 10 or 15",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((sex_2021 == "महिला") ~ twice , data = filter(up_all, treat_2021 == 0))
summary(m_twice)

m_twice_dfe <- feols((sex_2021 == "महिला") ~ twice  | district_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_twice_dfe)

m_twice_psfe <- feols((sex_2021 == "महिला") ~ twice  | district_name_eng_2021 + block_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_twice_psfe)

m_twice_gpfe <- feols((sex_2021 == "महिला") ~ twice  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_twice_gpfe)

# TeX
m_twice_list <- list(m_twice, m_twice_dfe, m_twice_psfe, m_twice_gpfe)

etable(m_twice_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_long_term_med_intensity.tex", 
       dict = c( 'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice" = "Treated twice in 2005, 2010 and 2010",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

# never treated

m_no_treat <- feols((sex_2021 == "महिला") ~ never_treated , data = filter(up_all, treat_2021 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2021 == "महिला") ~ never_treated  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , data = filter(up_all, treat_2021 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'sex_2021 == "महिला"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_name_eng_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)










# WWW v OOO seats ---------------------------------------------------------

filtered_data <- filter(up_all, treat_2021 == 0)

filtered_data <- filtered_data %>%
     mutate(max_compare = case_when(
          always_treated == 1 ~ "Always Treated",
          never_treated == 1 ~ "Never Treated",
          TRUE ~ "Other"
     ))

filtered_data <- filter(filtered_data, max_compare %in% c("Always Treated", "Never Treated"))


m_max_compare <- feols((sex_2021 == "महिला") ~ max_compare, data = filtered_data)
summary(m_max_compare) 

m_max_compare_dfe <- feols((sex_2021 == "महिला") ~ max_compare | district_name_eng_2021 , data = filtered_data)
summary(m_max_compare_dfe)

m_max_compare_psfe <- feols((sex_2021 == "महिला") ~ max_compare | district_name_eng_2021 + block_name_eng_2021 , data = filtered_data)
summary(m_max_compare_psfe)

m_max_compare_list <- list(m_max_compare, m_max_compare_dfe, m_max_compare_psfe)

etable(m_max_compare_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/up_long_term_max_compare.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "max_compareNever Treated" = "$WWW$ v $OOO$ seats comparison", 
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)

