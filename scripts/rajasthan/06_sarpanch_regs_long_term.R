# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)

# Load data ---------------------------------------------------------------

load("data/rajasthan/sarpanch_election_data/raj_panch.RData")

# summary(lm((sex_2021 =="महिला") ~ always_treated, data = subset(up_all, treat_2021 == 0)))
# summary(lm((sex_2021 =="महिला") ~ as.factor(count_treated), data = subset(up_all, treat_2021 == 0)))
# summary(lm((sex_2021 =="महिला") ~ as.factor(treat_all), data = subset(up_all, treat_2021 == 0)))
# summary(lm((sex_2021 =="महिला") ~ treat_2005*treat_2010*treat_2015, data = subset(up_all, treat_2021 == 0)))
#   
#   
# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | I(paste0(district_2020 ,ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_gpfe)




# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/raj_longterm_interaction.tex", 
       dict = c('sex_2020 == "F"' = "2020 Rep is a Woman in an Open Seat in Raj", 
                "treat_2010" = "Quota in 2010", 
                "treat_2005" = "Quota in 2005",
                "treat_2015" = "Quota in 2015",
                "always_treated" = "Always Treated (Quota in 2005, 2010, & 2015)",
                "district_2020" = "District (2020)",
                "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                "ps_2020" = "Panchayat Samiti (2020)",
                "gp_2020" = "Gram Panchayat (2020)"),
       se.row = FALSE,
       replace = TRUE)




  # always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2020 == "F") ~ always_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2020 == "F") ~ always_treated  | I(paste0(district_2020 ,ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/long_term_always.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)



# Never treated treat_2005 + treat_2010 + treat_2015 = 0

  
m_never <- feols((sex_2020 == "F") ~ never_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never)

m_never_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020,   data = filter(raj_panch, treat_2020 == 0))
summary(m_never_dfe)

m_never_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_psfe)

m_never_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020,ps_2020)) ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_gpfe)


# TeX
m_never_list <- list(m_never, m_never_dfe, m_never_psfe, m_never_gpfe)

etable(m_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/long_term_never.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "never_treated" = "Never treated (No quota in 2005, 10, & 15)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2020 == "F") ~ sometimes_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 + gp_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_sometimes.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)


# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2020 == "F") ~ once , data = filter(raj_panch, treat_2020 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2020 == "F") ~ once  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2020 == "F") ~ once  | district_2020 + ps_2020,   data = filter(raj_panch, treat_2020 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2020 == "F") ~ once  |  I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_low_intensity.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in either 2005, 2010 or 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((sex_2020 == "F") ~ twice , data = filter(raj_panch, treat_2020 == 0))
summary(m_twice)

m_twice_dfe <- feols((sex_2020 == "F") ~ twice  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_dfe)

m_twice_psfe <- feols((sex_2020 == "F") ~ twice  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_psfe)

m_twice_gpfe <- feols((sex_2020 == "F") ~ twice  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_gpfe)

# TeX
m_twice_list <- list(m_twice, m_twice_dfe, m_twice_psfe, m_twice_gpfe)

etable(m_twice_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_med_intensity.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice" = "Treated twice in 2005, 2010 and 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)



# never treated

m_no_treat <- feols((sex_2020 == "F") ~ never_treated , data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020 ,ps_2020)) ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_never.tex" , 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_2020" = "District (2020)",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)




# WWW v OOO seats ---------------------------------------------------------

raj_max_contrast <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast <- raj_max_contrast %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast$treatment_category <- factor(raj_max_contrast$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always <- feols((sex_2020 == "F") ~ treatment_category, data = raj_max_contrast)
summary(m_never_v_always) 

m_never_v_always_dfe <- feols((sex_2020 == "F") ~ treatment_category | district_2020   , data = raj_max_contrast)
summary(m_never_v_always_dfe) 

m_never_v_always_psfe <- feols((sex_2020 == "F") ~ treatment_category | district_2020 + ps_2020 , data = raj_max_contrast)
summary(m_never_v_always_psfe) 

m_never_v_always_gpfe <- feols((sex_2020 == "F") ~ treatment_category | I(paste0(district_2020 ,ps_2020)), data = raj_max_contrast)
summary(m_never_v_always_gpfe) 




m_raj_max_compare_list <- list(m_never_v_always, m_never_v_always_dfe, m_never_v_always_psfe,m_never_v_always_gpfe)


etable(m_raj_max_compare_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/raj_max_contrast.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treatment_categoryAlways Treated" = "Always treated vs. Never Treated", 
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)


# Correlation across treatment statuses

model1 <- feols(treat_2020 ~ treat_2015, data = raj_panch)
model2 <- feols(treat_2015 ~ treat_2010, data = raj_panch)
model3 <- feols(treat_2010 ~ treat_2005, data = raj_panch)
model4 <- feols(treat_2020 ~ treat_2005 + treat_2010 + treat_2015, data = raj_panch)

models <- list(model1, model2, model3, model4)

etable(models, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/raj_treatment_reg.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020"), 
       replace = TRUE)



# Main Tables -----------------------------------------------------------

m_always_lt_gpfe <- feols((sex_2020 == "F") ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)

m_never_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_gpfe)

m_sometimes_gpfe <- feols((sex_2020 == "F") ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_gpfe <- feols((sex_2020 == "F") ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_gpfe) 

models_long_term_list <- list(m_always_lt_gpfe, m_never_gpfe, m_sometimes_gpfe,m_never_v_always_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_main_long_term_models.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       replace = TRUE)
