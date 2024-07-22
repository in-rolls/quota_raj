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

summary(lm((sex_2015 =="महिला") ~ twice_treated, data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ as.factor(count_treated), data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ as.factor(treat_all), data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ treat_2005*treat_2010, data = subset(up_all, treat_2015 == 0)))

#always is twice here

summary(lm((sex_2015 =="महिला") ~ twice_treated, data = subset(up_all, treat_2015 == 0)))
# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2015 == "महिला") ~ twice_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2015 == "महिला") ~ twice_treated  | district_name_eng_2015,  data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2015 == "महिला") ~ twice_treated  |district_name_eng_2015 + block_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2015 == "महिला") ~ twice_treated | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_gpfe)



# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_always.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice_treated" = "Always treated (quota in 2005, & 10)",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District ,Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
se.row = FALSE,
       replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_never <- feols((sex_2015 == "महिला") ~ never_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_never)

m_never_dfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015,  data = filter(up_all, treat_2015 == 0))
summary(m_never_dfe)

m_never_psfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 + block_name_eng_2015 , data = filter(up_all, treat_2015 == 0))
summary(m_never_psfe)

m_never_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_never_gpfe)


# TeX
m_never_list <- list(m_never, m_never_dfe, m_never_psfe, m_never_gpfe)

etable(m_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated (No quota in 2005, & 10)",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2015 == "महिला") ~ sometimes_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_long_term_sometimes.tex", 
       dict = c(
            'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
            "treat_2010" = "Quota in 2010", 
            "treat_2005" = "Quota in 2005",
            "treat_2015" = "Quota in 2015",
            "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 > 0)",
            "district_name_eng_2015" = "District (2015)",
            "block_name_eng_2015" = "Panchayat Block (2015)",
            "gp_name_eng_2015" = "Gram Panchayat (2015)"),
       replace = TRUE)



# 2005 * 2010  interaction

m_long_term <- feols((sex_2015 == "महिला") ~   treat_2010 * treat_2005,  data = filter(up_all, treat_2015 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((sex_2015 == "महिला") ~ treat_2010 * treat_2005  | district_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((sex_2015 == "महिला") ~  treat_2010 * treat_2005  | district_name_eng_2015 + block_name_eng_2015 , data = filter(up_all, treat_2015 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((sex_2015 == "महिला") ~  treat_2010 * treat_2005  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_longterm_interaction.tex", 
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

coefs <- tidy(m_long_term_gpfe) %>%
     filter(term != "(Intercept)")  
ggplot(coefs, aes(x = term, y = estimate)) +
     geom_point(color = "red") +
     geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
     coord_flip() +
     labs(title = "DV: 2015 rep is a woman in an open seat in UP",
          caption = "Interaction of quotas in 2005 & 2010 (95% CI)")+ 
    
     theme_minimal() 
     



# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2015 == "महिला") ~ once , data = filter(up_all, treat_2015 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2015 == "महिला") ~ once  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2015 == "महिला") ~ once  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2015 == "महिला") ~ once  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_low_intensity.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in 2005, 10 or 15",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)


# never treated

m_no_treat <- feols((sex_2015 == "महिला") ~ never_treated , data = filter(up_all, treat_2015 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)






# WWW v OOO seats ---------------------------------------------------------

up_max_contrast <- filter(up_all, treat_2015 == 0 & (never_treated == 1 | twice_treated == 1))

up_max_contrast <- up_max_contrast %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Twice Treated"))


up_max_contrast$treatment_category <- factor(up_max_contrast$treatment_category, levels = c("Never Treated", "Twice Treated"))

m_never_v_twice <- feols((sex_2015 == "महिला") ~ treatment_category    , data = up_max_contrast)
summary(m_never_v_twice) 

m_never_v_twice_dfe <- feols((sex_2015 == "महिला") ~ treatment_category | district_name_eng_2015   , data = up_max_contrast)
summary(m_never_v_twice_dfe) 

m_never_v_twice_psfe <- feols((sex_2015 == "महिला") ~ treatment_category | district_name_eng_2015 + block_name_eng_2015 , data = up_max_contrast)
summary(m_never_v_twice_psfe) 

m_never_v_twice_gpfe <- feols((sex_2015 == "महिला") ~ treatment_category |I(paste0(district_name_eng_2015, block_name_eng_2015)), data = up_max_contrast)
summary(m_never_v_twice_gpfe) 




m_max_compare_list <- list(m_never_v_twice, m_never_v_twice_dfe, m_never_v_twice_psfe, m_never_v_twice_gpfe)
 
etable(m_max_compare_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/up_max_contrast.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open sea in UP", 
                 "treatment_categoryTwice Treated" = "Always treated vs. Never Treated", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Samiti (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015)" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)



# Correlation across treatment statuses

model2 <- feols(treat_2015 ~ treat_2010, data = up_all)
model3 <- feols(treat_2010 ~ treat_2005, data = up_all)
model4 <- feols(treat_2015 ~ treat_2005 + treat_2010, data = up_all)

models <- list(model2, model3, model4)

etable(models, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_treatment_reg.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015"), 
       replace = TRUE)






# Main Section Tables -----------------------------------------------------------


m_always_lt_gpfe <- feols((sex_2015 == "महिला") ~ twice_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_gpfe)


m_never_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_never_gpfe)

m_sometimes_gpfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_gpfe)

up_max_contrast_poster <- filter(up_all, treat_2015 == 0 & (never_treated == 1 | twice_treated == 1))

up_max_contrast_poster <- up_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Twice Treated"))


up_max_contrast_poster$treatment_category <- factor(up_max_contrast_poster$treatment_category, levels = c("Never Treated", "Twice Treated"))

m_never_v_twice_gpfe <- feols((sex_2015 == "महिला") ~ treatment_category | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = up_max_contrast_poster)
summary(m_never_v_twice_gpfe) 

models_long_term_list <- list(m_always_lt_gpfe, m_never_gpfe, m_sometimes_gpfe, m_never_v_twice_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_main_long_term_models.tex",
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open sea in UP", 
                 "treatment_categoryTwice Treated" = " Max Contrast ($WW$ v. $OO$)", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Samiti (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)",
                 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "never_treated" = "Never treated in 2005, and 2010",
                 "sometimes_treated" = "Sometimes treated (Treatment in 2005 or 2010)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)

