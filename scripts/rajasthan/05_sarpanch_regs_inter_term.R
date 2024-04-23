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

# Intermediate Effect treat_2005 * treat_2010 -----------------------------
# interaction of treat_2005 and treat_2010

#ecologiucal thing. Constsat capturing the eological trend. Treat_2010. Next step always trreated vs never treated. keep these units and compare. 

m_05_10_15 <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15)

m_05_10_15_dfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_dfe)

m_05_10_15_psfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_psfe)

m_05_10_15_gpfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_gpfe)

models_05_10_15_list <- list(m_05_10_15, m_05_10_15_dfe, m_05_10_15_psfe, m_05_10_15_gpfe)

etable(models_05_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "intermediate_05X10.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)


# etable(models_05_10_15_list, tex = TRUE, file = here("..", "tables", "intermediate_05*10.tex"), title = "Interaction Effects Treat 2005 * 2010", placement = "htbp", replace = TRUE)

# Intermediate Always Treated 

m_inter_always <- feols((sex_manual_2015 == "F") ~ inter_always_treated, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always)

m_inter_always_dfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  |  dist_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_dfe)

m_inter_always_psfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_psfe)

m_inter_always_gpfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_psfe)



# TeX
m_inter_always_list <- list(m_inter_always, m_inter_always_dfe, m_inter_always_psfe, m_inter_always_gpfe)

etable(m_inter_always_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_always_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_always_treated" = "Always treated (quota in 2005, \& 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# Intermediate Never Treated 

m_inter_never <- feols((sex_manual_2015 == "F") ~ inter_never_treated, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never)

m_inter_never_dfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015, vcov = ~gp_2015,   data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_dfe)

m_inter_never_psfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_psfe)

m_inter_never_gpfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_gpfe)

# TeX
m_inter_never_list <- list(m_inter_never, m_inter_never_dfe, m_inter_never_psfe, m_inter_never_gpfe)


etable(m_inter_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_never_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_never_treated" = "Never treated (no quota in 2005, \& 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# Intermediate Sometimes Treated

m_inter_sometimes <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes) 

m_inter_sometimes_dfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_dfe)

m_inter_sometimes_psfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_psfe)

m_inter_sometimes_gpfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_gpfe)


# TeX
m_inter_sometimes_list <- list(m_inter_sometimes, m_inter_sometimes_dfe, m_inter_sometimes_psfe, m_inter_sometimes_gpfe)

etable(m_inter_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_sometimes_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_sometimes_treated" = "Sometimes treated (quota in 2005 or 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)
