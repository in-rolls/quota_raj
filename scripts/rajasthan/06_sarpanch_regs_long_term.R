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

# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_gpfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | I(paste0(district_2020 ,ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_gpfe)

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

# Beamer
#par(bg = "#F5F5F5")
coefplot(m_long_term_gpfe,
         interaction.combine = " $\times $ ",
         dict = c('sex_2020 == "F"' = "2020 Rep is a Woman in an Open Seat in Raj", 
                  "treat_2010" = "2010 quota", 
                  "treat_2005" = "2005 quota",
                  "treat_2015" = "2015 quota",
                  'treat_2015:treat_2010:treat_2005' = "2005 x 2010 x 2015",
                  'treat_2015:treat_2010' = "2010 x 2015",
                  'treat_2015:treat_2005' = "2005 x 2015",
                  'treat_2010:treat_2005' = "2005 x 2010",
                  "always_treated" = "Always Treated (Quota in 2005, 2010, & 2015)",
                  "district_2020" = "District FE (2020)",
                  "ps_2020" = "Panchayat Samiti FE (2020)",
                  "gp_2020" = "Gram Panchayat FE (2020)",
                  '(Intercept)' = "Constant"),
         grid = FALSE,
         zero.par = list(col = "red", lwd = 1),
         # ci_level = 0.90,
         #= ci.fill = TRUE,
         # ci.join = TRUE,
          pt.join = TRUE,
         ci.lty = 1,
         horiz = TRUE,
         # bg = "#E0E0E0",
         main = "2020 rep is a woman in an open seat",
         sub = "Rajasthan")
