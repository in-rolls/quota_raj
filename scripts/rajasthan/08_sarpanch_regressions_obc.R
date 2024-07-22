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
# OBC short term effects --------------------------------------------------

# OBC 2005 --> 2010 -------------------------------------------------------

obc_subset <- raj_panch %>% filter(all_obc_2010==1)

obc_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(obc_subset, obc_2010 == 0))
summary(obc_05_10)

obc_05_10_fe <- feols((sex_2010 =="F") ~ treat_2005 | I(paste0(dist_name_2010, samiti_name_2010)), data = filter(obc_subset, obc_2010 == 0))
summary(obc_05_10_fe)


# OBC 2010 --> 2015 -------------------------------------------------------

obc_subset_15 <- raj_panch %>% filter(all_obc_2015==1)

obc_10_15 <- feols((sex_manual_2015 =="F") ~ treat_2010, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15)

obc_10_15_fe <- feols((sex_manual_2015 =="F") ~ treat_2010 | I(paste0(dist_name_2010, samiti_name_2010)), data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15_fe)


# OBC 2015 --> 2020 -------------------------------------------------------

obc_subset_20 <- raj_panch %>% filter(all_obc_2020==1)

obc_15_20 <- feols((sex_2020 =="F") ~ treat_2015, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_15_20)

obc_15_20_fe <- feols((sex_2020 =="F") ~ treat_2015 | I(paste0(district_2020, ps_2020)), data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_15_20_fe)


# TeX
obc_15_20_list <- list(obc_05_10, obc_05_10_fe, obc_10_15, obc_10_15_fe, obc_15_20, obc_15_20_fe)

etable(obc_15_20_list, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/raj_obc_short_term.tex", 
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 W rep in OBC seat", 
                 'sex_manual_2015 == "F"' = "2015 W rep in OBC seat", 
                 'sex_2010 == "F"' = "2010 W rep in OBC seat", 
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "I(paste0(district_2015, samiti_name_2015))" = "(District, Samiti)",
                 "I(paste0(district_2010, samiti_name_2010))" = "(District, Samiti)",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020"),
       se.row = FALSE, replace = TRUE)
       


# OBC Long Term -----------------------------------------------------------


obc_subset_20 <- raj_panch %>% filter(obc_2020==1)

obc_lt <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_lt)

obc_lt_fe <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | I(paste0(district_2020, ps_2020)), data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_lt_fe)

# TeX
obc_lt_list <- list(obc_lt, obc_lt_fe)
etable(obc_lt_list, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/raj_obc_long_term.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 W rep in dalit seat", 
                 'sex_manual_2015 == "F"' = "2015 W rep in dalit seat", 
                 'sex_2010 == "F"' = "2010 W rep in dalit seat", 
                 'I(paste0(district_2020, ps_2020))' = "(District, Samiti)",
                 
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = "Max Contrast ($WWW$ vs. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010, and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       se.row = FALSE,
       placement = "htbp", replace = TRUE)


