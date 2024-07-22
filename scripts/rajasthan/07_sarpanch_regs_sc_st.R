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

# SC/ST Short Term Effects ------------------------------------------------
# SC/ST 2005 --> 2010 -----------------------------------------------------

sc_subset_05 <- raj_panch %>% filter(sc_2010==1)

sc_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(sc_subset_05, dalit_2010 == 0))
summary(sc_05_10)

sc_05_10_fe <- feols((sex_2010 =="F") ~ treat_2005 | I(paste0(dist_name_2010, samiti_name_2010)), data = filter(sc_subset_05, dalit_2010 == 0))
summary(sc_05_10_fe)


# SC/ST 2010 --> 2015 -----------------------------------------------------

sc_subset_15 <- raj_panch %>% filter(sc_2015==1)

sc_10_15 <- feols((sex_manual_2015 =="F") ~ treat_2010, data = filter(sc_subset_15, dalit_2015 == 0))
summary(sc_10_15)

sc_10_15_fe <- feols((sex_manual_2015 =="F") ~ treat_2010 | I(paste0(dist_name_2015, samiti_name_2015)), data = filter(sc_subset_15, dalit_2015 == 0))
summary(sc_10_15_fe)



# SC/ST 2015 --> 2020 -----------------------------------------------------

sc_subset_20 <- raj_panch %>% filter(sc_2020==1)

sc_15_20 <- feols((sex_2020 =="F") ~ treat_2015, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_15_20)

sc_15_20_fe <- feols((sex_2020 =="F") ~ treat_2015 | I(paste0(district_2020, ps_2020)), data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_15_20_fe)


# TeX
sc_15_20_list <- list(sc_05_10, sc_05_10_fe, sc_10_15, sc_10_15_fe, sc_15_20, sc_15_20_fe)
etable(sc_15_20_list, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/raj_sc_short_term.tex", 
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 W rep in dalit seat", 
                 'sex_manual_2015 == "F"' = "2015 W rep in dalit seat", 
                 'sex_2010 == "F"' = "2010 W rep in dalit seat", 
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "I(paste0(district_2015, samiti_name_2015))" = "(District, Samiti)",
                 "I(paste0(district_2010, samiti_name_2010))" = "(District, Samiti)",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       se.row = FALSE,
       
       replace = TRUE)

# SC/ST Long Term Effects -------------------------------------------------

sc_subset_20 <- raj_panch %>% filter(sc_2020==1)

sc_lt <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_lt)

sc_lt_fe <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | I(paste0(dist_name_2020, samiti_name_2020)), data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_lt_fe)

# TeX
sc_lt_list <- list(sc_lt, sc_lt_fe)
etable(sc_lt_list, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/sc_lt.tex",
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