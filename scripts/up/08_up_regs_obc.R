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

# SC/ST Short Term Effects ------------------------------------------------
# SC/ST 2005 --> 2010 -----------------------------------------------------

obc_subset_10 <- up_all %>% filter(all_obc_2010==1)

obc_05_10 <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005, data = filter(obc_subset_10, obc_2010 == 0))
summary(obc_05_10)

obc_05_10_fe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | I(paste0(district_name_eng_2010, block_name_eng_2010)), data = filter(obc_subset_10, obc_2010 == 0))
summary(obc_05_10_fe)


# obc/ST 2010 --> 2015 -----------------------------------------------------

obc_subset_15 <- up_all %>% filter(all_obc_2015==1)

obc_10_15 <- feols((sex_2015 == "महिला") ~ treat_2010, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15)

obc_10_15_fe <- feols((sex_2015 == "महिला") ~ treat_2010 | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15_fe)



# TeX
obc_15_20_list <- list(obc_05_10, obc_05_10_fe, obc_10_15, obc_10_15_fe)
etable(obc_15_20_list, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/up_obc_short_term.tex", 
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2015 == "महिला"' = "2015 W rep in obc seat", 
                 'cand_sex_fin_2010 =="महिला"' = "2005 W rep in obc seat", 
                 'sex_2010 == "F"' = "2010 W rep in obc seat", 
                 " I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Samiti)",
                 "I(paste0(district_2015, samiti_name_2015))" = "(District, Samiti)",
                 " I(paste0(district_name_eng_2010, block_name_eng_2010))" = "(District, Samiti)",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       se.row = FALSE,
       
       replace = TRUE)

# obc/ST Long Term Effects -------------------------------------------------

obc_subset_15 <- up_all %>% filter(all_obc_2015==1)

obc_lt <- feols((sex_2015 == "महिला") ~ treat_2005 * treat_2010, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_lt)

obc_lt_fe <- feols((sex_2015 == "महिला")~ treat_2005 * treat_2010  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_lt_fe)

# TeX
obc_lt_list <- list(obc_lt, obc_lt_fe)
etable(obc_lt_list,  
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/up_obc_long_term.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2015 == "महिला"' = "2015 W rep in OBC seat", 
                 'sex_manual_2015 == "F"' = "2015 W rep in OBC seat", 
                 'sex_2010 == "F"' = "2010 W rep in OBC seat", 
                 "I(paste0(district_name_eng_2015, block_name_eng_2015)" = "(District, Samiti)",
                 "I(paste0(district_2015, samiti_name_2015))" = "(District, Samiti)",
                 "I(paste0(district_2010, samiti_name_2010))" = "(District, Samiti)",
                 'treat_2010' = "Treatment 2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = "Max Contrast ($WWW$ vs. $OO0$)", 
                 "never_treated" = "Never treated in 2005,  2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       se.row = FALSE,
       replace = TRUE)

