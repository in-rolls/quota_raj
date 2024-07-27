# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)


contest <- read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
colnames(contest) <- tolower(colnames(contest))

contest <- contest %>%
     mutate_all(tolower)

contest <- contest %>% 
     mutate(women_contestants_bin = ifelse((contest$gender=="f"), 1, 0))
contest$women_contestants_bin <- as.integer(contest$women_contestants_bin)


contest$panchayatsamiti <- gsub("panchayat samiti", "", as.character(contest$panchayatsamiti))

contest$key_2020 <- paste0(gsub(" ", "", contest$district),
                           gsub(" ", "", contest$panchayatsamiti),
                           gsub(" ", "", contest$nameofgrampanchayat))


contest <- contest %>% 
     group_by(key_2020) %>% 
     mutate(prop_women = mean(women_contestants_bin), na.rm=TRUE) %>% 
     mutate(women_present = ifelse(sum(women_contestants_bin, na.rm = TRUE) > 0, 1, 0)) %>%
     ungroup()


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")
raj_panch <- raj_panch %>% mutate(key_2020 = tolower(key_2020))


contest_prop <- contest %>% select(key_2020, prop_women, women_present)
contest_prop <- contest_prop %>% distinct()
raj_panch <- raj_panch %>% left_join(contest_prop, by = "key_2020") #raj_panch$contest_prop has proportion of female contestants. Logic test, prop_women should be 1 in treat_2020==1

test <- raj_panch %>% select(treat_2020, prop_women)
test <- test %>% filter(treat_2020==1)
test <- test %>% filter(prop_women<1) #about 1% observations. Safe to drop?


# Contestant models -------------------------------------------------------

     
m_always_lt_gpfe <- feols(prop_women ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)

m_never_gpfe <- feols(prop_women ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_gpfe)

m_sometimes_gpfe <- feols(prop_women ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_gpfe <- feols(prop_women ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_gpfe) 

models_contests_list <- list(m_always_lt_gpfe, m_never_gpfe, m_sometimes_gpfe,m_never_v_always_gpfe)


etable(models_contests_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_contesting.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'prop_women' == "Proportion of women running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       replace = TRUE)





# Additive Effect ---------------------------------------------------------


m_prop <- feols(prop_women ~ treat_2005 + treat_2010 + treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_prop)

m_prop_dfe <- feols(prop_women ~ treat_2005 + treat_2010 + treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_dfe)

m_prop_psfe <- feols(prop_women ~ treat_2005 + treat_2010 + treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_psfe)

m_prop_gpfe <- feols(prop_women ~ treat_2005 + treat_2010 + treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_gpfe)

models_add_list <- list(m_prop, m_prop_dfe, m_prop_psfe, m_prop_gpfe)


etable(models_add_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_add_contest.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'prop_women' == "Proportion of women running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'district_2020' = "District 2020",
                 'I(paste0(district_2020, ps_2020))' = "(District, Samiti)",
                 'ps_2020' = "Panchayat Samiti 2020",
                 'treat_2020' = "Treatment 2020"),
       se.row=FALSE,
       replace = TRUE)



# Multiplicative Effect ---------------------------------------------------------


m_prop_mul <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_mul)

m_prop_mul_dfe <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_mul_dfe)

m_prop_mul_psfe <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_mul_psfe)

m_prop_mul_gpfe <- feols(prop_women ~ treat_2005 * treat_2010 * treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_prop_mul_gpfe)

models_mul_list <- list(m_prop_mul, m_prop_mul_dfe, m_prop_mul_psfe, m_prop_mul_gpfe)


etable(models_mul_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_mul_contest.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'prop_women' == "Proportion of women running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 'district_2020' = "District 2020",
                 'I(paste0(district_2020, ps_2020))' = "(District, Samiti)",
                 'ps_2020' = "Panchayat Samiti 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       se.row=FALSE,
       replace = TRUE)


# At least One women Runs Models ------------------------------------------


m_always_atleast_gpfe <- feols(I(women_present) ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_atleast_gpfe)

m_never_atleast_gpfe <- feols(I(women_present) ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_atleast_gpfe)

m_sometimes_atleast_gpfe <- feols(I(women_present) ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_atleast_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_atleast_gpfe <- feols(I(women_present) ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_atleast_gpfe) 

models_contests_list <- list(m_always_atleast_gpfe, m_never_atleast_gpfe, m_sometimes_atleast_gpfe, m_never_v_always_atleast_gpfe)


etable(models_contests_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_atleast_one.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'I(women_present)' == "Proportion of women running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       replace = TRUE)



# Additive Effect ---------------------------------------------------------


m_oneW_mul <- feols(I(women_present) ~ treat_2005 + treat_2010 + treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_mul)

m_oneW_mul_dfe <- feols(I(women_present) ~ treat_2005 + treat_2010 + treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_mul_dfe)

m_oneW_mul_psfe <- feols(I(women_present) ~ treat_2005 + treat_2010 + treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_mul_psfe)

m_oneW_mul_gpfe <- feols(I(women_present) ~ treat_2005 + treat_2010 + treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_mul_gpfe)

m_oneW_mul_list <- list(m_oneW_mul, m_oneW_mul_dfe, m_oneW_mul_psfe, m_oneW_mul_gpfe)


etable(m_oneW_mul_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_oneW_add.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'I(women_present)' == "At least one woman running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'district_2020' = "District 2020",
                 'I(paste0(district_2020, ps_2020))' = "(District, Samiti)",
                 'ps_2020' = "Panchayat Samiti 2020",
                 'treat_2020' = "Treatment 2020"),
       se.row=FALSE,
       replace = TRUE)



# Multiplicative Effect ---------------------------------------------------------

m_oneW <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW)

m_oneW_dfe <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_dfe)

m_oneW_psfe <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_psfe)

m_oneW_gpfe <- feols(I(women_present) ~ treat_2005 * treat_2010 * treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_oneW_gpfe)

models_mul_list <- list(m_oneW, m_oneW_dfe, m_oneW_psfe, m_oneW_gpfe)


etable(models_mul_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $ \\times $ ",
       file = "tables/raj_oneW_mul.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'I(women_present)' == "At least one woman running in an open seat",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'district_2020' = "District 2020",
                 'I(paste0(district_2020, ps_2020))' = "(District, Samiti)",
                 'ps_2020' = "Panchayat Samiti 2020",
                 'treat_2020' = "Treatment 2020"),
       se.row=FALSE,
       replace = TRUE)


