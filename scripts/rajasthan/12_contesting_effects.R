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
     ungroup() %>% 
     group_by(nameofgrampanchayat) %>% 
     mutate(num_women = sum(women_contestants_bin, na.rm = TRUE),
            total_contestants = n()) %>% 
ungroup()


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")
raj_panch <- raj_panch %>% mutate(key_2020 = tolower(key_2020))


contest_prop <- contest %>% select(key_2020, prop_women, women_present, num_women,total_contestants)
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



# Number of Women ---------------------------------------------------------

m_always_num_gpfe <- feols(num_women ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_num_gpfe)


m_never_num_gpfe <- feols(num_women ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_num_gpfe)

m_sometimes_num_gpfe <- feols(num_women ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_num_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_num_gpfe <- feols(num_women ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_num_gpfe) 

models_contests_list <- list(m_always_num_gpfe, m_never_num_gpfe, m_sometimes_num_gpfe,m_never_v_always_num_gpfe)


etable(models_contests_list, 
        tex = TRUE, 
        style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
        interaction.combine = " $\times$ ",
        file = "tables/raj_contesting_numW.tex",
        dict = c( 
             'treat_2005' = "Treatment 2005",
             'num_women' = "Number of Women running in an Open Seat in Raj",
             'treat_2010' = "Treatment 2010",
             'treat_2015' = "Treatment 2015",
             'treat_2020' = "Treatment 2020",
             "treatment_categoryAlways Treated" = "Max Contrast ($WWW$ vs. $OO0$)", 
             "never_treated" = "Never treated in 2005, 2010, and 2015",
             "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
             "always_treated" = "Always treated (quota in 2005, 10, & 15)"
        ), 
        replace = TRUE)



# Total Contestats  ---------------------------------------------------------

m_always_num_gpfe <- feols(total_contestants ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_num_gpfe)


m_never_num_gpfe <- feols(total_contestants ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_num_gpfe)

m_sometimes_num_gpfe <- feols(total_contestants ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_num_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_num_gpfe <- feols(total_contestants ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_num_gpfe) 

models_contests_list <- list(m_always_num_gpfe, m_never_num_gpfe, m_sometimes_num_gpfe,m_never_v_always_num_gpfe)


etable(models_contests_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/raj_contesting_total.tex",
       dict = c( 
            'treat_2005' = "Treatment 2005",
            'total_contestants' = "Total Contestants in an Open Seat in Raj",
            'treat_2010' = "Treatment 2010",
            'treat_2015' = "Treatment 2015",
            'treat_2020' = "Treatment 2020",
            "treatment_categoryAlways Treated" = "Max Contrast ($WWW$ vs. $OO0$)", 
            "never_treated" = "Never treated in 2005, 2010, and 2015",
            "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
            "always_treated" = "Always treated (quota in 2005, 10, & 15)"
       ), 
       replace = TRUE)





# number of women multiplicative ------------------------------------------

m_num_mul <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_num_mul)

m_num_mul_dfe <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_num_mul_dfe)

m_num_mul_psfe <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_num_mul_psfe)

m_num_mul_gpfe <- feols(num_women ~ treat_2005 * treat_2010 * treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_num_mul_gpfe)

models_mul_list <- list(m_num_mul, m_num_mul_dfe, m_num_mul_psfe, m_num_mul_gpfe)


etable(models_mul_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_num_contest.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'num_women' = "Number of women running in an open seat",
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





# FEmale Cand Vote Share --------------------------------------------------


rm(list=ls())

#has 
winner_sarpanch <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/WinnerSarpanch_2020.csv")
winner_sarpanch <- winner_sarpanch %>% 
     rename_all(tolower) %>% 
     mutate_all(tolower) %>% 
     filter(electiontype!="by election") %>% 
     mutate(key_2020 = trimws(key_2020))

contest_sarpanch <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
contest_sarpanch <- contest_sarpanch %>% 
     rename_all(tolower) %>% 
     mutate_all(tolower) %>% 
     filter(electiontype!="by election") %>% 
     mutate(key_2020 = paste0(district,panchayatsamiti,nameofgrampanchayat),
            key_2020 = trimws(key_2020))


get_gender <- contest_sarpanch %>% 
     select(nameofcontestingcandidate, gender) %>% 
     distinct(nameofcontestingcandidate, .keep_all = TRUE) %>% 
     rename(sex = gender)#not the best move


winner_sarpanch <- winner_sarpanch %>%
     left_join(get_gender, 
               by = c("runnerupcandidatename" = "nameofcontestingcandidate")) %>% 
     rename(runnerup_gender = sex)

winner_sarpanch <- winner_sarpanch %>%
     left_join(get_gender, 
               by = c("runnerupcandidatename" = "nameofcontestingcandidate")) %>% 
     rename(runnerup_gender = sex)

# Merge again to get winner gender
winner_sarpanch <- winner_sarpanch %>%
     left_join(get_gender, 
               by = c("winnercandidatename" = "nameofcontestingcandidate")) %>% 
     rename(winner_gender = sex)



winner_sarpanch <- winner_sarpanch %>%
     mutate(
          votesecurebywinner = as.numeric(gsub("[^0-9]", "", votesecurebywinner)),
          votesecurebyrunnerup = as.numeric(gsub("[^0-9]", "", votesecurebyrunnerup)),
          totalvalidvotes = as.numeric(gsub("[^0-9]", "", totalvalidvotes))
     )
winner_sarpanch <- winner_sarpanch %>% 
     mutate(
          fem_vote_share = case_when(
               winner_gender == "f" & runnerup_gender == "f" ~ (votesecurebywinner + votesecurebyrunnerup) / totalvalidvotes,
               winner_gender == "f" & runnerup_gender != "f" ~ votesecurebywinner / totalvalidvotes,
               winner_gender != "f" & runnerup_gender == "f" ~ votesecurebyrunnerup / totalvalidvotes,
               TRUE ~ 0  # No female candidates
          )
     )


winner_sarpanch <- winner_sarpanch %>% 
     mutate(top_two = votesecurebywinner + votesecurebyrunnerup,
            top_two_prop = top_two/totalvalidvotes) 

winner_sarpanch <- winner_sarpanch %>% 
     mutate(
          treat = ifelse(categoryofgrampanchyat %in% c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
     )


summary(winner_sarpanch$top_two_prop) #need this to justify using two two candidates only
summary(winner_sarpanch$top_two_prop[winner_sarpanch$treat == 0]) #need this to justify using two two candidates only


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")
raj_panch <- raj_panch %>% mutate(key_2020 = tolower(key_2020))


winner_sarpanch$new_key <- paste0(winner_sarpanch$district, winner_sarpanch$panchayatsamiti, winner_sarpanch$nameofgrampanchyat)
winner_sarpanch <- winner_sarpanch %>% 
     mutate(new_key = trimws(new_key)) 


winner_sarpanch$panchayatsamiti <- gsub("panchayat samiti", "", as.character(winner_sarpanch$panchayatsamiti))

winner_sarpanch$new_key <- paste0(gsub(" ", "", winner_sarpanch$district),
                           gsub(" ", "", winner_sarpanch$panchayatsamiti),
                           gsub(" ", "", winner_sarpanch$nameofgrampanchyat))

vote_share_analysis <- winner_sarpanch %>% select(new_key, fem_vote_share)
raj_panch <- raj_panch %>% left_join(vote_share_analysis, c("key_2020" = "new_key")) 

 
m_fem_vote_sh <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015 , data = filter(raj_panch, treat_2020 == 0))
summary(m_fem_vote_sh)

m_fem_vote_sh_dfe <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_fem_vote_sh_dfe)

m_fem_vote_sh_psfe <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015  | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_fem_vote_sh_psfe)

m_fem_vote_sh_gpfe <- feols(fem_vote_share ~ treat_2005 * treat_2010 * treat_2015  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_fem_vote_sh_gpfe)


m_always_fem_vote_sh_gpfe <- feols(fem_vote_share ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_fem_vote_sh_gpfe)

m_never_fem_vote_sh_gpfe <- feols(fem_vote_share ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_fem_vote_sh_gpfe)

m_sometimes_fem_vote_sh_gpfe <- feols(fem_vote_share ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_fem_vote_sh_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_votesh_gpfe <- feols(fem_vote_share ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_votesh_gpfe) 


m_fem_vote_sh_list <- list(m_fem_vote_sh, m_fem_vote_sh_dfe, m_fem_vote_sh_psfe, m_fem_vote_sh_gpfe,
                           m_always_fem_vote_sh_gpfe,m_never_fem_vote_sh_gpfe,m_sometimes_fem_vote_sh_gpfe,m_never_v_always_votesh_gpfe)


etable(m_fem_vote_sh_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/raj_fem_vote_share.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'fem_vote_share' = "Vote share of winner and runner-up in $2020$ open seat",
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


