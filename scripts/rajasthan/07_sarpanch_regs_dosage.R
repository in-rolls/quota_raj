library(tidyverse)
library(fixest)
load("data/rajasthan/sarpanch_election_data/raj_panch.RData")


# Max Contrast Data Wrangling ---------------------------------------------
# always_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 3, 1, 0),
# never_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 0, 1, 0),
# sometimes_treated = ifelse(treat_2005 + treat_2010 + treat_2015 > 0, 1, 0),

# raj_panch <- raj_panch %>%
#      mutate(max_contrast = case_when(
#           always_treated == 1 ~ "Always Treated",
#           never_treated == 1 ~ "Never Treated",
#           TRUE ~ NA_character_ 
#      ))
# 
# 
# # # Convert max_contrast to a factor with levels for proper comparison
# # raj_panch$max_contrast <- factor(raj_panch$max_contrast, 
# #                                  levels = c("Never Treated", "Always Treated"))
# 

raj_panch <- raj_panch %>%
     mutate(treatment_status = case_when(
          treat_2005 + treat_2010 + treat_2015 == 0 ~ 0,  # Never treated
          treat_2005 + treat_2010 + treat_2015 > 0 & 
               treat_2005 + treat_2010 + treat_2015 < 3 ~ 1,  # Sometimes treated
          treat_2005 + treat_2010 + treat_2015 == 3 ~ 2   # Always treated
     ))

#  treatment_status to a factor
raj_panch$treatment_status <- as.factor(raj_panch$treatment_status)

#  '0' as the reference category
raj_panch$treatment_status <- relevel(raj_panch$treatment_status, ref = "0")

# Check levels 
levels(raj_panch$treatment_status)  

raj_panch <- raj_panch %>% 
     mutate(count_treated = (treat_2005 + treat_2010 + treat_2015))

unique(raj_panch$count_treated)

# Dosage Tables -----------------------------------------------------------

m_dosage_old <- feols((sex_2020 == "F") ~ treatment_status, 
                  data = filter(raj_panch, treat_2020 == 0))
summary(m_dosage_old)

m_dosage_gpfe <- feols((sex_2020 == "F") ~ treatment_status| gp_2020, 
                       data = filter(raj_panch, treat_2020 == 0))
summary(m_dosage_gpfe)


# new dosage spec ---------------------------------------------------------

m_dosage_new <- feols((sex_2020 == "F") ~ as.factor(count_treated), 
                  data = filter(raj_panch, treat_2020 == 0))
summary(m_dosage_new)

m_dosage_new_fe <- feols((sex_2020 == "F") ~ as.factor(count_treated)| gp_2020,, 
                      data = filter(raj_panch, treat_2020 == 0))
summary(m_dosage_new_fe)

coefplot(m_dosage_new_fe)
coefplot(m_dosage_new)


models_long_term_list <- list(m_dosage, m_dosage_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_main_long_dosage.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 'treatment_status' = "Treatment Status: Never Treated, Sometimes Treated, Always Treated", 
                 'treatment_status1' = "Sometimes Treated",
                 'treatment_status2' = "Always Treated",
                 'gp_2020' = "GP"),
       digits = 3,
       se.row = FALSE,
       replace = TRUE)




