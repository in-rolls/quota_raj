load("data/up/up_all_recoded.RData")


up_all <- up_all %>%
     mutate(treatment_status = case_when(
          (treat_2005 + treat_2010) == 2 ~ 2,  # Twice treated
          (treat_2005 + treat_2010) == 0 ~ 0,  # Never treated
          (treat_2005 + treat_2010) > 0 ~ 1    # Sometimes treated
     ))

# treatment_status to factor
up_all$treatment_status <- as.factor(up_all$treatment_status)

#  (Never treated) as the reference category
up_all$treatment_status <- relevel(up_all$treatment_status, ref = "0")

# Check levels to confirm g
levels(up_all$treatment_status)  


# Dosage Tables -----------------------------------------------------------

m_dosage <- feols((sex_2015 == "महिला") ~ treatment_status, 
                  data = filter(up_all, treat_2015 == 0))
summary(m_dosage)


m_dosage_gpfe <- feols((sex_2015 == "महिला") ~ treatment_status|gp_name_2015,
                       data = filter(up_all, treat_2015 == 0))
summary(m_dosage_gpfe)

models_long_term_list <- list(m_dosage, m_dosage_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_main_long_dosage.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2015 == "महिला"' = "2015 rep is a woman in an open sea in UP",
                 'treatment_status' = "Treatment Status: Never Treated, Sometimes Treated, Always Treated", 
                 'treatment_status1' = "Sometimes Treated",
                 'treatment_status2' = "Always Treated", 
                 'gp_name_2015' = "GP"),
       digits = 3,
       se.row = FALSE,
       replace = TRUE)




