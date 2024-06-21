

# Long Term ---------------------------------------------------------------

# Load data ---------------------------------------------------------------

# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(up_all, treat_2021 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021, vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((female_cand_2021 =="TRUE") ~ treat_2015 * treat_2010 * treat_2005  | district_name_eng_2021 + block_name_eng_2021 + gp_name_eng_2021 , vcov = ~gp_name_eng_2021, data = filter(up_all, treat_2021 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/up_longterm_interaction.tex", 
       dict = c( 'female_cand_2021 =="FALSE"' = "2021 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "district_2021" = "District (2021)",
                 "block_name_eng_2021" = "Panchayat Block (2021)",
                 "gp_name_eng_2021" = "Gram Panchayat (2021)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)






# Notes -------------------------------------------------------------------

# 1. Main analysis will be 05-10 for both Rajasthan and UP where we are super confident about the randomization
2. for 15-20 we will























# Load fuzzy
up_05_10_ff <- read_parquet("data/up/up_05_10_fuzzy.parquet")
up_10_15_ff <- read_parquet("data/up/up_10_15_fuzzy.parquet")

# Random or not
#------------------
with(up_05_10, summary(lm(female_res_2010 ~ female_res_2005)))
with(up_05_10_ff, summary(lm(female_res_2010 ~ female_res_2005)))

with(up_10_15, summary(lm(female_res_2015 ~ female_res_2010)))
with(up_10_15_ff, summary(lm(female_res_2015 ~ female_res_2010)))

with(up_15_21, summary(lm(female_res_2021 ~ female_res_2015)))

with(up_all, summary(lm(female_res_2010 ~ female_res_2005)))
with(up_all, summary(lm(female_res_2015 ~ female_res_2010)))

# ---2021 looks super non-random
with(up_all, summary(lm(female_res_2021 ~ female_res_2015)))
with(up_all, summary(lm(female_res_2021 ~ female_res_2015 + female_cand_2015 + female_res_2010 + female_res_2005)))

# Effects
# ------------------

# 10 on 05
with(up_all[up_all$female_res_2010 == 0,], summary(lm(female_cand_2010 ~ female_res_2005)))
with(up_05_10[up_05_10$female_res_2010 == 0,], summary(lm(female_cand_2010 ~ female_res_2005)))

# 15 on 10 and 05
with(up_all[up_all$female_res_2015 == 0,], summary(lm(female_cand_2015 ~ female_res_2010 + female_res_2005)))
with(up_all[up_all$female_res_2015 == 0,], summary(lm(female_cand_2015 ~ female_res_2010*female_res_2005)))

# 20 on 15, 10, and 05
with(up_all[up_all$female_res_2021 == 0,], summary(lm(female_cand_2021 ~ female_res_2015 + female_res_2010 + female_res_2005)))
with(up_all[up_all$female_res_2021 == 0,], summary(lm(female_cand_2021 ~ female_res_2015*female_res_2010*female_res_2005)))

with(up_all[up_all$female_res_2021 == 0,], summary(lm(female_cand_2021 ~ as.factor(total_res) + as.numeric(female_cand_2015))))

#Ward Members
chi_squared_results <- data.frame(
     "Comparison" = c( "2010-2015", "2010-2021", "2015-2021"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = "tables/up_chi_squared_results.tex")







# Dalit Models ------------------------------------------------------------
# Presidents and VPs ------------------------------------------------------


#Where the offices of Presidents and Chairpersons is reserved for women, the offices of the Vice Presidents and
# Deputy Chairpersons will be unreserved and where the offices of the Presidents and Chairpersons not reserved for women, 
# the offices of the Vice Presidents and Deputy Chairpersons will be reserved for women.


levels(as.factor(up_panch$role))

up_panch <- up_panch %>% 
     mutate(president = ifelse(up_panch$role == 'President',1,0),
            vice_president = ifelse(up_panch$role == 'Vice President',1,0))

up_panch <- up_panch %>% 
     mutate(prez_treat = ifelse(president == 1 & up_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            vp_treat = ifelse(vice_president == 1 & up_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"),1,0),
            prez_treat_2010 = ifelse(prez_treat == 1 & year == 2010,1,0),
            prez_treat_2015 = ifelse(prez_treat == 1 & year == 2015,1,0),
            prez_treat_2020 = ifelse(prez_treat == 1 & year == 2020,1,0),
            vp_treat_2010 = ifelse(vice_president == 1 & year == 2010,1,0),
            vp_treat_2015 = ifelse(vice_president == 1 & year == 2015,1,0),
            vp_treat_2020 = ifelse(vice_president == 1 & year == 2020,1,0),
            prez_always = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 3,1,0),
            prez_never = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 0,1,0),
            prez_twice = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 2,1,0),
            vp_always = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 3,1,0),
            vp_never = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 0,1,0),
            vp_twice = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 2,1,0))


levels(as.factor(up_panch$prez_always))
levels(as.factor(up_panch$prez_never))
levels(as.factor(up_panch$prez_twice))

levels(as.factor(up_panch$vp_always))
levels(as.factor(up_panch$vp_never))
levels(as.factor(up_panch$vp_twice))





