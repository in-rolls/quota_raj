# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)

# Random or not
#------------------
with(up_05_10, summary(lm(female_res_2010 ~ female_res_2005)))
with(up_10_15, summary(lm(female_res_2015 ~ female_res_2010)))
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
     "Comparison" = c( "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = here("..", "tables", "up_chi_squared_results.tex"))







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





