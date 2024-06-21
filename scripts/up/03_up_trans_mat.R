# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)


# Transition Matrices -----------------------------------------------------

load("data/up/up_all_recoded.RData")
# comparing wiht previous reservation status

trans_05_10 <- table(up_all$treat_2005, up_all$treat_2010)
trans_10_15 <- table(up_all$treat_2010, up_all$treat_2015)
trans_15_21 <- table(up_all$treat_2015, up_all$treat_2021)

trans_05_15 <- table(up_all$treat_2005, up_all$treat_2015)
trans_05_21 <- table(up_all$treat_2005, up_all$treat_2021)
trans_10_21 <- table(up_all$treat_2010, up_all$treat_2021)

print(trans_matrices <- list(
     `2005-2010` = trans_05_10,
     `2010-2015` = trans_10_15,
     `2015-2021` = trans_15_21,
     `2005-2015` = trans_05_15,
     `2005-2021` = trans_05_21,
     `2010-2015` = trans_10_15,
     `2010-2021` = trans_10_21
))

# Chi-Squared Test --------------------------------------------------------

chi_sq_test_05_10 <- chisq.test(trans_05_10) # continuity' correction not changing anything
chi_sq_test_05_10 #done

chi_sq_test_10_15 <- chisq.test(trans_10_15)
chi_sq_test_10_15 #done

chi_sq_test_15_21 <- chisq.test(trans_15_21)
chi_sq_test_15_21#

chi_sq_test_05_15 <- chisq.test(trans_05_15)
chi_sq_test_05_15#done

chi_sq_test_05_21 <- chisq.test(trans_05_21)
chi_sq_test_05_21#done

chi_sq_test_10_21 <- chisq.test(trans_10_21)
chi_sq_test_10_21#done


chi_squared_results <- data.frame(
     "Comparison" = c("2005-2010", "2005-2015", "2005-2021", "2010-2015", "2010-2021", "2015-2021"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_05_10$statistic, chi_sq_test_05_15$statistic, chi_sq_test_05_21$statistic, 
                                      chi_sq_test_10_15$statistic, chi_sq_test_10_21$statistic, chi_sq_test_15_21$statistic),
     "Degrees of Freedom" = c(chi_sq_test_05_10$parameter, chi_sq_test_05_15$parameter, chi_sq_test_05_21$parameter, 
                              chi_sq_test_10_15$parameter, chi_sq_test_10_21$parameter, chi_sq_test_15_21$parameter),
     "P-Value" = format(c(chi_sq_test_05_10$p.value, chi_sq_test_05_15$p.value, chi_sq_test_05_21$p.value, 
                          chi_sq_test_10_15$p.value, chi_sq_test_10_21$p.value, chi_sq_test_15_21$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = "tables/up_chi_squared_results.tex")


# Correlation across treatment statuses

model1 <- lm(treat_2021 ~ treat_2015, data = up_all)
model2 <- lm(treat_2015 ~ treat_2010, data = up_all)
model3 <- lm(treat_2010 ~ treat_2005, data = up_all)
model4 <- lm(treat_2021 ~ treat_2005 + treat_2010 + treat_2015, data = up_all)
library(stargazer)
stargazer(model1, model2, model3, model4,
          title = "Regression Results",
          dep.var.labels = c("Treatment 2021", "Treatment 2015", "Treatment 2010"),
          covariate.labels = c("Treatment 2015", "Treatment 2010", "Treatment 2005"),
          column.labels = c("21 $\\sim$ 15", "15 $\\sim$ 10", "10 $\\sim$ 5", "21 $\\sim$ 05 + 10 + 15"),
          align = TRUE,
          out = "tables/up_treatment_reg.tex")


