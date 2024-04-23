
library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(tidyr)
library(fixest)
library(kableExtra)
# Load data ---------------------------------------------------------------

data_dir <- here("..", "data/kerala")
kerala_files <- here(data_dir, "kerala_wide.csv")
kerala_wide <- readr::read_csv(kerala_files)


# Transition Matrices -----------------------------------------------------

trans_10_15 <- table(kerala_wide$treat_2010, kerala_wide$treat_2015)
trans_15_20 <- table(kerala_wide$treat_2015, kerala_wide$treat_2020)

#comparison with 2010 - 2015

trans_10_15 <- table(kerala_wide$treat_2010, kerala_wide$treat_2015)
trans_10_20 <- table(kerala_wide$treat_2010, kerala_wide$treat_2020)

print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20,
     `2010-2015` = trans_10_15,
     `2010-2020` = trans_10_20
))


# Chi-Squared Test --------------------------------------------------------

chi_sq_test_10_15 <- chisq.test(trans_10_15)
chi_sq_test_10_15 #done

chi_sq_test_15_20 <- chisq.test(trans_15_20)
chi_sq_test_15_20#

chi_sq_test_10_20 <- chisq.test(trans_10_20)
chi_sq_test_10_20#done


chi_squared_results <- data.frame(
     "Comparison" = c( "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = here("..", "tables", "kerala_chi_squared_results.tex"))



# Models ------------------------------------------------------------------

m_10_15 <- feols(I(gender_2015 == "Female") ~ treat_2010, data = filter(kerala_wide, treat_2015 == 0))
summary(m_10_15)

m_15_20 <- feols(I(gender_2020 == "Female") ~ treat_2015, data = filter(kerala_wide, treat_2020 == 0))
summary(m_15_20)

m_interact <- feols(I(gender_2020 == "Female") ~ treat_2010 * treat_2015, data = filter(kerala_wide, treat_2020 == 0))
summary(m_interact)



kerala_models_list <- list(m_10_15, m_15_20, m_interact)
etable(kerala_models_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "kerala_models.tex"), 
       dict = c('gender_2015 == "Female"' = "2015 representative is a woman in an open seat", 
                'gender_2020 == "Female"' = "2020 representative is a woman in an open seat", 
                "treat_2010" = "Quota Treatment in 2010", 
                "treat_2015" = "Quota Treatment in 2015"),
       replace = TRUE)

levels(as.factor(kerala_wide$never_treated))
levels(as.factor(kerala_wide$always_treated))
levels(as.factor(kerala_wide$sometimes_treated))

mean(kerala_wide$sometimes_treated)# 0.52
mean(kerala_wide$always_treated) # 0.03568372
mean((kerala_wide$never_treated)) # 0.004740273


