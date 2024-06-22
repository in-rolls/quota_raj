# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(broom)
library(purrr)
library(fixest)
library(stargazer)

# Load all
load("data/up/up_all_recoded.RData")
# Transition Matrices -----------------------------------------------------
# comparing with previous reservation status

trans_05_10 <- with(up_all, table(treat_2005, treat_2010))
trans_10_15 <- with(up_all, table(treat_2010, treat_2015))
trans_15_21 <- with(up_all, table(treat_2015, treat_2021))

trans_05_15 <- with(up_all, table(treat_2005, treat_2015))
trans_05_21 <- with(up_all, table(treat_2005, treat_2021))
trans_10_21 <- with(up_all, table(treat_2010, treat_2021))

print(trans_matrices <- list(
     `2005-2010` = trans_05_10,
     `2010-2015` = trans_10_15,
     `2015-2021` = trans_15_21,
     `2005-2015` = trans_05_15,
     `2005-2021` = trans_05_21,
     `2010-2015` = trans_10_15,
     `2010-2021` = trans_10_21
))

comparisons <- list(
     "2005-2010" = trans_05_10,
     "2010-2015" = trans_10_15,
     "2015-2021" = trans_15_21,
     "2005-2015" = trans_05_15,
     "2005-2021" = trans_05_21,
     "2010-2021" = trans_10_21
)

# Perform chi-squared tests and collect results
chi_squared_results <- lapply(comparisons, chisq.test)

# Create a data frame from the results
chi_squared_summary <- data.frame(
     "Comparison" = names(chi_squared_results),
     "Chi-Squared Test Statistic" = sapply(chi_squared_results, function(x) x$statistic),
     "Degrees of Freedom" = sapply(chi_squared_results, function(x) x$parameter),
     "P-Value" = format(sapply(chi_squared_results, function(x) x$p.value), digits = 4)
)

# Print the summary table
print(chi_squared_summary)

kable(chi_squared_summary,
      format = "latex", 
      caption = "Chi-Squared Test Results", 
      booktabs = TRUE) %>%
     save_kable("tables/up_chi_squared_results.tex")

# Correlation across treatment statuses

model1 <- lm(treat_2021 ~ treat_2015, data = up_all)
model2 <- lm(treat_2015 ~ treat_2010, data = up_all)
model3 <- lm(treat_2010 ~ treat_2005, data = up_all)
model4 <- lm(treat_2021 ~ treat_2005 + treat_2010 + treat_2015, data = up_all)

models <- list(model1, model2, model3, model4)

models %>%
     imap(~ glance(.x) %>% mutate(model = .y)) %>%
     bind_rows()

models %>%
     imap(~ tidy(.x) %>% mutate(model = .y)) %>%
     bind_rows()

stargazer(model1, model2, model3, model4,
          title = "Regression Results",
          dep.var.labels = c("Treatment 2021", "Treatment 2015", "Treatment 2010"),
          covariate.labels = c("Treatment 2015", "Treatment 2010", "Treatment 2005"),
          column.labels = c("21 $\\sim$ 15", 
                            "15 $\\sim$ 10", 
                            "10 $\\sim$ 5", 
                            "21 $\\sim$ 05 + 10 + 15"),
          align = TRUE,
          out = "tables/up_treatment_reg.tex")
