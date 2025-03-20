# Load required libraries
library(readr)
library(arrow)
library(tibble)
library(tidyverse)   # includes dplyr, tidyr, purrr, etc.
library(stringi)
library(kableExtra)
library(here)
library(fixest)
library(broom)       # For tidying test results
library(stargazer)   # For regression output

# Source utility functions if any
source(here("scripts/00_utils.R"))

# Load data from Parquet
up_all <- read_parquet("data/up/up_all_fuzzy_recoded.parquet")

# Helper function to create transition matrix using tidyverse
make_transition_matrix <- function(data, from, to) {
     data %>%
          count({{ from }}, {{ to }}) %>%                     # Count occurrences for each combination
          pivot_wider(
               names_from = {{ to }},
               values_from = n,
               values_fill = list(n = 0)
          ) %>%
          # Convert the 'from' column into row names and convert to a matrix
          column_to_rownames(var = as_label(enquo(from))) %>%
          as.matrix()
}

# Create transition matrices for different time periods
trans_05_10 <- make_transition_matrix(up_all, treat_2005, treat_2010)
trans_10_15 <- make_transition_matrix(up_all, treat_2010, treat_2015)
trans_15_21 <- make_transition_matrix(up_all, treat_2015, treat_2021)
trans_05_15 <- make_transition_matrix(up_all, treat_2005, treat_2015)
trans_05_21 <- make_transition_matrix(up_all, treat_2005, treat_2021)
trans_10_21 <- make_transition_matrix(up_all, treat_2010, treat_2021)

# Store the transition matrices in a list
trans_matrices <- list(
     `2005-2010` = trans_05_10,
     `2010-2015` = trans_10_15,
     `2015-2021` = trans_15_21,
     `2005-2015` = trans_05_15,
     `2005-2021` = trans_05_21,
     `2010-2021` = trans_10_21
)

# Print the list of transition matrices
print(trans_matrices)

# Perform chi-squared tests and collect results
chi_squared_results <- lapply(trans_matrices, chisq.test)

# Use broom and purrr to tidy the results into a data frame
chi_squared_summary <- purrr::imap_dfr(chi_squared_results, function(test_result, name) {
     broom::tidy(test_result) %>%
          mutate(Comparison = name)
}) %>%
     select(Comparison, statistic, parameter, p.value)

# Print the tidy summary table
print(chi_squared_summary)

# Create a LaTeX table with kable and save it to file
chi_squared_summary %>%
     knitr::kable(
          format = "latex", 
          caption = "Chi-Squared Test Results", 
          booktabs = TRUE,
          col.names = c("Comparison", "Chi-Squared Statistic", "Degrees of Freedom", "P-Value")
     ) %>%
     kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>%
     save_kable("tables/up_chi_squared_results.tex")

# Run regression models to examine correlations across treatment statuses

model1 <- lm(treat_2021 ~ treat_2015, data = up_all)
model2 <- lm(treat_2015 ~ treat_2010, data = up_all)
model3 <- lm(treat_2010 ~ treat_2005, data = up_all)
model4 <- lm(treat_2021 ~ treat_2005 + treat_2010 + treat_2015, data = up_all)

models <- list(model1, model2, model3, model4)

# Summarize models using broom and purrr
model_glance <- models %>%
     purrr::imap_dfr(~ glance(.x) %>% mutate(model = .y))
model_tidy   <- models %>%
     purrr::imap_dfr(~ tidy(.x) %>% mutate(model = .y))

print(model_glance)
print(model_tidy)

# Generate a LaTeX table with regression results using stargazer
stargazer(model1, model2, model3, model4,
          title = "Regression Results",
          dep.var.labels = c("Treatment 2021", "Treatment 2015", "Treatment 2010"),
          covariate.labels = c("Treatment 2015", "Treatment 2010", "Treatment 2005"),
          column.labels = c("21 ~ 15", "15 ~ 10", "10 ~ 5", "21 ~ 05 + 10 + 15"),
          align = TRUE,
          out = "tables/up_treatment_reg.tex")
