# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)

# Load data
data_dir <- here("..", "data/sarpanch_election_data")
sp_fin_file <- here(data_dir, "sp_2005_2010_2015_2020_fin.csv")
raj_panch <- readr::read_csv(sp_fin_file)
tables_folder <- here("tables")

# Transition Matrces ------------------------------------------------------

trans_05_10 <- table(raj_panch$reservation_2005, raj_panch$reservation_2010)
trans_10_15 <- table(raj_panch$reservation_2010, raj_panch$reservation_2015)
trans_15_20 <- table(raj_panch$reservation_2015, raj_panch$reservation_2020)

# blank dataframe to store results
trans_matrices_store <- data.frame(
     "Year Comparison" = c("2005-2010", "2010-2015", "2015-2020"),
     "Transition Matrix" = c(
          kable(trans_05_10, format = "latex", escape = FALSE, booktabs = TRUE),
          kable(trans_10_15, format = "latex", escape = FALSE, booktabs = TRUE),
          kable(trans_15_20, format = "latex", escape = FALSE, booktabs = TRUE)
     )
)

# TeX export Beautify this later
trans_matrices_table <- trans_matrices_store %>%
     kable("latex", escape = FALSE, booktabs = TRUE, caption = "Transition Matrices")
cat(trans_matrices_table, file = here("tables", "tran_matrices.tex"))


# Chi-Squared Test --------------------------------------------------------

chi_sq_test_05_10 <- chisq.test(trans_05_10, simulate.p.value = TRUE)
chi_sq_test_10_15 <- chisq.test(trans_10_15, simulate.p.value = TRUE)
chi_sq_test_15_20 <- chisq.test(trans_15_20, simulate.p.value = TRUE)

# blank dataframe to store results

results_store <- data.frame(
     "Year Comparison" = c("2005-2010", "2010-2015", "2015-2020"),
     "Chi-Squared Test Result" = c(
          format(chi_sq_test_05_10$p.value, scientific = TRUE, digits = 2),
          format(chi_sq_test_10_15$p.value, scientific = TRUE, digits = 2),
          format(chi_sq_test_15_20$p.value, scientific = TRUE, digits = 2)
     )
)

#teX export Beautify this later
chisq_table <- results_store %>%
     kable("latex", escape = FALSE, booktabs = TRUE, caption = "Chi-Squared Test Results") %>%
     kable_styling(full_width = FALSE, position = "center")
cat(chisq_table, file = here("tables", "chisq_table.tex"))
