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


#check chisquared results when you are actually awake! 

# Regressions -------------------------------------------------------------

# create reservation dummies

raj_panch <- raj_panch %>%
     dplyr::mutate(treat_2005 = ifelse(
          raj_panch$reservation_2005 %in% c("GEN W", "OBC W", "SC W", "ST W"), 1, 0
     ))

raj_panch <- raj_panch %>%
     dplyr::mutate(treat_2010 = ifelse(
          raj_panch$reservation_2010 %in% c("GENW", "OBCW", "SCW", "STW"), 1, 0
     ))

raj_panch <- raj_panch %>%
     dplyr::mutate(treat_2015 = ifelse(
          raj_panch$reservation_2015 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0
     ))

raj_panch <- raj_panch %>%
     dplyr::mutate(treat_2020 = ifelse(
          raj_panch$reservation_2020 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0
     ))


# test <- raj_panch %>% select(reservation_2005, treat_2005, reservation_2010,treat_2010, reservation_2015,treat_2015, reservation_2020,treat_2020)
