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

# Create reservation dummies, caste group dummies -------------------------

# inefficient but does the job
raj_panch <- raj_panch %>%
     mutate(
          treat_2005 = ifelse(raj_panch$reservation_2005 %in% c("GEN W", "OBC W", "SC W", "ST W"), 1, 0),
          treat_2010 = ifelse(raj_panch$reservation_2010 %in% c("GENW", "OBCW", "SCW", "STW"), 1, 0),
          treat_2015 = ifelse(raj_panch$reservation_2015 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0),
          treat_2020 = ifelse(raj_panch$reservation_2020 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0),
          obc_2005 = ifelse(raj_panch$reservation_2005 %in% c("OBC W"), 1, 0),
          obc_2010 = ifelse(raj_panch$reservation_2010 %in% c("OBCW"), 1, 0),
          obc_2015 = ifelse(raj_panch$reservation_2015 %in% c("OBC (Woman)"), 1, 0),
          obc_2020 = ifelse(raj_panch$reservation_2020 %in% c("OBC (Woman)"), 1, 0),
          dalit_2005 = ifelse(raj_panch$reservation_2005 %in% c("SC W", "ST W"), 1, 0),
          dalit_2010 = ifelse(raj_panch$reservation_2010 %in% c("SCW", "STW"), 1, 0),
          dalit_2015 = ifelse(raj_panch$reservation_2015 %in% c("SC (Woman)", "ST (Woman)"), 1, 0),
          dalit_2020 = ifelse(raj_panch$reservation_2020 %in% c("SC (Woman)", "ST (Woman)"), 1, 0),
          always_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 3, 1, 0),
          never_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 0, 1, 0),
          sometimes_treated = ifelse(treat_2005 + treat_2010 + treat_2015 > 0, 1, 0),
          treatment_intensity = treat_2005 + treat_2010 + treat_2015
     )

table(raj_panch$sometimes_treated)
table(raj_panch$never_treated)
table(raj_panch$always_treated)
table(raj_panch$treatment_intensity)


# Transition Matrces ------------------------------------------------------

trans_05_10 <- table(raj_panch$reservation_2005, raj_panch$reservation_2010)
trans_10_15 <- table(raj_panch$reservation_2010, raj_panch$reservation_2015)
trans_15_20 <- table(raj_panch$reservation_2015, raj_panch$reservation_2020)

trans_05_15 <- table(raj_panch$reservation_2005, raj_panch$reservation_2015)
trans_05_20 <- table(raj_panch$reservation_2005, raj_panch$reservation_2020)

# trans_05_10 <- table(raj_panch$treat_2005, raj_panch$treat_2010)
# trans_10_15 <- table(raj_panch$treat_2010, raj_panch$treat_2015)
# trans_15_20 <- table(raj_panch$treat_2015, raj_panch$treat_2020)


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

chi_sq_test_05_10 <- chisq.test(trans_05_10)
chi_sq_test_05_10
chi_sq_test_10_15 <- chisq.test(trans_10_15)
chi_sq_test_10_15
chi_sq_test_15_20 <- chisq.test(trans_15_20)
chi_sq_test_15_20

#p-values are nearly identical, check again!  



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


# immediate effect

m_05_10 <- lm((raj_panch$sex_2010 =="F") ~ treat_2005, data = raj_panch)
m_05_10_fe <- lm((sex_2010 =="F") ~ treat_2005 + factor(gp_2010), data = raj_panch)
