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

# comparing wiht previous reservation status

trans_05_10 <- table(raj_panch$reservation_2005, raj_panch$reservation_2010)
trans_10_15 <- table(raj_panch$reservation_2010, raj_panch$reservation_2015)
trans_15_20 <- table(raj_panch$reservation_2015, raj_panch$reservation_2020)

#comparison with 2005 that had 33% 

trans_05_15 <- table(raj_panch$reservation_2005, raj_panch$reservation_2015) #comparison with 2005 that had 33%. (not necessary, but for completeness sake!) 
trans_05_20 <- table(raj_panch$reservation_2005, raj_panch$reservation_2020)


#comparison with 2010, first election with 50%

trans_10_15 <- table(raj_panch$reservation_2010, raj_panch$reservation_2015)
trans_10_20 <- table(raj_panch$reservation_2010, raj_panch$reservation_2020)


print(trans_matrices <- list(
     `2005-2010` = trans_05_10,
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20,
     `2005-2015` = trans_05_15,
     `2005-2020` = trans_05_20,
     `2010-2015` = trans_10_15,
     `2010-2020` = trans_10_20
))




#write_to_latex(trans_matrices_store, here("..", "tables", "tran_matrices.tex"))

#only look at 0-1 transitions
bin_trans_05_10 <- table(raj_panch$treat_2005, raj_panch$treat_2010)
bin_trans_10_15 <- table(raj_panch$treat_2010, raj_panch$treat_2015)
bin_trans_15_20 <- table(raj_panch$treat_2015, raj_panch$treat_2020)
bin_trans_05_15 <- table(raj_panch$treat_2005, raj_panch$treat_2015)
bin_trans_05_20 <- table(raj_panch$treat_2005, raj_panch$treat_2020)
bin_trans_10_15 <- table(raj_panch$treat_2010, raj_panch$treat_2015)
bin_trans_10_20 <- table(raj_panch$treat_2010, raj_panch$treat_2020)

bin_trans_matrices <- list(
     `2005-2010` = bin_trans_05_10,
     `2010-2015` = bin_trans_10_15,
     `2015-2020` = bin_trans_15_20,
     `2005-2015` = bin_trans_05_15,
     `2005-2020` = bin_trans_05_20,
     `2010-2015` = bin_trans_10_15,
     `2010-2020` = bin_trans_10_20
)



# Chi-Squared Test --------------------------------------------------------

chi_sq_test_05_10 <- chisq.test(trans_05_10) # continuity' correction not changing anything
chi_sq_test_05_10

chi_sq_test_10_15 <- chisq.test(trans_10_15)
chi_sq_test_10_15

chi_sq_test_15_20 <- chisq.test(trans_15_20)
chi_sq_test_15_20

chi_sq_test_05_15 <- chisq.test(trans_05_15)
chi_sq_test_05_15

chi_sq_test_05_20 <- chisq.test(trans_05_20)
chi_sq_test_05_20

chi_sq_test_10_15 <- chisq.test(trans_10_15)
chi_sq_test_10_15

chi_sq_test_10_20 <- chisq.test(trans_10_20)
chi_sq_test_10_20


chi_squared_results <- data.frame(
     "Comparison" = c("2005-2010", "2005-2015", "2005-2020", "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_05_10$statistic, chi_sq_test_05_15$statistic, chi_sq_test_05_20$statistic, 
                                      chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_05_10$parameter, chi_sq_test_05_15$parameter, chi_sq_test_05_20$parameter, 
                              chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_05_10$p.value, chi_sq_test_05_15$p.value, chi_sq_test_05_20$p.value, 
                          chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), scientific = TRUE)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = here("..", "tables", "chi_squared_results.tex"))


