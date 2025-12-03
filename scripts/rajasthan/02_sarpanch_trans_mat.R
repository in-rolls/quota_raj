# Load libs
library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)

# Get utils
source("scripts/00_utils.R")

# Load data
raj_panch <- arrow::read_parquet("data/rajasthan/sarpanch_election_data/raj_panch.parquet")

# Transition Matrices ------------------------------------------------------

# comparing with previous reservation status
trans_05_10 <- table(raj_panch$female_res_2005, raj_panch$female_res_2010)
trans_10_15 <- table(raj_panch$female_res_2010, raj_panch$female_res_2015)
trans_15_20 <- table(raj_panch$female_res_2015, raj_panch$female_res_2020)

#comparison with 2005 that had 33% 
trans_05_15 <- table(raj_panch$female_res_2005, raj_panch$female_res_2015) #comparison with 2005 that had 33%. (not necessary, but for completeness sake!) 
trans_05_20 <- table(raj_panch$female_res_2005, raj_panch$female_res_2020)


#comparison with 2010, first election with 50%
trans_10_15 <- table(raj_panch$female_res_2010, raj_panch$female_res_2015)
trans_10_20 <- table(raj_panch$female_res_2010, raj_panch$female_res_2020)

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
bin_trans_05_10 <- table(raj_panch$female_res_2005, raj_panch$female_res_2010)
bin_trans_10_15 <- table(raj_panch$female_res_2010, raj_panch$female_res_2015)
bin_trans_15_20 <- table(raj_panch$female_res_2015, raj_panch$female_res_2020)
bin_trans_05_15 <- table(raj_panch$female_res_2005, raj_panch$female_res_2015)
bin_trans_05_20 <- table(raj_panch$female_res_2005, raj_panch$female_res_2020)
bin_trans_10_15 <- table(raj_panch$female_res_2010, raj_panch$female_res_2015)
bin_trans_10_20 <- table(raj_panch$female_res_2010, raj_panch$female_res_2020)

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

chi_sq_test_05_10 <- chisq.test(bin_trans_05_10) # continuity' correction not changing anything
chi_sq_test_05_10 #done

chi_sq_test_10_15 <- chisq.test(bin_trans_10_15)
chi_sq_test_10_15 #done

chi_sq_test_15_20 <- chisq.test(bin_trans_15_20)
chi_sq_test_15_20#

chi_sq_test_05_15 <- chisq.test(bin_trans_05_15)
chi_sq_test_05_15#done

chi_sq_test_05_20 <- chisq.test(bin_trans_05_20)
chi_sq_test_05_20#done

chi_sq_test_10_20 <- chisq.test(bin_trans_10_20)
chi_sq_test_10_20#done


chi_squared_results <- data.frame(
     "Comparison" = c("2005-2010", "2005-2015", "2005-2020", "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_05_10$statistic, chi_sq_test_05_15$statistic, chi_sq_test_05_20$statistic, 
                                      chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_05_10$parameter, chi_sq_test_05_15$parameter, chi_sq_test_05_20$parameter, 
                              chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_05_10$p.value, chi_sq_test_05_15$p.value, chi_sq_test_05_20$p.value, 
                          chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_sq_out <- chi_squared_results_table %>% kable("latex", 
                                   caption = "Are Reservations Predictable Over Time", 
                                   booktabs = TRUE,
                                   label = "chi_square_reserved_over_time",
                                   col_names = c("Years", "Chi-Square", "DF", "p-value"),
                                   escape = FALSE,
                                   align = c("l", "r", "r", "r"))
                         

save_kable(chi_sq_out, file = here("tables/balance_table_raj.tex"))

