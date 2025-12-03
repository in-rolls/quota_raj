# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)


# Load data
file_path <- "data/rajasthan/sarpanch_election_data/background/WinnerSarpanch_2020.csv"
raj_sarpanch <- readr::read_csv(file_path)


# only general election, removing by elections

raj_sarpanch <- raj_sarpanch %>% 
     filter(ElectionType=="General Election")

raj_sarpanch <- raj_sarpanch %>% 
     mutate(treat_2020 = ifelse(raj_sarpanch$CategoryOfGramPanchyat %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0))


treated_units_df <- raj_sarpanch %>% 
     filter(treat_2020 == 1 & MobileNo != 0)


n_districts <- length(unique(raj_sarpanch$District))
n_cat_gp <- 4 #only G(W), OBC(@), SC(W), ST
samples_per_group <- ceiling(500 / (n_districts * n_cat_gp)) 



set.seed(12091986)  
sampled_mobile_nos <- treated_units_df %>%
     group_by(District, CategoryOfGramPanchyat) %>%
     sample_n(min(samples_per_group, n()), replace = FALSE) %>%
     ungroup()


remaining_samples <- 500 - nrow(sampled_mobile_nos)

#sample additional observations from the treated data to reach 750
if (remaining_samples > 0) {
     set.seed(12101986)
     additional_samples <- treated_units_df %>%
          filter(!MobileNo %in% sampled_mobile_nos$MobileNo) %>%
          sample_n(remaining_samples, replace = FALSE) 

     sampled_mobile_nos <- bind_rows(sampled_mobile_nos, additional_samples)
}


# write.csv(sampled_mobile_nos, "data/rajasthan/sarpanch_election_data/background/sampled_mobile_nos.csv", row.names = FALSE)
 
library(writexl)


write_xlsx(sampled_mobile_nos, "data/rajasthan/sarpanch_election_data/background/sampled_mobile_nos.xlsx")


ronit_trial <- anti_join(raj_sarpanch, sampled_mobile_nos)
ronit_trial <- ronit_trial %>% 
     filter(treat_2020==1)

ronit_trial_nos <-sample_n(ronit_trial, 25)


#  write.csv(ronit_trial_nos, "data/rajasthan/sarpanch_election_data/background/trial_nos.csv", row.names = FALSE)


# Non-Quota Seats ---------------------------------------------------------



open_units_df <- raj_sarpanch %>% 
     filter(treat_2020 == 0 & MobileNo != 0)


n_districts <- length(unique(raj_sarpanch$District))
n_cat_gp <- 4 #only G(W), OBC(@), SC(W), ST
samples_per_group <- ceiling(500 / (n_districts * n_cat_gp)) 



set.seed(12091986)  
sampled_mobile_nos_open <- open_units_df %>%
     group_by(District, CategoryOfGramPanchyat) %>%
     sample_n(min(samples_per_group, n()), replace = FALSE) %>%
     ungroup()


remaining_samples_open <- 500 - nrow(sampled_mobile_nos_open)

#sample additional observations from the treated data to reach 750
if (remaining_samples_open > 0) {
     set.seed(12101986)
     additional_samples <- open_units_df %>%
          filter(!MobileNo %in% sampled_mobile_nos_open$MobileNo) %>%
          sample_n(remaining_samples, replace = FALSE) 
     
     sampled_mobile_nos_open <- bind_rows(sampled_mobile_nos_open, additional_samples)
}

 write_xlsx(sampled_mobile_nos_open, "data/rajasthan/sarpanch_election_data/background/sampled_mobile_nos_open.xlsx")


