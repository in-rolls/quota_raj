# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)


# Load data
data_dir <- here("..", "data/rajasthan/sarpanch_election_data/background")
sp_fin_file <- here(data_dir, "WinnerSarpanch_2020.csv")
raj_sarpanch <- readr::read_csv(sp_fin_file)

# only general election, removing by elections

raj_sarpanch <- raj_sarpanch %>% 
     filter(ElectionType=="General Election")

raj_sarpanch <- raj_sarpanch %>% 
     mutate(treat_2020 = ifelse(raj_sarpanch$CategoryOfGramPanchyat %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0))


treated_units_df <- raj_sarpanch %>% 
     filter(treat_2020 == 1)


n_districts <- length(unique(raj_sarpanch$District))
n_cat_gp <- 4 #only G(W), OBC(@), SC(W), ST
samples_per_group <- ceiling(2000 / (n_districts * n_cat_gp)) ## Artificially inflate the number of samples per group to ensure a larger pool



set.seed(12091986)  
sampled_mobile_nos <- treated_units_df %>%
     group_by(District) %>%
     sample_n(samples_per_group, replace = FALSE) %>%
     ungroup()

# write.csv(sampled_mobile_nos, "sampled_mobile_nos.csv", row.names = FALSE)
