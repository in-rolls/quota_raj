# Load libs
library(readr)
library(dplyr)
library(arrow)
library(stringi)
library(here)

# Get utils
source("scripts/00_utils.R")

# Load data
raj_panch <- read_csv("data/rajasthan/sarpanch_election_data/sp_2005_2010_2015_2020_fin2.csv")

# Recode
raj_panch <- raj_panch %>% 
     mutate(match_name = normalize_string(paste(dist_name_new_2010, samiti_name_new_2010, gp_new_2010))) %>%
     group_by(match_name) %>%
     filter(n() == 1) %>%
     ungroup() %>%
     filter(!(dist_name_new_2010 == 'BARAN')) %>%
     mutate(dist_name_new_2010 = tolower(dist_name_new_2010),
            female_res_2005 = ifelse(grepl("W",  reservation_2005) == TRUE, 1, 0),
            female_res_2010 = ifelse(grepl("W",  reservation_2010) == TRUE, 1, 0),
            female_res_2015 = ifelse(grepl("W",  reservation_2015) == TRUE, 1, 0),
            female_res_2020 = ifelse(grepl("W",  reservation_2020) == TRUE, 1, 0),
            case = paste(female_res_2005, female_res_2010, female_res_2015, female_res_2020, sep = "_"),
            caste_res_2005 = trimws(gsub("\\s*W$", "", reservation_2005)),
            caste_res_2010 = trimws(gsub("\\s*W$", "", reservation_2010)),
            caste_res_2015 = case_when(
                 grepl("general", reservation_2015, ignore.case = TRUE) ~ "GEN",
                 grepl("obc", reservation_2015, ignore.case = TRUE)     ~ "OBC",
                 grepl("sc", reservation_2015, ignore.case = TRUE)      ~ "SC",
                 grepl("st", reservation_2015, ignore.case = TRUE)      ~ "ST",
                 TRUE  ~ reservation_2015),
            caste_res_2020 = case_when(
                 grepl("general", reservation_2020, ignore.case = TRUE) ~ "GEN",
                 grepl("obc", reservation_2020, ignore.case = TRUE)     ~ "OBC",
                 grepl("sc", reservation_2020, ignore.case = TRUE)      ~ "SC",
                 grepl("st", reservation_2020, ignore.case = TRUE)      ~ "ST",
                 TRUE  ~ reservation_2020
            )
     )

write_parquet(raj_panch, "data/rajasthan/sarpanch_election_data/raj_panch.parquet")