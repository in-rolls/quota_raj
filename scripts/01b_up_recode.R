# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(fixest)

# Source utils
source(here("scripts/00_utils.R"))

# Load dat.
up_all <- read_parquet(here("data/up/up_all_fuzzy.parquet"))

# Recode
up_all <- up_all %>%
     mutate(
          treat_2005 = ifelse(grepl("Female", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          treat_2010 = ifelse(grepl("Female", gp_res_status_fin_eng_2010, ignore.case = TRUE), 1, 0), 
          treat_2015 = ifelse(grepl("Female", gp_reservation_status_eng_2015, ignore.case = TRUE), 1, 0),
          treat_2021 = ifelse(grepl("Female", gp_reservation_status_eng_2021, ignore.case = TRUE), 1, 0),
          
          obc_2005 = ifelse(up_all$gp_res_status_fin_eng_2005 %in% c("Other Backward Class - Female"), 1, 0),
          obc_2010 = ifelse(up_all$gp_res_status_fin_eng_2010 %in% c("Other Backward Class - Female"), 1, 0),
          obc_2015 = ifelse(up_all$gp_reservation_status_eng_2015 %in% c("Other Backward Class Female"), 1, 0),
          obc_2021 = ifelse(up_all$gp_reservation_status_eng_2021 %in% c("Other Backward Class Female"), 1, 0),
          
          dalit_2005 = ifelse(up_all$gp_res_status_fin_eng_2005 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
          dalit_2010 = ifelse(up_all$gp_res_status_fin_eng_2010 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
          dalit_2015 = ifelse(up_all$gp_reservation_status_eng_2015 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
          dalit_2020 = ifelse(up_all$gp_reservation_status_eng_2021 %in% c("Scheduled Caste Female", "Scheduled Tribe Female"), 1, 0),
          
          twice_treated = ifelse((treat_2005 + treat_2010) == 2, 1, 0),
          never_treated = ifelse((treat_2005 + treat_2010 ) == 0, 1, 0),
          sometimes_treated = ifelse((treat_2005 + treat_2010 ) > 0, 1, 0),
          
          count_treated = (treat_2005 + treat_2010 ),

          once = ifelse((treat_2005 + treat_2010 ) == 1, 1, 0),
         
          inter_always_treated = ifelse((treat_2010 == 1) & (treat_2005 == 1), 1, 0),
          inter_sometimes_treated = ifelse((treat_2010 == 1) | (treat_2005 == 1), 1, 0),
          inter_never_treated = ifelse(treat_2005 + treat_2010 == 0, 1, 0),
          treat_all = paste(treat_2005, treat_2010, sep = "_"),
          
          all_sc_2005 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          all_sc_2010 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2010, ignore.case = TRUE), 1, 0),
          all_sc_2015 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          all_sc_2020 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          
          all_obc_2005 = ifelse(grepl("Backward", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          all_obc_2010 = ifelse(grepl("Backward", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          all_obc_2015 = ifelse(grepl("Backward", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          all_obc_2020 = ifelse(grepl("Backward", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
          
          fe_key_2010 = paste(district_name_eng_2010, block_name_eng_2010),
          cluster_key_2010 = paste(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010),
          
          fe_key_2015 = paste(district_name_eng_2015,block_name_eng_2015),
          cluster_key_2015 = paste(district_name_eng_2015,block_name_eng_2015, gp_name_eng_2015),
          
          fe_key_2021 = paste(district_name_eng_2021, block_name_eng_2021),
          cluster_key_2021 = paste(district_name_eng_2021, block_name_eng_2021, gp_name_eng_2021))


write_parquet(up_all, here("data/up/up_all_fuzzy_recoded.parquet"))

