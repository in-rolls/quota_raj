# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)

# Load data ---------------------------------------------------------------
data_dir <- here("..", "data/rajasthan/sarpanch_election_data")
sp_fin_file <- here(data_dir, "sp_2005_2010_2015_2020_fin2.csv")
raj_panch <- readr::read_csv(sp_fin_file)


delim_dir <- here("..", "data/rajasthan/delim")
delim_2014_file <- here(delim_dir, "unique_gp_ps_d_2014.csv")
delim_2014 <- readr::read_csv(delim_2014_file)

names(delim_2014)

# key_2015 = paste(dist_name_2015,samiti_name_2015, gp_2015)

delim_2014$key_2014 <- paste0(delim_2014$district, delim_2014$ps_goog_translate, delim_2014$gp_goog_translate)
delim_2014 <- delim_2014 %>%
     mutate(key_2014 = str_trim(key_2014)) %>% 
     mutate(key_2014 = tolower(key_2014))



#  first delim in 2014 ----------------------------------

data_15 <- raj_panch %>%
     select(contains("_2005"), contains("_2010"), contains("_2015"))


data_20 <- raj_panch %>%
     select(contains("_2020"))

data_15 <- data_15 %>%
     mutate(key_2015 = str_trim(key_2015)) %>% 
     mutate(key_2015 = tolower(key_2015))

data_15_anti_join <- data_15 %>%
     anti_join(delim_2014, by = c("key_2015" = "key_2014")) # antijoin - left table with no matches in right table



# 2019 --------------------------------------------------------------------


 delim_2019_file <- here(delim_dir, "gp_delim_2019.csv")
 delim_2019 <- readr::read_csv(delim_2019_file)


 
 names(delim_2019)
 
 # key_2015 = paste(dist_name_2015,samiti_name_2015, gp_2015)
 
 delim_2019$key_2019 <- paste0(delim_2019$district, delim_2019$ps_goog_translate, delim_2019$gp_name_goog_translate)
 
 delim_2019 <- delim_2019 %>% filter(verified_or_not==1)
 
 delim_2019 <- delim_2019 %>%
      mutate(key_2019 = str_trim(key_2019)) %>% 
      mutate(key_2019 = tolower(key_2019))
 
 data_20 <- data_20 %>%
      mutate(key_2020 = str_trim(key_2020)) %>% 
      mutate(key_2020 = tolower(key_2020)) %>% 
      select(-nuke_2020, -dist_2020)
 

 #This is the more principled approach. Merge the data_15_anti_joined data with raw 2020 data, then antijoin with 2019 delim files
 
 merge_5_10_15_20<- data_20  %>% 
      stringdist_left_join(data_15_anti_join,
                           by = c('key_2020' = 'key_2015'), 
                           method = "lv",
                           distance_col = "dist",
                           max_dist = 1,
                           ignore_case = TRUE
      )
 
 
 
 
 raj_panch_delim_v1 <- merge_5_10_15_20 %>%
      anti_join(delim_2019, by = c("key_2020" = "key_2019")) # antijoin - left table with no matches in right table
 
 
