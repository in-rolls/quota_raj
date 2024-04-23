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
data_dir <- here("..", "data/sarpanch_election_data")
sp_fin_file <- here(data_dir, "sp_2005_2010_2015_2020_fin2.csv")
raj_panch <- readr::read_csv(sp_fin_file)

delim_dir <- here("..", "data/delim")


delim_2014_file <- here(delim_dir, "gp_2014_delim_processed.csv")
delim_2014 <- readr::read_csv(delim_2014_file)


delim_2019_file <- here(delim_dir, "delim_2019_gp_to_village.csv")
delim_2019 <- readr::read_csv(delim_2019_file)


#clean delim imported files if necessary for 2019
count <- which(names(delim_2019) == "ps_goog_translate")
# Drop all columns after count
delim_2019 <- delim_2019[, 1:count]


#check if dist-ps-panch exists after delim. Compare only those that exist! 
# 2015 first to check delim in 2014

names(delim_2014)
raj_panch$key_2015

delim_2014$key_2014 <- paste0(delim_2014$district, delim_2014$ps_goog_translate, delim_2014$gp_village_fixed)


# Assuming df is your dataframe

raj_panch$indicator_15 <- as.integer(ifelse(raj_panch$key_2015 %in% delim_2014$key_2014, 1, 0))

sum(raj_panch$indicator_15)


raj_delim_join <- stringdist_left_join(raj_panch, delim_2014,
                                       by = c('key_2015' = 'key_2014'), 
                                       method = "lv",
                                       distance_col = "dist",
                                       max_dist = 3,
                                       ignore_case = TRUE
)



# Create reservation dummies, caste group dummies -------------------------


gps <- c(
     "gp_2020" = length(unique(raj_panch$gp_2020)),
     "gp_2015" = length(unique(raj_panch$gp_2015)),
     "gp_2010" = length(unique(raj_panch$gp_2010)),
     "gp_2005" = length(unique(raj_panch$gp_2005))
)

pss <- c(
     "ps_2020" = length(unique(raj_panch$ps_2020)),
     "ps_2015" = length(unique(raj_panch$samiti_name_2015)),
     "ps_2010" = length(unique(raj_panch$samiti_name_2010)),
     "ps_2005" = length(unique(raj_panch$samiti_name_2005))
)

districts <- c(
     "dist_2020" = length(unique(raj_panch$district_2020)),
     "dist_2015" = length(unique(raj_panch$dist_name_2015)),
     "dist_2010" = length(unique(raj_panch$dist_name_2010)),
     "dist_2005" = length(unique(raj_panch$dist_name_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(raj_panch$sex_2005)
table(raj_panch$sex_2010)
table(raj_panch$sex_2015)
table(raj_panch$sex_2020)

table(raj_panch$district_2020)




     
            