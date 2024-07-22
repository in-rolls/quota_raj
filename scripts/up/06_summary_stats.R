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

load("data/up/up_all_recoded.RData")
names(up_all)
gps <- c(
     "gp_name_eng_2015" = length(unique(up_all$gp_name_eng_2015)),
     "gp_name_eng_2010" = length(unique(up_all$gp_name_eng_2010)),
     "gp_name_eng_2005" = length(unique(up_all$gp_name_eng_2005))
)

pss <- c(
    
     "ps_2015" = length(unique(up_all$block_name_eng_2015)),
     "ps_2010" = length(unique(up_all$block_name_eng_2010)),
     "ps_2005" = length(unique(up_all$block_name_eng_2005))
)

districts <- c(

     "dist_2015" = length(unique(up_all$district_name_eng_2015)),
     "dist_2010" = length(unique(up_all$district_name_eng_2010)),
     "dist_2005" = length(unique(up_all$district_name_eng_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(up_all$sex_2005)
table(up_all$sex_2010)
table(up_all$sex_2015)
table(up_all$sex_2020)

table(up_all$district_2020)





