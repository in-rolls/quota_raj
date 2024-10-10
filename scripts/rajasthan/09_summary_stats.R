# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)
library(tidyverse)
library(broom)
library(pwr)
# Load data ---------------------------------------------------------------


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")


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

gps <- c(
     "gp_2020" = length(unique(raj_panch$key_2020)),
     "gp_2015" = length(unique(raj_panch$key_2015)),
     "gp_2010" = length(unique(raj_panch$key_2010)),
     "gp_2005" = length(unique(raj_panch$key_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(raj_panch$sex_2005)
table(raj_panch$sex_2010)
table(raj_panch$sex_2015)
table(raj_panch$sex_2020)

table(raj_panch$district_2020)



# DV means ----------------------------------------------------------------

dv_2005 <- filter(raj_panch, treat_2005 == 0) %>% pull(sex_2005)
dv_m_2005 <- mean(dv_2005 == "FEMALE", na.rm = TRUE)
print(dv_m_2005)

dv_2010 <- filter(raj_panch, treat_2010 == 0) %>% pull(sex_2010)
dv_m_2010 <- mean(dv_2010 == "F", na.rm = TRUE)
print(dv_m_2010)

dv_2015 <- filter(raj_panch, treat_2015 == 0) %>% pull(sex_manual_2015)
dv_m_2015 <- mean(dv_2015 == "F", na.rm = TRUE)
print(dv_m_2015)

dv_2020 <- filter(raj_panch, treat_2020 == 0) %>% pull(sex_2020)
dv_m_2020 <- mean(dv_2020 == "F", na.rm = TRUE)
print(dv_m_2020)

