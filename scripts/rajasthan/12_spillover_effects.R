# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")

raj_panch <- raj_panch %>%
     group_by(district_2020) %>%
     mutate(info_spillover_2015 = if_else(any(treat_2015), 1, 0)) %>%
     ungroup()

m_info_spillover <- feols((sex_2020 == "F") ~ treat_2015  | district_2020 + ps_2020, 
                          data = filter(raj_panch, treat_2020 == 0))

summary(m_info_spillover)
