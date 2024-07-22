# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)


contest <- read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
colnames(contest) <- tolower(colnames(contest))

contest <- contest %>%
     mutate_all(tolower)

contest <- contest %>% 
     mutate(women_contestants_bin = ifelse((contest$gender=="f"), 1, 0))
contest$women_contestants_bin <- as.integer(contest$women_contestants_bin)


contest$panchayatsamiti <- gsub("panchayat samiti", "", as.character(contest$panchayatsamiti))

contest$key_2020 <- paste0(gsub(" ", "", contest$district),
                           gsub(" ", "", contest$panchayatsamiti),
                           gsub(" ", "", contest$nameofgrampanchayat))


contest <- contest %>% 
     group_by(key_2020) %>% 
     mutate(prop_women = mean(women_contestants_bin), na.rm=TRUE)


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")
raj_panch <- raj_panch %>% mutate(key_2020 = tolower(key_2020))


contest_prop <- contest %>% select(key_2020, prop_women)

raj_panch <- raj_panch %>% left_join(contest_prop, by = "key_2020") #raj_panch$contest_prop has proportion of female contestants. Logic test, prop_women should be 1 in treat_2020==1

test <- raj_panch %>% select(treat_2020, prop_women)
test <- test %>% filter(treat_2020==1)
test <- test %>% filter(prop_women<1) #about 1% observations. Safe to drop?

