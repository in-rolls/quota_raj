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
# sarpanch_name_2020_file <- here(data_dir, "WardWinningPanch_2020.csv")
# sarpanch_name_2020 <- readr::read_csv(sarpanch_name_2020_file)

# Create reservation dummies, caste group dummies -------------------------

# inefficient but does the job
raj_panch <- raj_panch %>%
     mutate(
          treat_2005 = ifelse(raj_panch$reservation_2005 %in% c("GEN W", "OBC W", "SC W", "ST W"), 1, 0),
          treat_2010 = ifelse(raj_panch$reservation_2010 %in% c("GENW", "OBCW", "SCW", "STW"), 1, 0),
          treat_2015 = ifelse(raj_panch$reservation_2015 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0),
          treat_2020 = ifelse(raj_panch$reservation_2020 %in% c("General (Woman)", "OBC (Woman)", "SC (Woman)", "ST (Woman)"), 1, 0),
          obc_2005 = ifelse(raj_panch$reservation_2005 %in% c("OBC W"), 1, 0),
          obc_2010 = ifelse(raj_panch$reservation_2010 %in% c("OBCW"), 1, 0),
          obc_2015 = ifelse(raj_panch$reservation_2015 %in% c("OBC (Woman)"), 1, 0),
          obc_2020 = ifelse(raj_panch$reservation_2020 %in% c("OBC (Woman)"), 1, 0),
          dalit_2005 = ifelse(raj_panch$reservation_2005 %in% c("SC W", "ST W"), 1, 0),
          dalit_2010 = ifelse(raj_panch$reservation_2010 %in% c("SCW", "STW"), 1, 0),
          dalit_2015 = ifelse(raj_panch$reservation_2015 %in% c("SC (Woman)", "ST (Woman)"), 1, 0),
          dalit_2020 = ifelse(raj_panch$reservation_2020 %in% c("SC (Woman)", "ST (Woman)"), 1, 0),
          always_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 3, 1, 0),
          never_treated = ifelse(treat_2005 + treat_2010 + treat_2015 == 0, 1, 0),
          sometimes_treated = ifelse(treat_2005 + treat_2010 + treat_2015 > 0, 1, 0),
          once = ifelse(treat_2005 + treat_2010 + treat_2015 == 1, 1, 0),
          twice = ifelse(treat_2005 + treat_2010 + treat_2015 == 2, 1, 0),
          inter_always_treated = ifelse(treat_2010 == 1 & treat_2005 == 1, 1, 0),
          inter_sometimes_treated = ifelse(treat_2010 == 1 | treat_2005 == 1, 1, 0),
          inter_never_treated = ifelse(treat_2005 + treat_2010 == 0, 1, 0),
          sc_2010 = ifelse(raj_panch$reservation_2010 %in% c("SC",  "ST" , "SCW" , "STW"), 1, 0),
          sc_2015 = ifelse(raj_panch$reservation_2015 %in% c("SC",  "SC (Woman)" , "SCW" , "ST (Woman)"), 1, 0),
          sc_2020 = ifelse(raj_panch$reservation_2020 %in% c("SC",  "SC (Woman)" , "SCW" , "ST (Woman)"), 1, 0),
          all_obc_2010 = ifelse(raj_panch$reservation_2010 %in% c("OBC",  "OBCW"), 1, 0),
          all_obc_2015 = ifelse(raj_panch$reservation_2015 %in% c("OBC",  "OBC (Woman)"), 1, 0),
          all_obc_2020 = ifelse(raj_panch$reservation_2020 %in% c("OBC",  "OBC (Woman)"), 1, 0),
          fe_key_2010 = paste(dist_name_2010, samiti_name_2010),
          cluster_key_2010 = paste(dist_name_2010, samiti_name_2010, gp_2010),
          fe_key_2015 = paste(dist_name_2015,samiti_name_2015),
          cluster_key_2015 = paste(dist_name_2015,samiti_name_2015, gp_2015),
          fe_key_2020 = paste(district_2020, ps_2020),
          cluster_key_2020 = paste(district_2020, ps_2020, gp_2020)
          )



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




     
            