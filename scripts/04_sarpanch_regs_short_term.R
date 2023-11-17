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

# Regressions -------------------------------------------------------------

# Short Run Effects of Quotas (05-->10) -----------------------------------

m_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10)

m_05_10_fe <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010,  data = filter(raj_panch, treat_2010 == 0) )
summary(m_05_10_fe)

m_05_10_clust <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010, vcov = ~cluster_key_2010, data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10_clust)


# TeX
models_05_10_list <- list(m_05_10, m_05_10_fe, m_05_10_clust)
etable(models_05_10_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_05_10.tex"), title = "Short-run Effects 2005 -- 2010",    replace = TRUE)

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((sex_manual_2015 == "F") ~ treat_2010, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15)

m_10_15_fe <- feols((sex_manual_2015 == "F") ~ treat_2010 | fe_key_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_fe)

m_10_15_clust <- feols((sex_manual_2015 == "F") ~ treat_2010 | fe_key_2015, vcov = ~cluster_key_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_clust)

models_10_15_list <- list(m_10_15, m_10_15_fe, m_10_15_clust)
etable(models_10_15_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_10_15.tex"), title = "Short-run Effects 2010 -- 2015",    replace = TRUE)

# 2015 ---> 2020 ----------------------------------------------------------

m_15_20 <- feols((sex_2020 == "F") ~ treat_2015, data = filter(raj_panch, treat_2020 == 0))
summary(m_10_15)

m_15_20_fe <- feols((sex_2020 == "F") ~ treat_2015 | fe_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_fe)

m_15_20_clust <- feols((sex_2020 == "F") ~ treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_clust)

models_15_20_list <- list(m_15_20, m_15_20_fe, m_15_20_clust)
etable(models_15_20_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_15_20.tex"), title = "Short-run Effects 2015 -- 2020",    replace = TRUE)
