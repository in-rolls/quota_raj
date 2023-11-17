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
# Long Run Effects of Quotas 05_10_15 ---> 20 ----------------------------------

m_15_20 <- feols((sex_2020 == "F") ~ treat_2015, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20)

m_15_20_fe <- feols((sex_2020 == "F") ~ treat_2015 | fe_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_fe)

m_15_20_clust <- feols((sex_2020 == "F") ~ treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_clust)

# TeX
models_15_20_list <- list(m_15_20, m_15_20_fe, m_15_20_clust)
etable(models_15_20_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_15_20.tex"), title = "Short-run Effects 2015 -- 2020",    replace = TRUE)

# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_fe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | fe_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_fe)

m_long_term_clust <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_clust)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_fe, m_long_term_clust)
etable(models_long_term_list, tex = TRUE, file = here("..", "tables", "longterm_interaction"), title = "Interaction Effects Treat 2005 * 2010 * 2015", placement = "htbp", replace = TRUE)

# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2020 == "F") ~ always_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt)

m_always_lt_fe <- feols((sex_2020 == "F") ~ always_treated  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_fe)

m_always_lt_clust <- feols((sex_2020 == "F") ~ always_treated  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_clust)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_fe, m_always_lt_clust)
etable(m_always_lt_list, tex = TRUE, file = here("..", "tables", "long_term_always.tex"), title = "Always treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_always_never <- feols((sex_2020 == "F") ~ never_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never)

m_always_never_fe <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never_fe)

m_always_never_clust <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never_clust)


# TeX
m_always_never_list <- list(m_always_never, m_always_never_fe, m_always_never_clust)
etable(m_always_never_list, tex = TRUE, file = here("..", "tables", "long_term_never.tex"), title = "Never treated in 2005, 2010, abd 2015", placement = "htbp", replace = TRUE)


# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2020 == "F") ~ sometimes_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes)

m_sometimes_fe <- feols((sex_2020 == "F") ~ sometimes_treated  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_fe)

m_sometimes_clust <- feols((sex_2020 == "F") ~ sometimes_treated  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_clust)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_fe, m_sometimes_clust)
etable(m_sometimes_list, tex = TRUE, file = here("..", "tables", "long_term_sometimes.tex"), title = "Sometimes treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2020 == "F") ~ once , data = filter(raj_panch, treat_2020 == 0))
summary(m_once)

m_once_fe <- feols((sex_2020 == "F") ~ once  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_once_fe)

m_once_clust <- feols((sex_2020 == "F") ~ once  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_once_clust)

# TeX
m_once_list <- list(m_once, m_once_fe, m_once_clust)
etable(m_once_list, tex = TRUE, file = here("..", "tables", "long_term_low_intensity.tex"), title = "Once treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((sex_2020 == "F") ~ twice,  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice)

m_twice_fe <- feols((sex_2020 == "F") ~ twice  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_fe)

m_twice_clust <- feols((sex_2020 == "F") ~ twice  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_clust)

# TeX
m_twice_list <- list(m_twice, m_twice_fe, m_twice_clust)
etable(m_twice_list, tex = TRUE, file = here("..", "tables", "long_term_med_intensity.tex"), title = "twice treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# never treated

m_no_treat <- feols((sex_2020 == "F") ~ never_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat)

m_no_treat_fe <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_fe)

m_no_treat_clust <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_clust)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_fe, m_no_treat_clust)
etable(m_no_treat_list, tex = TRUE, file = here("..", "tables", "long_term_never.tex"), title = "never treated 2005 * 2010 * 2015", placement = "htbp", replace = TRUE)
