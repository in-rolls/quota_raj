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



# 2005 * 2010 * 2015 full interaction

m_long_term <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | district_2020, vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_psfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | district_2020 + ps_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_psfe)

m_long_term_gpfe <- feols((sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

# TeX
models_long_term_list <- list(m_long_term, m_long_term_dfe, m_long_term_psfe, m_long_term_gpfe)

etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "longterm_interaction.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)



# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2020 == "F") ~ always_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 + ps_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 + ps_2020 + gp_2020, vcov = ~gp_2020,, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "long_term_always.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_always_never <- feols((sex_2020 == "F") ~ never_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never)

m_always_never_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020, vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never_dfe)

m_always_never_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never_psfe)

m_always_never_gpfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_always_never_gpfe)


# TeX
m_always_never_list <- list(m_always_never, m_always_never_dfe, m_always_never_psfe, m_always_never_gpfe)

etable(m_always_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "long_term_always.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated (No quota in 2005, 10, & 15)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2020 == "F") ~ sometimes_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "long_term_sometimes.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)




# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2020 == "F") ~ once , data = filter(raj_panch, treat_2020 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2020 == "F") ~ once  | district_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2020 == "F") ~ once  | district_2020 + ps_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2020 == "F") ~ once  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "long_term_low_intensity.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in either 2005, 2010 or 2010",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((sex_2020 == "F") ~ twice , data = filter(raj_panch, treat_2020 == 0))
summary(m_twice)

m_twice_dfe <- feols((sex_2020 == "F") ~ twice  | district_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_dfe)

m_twice_psfe <- feols((sex_2020 == "F") ~ twice  | district_2020 + ps_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_psfe)

m_twice_gpfe <- feols((sex_2020 == "F") ~ twice  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_gpfe)

# TeX
m_twice_list <- list(m_twice, m_twice_dfe, m_twice_psfe, m_twice_gpfe)

etable(m_twice_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "long_term_med_intensity.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice" = "Treated twice in 2005, 2010 and 2010",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# never treated

m_no_treat <- feols((sex_2020 == "F") ~ never_treated , data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 , vcov = ~gp_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 + gp_2020 , vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = here("..", "tables", "long_term_never.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)
