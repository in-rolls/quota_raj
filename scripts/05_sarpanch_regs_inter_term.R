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


# Intermediate Effect treat_2005 * treat_2010 -----------------------------
# interaction of treat_2005 and treat_2010

#ecologiucal thing. Constsat capturing the eological trend. Treat_2010. Next step always trreated vs never treated. keep these units and compare. 

m_05_10_15 <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15)

m_05_10_15_dfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_dfe)

m_05_10_15_psfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_psfe)

m_05_10_15_gpfe <- feols((sex_manual_2015 == "F") ~ treat_2010 * treat_2005  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_05_10_15_gpfe)

models_05_10_15_list <- list(m_05_10_15, m_05_10_15_dfe, m_05_10_15_psfe, m_05_10_15_gpfe)

etable(models_05_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "intermediate_05X10.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)


# etable(models_05_10_15_list, tex = TRUE, file = here("..", "tables", "intermediate_05*10.tex"), title = "Interaction Effects Treat 2005 * 2010", placement = "htbp", replace = TRUE)

# Intermediate Always Treated 

m_inter_always <- feols((sex_manual_2015 == "F") ~ inter_always_treated, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always)

m_inter_always_dfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  |  dist_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_dfe)

m_inter_always_psfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_psfe)

m_inter_always_gpfe <- feols((sex_manual_2015 == "F") ~ inter_always_treated  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_always_psfe)



# TeX
m_inter_always_list <- list(m_inter_always, m_inter_always_dfe, m_inter_always_psfe, m_inter_always_gpfe)

etable(m_inter_always_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_always_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_always_treated" = "Always treated (quota in 2005, \& 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# Intermediate Never Treated 

m_inter_never <- feols((sex_manual_2015 == "F") ~ inter_never_treated, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never)

m_inter_never_dfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015, vcov = ~gp_2015,   data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_dfe)

m_inter_never_psfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_psfe)

m_inter_never_gpfe <- feols((sex_manual_2015 == "F") ~ inter_never_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_never_gpfe)

# TeX
m_inter_never_list <- list(m_inter_never, m_inter_never_dfe, m_inter_never_psfe, m_inter_never_gpfe)


etable(m_inter_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_never_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_never_treated" = "Never treated (no quota in 2005, \& 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)

# Intermediate Sometimes Treated

m_inter_sometimes <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes) 

m_inter_sometimes_dfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_dfe)

m_inter_sometimes_psfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_psfe)

m_inter_sometimes_gpfe <- feols((sex_manual_2015 == "F") ~ inter_sometimes_treated  | dist_name_2015 + samiti_name_2015 + gp_2015, vcov = ~gp_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_inter_sometimes_gpfe)


# TeX
m_inter_sometimes_list <- list(m_inter_sometimes, m_inter_sometimes_dfe, m_inter_sometimes_psfe, m_inter_sometimes_gpfe)

etable(m_inter_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "inter_sometimes_treated.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "inter_sometimes_treated" = "Sometimes treated (quota in 2005 or 2010)", 
                 "treat_2005" = "Quota in 2005",
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)
