# Libraries ---------------------------------------------------------------

# Regressions -------------------------------------------------------------

# Short Run Effects of Quotas (05-->10) -----------------------------------

m_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10)

m_05_10_dfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010, vcov = ~gp_2010, data = filter(raj_panch, treat_2010 == 0) )
summary(m_05_10_dfe)

m_05_10_psfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010 + samiti_name_2010, vcov = ~gp_2010,  data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10_psfe)


# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe)

etable(models_05_10_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "models_05_10.tex"), 
       dict = c( 'sex_2010 == "F"' = "2010 rep is a woman in an open seat", 
                 "treat_2005" = "Quota Treatment in 2005", 
                 "dist_name_2010" = "District (2010)",
                 "samiti_name_2010" = "Panchayat Samiti (2010)",
                 "gp_2010" = "Gram Panchayat (2010)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
   
       replace = TRUE)

# etable(models_05_10_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_05_10.tex"),     replace = TRUE) #title = "Short-run Effects 2005 -- 2010",

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((sex_manual_2015 == "F") ~ treat_2010, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15)

m_10_15_dfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | dist_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_dfe)

m_10_15_psfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, vcov = ~gp_2015, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_psfe)


models_10_15_list <- list(m_10_15, m_10_15_dfe, m_10_15_psfe)


etable(models_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = here("..", "tables", "models_10_15.tex"), 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat", 
                 "treat_2010" = "Quota Treatment in 2010", 
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       signif.code = NA,
       replace = TRUE)


# 2015 ---> 2020 ----------------------------------------------------------

m_15_20 <- feols((sex_2020 == "F") ~ treat_2015, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20)

m_15_20_dfe <- feols((sex_2020 == "F") ~ treat_2015 | district_2020, vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_dfe)

m_15_20_psfe <- feols((sex_2020 == "F") ~ treat_2015 | district_2020 + ps_2020, vcov = ~gp_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_psfe)


models_15_20_list <- list(m_15_20, m_15_20_dfe, m_15_20_psfe)


etable(models_15_20_list, 
      # vcov = list(NULL, ~gp_2020, ~gp_2020),
      # se = c("standard", "cluster","cluster"),
      style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
      interaction.combine = " $\times $ ",
       tex = TRUE, 
       file = here("..", "tables", "models_15_20.tex"), 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2015" = "Quota Treatment in 2015", 
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"),
                 #"SE_Type" = "S.E. Type"), #  notes = "Robust standard errors clustered at gram panchayat level"
       replace = TRUE)
