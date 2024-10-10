
# Short Run Models  -------------------------------------------------------

# Rajasthan  --------------------------------------------------------------


m_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10)

m_05_10_dfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010,  data = filter(raj_panch, treat_2010 == 0) )
summary(m_05_10_dfe)


m_05_10_psfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010 + samiti_name_2010,  data = filter(raj_panch, treat_2010 == 0) )
summary(m_05_10_dfe)

m_05_10_gpfe <- feols((sex_2010 =="F") ~ treat_2005 | I(paste0(dist_name_2010, samiti_name_2010)),   data = filter(raj_panch, treat_2010 == 0))
summary(m_05_10_gpfe)

# m_05_10_combfe <- feols((sex_2010 == "F") ~ treat_2005 | , data = filter(raj_panch, treat_2010 == 0))
# summary(m_05_10_combfe)

# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe, m_05_10_gpfe)

etable(models_05_10_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/models_05_10.tex", 
       dict = c( 'sex_2010 == "F"' = "2010 rep is a woman in an open seat in Raj", 
                 "treat_2005" = "Quota Treatment in 2005", 
                 "dist_name_2010" = "District (2010)",
                 "samiti_name_2010" = "Panchayat Samiti (2010)",
                 "I(paste0(dist_name_2010, samiti_name_2010))" = "(District, Samiti)",
                 "gp_2010" = "Gram Panchayat (2010)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,        replace = TRUE)

# etable(models_05_10_list, tex=TRUE, style.df = style.df, file = here("..", "tables", "models_05_10.tex"),     se.row=FALSE,        replace = TRUE) #title = "Short-run Effects 2005 -- 2010",

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((sex_manual_2015 == "F") ~ treat_2010, data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15)

m_10_15_dfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | dist_name_2015,  data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_dfe)

m_10_15_psfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 , data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_psfe)

m_10_15_gpfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | I(paste0(dist_name_2015, samiti_name_2015)) , data = filter(raj_panch, treat_2015 == 0))
summary(m_10_15_gpfe)

# m_10_15_combfe <- feols((sex_manual_2015 == "F") ~ treat_2010 | , data = filter(raj_panch, treat_2015 == 0))
# summary(m_10_15_combfe)

models_10_15_list <- list(m_10_15, m_10_15_dfe, m_10_15_psfe, m_10_15_gpfe)


etable(models_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/models_10_15.tex", 
       dict = c( 'sex_manual_2015 == "F"' = "2015 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota Treatment in 2010", 
                 "dist_name_2015" = "District (2015)",
                 "samiti_name_2015" = "Panchayat Samiti (2015)",
                 "I(paste0(dist_name_2015, samiti_name_2015))" = "(District, Panchayat Samiti)",
                 "gp_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,        replace = TRUE)


# 2015 ---> 2020 ----------------------------------------------------------

m_15_20 <- feols((sex_2020 == "F") ~ treat_2015, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20)

m_15_20_dfe <- feols((sex_2020 == "F") ~ treat_2015 | district_2020, data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_dfe)

m_15_20_psfe <- feols((sex_2020 == "F") ~ treat_2015 | district_2020 + ps_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_psfe)

m_15_20_gpfe <- feols((sex_2020 == "F") ~ treat_2015 | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_15_20_gpfe)
# 
# m_15_20_combfe <- feols((sex_2020 == "F") ~ treat_2015 | I(paste0(district_2020, ps_2020)),data = filter(raj_panch, treat_2020 == 0))
# summary(m_15_20_combfe)

models_15_20_list <- list(m_15_20, m_15_20_dfe, m_15_20_psfe, m_15_20_gpfe)


etable(models_15_20_list, 
       # vcov = list(NULL, ~gp_2020, ~gp_2020),
       # se = c("standard", "cluster","cluster"),
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       tex = TRUE, 
       file = "tables/models_15_20.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2015" = "Quota Treatment in 2015", 
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)",
                 "I(paste0(district_2020, ps_2020))" = "(District, Panchayat Samiti)"),
       #"SE_Type" = "S.E. Type"), #  notes = "Robust standard errors clustered at gram panchayat level"
       se.row=FALSE,        replace = TRUE)

# UP -----------------------------------
# summary(lm((cand_sex_fin_2010 =="महिला") ~ treat_2005, data = subset(up_all, treat_2010 == 0)))

m_05_10 <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005, data = filter(up_all, treat_2010 == 0))
summary(m_05_10)

m_05_10_dfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | district_name_eng_2010,  data = filter(up_all, treat_2010 == 0) )
summary(m_05_10_dfe)

m_05_10_psfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | district_name_eng_2010+ block_name_eng_2010, data = filter(up_all, treat_2010 == 0))
summary(m_05_10_psfe)

m_05_10_gpfe <- feols((cand_sex_fin_2010 =="महिला") ~ treat_2005 | I(paste0(district_name_eng_2010, block_name_eng_2010)), data = filter(up_all, treat_2010 == 0))
summary(m_05_10_gpfe)
# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe, m_05_10_gpfe)

etable(models_05_10_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_models_05_10.tex", 
       dict = c('cand_sex_fin_2010 =="महिला"' = "2010 rep is a woman in an open seat in UP", 
                "treat_2005" = "Quota Treatment in 2005", 
                "district_name_eng_2010" = "District (2010)",
                "block_name_eng_2010" = "Panchayat Block (2010)",
                "gp_name_eng_2010" = "Gram Panchayat (2010)", 
                "I(paste0(district_name_eng_2010, block_name_eng_2010))" = "District - Block"),
       se.row= FALSE,        replace = TRUE)

# 2010 ---> 2015 ----------------------------------------------------------

m_10_15 <- feols((sex_2015 =="महिला") ~ treat_2010, data = filter(up_all, treat_2015 == 0))
summary(m_10_15)

m_10_15_dfe <- feols((sex_2015 == "महिला") ~ treat_2010 | district_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_dfe)

m_10_15_psfe <- feols((sex_2015 == "महिला") ~ treat_2010 | district_name_eng_2015 + block_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_10_15_psfe)

m_10_15_gpfe <- feols((sex_2015 =="महिला") ~ treat_2010 | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_10_15_gpfe)

models_10_15_list <- list(m_10_15, m_10_15_dfe, m_10_15_psfe,m_10_15_gpfe)

etable(models_10_15_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_models_10_15.tex", 
       dict = c( 'sex_2015 =="महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota Treatment in 2010", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "District - Block"),#  notes = "Robust standard errors clustered at gram panchayat level",
       se.row= FALSE,        replace = TRUE)





# Long Term Models --------------------------------------------------------


# Rajasthan Additive ---------------------------------------------------------------


# 2005 + 2010 + 2015 additive

m_long_term_add <- feols((sex_2020 == "F") ~ treat_2015 + treat_2010 + treat_2005,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_add)

m_long_term_add_dfe <- feols((sex_2020 == "F") ~ treat_2015 + treat_2010 + treat_2005  | district_2020,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_add_dfe)

m_long_term_add_psfe <- feols((sex_2020 == "F") ~ treat_2015 + treat_2010 + treat_2005  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_add_psfe)

m_long_term_add_gpfe <- feols((sex_2020 == "F") ~ treat_2015 + treat_2010 + treat_2005  | I(paste0(district_2020 ,ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_long_term_add_gpfe)




# TeX
models_long_term_add_list <- list(m_long_term_add, m_long_term_add_dfe, m_long_term_add_psfe, m_long_term_add_gpfe)

etable(models_long_term_add_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/raj_longterm_add.tex", 
       dict = c('sex_2020 == "F"' = "2020 Rep is a Woman in an Open Seat in Raj", 
                "treat_2010" = "Quota in 2010", 
                "treat_2005" = "Quota in 2005",
                "treat_2015" = "Quota in 2015",
                "always_treated" = "Always Treated (Quota in 2005, 2010, & 2015)",
                "district_2020" = "District (2020)",
                "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                "ps_2020" = "Panchayat Samiti (2020)",
                "gp_2020" = "Gram Panchayat (2020)"),
       se.row = FALSE,
       replace = TRUE)


# UP Additive -------------------------------------------------------------



# 2005 + 2010 additive

m_long_term_add <- feols((sex_2015 == "महिला") ~ treat_2010 + treat_2005,  data = filter(up_all, treat_2015 == 0))
summary(m_long_term_add)

m_long_term_add_dfe <- feols((sex_2015 == "महिला") ~ treat_2010 + treat_2005  | district_name_eng_2015,  data = filter(up_all, treat_2015 == 0))
summary(m_long_term_add_dfe)

m_long_term_add_psfe <- feols((sex_2015 == "महिला") ~ treat_2010 + treat_2005  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_long_term_add_psfe)

m_long_term_add_gpfe <- feols((sex_2015 == "महिला") ~+ treat_2010 + treat_2005  | I(paste0(district_name_eng_2015 ,block_name_eng_2015)),  data = filter(up_all, treat_2015 == 0))
summary(m_long_term_add_gpfe)




# TeX
models_long_term_add_list <- list(m_long_term_add, m_long_term_add_dfe, m_long_term_add_psfe, m_long_term_add_gpfe)

etable(models_long_term_add_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_longterm_add.tex", 
       dict = c('sex_2015 == "महिला"' = "2015 Rep is a Woman in an Open Seat in UP", 
                "treat_2010" = "Quota in 2010", 
                "treat_2005" = "Quota in 2005",
                "treat_2015" = "Quota in 2015",
                "always_treated" = "Always Treated (Quota in 2005, 2010, & 2015)",
                "district_name_eng_2015" = "District (2015)",
                "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Samiti)",
                "block_name_eng_2015" = "Panchayat Samiti (2015)",
                "gp_2020" = "Gram Panchayat (2020)"),
       se.row = FALSE,
       replace = TRUE)



# OLD DOSAGE RAJASTHAN ----------------------------------------------------

# Separate always Never etc Main Tables -----------------------------------------------------------

m_always_lt_gpfe <- feols((sex_2020 == "F") ~ always_treated  | I(paste0(district_2020, ps_2020)) , data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)

m_never_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_gpfe)

m_sometimes_gpfe <- feols((sex_2020 == "F") ~ sometimes_treated  | I(paste0(district_2020, ps_2020))  ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)


raj_max_contrast_poster <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast_poster <- raj_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast_poster$treatment_category <- factor(raj_max_contrast_poster$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always_gpfe <- feols((sex_2020 == "F") ~ treatment_category | I(paste0(district_2020, ps_2020)),  data = raj_max_contrast_poster)
summary(m_never_v_always_gpfe) 

models_long_term_list <- list(m_always_lt_gpfe, m_never_gpfe, m_sometimes_gpfe,m_never_v_always_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/raj_main_long_term_models.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020",
                 "treatment_categoryAlways Treated" = " Max Contrast ($WWW$ v. $OO0$)", 
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)"), 
       replace = TRUE)




# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2020 == "F") ~ always_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2020 == "F") ~ always_treated  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2020 == "F") ~ always_treated  | I(paste0(district_2020 ,ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_always_lt_gpfe)


# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/long_term_always.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "always_treated" = "Always treated (quota in 2005, 10, & 15)",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       replace = TRUE)



# Never treated treat_2005 + treat_2010 + treat_2015 = 0


m_never <- feols((sex_2020 == "F") ~ never_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never)

m_never_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020,   data = filter(raj_panch, treat_2020 == 0))
summary(m_never_dfe)

m_never_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_psfe)

m_never_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020,ps_2020)) ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_never_gpfe)


# TeX
m_never_list <- list(m_never, m_never_dfe, m_never_psfe, m_never_gpfe)

etable(m_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/long_term_never.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "never_treated" = "Never treated (No quota in 2005, 10, & 15)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2020 == "F") ~ sometimes_treated,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2020 == "F") ~ sometimes_treated  | district_2020 + ps_2020 + gp_2020 ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_sometimes.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 + treat_15 > 0)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)


# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2020 == "F") ~ once , data = filter(raj_panch, treat_2020 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2020 == "F") ~ once  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2020 == "F") ~ once  | district_2020 + ps_2020,   data = filter(raj_panch, treat_2020 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2020 == "F") ~ once  |  I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_low_intensity.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in either 2005, 2010 or 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

m_twice <- feols((sex_2020 == "F") ~ twice , data = filter(raj_panch, treat_2020 == 0))
summary(m_twice)

m_twice_dfe <- feols((sex_2020 == "F") ~ twice  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_dfe)

m_twice_psfe <- feols((sex_2020 == "F") ~ twice  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_psfe)

m_twice_gpfe <- feols((sex_2020 == "F") ~ twice  | I(paste0(district_2020, ps_2020)),  data = filter(raj_panch, treat_2020 == 0))
summary(m_twice_gpfe)

# TeX
m_twice_list <- list(m_twice, m_twice_dfe, m_twice_psfe, m_twice_gpfe)

etable(m_twice_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_med_intensity.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice" = "Treated twice in 2005, 2010 and 2015",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)



# never treated

m_no_treat <- feols((sex_2020 == "F") ~ never_treated , data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2020 == "F") ~ never_treated  | district_2020 + ps_2020 ,   data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2020 == "F") ~ never_treated  | I(paste0(district_2020 ,ps_2020)) ,  data = filter(raj_panch, treat_2020 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/long_term_never.tex" , 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_2020" = "District (2020)",
                 "I(paste0(district_2020, ps_2020))" = "(District, Samiti)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)




# WWW v OOO seats ---------------------------------------------------------

raj_max_contrast <- filter(raj_panch, treat_2020 == 0 & (never_treated == 1 | always_treated == 1))

raj_max_contrast <- raj_max_contrast %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Always Treated"))


raj_max_contrast$treatment_category <- factor(raj_max_contrast$treatment_category, levels = c("Never Treated", "Always Treated"))

m_never_v_always <- feols((sex_2020 == "F") ~ treatment_category, data = raj_max_contrast)
summary(m_never_v_always) 

m_never_v_always_dfe <- feols((sex_2020 == "F") ~ treatment_category | district_2020   , data = raj_max_contrast)
summary(m_never_v_always_dfe) 

m_never_v_always_psfe <- feols((sex_2020 == "F") ~ treatment_category | district_2020 + ps_2020 , data = raj_max_contrast)
summary(m_never_v_always_psfe) 

m_never_v_always_gpfe <- feols((sex_2020 == "F") ~ treatment_category | I(paste0(district_2020 ,ps_2020)), data = raj_max_contrast)
summary(m_never_v_always_gpfe) 




m_raj_max_compare_list <- list(m_never_v_always, m_never_v_always_dfe, m_never_v_always_psfe,m_never_v_always_gpfe)


etable(m_raj_max_compare_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/raj_max_contrast.tex", 
       dict = c( 'sex_2020 == "F"' = "2020 rep is a woman in an open seat in Raj", 
                 "treatment_categoryAlways Treated" = "Always treated vs. Never Treated", 
                 "district_2020" = "District (2020)",
                 "ps_2020" = "Panchayat Samiti (2020)",
                 "gp_2020" = "Gram Panchayat (2020)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)




# UP Dosage Old -----------------------------------------------------------



# Always, Never etc ---------------------------------------------------------------

summary(lm((sex_2015 =="महिला") ~ twice_treated, data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ as.factor(count_treated), data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ as.factor(treat_all), data = subset(up_all, treat_2015 == 0)))
summary(lm((sex_2015 =="महिला") ~ treat_2005*treat_2010, data = subset(up_all, treat_2015 == 0)))

#always is twice here

summary(lm((sex_2015 =="महिला") ~ twice_treated, data = subset(up_all, treat_2015 == 0)))
# always treated treat_2005 + treat_2010 + treat_2015 = 3

m_always_lt <- feols((sex_2015 == "महिला") ~ twice_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_always_lt)

m_always_lt_dfe <- feols((sex_2015 == "महिला") ~ twice_treated  | district_name_eng_2015,  data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_dfe)

m_always_lt_psfe <- feols((sex_2015 == "महिला") ~ twice_treated  |district_name_eng_2015 + block_name_eng_2015, data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_psfe)

m_always_lt_gpfe <- feols((sex_2015 == "महिला") ~ twice_treated | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_gpfe)



# TeX
m_always_lt_list <- list(m_always_lt, m_always_lt_dfe, m_always_lt_psfe, m_always_lt_gpfe)

etable(m_always_lt_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_always.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "twice_treated" = "Always treated (quota in 2005, & 10)",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District ,Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row = FALSE,
       replace = TRUE)

# Never treated treat_2005 + treat_2010 + treat_2015 = 0

m_never <- feols((sex_2015 == "महिला") ~ never_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_never)

m_never_dfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015,  data = filter(up_all, treat_2015 == 0))
summary(m_never_dfe)

m_never_psfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 + block_name_eng_2015 , data = filter(up_all, treat_2015 == 0))
summary(m_never_psfe)

m_never_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_never_gpfe)


# TeX
m_never_list <- list(m_never, m_never_dfe, m_never_psfe, m_never_gpfe)

etable(m_never_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated (No quota in 2005, & 10)",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       
       replace = TRUE)



# Sometimes treat_2005 + treat_2010 + treat_2015 > 0. Basically dummy activated if at least once the gp received treatment

m_sometimes <- feols((sex_2015 == "महिला") ~ sometimes_treated,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes)

m_sometimes_dfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_dfe)

m_sometimes_psfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_psfe)

m_sometimes_gpfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_gpfe)

# TeX
m_sometimes_list <- list(m_sometimes, m_sometimes_dfe, m_sometimes_psfe, m_sometimes_gpfe)

etable(m_sometimes_list, 
       tex = TRUE, 
       style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
       interaction.combine = " $\times$ ",
       file = "tables/up_long_term_sometimes.tex", 
       dict = c(
            'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
            "treat_2010" = "Quota in 2010", 
            "treat_2005" = "Quota in 2005",
            "treat_2015" = "Quota in 2015",
            "sometimes_treated" = "Sometimes treated (treat_05 + treat_10 > 0)",
            "district_name_eng_2015" = "District (2015)",
            "block_name_eng_2015" = "Panchayat Block (2015)",
            "gp_name_eng_2015" = "Gram Panchayat (2015)"),
       replace = TRUE)










# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years

m_once <- feols((sex_2015 == "महिला") ~ once , data = filter(up_all, treat_2015 == 0))
summary(m_once)

m_once_dfe <- feols((sex_2015 == "महिला") ~ once  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_once_dfe)

m_once_psfe <- feols((sex_2015 == "महिला") ~ once  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_once_psfe)

m_once_gpfe <- feols((sex_2015 == "महिला") ~ once  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_once_gpfe)

# TeX
m_once_list <- list(m_once, m_once_dfe, m_once_psfe, m_once_gpfe)

etable(m_once_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_low_intensity.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "once" = "Treated once in 2005, 10 or 15",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)


# never treated

m_no_treat <- feols((sex_2015 == "महिला") ~ never_treated , data = filter(up_all, treat_2015 == 0))
summary(m_no_treat)

m_no_treat_dfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_dfe)

m_no_treat_psfe <- feols((sex_2015 == "महिला") ~ never_treated  | district_name_eng_2015 + block_name_eng_2015 ,  data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_psfe)

m_no_treat_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  |I(paste0(district_name_eng_2015, block_name_eng_2015)) , data = filter(up_all, treat_2015 == 0))
summary(m_no_treat_gpfe)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_dfe, m_no_treat_psfe, m_no_treat_gpfe)

etable(m_no_treat_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times$ ",
       file = "tables/up_long_term_never.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open seat in UP", 
                 "treat_2010" = "Quota in 2010", 
                 "treat_2005" = "Quota in 2005",
                 "treat_2015" = "Quota in 2015",
                 "never_treated" = "Never treated in 2005, 2010 and 2010",
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Block (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)






# WWW v OOO seats ---------------------------------------------------------

up_max_contrast <- filter(up_all, treat_2015 == 0 & (never_treated == 1 | twice_treated == 1))

up_max_contrast <- up_max_contrast %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Twice Treated"))


up_max_contrast$treatment_category <- factor(up_max_contrast$treatment_category, levels = c("Never Treated", "Twice Treated"))

m_never_v_twice <- feols((sex_2015 == "महिला") ~ treatment_category    , data = up_max_contrast)
summary(m_never_v_twice) 

m_never_v_twice_dfe <- feols((sex_2015 == "महिला") ~ treatment_category | district_name_eng_2015   , data = up_max_contrast)
summary(m_never_v_twice_dfe) 

m_never_v_twice_psfe <- feols((sex_2015 == "महिला") ~ treatment_category | district_name_eng_2015 + block_name_eng_2015 , data = up_max_contrast)
summary(m_never_v_twice_psfe) 

m_never_v_twice_gpfe <- feols((sex_2015 == "महिला") ~ treatment_category |I(paste0(district_name_eng_2015, block_name_eng_2015)), data = up_max_contrast)
summary(m_never_v_twice_gpfe) 




m_max_compare_list <- list(m_never_v_twice, m_never_v_twice_dfe, m_never_v_twice_psfe, m_never_v_twice_gpfe)

etable(m_max_compare_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = "$\times$",
       file = "tables/up_max_contrast.tex", 
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open sea in UP", 
                 "treatment_categoryTwice Treated" = "Always treated vs. Never Treated", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Samiti (2015)",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015)" = "(District, Block)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)






# Main Section Tables -----------------------------------------------------------


m_always_lt_gpfe <- feols((sex_2015 == "महिला") ~ twice_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_always_lt_gpfe)


m_never_gpfe <- feols((sex_2015 == "महिला") ~ never_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_never_gpfe)

m_sometimes_gpfe <- feols((sex_2015 == "महिला") ~ sometimes_treated  | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = filter(up_all, treat_2015 == 0))
summary(m_sometimes_gpfe)

up_max_contrast_poster <- filter(up_all, treat_2015 == 0 & (never_treated == 1 | twice_treated == 1))

up_max_contrast_poster <- up_max_contrast_poster %>%
     mutate(treatment_category = ifelse(never_treated == 1, "Never Treated", "Twice Treated"))


up_max_contrast_poster$treatment_category <- factor(up_max_contrast_poster$treatment_category, levels = c("Never Treated", "Twice Treated"))

m_never_v_twice_gpfe <- feols((sex_2015 == "महिला") ~ treatment_category | I(paste0(district_name_eng_2015, block_name_eng_2015)), data = up_max_contrast_poster)
summary(m_never_v_twice_gpfe) 

models_long_term_list <- list(m_always_lt_gpfe, m_never_gpfe, m_sometimes_gpfe, m_never_v_twice_gpfe)


etable(models_long_term_list, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\times $ ",
       file = "tables/up_main_long_term_models.tex",
       dict = c( 'sex_2015 == "महिला"' = "2015 rep is a woman in an open sea in UP", 
                 "treatment_categoryTwice Treated" = " Max Contrast ($WW$ v. $OO$)", 
                 "district_name_eng_2015" = "District (2015)",
                 "block_name_eng_2015" = "Panchayat Samiti (2015)",
                 "gp_name_eng_2015" = "Gram Panchayat (2015)",
                 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 "I(paste0(district_name_eng_2015, block_name_eng_2015))" = "(District, Block)",
                 "never_treated" = "Never treated in 2005, and 2010",
                 "sometimes_treated" = "Sometimes treated (Treatment in 2005 or 2010)"), #  notes = "Robust standard errors clustered at gram panchayat level",
       se.row=FALSE,
       replace = TRUE)


