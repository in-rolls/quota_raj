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

# SC/ST Short Term Effects ------------------------------------------------
# SC/ST 2005 --> 2010 -----------------------------------------------------

sc_subset_05 <- raj_panch %>% filter(sc_2010==1)

sc_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(sc_subset_05, dalit_2010 == 0))
summary(sc_05_10)

sc_05_10_fe <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010, data = filter(sc_subset_05, dalit_2010 == 0))
summary(sc_05_10_fe)

sc_05_10_clust <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010, vcov = ~cluster_key_2010, data = filter(sc_subset_05, dalit_2010 == 0))
summary(sc_05_10_clust)

# TeX
sc_05_10_list <- list(sc_05_10, sc_05_10_fe, sc_05_10_clust)
etable(sc_05_10_list, tex = TRUE, file = here("..", "tables", "sc_st_05_10.tex"), title = "SC/ST 2005 ~ 2010", placement = "htbp", replace = TRUE)

# SC/ST 2010 --> 2015 -----------------------------------------------------

sc_subset_15 <- raj_panch %>% filter(sc_2015==1)

sc_10_15 <- feols((sex_manual_2015 =="F") ~ treat_2010, data = filter(sc_subset_15, dalit_2015 == 0))
summary(sc_10_15)

sc_10_15_fe <- feols((sex_manual_2015 =="F") ~ treat_2010 | fe_key_2015, data = filter(sc_subset_15, dalit_2015 == 0))
summary(sc_10_15_fe)

sc_10_15_clust <- feols((sex_manual_2015 =="F") ~ treat_2010 | fe_key_2015, vcov = ~cluster_key_2015, data = filter(sc_subset_15, dalit_2015 == 0))
summary(sc_10_15_clust)

# TeX
sc_10_15_list <- list(sc_10_15, sc_10_15_fe, sc_10_15_clust)
etable(sc_10_15_list, tex = TRUE, file = here("..", "tables", "sc_10_15.tex"), title = "SC/ST 2010 ~ 2015", placement = "htbp", replace = TRUE)


# SC/ST 2015 --> 2020 -----------------------------------------------------

sc_subset_20 <- raj_panch %>% filter(sc_2020==1)

sc_15_20 <- feols((sex_2020 =="F") ~ treat_2015, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_15_20)

sc_15_20_fe <- feols((sex_2020 =="F") ~ treat_2015 | fe_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_15_20_fe)

sc_15_20_clust <- feols((sex_2020 =="F") ~ treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_15_20_clust)

# TeX
sc_15_20_list <- list(sc_15_20, sc_15_20_fe, sc_15_20_clust)
etable(sc_15_20_list, tex = TRUE, file = here("..", "tables", "sc_15_20.tex"), title = "SC/ST 2015 ~ 2020", placement = "htbp", replace = TRUE)



# SC/ST Long Term Effects -------------------------------------------------

sc_subset_20 <- raj_panch %>% filter(sc_2020==1)

sc_lt <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_lt)

sc_lt_fe <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | fe_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_lt_fe)

sc_lt_clust <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_lt_clust)

# TeX
sc_lt_list <- list(sc_lt, sc_lt_fe, sc_lt_clust)
etable(sc_lt_list, tex = TRUE, file = here("..", "tables", "sc_lt.tex"), title = "SC/ST 2005 * 2010 * 2015", placement = "htbp", replace = TRUE)


# Intensity of treatment 
# low intensity: Once treated (treat_2005 + treat_2010 + treat_2015 == 1). Dummy activated if the gp received treatment once over 15 years
sc_subset_20 <- raj_panch %>% filter(sc_2020==1)

sc_once <- feols((sex_2020 == "F") ~ once , data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_once)

sc_once_fe <- feols((sex_2020 == "F") ~ once  | fe_key_2020,  data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_once_fe)

sc_once_clust <- feols((sex_2020 == "F") ~ once  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_once_clust)


# TeX
sc_once_list <- list(sc_once, sc_once_fe, sc_once_clust)
etable(sc_once_list, tex = TRUE, file = here("..", "tables", "sc_low_intensity.tex"), title = "SC Once treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# med intensity: Twice treated (treat_2005 + treat_2010 + treat_2015 == 2) Dummy activated if the gp received treatment twice over 15 years

sc_twice <- feols((sex_2020 == "F") ~ twice,  data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_twice)

sc_twice_fe <- feols((sex_2020 == "F") ~ twice  | fe_key_2020,  data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_twice_fe)

sc_twice_clust <- feols((sex_2020 == "F") ~ twice  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(sc_twice_clust)

# TeX
sc_twice_list <- list(sc_twice, sc_twice_fe, sc_twice_clust)
etable(sc_twice_list, tex = TRUE, file = here("..", "tables", "sc_med_intensity.tex"), title = " SC twice treated in 2005, 2010, 2015", placement = "htbp", replace = TRUE)

# high intensity Thrice treated == same as always_treated
# never treated

m_no_treat <- feols((sex_2020 == "F") ~ never_treated,  data = filter(sc_subset_20, dalit_2020 == 0))
summary(m_no_treat)

m_no_treat_fe <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020,  data = filter(sc_subset_20, dalit_2020 == 0))
summary(m_no_treat_fe)

m_no_treat_clust <- feols((sex_2020 == "F") ~ never_treated  | fe_key_2020, vcov = ~cluster_key_2020, data = filter(sc_subset_20, dalit_2020 == 0))
summary(m_no_treat_clust)

# TeX
m_no_treat_list <- list(m_no_treat, m_no_treat_fe, m_no_treat_clust)
etable(m_no_treat_list, tex = TRUE, file = here("..", "tables", "SC_never.tex"), title = "SC never treated 2005 * 2010 * 2015", placement = "htbp", replace = TRUE)

