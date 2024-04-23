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

# OBC short term effects --------------------------------------------------

# OBC 2005 --> 2010 -------------------------------------------------------

obc_subset <- raj_panch %>% filter(all_obc_2010==1)

obc_05_10 <- feols((sex_2010 =="F") ~ treat_2005, data = filter(obc_subset, obc_2010 == 0))
summary(obc_05_10)

obc_05_10_fe <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010, data = filter(obc_subset, obc_2010 == 0))
summary(obc_05_10_fe)

obc_05_10_clust <- feols((sex_2010 =="F") ~ treat_2005 | fe_key_2010, vcov = ~cluster_key_2010, data = filter(obc_subset, obc_2010 == 0))
summary(obc_05_10_clust)



# TeX
obc_05_10_list <- list(obc_05_10, obc_05_10_fe, obc_05_10_clust)
etable(obc_05_10_list, tex = TRUE, file = here("..", "tables", "obc_st_05_10.tex"), title = "obc 2005 ~ 2010", placement = "htbp", replace = TRUE)


# OBC 2010 --> 2015 -------------------------------------------------------

obc_subset_15 <- raj_panch %>% filter(all_obc_2015==1)

obc_10_15 <- feols((sex_manual_2015 =="F") ~ treat_2010, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15)

obc_10_15_fe <- feols((sex_manual_2015 =="F") ~ treat_2010 | fe_key_2015, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15_fe)

obc_10_15_clust <- feols((sex_manual_2015 =="F") ~ treat_2010 | fe_key_2015, vcov = ~cluster_key_2015, data = filter(obc_subset_15, obc_2015 == 0))
summary(obc_10_15_clust)

# TeX
obc_10_15_list <- list(obc_10_15, obc_10_15_fe, obc_10_15_clust)
etable(obc_10_15_list, tex = TRUE, file = here("..", "tables", "obc_10_15.tex"), title = "OBC 2010 ~ 2015", placement = "htbp", replace = TRUE)


# OBC 2015 --> 2020 -------------------------------------------------------

obc_subset_20 <- raj_panch %>% filter(all_obc_2020==1)

obc_15_20 <- feols((sex_2020 =="F") ~ treat_2015, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_15_20)

obc_15_20_fe <- feols((sex_2020 =="F") ~ treat_2015 | fe_key_2020, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_15_20_fe)

obc_15_20_clust <- feols((sex_2020 =="F") ~ treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_15_20_clust)

# TeX
obc_15_20_list <- list(obc_15_20, obc_15_20_fe, obc_15_20_clust)
etable(obc_15_20_list, tex = TRUE, file = here("..", "tables", "obc_15_20.tex"), title = "OBC 2015 ~ 2020", placement = "htbp", replace = TRUE)


# OBC Long Term -----------------------------------------------------------

obc_subset_20 <- raj_panch %>% filter(all_obc_2020==1)

obc_lt <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_lt)

obc_lt_fe <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | fe_key_2020, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_lt_fe)

obc_lt_clust <- feols((sex_2020 =="F") ~ treat_2005 * treat_2010 * treat_2015 | fe_key_2020, vcov = ~cluster_key_2020, data = filter(obc_subset_20, obc_2020 == 0))
summary(obc_lt_clust)

# TeX
obc_lt_list <- list(obc_lt, obc_lt_fe, obc_lt_clust)
etable(obc_lt_list, tex = TRUE, file = here("..", "tables", "obc_lt.tex"), title = "OBC 2005 * 2010 * 2015", placement = "htbp", replace = TRUE)

