# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)



# Load data
data_dir <- here("..", "data/sarpanch_election_data")
sp_fin_file <- here(data_dir, "sp_2005_2010_2015_2020_fin.csv")
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
          treatment_intensity = treat_2005 + treat_2010 + treat_2015
     )

# Regressions -------------------------------------------------------------

# Short Run Effects of Quotas (05-->10) -----------------------------------

# clustering = gp (identifying source of variation) to account within-gp correlations
# alternative clustering  D_ps_gp (key_2010)  (if we are to think that the identifying source of variation is a function of the D-Ps-GP structure, unlikely)
# fixed effects= 3 way. D-->PS-->GP #GP_FE  should absord most of the shocks that could be correlated with the proportion of women sarpanchs in the panchayats 

library(fixest)

m_05_10 <- feols((sex_2010 =="F") ~ treat_2005, vcov = ~gp_2010, data = raj_panch)
summary(m_05_10)

m_05_10_dfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010, vcov = ~gp_2010,  data = raj_panch)
summary(m_05_10_dfe)

m_05_10_psfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010 + samiti_name_2010,vcov = ~gp_2010, data = raj_panch)
summary(m_05_10_psfe)

m_05_10_gpfe <- feols((sex_2010 =="F") ~ treat_2005 | dist_name_2010 + samiti_name_2010 + gp_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2010, data = raj_panch)
summary(m_05_10_gpfe)

fixedEffects = feols(m_05_10_gpfe)
summary(fixedEffects)

plot(fixedEffects)

# TeX
models_05_10_list <- list(m_05_10, m_05_10_dfe, m_05_10_psfe, m_05_10_gpfe)


etable(models_05_10_list, tex=T, file = here("..", "tables", "models_05_10.tex"))


#fix model names and labels
models_05_10_plot <- modelplot(models_05_10_list, coef_omit = 'Interc', facet = TRUE) + 
     geom_vline(xintercept = 0, color = "red") +  # Red vertical line at x = 0
     theme_minimal() +  
     theme(panel.grid = element_blank())  

ggsave(file = here("..", "plots", "models_05_10_plot.png"), plot = models_05_10_plot, width = 6, height = 4, dpi = 300)


 

# Intermediate Effects of Quotas 05_10_15 ---------------------------------


# Predicting sex for 2015 names (using naampy) -------------------------------------------

extract_names_2015 <- raj_panch %>% select(name_2015)

library(stringr)
extract_names_2015 <- extract_names_2015 %>%
     mutate(first_name = str_extract(name_2015, "\\w+")) %>%
     mutate(first_name = ifelse(nchar(first_name) == 1, "", first_name))

extract_names_2015 <- extract_names_2015[,2, drop = FALSE]
extract_names_2015 <- extract_names_2015 <- extract_names_2015 %>% 
     mutate_all(tolower)  

#naampy takes a vector of names, and doesn't handle blanks hence replace blanks with "asdfghjkl"
extract_names_2015 <- extract_names_2015 %>%
     mutate(first_name = ifelse(first_name == "", "asdfghjkl", first_name))

# check number of obs with first_name=="asdfghjkl"
# obs_check <- extract_names_2015 %>% filter(first_name=="asdfghjkl")

# write csv to feed it to naampy
# write_csv(extract_names_2015, "names_list_2015.csv")


dir <- here("..", "data/naampy_pred_names_2015")
pred_name_file <- here(dir, "pred_names_2015.csv")
pred_names <- read_csv(pred_name_file)


# to check if the lengths are the same
# length(pred_names$name) == length(extract_names_2015$first_name)
names(pred_names)

pred_names <-  pred_names %>% 
     rename(first_name_15 = name) %>% 
     rename(pred_gender_15 = pred_gender) %>% 
     rename(pred_prob_15 = pred_prob)

names(pred_names)

# replace asdfghjkl with "" to return to how stuff was in raj_panch, and replace corresponding columns with .

# obs_check1 <- pred_names %>% filter(first_name_15=="asdfghjkl")

pred_names <- pred_names %>%
     mutate(across(everything(), ~ ifelse(first_name_15 == "asdfghjkl",NA, .)))

# the next line should result in 0 obs if things are working correctly
 # obs_check2 <- pred_names %>% filter(first_name_15=="asdfghjkl")

naampy_merge_raj_panch <- cbind(raj_panch, pred_names)

# to check if cbind resulted in a proper match. 
# extract first name from original data, ie., from how it was in raj_panch
naampy_merge_raj_panch <- naampy_merge_raj_panch %>%
     mutate(first_name_15_og = str_extract(name_2015, "\\w+")) %>%
     mutate(first_name_15_og = ifelse(nchar(first_name_15_og) == 1, "", first_name_15_og))


# check if first names in raj_panch  and naampy merge are the same
naampy_merge_raj_panch <- naampy_merge_raj_panch %>%
     mutate(first_name_15_og = ifelse(first_name_15_og == "", NA, first_name_15_og))

naampy_merge_raj_panch$first_name_15_og <- tolower(naampy_merge_raj_panch$first_name_15_og)

#if true, extraction of firstnames, naampy pred, and then remerge (cbind) has worked

print(identical(naampy_merge_raj_panch$first_name_15_og[2:length(naampy_merge_raj_panch$first_name_15_og)],
          naampy_merge_raj_panch$first_name_15[2:length(naampy_merge_raj_panch$first_name_15)]))




# Intermediate Effect Regressions -----------------------------------------

unique(naampy_merge_raj_panch$pred_gender_15)

naampy_merge_raj_panch <- naampy_merge_raj_panch %>%
     mutate(sex_2015 = ifelse(naampy_merge_raj_panch$pred_gender_15 == "female", "F",
                              ifelse(pred_gender_15 == "male", "M", "NA")))

# unique(naampy_merge_raj_panch$sex_2015)

summary(naampy_merge_raj_panch$pred_prob_15) # pred_prob \in [0.5006, 0.9998]


# Naampy Pred_prob >0.5006

data_threshold_50 <- naampy_merge_raj_panch %>% filter(pred_prob_15 > 0.5006)


m_50_10_15 <- feols((sex_2015 == "F") ~ treat_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_50)
summary(m_50_10_15)

m_50_10_15_dfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_50)
summary(m_50_10_15_dfe)

m_50_10_15_psfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_50)
summary(m_50_10_15_psfe)

m_50_10_15_gpfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_50)
summary(m_50_10_15_gpfe) 


models_50_list <- list(m_50_10_15, m_50_10_15_dfe, m_50_10_15_psfe, m_50_10_15_gpfe)
etable(models_50_list, tex = TRUE, file = here("..", "tables", "models_50_10_15.tex"),   placement = "htbp", title = "Results for Threshold 0.52")



     
# Naampy Pred_prob > 0.6
 
data_threshold_60 <- naampy_merge_raj_panch %>% filter(pred_prob_15 > 0.6)

m_60_10_15 <- feols((sex_2015 == "F") ~ treat_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_60)
summary(m_60_10_15)

m_60_10_15_dfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_60)
summary(m_60_10_15_dfe)

m_60_10_15_psfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_60)
summary(m_60_10_15_psfe)

m_60_10_15_gpfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_60)
summary(m_60_10_15_gpfe) 


models_60_list <- list(m_60_10_15, m_60_10_15_dfe, m_60_10_15_psfe, m_60_10_15_gpfe)
etable(models_60_list, tex = TRUE, file = here("..", "tables", "models_60_10_15.tex"),   placement = "htbp", title = "Results for Threshold 0.6")


# Naampy Pred_Prob > 0.7

data_threshold_70 <- naampy_merge_raj_panch %>% filter(pred_prob_15 > 0.7)

m_70_10_15 <- feols((sex_2015 == "F") ~ treat_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_70)
summary(m_70_10_15)

m_70_10_15_dfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_70)
summary(m_70_10_15_dfe)

m_70_10_15_psfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_70)
summary(m_70_10_15_psfe)

m_70_10_15_gpfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_70)
summary(m_70_10_15_gpfe)


models_70_list <- list(m_70_10_15, m_70_10_15_dfe, m_70_10_15_psfe, m_70_10_15_gpfe)
etable(models_70_list, tex = TRUE, file = here("..", "tables", "models_70_10_15.tex"), placement = "htbp", title = "Results for Threshold 0.7")


# Naampy Pred_Prob > 0.8
data_threshold_80 <- naampy_merge_raj_panch %>% filter(pred_prob_15 > 0.8)

m_80_10_15 <- feols((sex_2015 == "F") ~ treat_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_80)
summary(m_80_10_15)

m_80_10_15_dfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_80)
summary(m_80_10_15_dfe)

m_80_10_15_psfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_80)
summary(m_80_10_15_psfe)

m_80_10_15_gpfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_80)
summary(m_80_10_15_gpfe)

models_80_list <- list(m_80_10_15, m_80_10_15_dfe, m_80_10_15_psfe, m_80_10_15_gpfe)
etable(models_80_list, tex = TRUE, file = here("..", "tables", "models_80_10_15.tex"), placement = "htbp", title = "Results for Threshold 0.8")


# Naampy Pred_prob > 0.90
data_threshold_90 <- naampy_merge_raj_panch %>% filter(pred_prob_15 > 0.9)

m_90_10_15 <- feols((sex_2015 == "F") ~ treat_2010, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_90)
summary(m_90_10_15)

m_90_10_15_dfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_90)
summary(m_90_10_15_dfe)

m_90_10_15_psfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_90)
summary(m_90_10_15_psfe)

m_90_10_15_gpfe <- feols((sex_2015 == "F") ~ treat_2010 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = data_threshold_90)
summary(m_90_10_15_gpfe)

#weird. co-eff >1! 

models_90_list <- list(m_90_10_15, m_90_10_15_dfe, m_90_10_15_psfe, m_90_10_15_gpfe)
etable(models_90_list, tex = TRUE, file = here("..", "tables", "models_90_10_15.tex"), placement = "htbp", title = "Results for Threshold 0.9")



# Intermediate Effect treat_2005*treat_2010 -------------------------------

# feols((sex_2015 == "F") ~ treat_2010 * treat_2005 | dist_name_2015 + samiti_name_2015 + gp_2015, ssc = ssc(fixef.K = "full"), vcov = ~gp_2015, data = naampy_merge_raj_panch)







# Long Run Effects of Quotas 05_10_15_20 ----------------------------------

# 2005*2010 full interaction
# 
# intensity of treatment. iterated by 2, 1, 0
# 
# MMM
# WWW
# max contrast estimator
# 

