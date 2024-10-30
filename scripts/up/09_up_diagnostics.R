# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)
library(broom)
library(purrr)
library(fixest)
library(stargazer)

# Load all
load("data/up/up_all_recoded.RData")

# data_2021 <- up_all %>%
#      select(ends_with("_2021"))

vanila <- feols((sex_2021 =="महिला") ~ treat_2015 ,  data = filter(up_all, treat_2021== 0))
summary(vanila) #5pp

vanila_interact <- feols((sex_2021 =="महिला") ~ treat_2015 * district_name_2021 ,  data = filter(up_all, treat_2021 == 0))
summary(vanila_interact) #8pp strange


coefficients_df <- data.frame(term = names(vanila_interact$coefficients),
                              estimate = vanila_interact$coefficients) %>% 
                     filter(term != "(Intercept)") %>% 
                     mutate(term = gsub("district_name_2021", "", term)) 

positive_coefficients <- coefficients_df %>%
     filter(estimate > 0.046) %>% #vanila estimate is 4pp and hence 
     sort_by(positive_coefficients$estimate)
   


vanila_fe <- feols((sex_2021 =="महिला") ~ treat_2015 |I(paste0(district_name_2021, block_name_2021)) ,  data = filter(up_all, treat_2021 == 0))
summary(vanila_fe) # consistent with vanila


vanila_fe <- feols((sex_2021 =="महिला") ~ treat_2015 |I(paste0(district_name_2021, block_name_2021)) ,  data = filter(up_all, treat_2021 == 0))
summary(vanila_fe) # consistent with vanila

long_term<- feols((sex_2021 =="महिला") ~ treat_2015 * treat_2010 * treat_2005 ,  data = filter(up_all, treat_2021 == 0))
summary(long_term) #8pp 


long_term_fe <- feols((sex_2021 =="महिला") ~ treat_2015 * treat_2010 * treat_2005 |I(paste0(district_name_2021, block_name_2021)) ,  data = filter(up_all, treat_2021 == 0))
summary(long_term_fe) #8pp


up_all <- up_all %>%
     mutate(
          count_treated = (treat_2005 + treat_2010 + treat_2015))




summary(lm((sex_2021 =="महिला")  ~as.factor(count_treated), data = subset(up_all, treat_2021 == 0)))

summary(lm((sex_2021 =="महिला")  ~(count_treated==3), data = subset(up_all, treat_2021 == 0)))
summary(lm((sex_2021 =="महिला")  ~(count_treated==3)*district_name_2021, data = subset(up_all, treat_2021 == 0)))



  
