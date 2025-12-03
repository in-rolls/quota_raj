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
     filter(estimate > 0.046) 
 #vanila estimate is 4pp and hence 

   


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




# winners -----------------------------------------------------------------

count_treat <- colSums(up_all[, c("treat_2005", "treat_2010", "treat_2015", "treat_2021")] == 1)
count_open <- colSums(up_all[, c("treat_2005", "treat_2010", "treat_2015", "treat_2021")] == 0)

open_seats <- c(
     sum( up_all$treat_2005 == 0),
     sum( up_all$treat_2010 == 0),
     sum(up_all$treat_2015 == 0),
     sum(up_all$treat_2021 == 0)
)

open_seats

quota_seats <- c(
     sum( up_all$treat_2005 == 1),
     sum( up_all$treat_2010 == 1),
     sum(up_all$treat_2015 == 1),
     sum(up_all$treat_2021 == 1)
)

quota_seats

count_open_women <- c(
     sum(up_all$cand_sex_fin_2005 == "महिला" & up_all$treat_2005 == 0, na.rm = TRUE),
     sum(up_all$cand_sex_fin_2010 == "महिला" & up_all$treat_2010 == 0, na.rm = TRUE),
     sum(up_all$sex_2015 == "महिला" & up_all$treat_2015 == 0),
     sum(up_all$sex_2021 == "महिला" & up_all$treat_2021 == 0)
)


count_open_women

num_districts <- c(
     length(unique(up_all$district_name_eng_2005)),
     length(unique(up_all$district_name_eng_2010)),
     length(unique(up_all$district_name_eng_2015)),
     length(unique(up_all$district_name_eng_2021))
)

num_districts


count_obc_women <- c(
     sum(up_all$cand_sex_fin_2005 == "महिला" & up_all$gp_res_status_fin_eng_2005 == "Other Backward Class", na.rm = TRUE),
     sum(up_all$cand_sex_fin_2010 == "महिला" & up_all$gp_res_status_fin_eng_2005 ==  "Other Backward Class", na.rm = TRUE),
     sum(up_all$sex_2015 == "महिला" & up_all$gp_reservation_status_eng_2015 == "Other Backward Class"),
     sum(up_all$sex_2021 == "महिला" & up_all$gp_reservation_status_eng_2021 =="Other Backward Class")
)

count_obc_women


count_obc_seats <- c(
     sum(up_all$obc_2005 == 1),
     sum(up_all$obc_2010 == 1),
     sum(up_all$obc_2015 == 1),
     sum(up_all$obc_2021 == 1)
)

count_obc_seats

count_scst_women <- c(
     sum(up_all$all_sc_2005 == 1 & up_all$cand_sex_fin_2005 == "महिला"),
     sum(up_all$all_sc_2010 == 1 & up_all$cand_sex_fin_2010 == "महिला"),
     sum(up_all$all_sc_2015 == 1 & sex_2015 == "महिला"),
     sum(up_all$all_sc_2020 == 1 & sex_2021 == "महिला")
)

count_scst_women


count_scst_seats <- c(
     sum(up_all$all_sc_2005 == 1),
     sum(up_all$all_sc_2010 == 1),
     sum(up_all$all_sc_2015 == 1),
     sum(up_all$all_sc_2020 == 1)
)

count_obc_seats

summary_table <- data.frame(
     Year = c("2005", "2010", "2015", "2021"),
     Open_Seats = open_seats,
     Quota_Seats = quota_seats,
     Count_Open_Women = count_open_women,
     Num_Districts = num_districts,
     OBC_Open_Women = count_obc_women,
     OBC_Seats = count_obc_seats
     
)
summary_table



# Verify sex of open winners ----------------------------------------------


data_muradabad_open <- up_all %>% 
     filter(district_name_2021 == "मुरादाबाद" & treat_2021 == 0) %>% 
     select(matches("_2021$|_2015$")) %>%
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
     filter(sex_2021 == "महिला")

# data_muradabad_quota <- up_all %>% 
#      filter(district_name_2021 == "मुरादाबाद" & treat_2021 == 1) %>% 
#      select(matches("_2021$|_2015$")) %>%
#      select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
#      filter(sex_2021 == "महिला")

bagpat_open <- up_all %>% 
     filter(district_name_2021 == "बागपत" & treat_2021 == 0) %>% 
     select(ends_with("_2021")) %>% 
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, )%>% 
     filter(sex_2021 == "महिला")


rampur_open <- up_all %>% 
     filter(district_name_2021 == "रामपुर" & treat_2021 == 0) %>% 
     select(matches("_2021$|_2015$")) %>%
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
     filter(sex_2021 == "महिला")
#मिथलेश,

basti_open <- up_all %>% 
     filter(district_name_2021 == "बस्ती" & treat_2021 == 0) %>% 
     select(matches("_2021$|_2015$")) %>%
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
     filter(sex_2021 == "महिला")
#Babulal is female? Mahendra Pratap Singh, रामजियावन, Dhiraj Kumar, Lakshman Kumar,रामबली,kamlesh can go either way,इश्तियाक,सूर्यनाथ,कलाचन्द्रcan go either way

hardohi_open <- up_all %>% 
     filter(district_name_2021 == "हरदोई" & treat_2021 == 0) %>% 
     select(matches("_2021$|_2015$")) %>%
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
     filter(sex_2021 == "महिला")
#सुदामा,विटानश्री?, शिवलखनमो० मुख्तार,रतनेश सिंह,विशुन देई, अनुपम सिंहcan go either way, 

shravasti_open <- up_all %>% 
     filter(district_name_2021 == "श्रावस्ती" & treat_2021 == 0) %>% 
     select(matches("_2021$|_2015$")) %>%
     select(elected_sarpanch_name_2021, sex_2021, treat_2021, gp_reservation_status_eng_2021, result_2021, gp_2021, gp_2015) %>%
     filter(sex_2021 == "महिला")
#गुलाम वारिश, अमित कुमारamit v amita?






