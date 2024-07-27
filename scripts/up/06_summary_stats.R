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

load("data/up/up_all_recoded.RData")
names(up_all)
gps <- c(
     "gp_name_eng_2015" = length(unique(up_all$gp_name_eng_2015)),
     "gp_name_eng_2010" = length(unique(up_all$gp_name_eng_2010)),
     "gp_name_eng_2005" = length(unique(up_all$gp_name_eng_2005))
)

pss <- c(
    
     "ps_2015" = length(unique(up_all$block_name_eng_2015)),
     "ps_2010" = length(unique(up_all$block_name_eng_2010)),
     "ps_2005" = length(unique(up_all$block_name_eng_2005))
)

districts <- c(

     "dist_2015" = length(unique(up_all$district_name_eng_2015)),
     "dist_2010" = length(unique(up_all$district_name_eng_2010)),
     "dist_2005" = length(unique(up_all$district_name_eng_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(up_all$sex_2005)
table(up_all$sex_2010)
table(up_all$sex_2015)
table(up_all$sex_2020)

table(up_all$district_2020)


## Candidate Quality

### Load libs
rm(list=ls())

load("data/up/up_all_recoded.RData")

up_all$gp_category_2021 <- NA
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Unreserved"] <- "General"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Female"] <- "General (Woman)"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Other Backward Class"] <- "OBC"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Other Backward Class Female"] <- "OBC (Woman)"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Other Backward Class Female"] <- "OBC (Woman)"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Scheduled Caste"] <- "SC"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Scheduled Caste Female"] <- "SC (Woman)"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Scheduled Tribe"] <- "ST"
up_all$gp_category_2021[up_all$gp_reservation_status_eng_2021 == "Scheduled Tribe Female"] <- "ST (Woman)"

tab1 <- up_all %>%
     group_by(gp_category_2021) %>%
     summarize(
          mean_age = round(mean(as.integer(age)), 2),
          mean_fixed_assets = scales::comma(mean(as.integer(immovable_property))),
          mean_liquid_assets = scales::comma(mean(as.integer(movable_property))),
          n = n()
     )
#prop_not_criminal = round(mean(criminal_history_2021 %in% c("नहीं"), na.rm = TRUE), 2),

tab2 <- up_all %>%
     group_by(gp_category_2021) %>%
     summarize(
          prop_hs_or_less = round(mean(education %in% c("प्राईमरी", "हाईस्कूल", "इंटर","जूनियर हाईस्कूल"), na.rm = T), 2), 
          prop_grad = round(mean(education %in% c("स्नातक","परास्नातक","पी० एच० डी०","डिप्लोमा"), na.rm = T), 2), 
          prop_illiterate = round(mean(education %in% c("निरक्षर"),na.rm=T),2
          n = n()
     )

library(stargazer)
stargazer(tab1, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Summary Statistics by Category of Gram Panchayat",
          align = TRUE, 
          digits = 2, 
          style = "default",
          out = "tables/up_desc_stat1.tex")

stargazer(tab2, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Summary Statistics by Category of Gram Panchayat",
          align = TRUE, 
          digits = 2, 
          style = "default",
          out = "tables/up_desc_stat2.tex")

tab1_latex <- kable(tab1, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 1", label = "tab:summary_statistics1") %>%
     kable_styling(latex_options = c("hold_position"))

tab2_latex <- kable(tab2, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 2", label = "tab:summary_statistics2") %>%
     kable_styling(latex_options = c("hold_position"))

# Save LaTeX tables to files
writeLines(as.character(tab1_latex), con = "tables/raj_desc_stat1.tex")
writeLines(as.character(tab2_latex), con = "tables/raj_desc_stat2.tex")




