# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)
library(tidyverse)

# Load data ---------------------------------------------------------------



gps <- c(
     "gp_2020" = length(unique(raj_panch$gp_2020)),
     "gp_2015" = length(unique(raj_panch$gp_2015)),
     "gp_2010" = length(unique(raj_panch$gp_2010)),
     "gp_2005" = length(unique(raj_panch$gp_2005))
)

pss <- c(
     "ps_2020" = length(unique(raj_panch$ps_2020)),
     "ps_2015" = length(unique(raj_panch$samiti_name_2015)),
     "ps_2010" = length(unique(raj_panch$samiti_name_2010)),
     "ps_2005" = length(unique(raj_panch$samiti_name_2005))
)

districts <- c(
     "dist_2020" = length(unique(raj_panch$district_2020)),
     "dist_2015" = length(unique(raj_panch$dist_name_2015)),
     "dist_2010" = length(unique(raj_panch$dist_name_2010)),
     "dist_2005" = length(unique(raj_panch$dist_name_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(raj_panch$sex_2005)
table(raj_panch$sex_2010)
table(raj_panch$sex_2015)
table(raj_panch$sex_2020)

table(raj_panch$district_2020)




## Candidate Quality

### Load libs

raj_summ <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
names(raj_summ) <- tolower(names(raj_summ))
raj_summ <- raj_summ %>%
     mutate_all(tolower)

raj_summ <- raj_summ %>% 
     filter(electiontype!="by election")

unique_marital <- unique(raj_summ$martialstatus)
# marital_string <- paste0('c("', paste(unique_marital, collapse = '", "'), '")')


tab1 <- raj_summ %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_age = round(mean(as.integer(age)), 2),
          prop_married = round(mean(martialstatus == "married", na.rm = TRUE), 2),
          prop_unmarried = round(mean(martialstatus == "unmarried", na.rm = TRUE), 2),
          mean_children = round(mean(as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995)), 2),
          n = n()
     )

tab2 <- raj_summ %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_assets = scales::comma(mean(as.integer(totalvalueofcapitalassets))),
          prop_hs_or_less = round(mean(educationstatus %in% c("8th", "secondary", "literate", "5th", "higher secondary"), na.rm = T), 2), 
          prop_grad = round(mean(educationstatus %in% c("postgraduate", "graduate", "professional graduate", "professional post graduate"), na.rm = T), 2), 
          prop_unemployed = round(mean(contestingcandidateoccupation %in% c("unemployed"), na.rm = T), 2),
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
