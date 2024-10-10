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
library(broom)
library(pwr)
# Load data ---------------------------------------------------------------


# MDE ---------------------------------------------------------------------


alpha <- 0.05  
power <- 0.80  
n <- 20000     # UP
benchmark_odds <- 5  # Odds ratio of 5, Bhavnani 5 times


p_control <- 0.16

# Calculate p_treatment based on the odds ratio
p_treatment <- benchmark_odds * p_control / (1 + (benchmark_odds - 1) * p_control)

effect_size <- p_treatment - p_control

z_alpha <- qnorm(1 - alpha / 2)  # Two-tailed
z_beta <- qnorm(power)

# Calculate MDE
mde <- (z_alpha + z_beta) * sqrt((p_control * (1 - p_control) / (n / 2)) + (p_treatment * (1 - p_treatment) / (n / 2)))

# Print the results
cat("Minimum Detectable Effect (MDE) based on given parameters:", mde, "\n")


load("data/rajasthan/sarpanch_election_data/raj_panch.RData")


# gps <- c(
#      "gp_2020" = length(unique(raj_panch$gp_2020)),
#      "gp_2015" = length(unique(raj_panch$gp_2015)),
#      "gp_2010" = length(unique(raj_panch$gp_2010)),
#      "gp_2005" = length(unique(raj_panch$gp_2005))
# )

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

gps <- c(
     "gp_2020" = length(unique(raj_panch$key_2020)),
     "gp_2015" = length(unique(raj_panch$key_2015)),
     "gp_2010" = length(unique(raj_panch$key_2010)),
     "gp_2005" = length(unique(raj_panch$key_2005))
)


gp_summary <- data.frame(gps, pss, districts)
print(gp_summary)



table(raj_panch$sex_2005)
table(raj_panch$sex_2010)
table(raj_panch$sex_2015)
table(raj_panch$sex_2020)

table(raj_panch$district_2020)



# DV means ----------------------------------------------------------------

dv_2005 <- filter(raj_panch, treat_2005 == 0) %>% pull(sex_2005)
dv_m_2005 <- mean(dv_2005 == "FEMALE", na.rm = TRUE)
print(dv_m_2005)

dv_2010 <- filter(raj_panch, treat_2010 == 0) %>% pull(sex_2010)
dv_m_2010 <- mean(dv_2010 == "F", na.rm = TRUE)
print(dv_m_2010)

dv_2015 <- filter(raj_panch, treat_2015 == 0) %>% pull(sex_manual_2015)
dv_m_2015 <- mean(dv_2015 == "F", na.rm = TRUE)
print(dv_m_2015)

dv_2020 <- filter(raj_panch, treat_2020 == 0) %>% pull(sex_2020)
dv_m_2020 <- mean(dv_2020 == "F", na.rm = TRUE)
print(dv_m_2020)

## Candidate Quality

### Load libs
library(broom)

raj_summ <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
names(raj_summ) <- tolower(names(raj_summ))
raj_summ <- raj_summ %>%
     mutate_all(tolower) %>%
     filter(electiontype != "by election")

unique_marital <- unique(raj_summ$martialstatus)
print(unique_marital)

tab1 <- raj_summ %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_age = round(mean(as.integer(age), na.rm = TRUE), 2),
          prop_married = round(mean(martialstatus == "married", na.rm = TRUE), 2),
          prop_unmarried = round(mean(martialstatus == "unmarried", na.rm = TRUE), 2),
          mean_children = round(mean(as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995), na.rm = TRUE), 2),
          n = n()
     )
raj_summ <- raj_summ %>%  
     mutate(win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE))



tab1 <- raj_summ %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_age = round(mean(as.integer(age)), 2),
          prop_married = round(mean(martialstatus == "married", na.rm = TRUE), 2),
          prop_unmarried = round(mean(martialstatus == "unmarried", na.rm = TRUE), 2),
          mean_children = round(mean(as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995)), 2),
          n = n()
     )

library(psych)
raj_summ <- raj_summ %>%  
     mutate(win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE))


tab2 <- raj_summ %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_win_assets = scales::comma(mean(as.integer(win), na.rm = TRUE)),
          prop_hs_or_less = round(mean(educationstatus %in% c("8th", "secondary", "literate", "5th", "higher secondary"), na.rm = TRUE), 2), 
          prop_grad = round(mean(educationstatus %in% c("postgraduate", "graduate", "professional graduate", "professional post graduate"), na.rm = TRUE), 2), 
          prop_unemployed = round(mean(contestingcandidateoccupation %in% c("unemployed"), na.rm = TRUE), 2),
          n = n()
     )

tab1_latex <- kable(tab1, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 1", label = "tab:summary_statistics1") %>%
     kable_styling(latex_options = c("hold_position"))

tab2_latex <- kable(tab2, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 2", label = "tab:summary_statistics2") %>%
     kable_styling(latex_options = c("hold_position"))

# Save LaTeX tables to files
writeLines(as.character(tab1_latex), con = "tables/raj_desc_stat1.tex")
writeLines(as.character(tab2_latex), con = "tables/raj_desc_stat2.tex")




# T-test ------------------------------------------------------------------

raj_summ <- raj_summ %>% 
     mutate(
          treat = ifelse(categoryofgrampanchayat %in% c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
          unemployed_bin = ifelse(contestingcandidateoccupation %in% c("unemployed"), 1, 0),
          children = as.integer(childrenonorafter28111995) + as.integer(childrenbefore27111995)
     )


# List of paired categories to analyze
category_pairs <- list(
     c("general", "general (woman)"),
     c("obc", "obc (woman)"),
     c("sc", "sc (woman)"),
     c("st", "st (woman)")
)

# Function to perform t-tests for each category pair
perform_ttests <- function(df1, df2) {
     # Check if columns exist and have sufficient data
     if (nrow(df1) > 1 && nrow(df2) > 1) {
          age_ttest <- t.test(as.integer(df1$age), as.integer(df2$age), var.equal = FALSE) %>% tidy()
          children_ttest <- t.test(as.integer(df1$children), as.integer(df2$children), var.equal = FALSE) %>% tidy()
          assets_ttest <- t.test(as.integer(df1$win), as.integer(df2$win), var.equal = FALSE) %>% tidy()
          unemployed_ttest <- t.test(as.integer(df1$unemployed_bin), as.integer(df2$unemployed_bin), var.equal = FALSE) %>% tidy()
     } else {
          # If not enough data, return NA values
          age_ttest <- tibble(variable = "Age", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          children_ttest <- tibble(variable = "Number of Children", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          assets_ttest <- tibble(variable = "Total Value of Capital Assets", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          unemployed_ttest <- tibble(variable = "Unemployment Status", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
     }
     
     results <- bind_rows(
          age_ttest %>% mutate(variable = "Age"),
          children_ttest %>% mutate(variable = "Number of Children"),
          assets_ttest %>% mutate(variable = "Total Value of Capital Assets"),
          unemployed_ttest %>% mutate(variable = "Unemployment Status")
     )
     
     return(results)
}

all_results <- list()

for (pair in category_pairs) {
     df1 <- raj_summ %>% filter(categoryofgrampanchayat == pair[1])
     df2 <- raj_summ %>% filter(categoryofgrampanchayat == pair[2])
     cat_results <- perform_ttests(df1, df2)
     cat_results <- cat_results %>% mutate(category_pair = paste(pair[1], "vs", pair[2]))
     all_results[[paste(pair[1], pair[2], sep = "_")]] <- cat_results
}


combined_results <- bind_rows(all_results)
final_results <- combined_results %>%
     mutate(
          significance = case_when(
               p.value < 0.001 ~ "***",
               p.value < 0.01 ~ "**",
               p.value < 0.05 ~ "*",
               TRUE ~ ""
          ),
          estimate = sprintf("%.3f%s", estimate, significance),
          p.value = sprintf("%.3f", p.value),
          conf_interval = sprintf("[%.3f, %.3f]", conf.low, conf.high)
     ) %>%
     select(category_pair, variable, estimate, p.value, conf_interval)

#  LaTeX 
kable(final_results, format = "latex", booktabs = TRUE, caption = "T-Test Results by Panchayat Category Pairs") %>%
     kable_styling(latex_options = c("hold_position", "scale_down")) %>%
     save_kable("tables/raj_ttest.tex")



# Winner only -------------------------------------------------------------

rm(list = ls())
# Libraries ---------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(readr)
library(psych)
library(broom)
library(knitr)
library(kableExtra)

# Read and preprocess the data
raj_summ <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")
names(raj_summ) <- tolower(names(raj_summ))
raj_summ <- raj_summ %>%
     mutate_all(tolower) %>%
     rename(key_2020 = key) %>%
     filter(electiontype != "by election") %>%
     distinct(key_2020, .keep_all = TRUE)

raj_win <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/WinnerSarpanch_2020.csv")
names(raj_win) <- tolower(names(raj_win))
raj_win <- raj_win %>%
     mutate_all(tolower) %>%
     filter(electiontype != "by election") %>%
     distinct(key_2020, .keep_all = TRUE)

# Merge datasets
winner_ttest <- raj_win %>%
     inner_join(raj_summ, by = "key_2020")

# Prepare summary statistics - Panel 1
winner_tab1 <- winner_ttest %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_age = round(mean(as.integer(age), na.rm = TRUE), 2),
          prop_married = round(mean(martialstatus == "married", na.rm = TRUE), 2),
          prop_unmarried = round(mean(martialstatus == "unmarried", na.rm = TRUE), 2),
          mean_children = round(mean(as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995), na.rm = TRUE), 2),
          n = n()
     )

# Winsorizing the total value of capital assets
winner_ttest <- winner_ttest %>%
     mutate(win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE))

# Prepare summary statistics - Panel 2
winner_tab2 <- winner_ttest %>%
     group_by(categoryofgrampanchayat) %>%
     summarize(
          mean_win_assets = scales::comma(mean(as.integer(win), na.rm = TRUE)),
          prop_hs_or_less = round(mean(educationstatus %in% c("8th", "secondary", "literate", "5th", "higher secondary"), na.rm = TRUE), 2),
          prop_grad = round(mean(educationstatus %in% c("postgraduate", "graduate", "professional graduate", "professional post graduate"), na.rm = TRUE), 2),
          prop_unemployed = round(mean(contestingcandidateoccupation %in% c("unemployed"), na.rm = TRUE), 2),
          n = n()
     )

# Generate LaTeX tables for summary statistics
winner_tab1_latex <- kable(winner_tab1, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 1", label = "tab:summary_statistics1") %>%
     kable_styling(latex_options = c("hold_position"))

winner_tab2_latex <- kable(winner_tab2, format = "latex", booktabs = TRUE, caption = "Summary Statistics by Category of Gram Panchayat - Panel 2", label = "tab:summary_statistics2") %>%
     kable_styling(latex_options = c("hold_position"))

# Save LaTeX tables to files
writeLines(as.character(winner_tab1_latex), con = "tables/raj_win_desc_stat1.tex")
writeLines(as.character(winner_tab2_latex), con = "tables/raj_win_desc_stat2.tex")

# t-tests


# Define category pairs for comparison
category_pairs <- list(
     c("general", "general (woman)"),
     c("obc", "obc (woman)"),
     c("sc", "sc (woman)"),
     c("st", "st (woman)")
)

winner_ttest <- winner_ttest %>%
     mutate(
          treat = ifelse(categoryofgrampanchayat %in% c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
          unemployed_bin = ifelse(contestingcandidateoccupation %in% c("unemployed"), 1, 0),
          children = as.integer(childrenonorafter28111995) + as.integer(childrenbefore27111995),
          win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE)
     )
# Function to perform t-tests for each category pair
perform_ttests <- function(df1, df2) {
     # Check if columns exist and have sufficient data
     if (nrow(df1) > 1 && nrow(df2) > 1) {
          age_ttest <- t.test(as.integer(df1$age), as.integer(df2$age), var.equal = FALSE) %>% tidy()
          children_ttest <- t.test(as.integer(df1$children), as.integer(df2$children), var.equal = FALSE) %>% tidy()
          assets_ttest <- t.test(as.integer(df1$win), as.integer(df2$win), var.equal = FALSE) %>% tidy()
          unemployed_ttest <- t.test(as.integer(df1$unemployed_bin), as.integer(df2$unemployed_bin), var.equal = FALSE) %>% tidy()
     } else {
          # If not enough data, return NA values
          age_ttest <- tibble(variable = "Age", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          children_ttest <- tibble(variable = "Number of Children", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          assets_ttest <- tibble(variable = "Total Value of Capital Assets", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
          unemployed_ttest <- tibble(variable = "Unemployment Status", statistic = NA, p.value = NA, estimate = NA, conf.low = NA, conf.high = NA)
     }
     
     results <- bind_rows(
          age_ttest %>% mutate(variable = "Age"),
          children_ttest %>% mutate(variable = "Number of Children"),
          assets_ttest %>% mutate(variable = "Total Value of Capital Assets"),
          unemployed_ttest %>% mutate(variable = "Unemployment Status")
     )
     
     return(results)
}

# Perform t-tests for each category pair
all_results <- list()
for (pair in category_pairs) {
     df1 <- winner_ttest %>% filter(categoryofgrampanchayat == pair[1])
     df2 <- winner_ttest %>% filter(categoryofgrampanchayat == pair[2])
     cat_results <- perform_ttests(df1, df2)
     cat_results <- cat_results %>% mutate(category_pair = paste(pair[1], "vs", pair[2]))
     all_results[[paste(pair[1], pair[2], sep = "_")]] <- cat_results
}

# Combine and format results for LaTeX output
final_results <- bind_rows(all_results)
final_results <- final_results %>%
     mutate(
          significance = case_when(
               p.value < 0.001 ~ "***",
               p.value < 0.01 ~ "**",
               p.value < 0.05 ~ "*",
               TRUE ~ ""
          ),
          estimate = sprintf("%.3f%s", estimate, significance),
          p.value = sprintf("%.3f", p.value),
          conf_interval = sprintf("[%.3f, %.3f]", conf.low, conf.high)
     ) %>%
     select(category_pair, variable, estimate, p.value, conf_interval)

# Save LaTeX results to file
final_results_latex <- kable(final_results, format = "latex", booktabs = TRUE, caption = "T-Test Results by Panchayat Category Pairs", label = "tab:ttest_results") %>%
     kable_styling(latex_options = c("hold_position"))

writeLines(as.character(final_results_latex), con = "tables/raj_win_ttest_results.tex")




# 2020 Close elections? ---------------------------------------------------


close_elec <- readr::read_csv("data/rajasthan/sarpanch_election_data/background/WinnerSarpanch_2020.csv")
names(close_elec) <- tolower(names(close_elec))
close_elec <- close_elec %>%
     mutate_all(tolower) %>%
     rename(key = key_2020) %>%
     filter(electiontype != "by election") %>%
     distinct(key, .keep_all = TRUE) %>% 
     mutate(margin = (as.integer(votesecurebywinner) - as.integer(votesecurebyrunnerup))/as.integer(totalvalidvotes),
            winner_percentage = (as.integer(votesecurebywinner)/as.integer(totalvalidvotes)))%>% 
     filter(margin > 0) %>% 
            # new_key = paste0(district,panchayatsamiti)) %>% 
     mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
     group_by(district) %>% 
     summarize(
          avg_valid_votes = mean(as.numeric(totalvalidvotes), na.rm = TRUE),
          avg_winner_vote_percentage = mean(winner_percentage, na.rm = TRUE), 
          lowest_percentage = min(winner_percentage, na.rm = TRUE), 
          highest_percentage = max(winner_percentage, na.rm = TRUE), 
          median_percentage = median(winner_percentage, na.rm = TRUE), 
          avg_winner_margin = mean(margin, na.rm = TRUE), 
          closest_margin = min(margin, na.rm = TRUE), 
          median_margin = median(margin, na.rm = TRUE), 
          largest_margin = max(margin, na.rm = TRUE) 
     ) %>% 
     mutate(across(where(is.numeric), ~ round(.x, 3)),
            avg_valid_votes = round(avg_valid_votes, 0),
            closest_margin = round(closest_margin, 3) )

library(kableExtra)
summary_table <- kable(close_elec, format = "latex", booktabs = TRUE, 
                       caption = "Summary Statistics for Election Metrics by District") %>%
     kable_styling(latex_options = c("hold_position")) 


writeLines(summary_table, "tables/close_elec.tex")

