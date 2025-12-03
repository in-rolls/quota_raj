## Candidate Quality

library(broom)
library(psych)
raj_summ <- read_csv("data/rajasthan/sarpanch_election_data/background/ContestingSarpanch_2020.csv")

# Clean the column names and filter the data
names(raj_summ) <- tolower(names(raj_summ))
raj_summ <- raj_summ %>%
     mutate_all(tolower) %>%
     filter(electiontype != "by election")

# Clean and transform the data
raj_summ_clean <- raj_summ %>%
     mutate(
          treat = ifelse(categoryofgrampanchayat %in% c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
          win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE),
          grad_status = ifelse(educationstatus %in% c("postgraduate", "graduate", "professional graduate", "professional post graduate"), 1, 0)
     ) %>%
     filter(
          !is.na(age), 
          !is.na(childrenbefore27111995), 
          !is.na(childrenonorafter28111995),
          !is.na(win), 
          !is.na(grad_status),
          !is.na(contestingcandidateoccupation)
     ) %>%
     mutate(total_children = as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995)) %>% 
     mutate(age = as.integer(age),
            total_children = as.integer(total_children),
            grad_status = as.integer(grad_status))

age_test <- t.test(age ~ treat, data = raj_summ_clean)
total_children_test <- t.test(total_children ~ treat, data = raj_summ_clean)
grad_status_test <- t.test(grad_status ~ treat, data = raj_summ_clean)

#unemployed bin

raj_summ_clean <- raj_summ_clean %>%
     mutate(unemployed = ifelse(contestingcandidateoccupation == "unemployed", 1, 0))

unemployed_test <- t.test(unemployed ~ treat, data = raj_summ_clean)
assets_test <- t.test(log(win) ~ treat, data = raj_summ_clean)

results <- data.frame(
     Variable = c("Age", "Total Children", "Graduation Status", "Unemployed", "Assets (log)"),
     Mean_Non_Gender_Quota = c(age_test$estimate[1], total_children_test$estimate[1],
                               grad_status_test$estimate[1], unemployed_test$estimate[1],
                               assets_test$estimate[1]),
     Mean_Gender_Quota = c(age_test$estimate[2], total_children_test$estimate[2],
                           grad_status_test$estimate[2], unemployed_test$estimate[2],
                           assets_test$estimate[2]),
     Difference = c(age_test$estimate[2] - age_test$estimate[1],
                    total_children_test$estimate[2] - total_children_test$estimate[1],
                    grad_status_test$estimate[2] - grad_status_test$estimate[1],
                    unemployed_test$estimate[2] - unemployed_test$estimate[1],
                    assets_test$estimate[2] - assets_test$estimate[1]),
     t_value = c(age_test$statistic, total_children_test$statistic,
                 grad_status_test$statistic, unemployed_test$statistic,
                 assets_test$statistic),
     Std_Error = c(age_test$stderr, total_children_test$stderr,
                   grad_status_test$stderr, unemployed_test$stderr,
                   assets_test$stderr),
     # p_value = c(age_test$p.value, total_children_test$p.value,
     #             grad_status_test$p.value, unemployed_test$p.value,
     #             assets_test$p.value),
     Significance = c(ifelse(age_test$p.value < 0.001, "***", 
                             ifelse(age_test$p.value < 0.01, "**", 
                                    ifelse(age_test$p.value < 0.05, "*", "ns"))),
                      ifelse(total_children_test$p.value < 0.001, "***", 
                             ifelse(total_children_test$p.value < 0.01, "**", 
                                    ifelse(total_children_test$p.value < 0.05, "*", "ns"))),
                      ifelse(grad_status_test$p.value < 0.001, "***", 
                             ifelse(grad_status_test$p.value < 0.01, "**", 
                                    ifelse(grad_status_test$p.value < 0.05, "*", "ns"))),
                      ifelse(unemployed_test$p.value < 0.001, "***", 
                             ifelse(unemployed_test$p.value < 0.01, "**", 
                                    ifelse(unemployed_test$p.value < 0.05, "*", "ns"))),
                      ifelse(assets_test$p.value < 0.001, "***", 
                             ifelse(assets_test$p.value < 0.01, "**", 
                                    ifelse(assets_test$p.value < 0.05, "*", "ns")))))

# Print results
print(results)

output_file <- results %>%
     kable("latex", 
           caption = "T-Test Results: Rajasthan Candidates", 
           col.names = c("Variable", "Mean Non-Gender Quota", "Mean Gender Quota", "Difference", "t-value", "Std. Error", "Significance"), 
           digits = 3) %>%
     row_spec(0, bold = TRUE)  # Bold header row

output_path <- "tables/cand_t_test_results.tex"
save_kable(output_file, file = output_path)


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
rm(raj_summ)
rm(raj_win)

winner_ttest_clean <- winner_ttest %>%
     mutate(
          treat = ifelse(categoryofgrampanchayat %in% c("general (woman)", "obc (woman)", "sc (woman)", "st (woman)"), 1, 0),
          win = winsor(as.integer(totalvalueofcapitalassets), trim = 0.1, na.rm = TRUE),
          grad_status = ifelse(educationstatus %in% c("postgraduate", "graduate", "professional graduate", "professional post graduate"), 1, 0)
     ) %>%
     filter(
          !is.na(age), 
          !is.na(childrenbefore27111995), 
          !is.na(childrenonorafter28111995),
          !is.na(win), 
          !is.na(grad_status),
          !is.na(contestingcandidateoccupation)
     ) %>%
     mutate(total_children = as.integer(childrenbefore27111995) + as.integer(childrenonorafter28111995)) %>% 
     mutate(age = as.integer(age),
            total_children = as.integer(total_children),
            grad_status = as.integer(grad_status))

#unemployed bin

winner_ttest_clean <- winner_ttest_clean %>%
     mutate(unemployed = ifelse(contestingcandidateoccupation == "unemployed", 1, 0))

age_test <- t.test(age ~ treat, data = winner_ttest_clean)
total_children_test <- t.test(total_children ~ treat, data = winner_ttest_clean)
grad_status_test <- t.test(grad_status ~ treat, data = winner_ttest_clean)



unemployed_test <- t.test(unemployed ~ treat, data = winner_ttest_clean)
assets_test <- t.test(log(win) ~ treat, data = winner_ttest_clean)


winner_results <- data.frame(
     Variable = c("Age", "Total Children", "Graduation Status", "Unemployed", "Assets (log)"),
     Mean_Non_Gender_Quota = c(age_test$estimate[1], total_children_test$estimate[1],
                               grad_status_test$estimate[1], unemployed_test$estimate[1],
                               assets_test$estimate[1]),
     Mean_Gender_Quota = c(age_test$estimate[2], total_children_test$estimate[2],
                           grad_status_test$estimate[2], unemployed_test$estimate[2],
                           assets_test$estimate[2]),
     Difference = c(age_test$estimate[2] - age_test$estimate[1],
                    total_children_test$estimate[2] - total_children_test$estimate[1],
                    grad_status_test$estimate[2] - grad_status_test$estimate[1],
                    unemployed_test$estimate[2] - unemployed_test$estimate[1],
                    assets_test$estimate[2] - assets_test$estimate[1]),
     t_value = c(age_test$statistic, total_children_test$statistic,
                 grad_status_test$statistic, unemployed_test$statistic,
                 assets_test$statistic),
     Std_Error = c(age_test$stderr, total_children_test$stderr,
                   grad_status_test$stderr, unemployed_test$stderr,
                   assets_test$stderr),
     # p_value = c(age_test$p.value, total_children_test$p.value,
     #             grad_status_test$p.value, unemployed_test$p.value,
     #             assets_test$p.value),
     Significance = c(ifelse(age_test$p.value < 0.001, "***", 
                             ifelse(age_test$p.value < 0.01, "**", 
                                    ifelse(age_test$p.value < 0.05, "*", "ns"))),
                      ifelse(total_children_test$p.value < 0.001, "***", 
                             ifelse(total_children_test$p.value < 0.01, "**", 
                                    ifelse(total_children_test$p.value < 0.05, "*", "ns"))),
                      ifelse(grad_status_test$p.value < 0.001, "***", 
                             ifelse(grad_status_test$p.value < 0.01, "**", 
                                    ifelse(grad_status_test$p.value < 0.05, "*", "ns"))),
                      ifelse(unemployed_test$p.value < 0.001, "***", 
                             ifelse(unemployed_test$p.value < 0.01, "**", 
                                    ifelse(unemployed_test$p.value < 0.05, "*", "ns"))),
                      ifelse(assets_test$p.value < 0.001, "***", 
                             ifelse(assets_test$p.value < 0.01, "**", 
                                    ifelse(assets_test$p.value < 0.05, "*", "ns")))))
print(winner_results)


output_file <- winner_results %>%
     kable("latex", 
           caption = "T-Test Results: Rajasthan Winners", 
           col.names = c("Variable", "Mean Non-Gender Quota", "Mean Gender Quota", "Difference", "t-value", "Std. Error", "Significance"), 
           digits = 3) %>%
     row_spec(0, bold = TRUE)  
output_path <- "tables/winner_t_test_results.tex"
save_kable(output_file, file = output_path)





# Members Who Answered the Phone ------------------------------------------



library(broom)
library(psych)
library(kableExtra)
library(tidyverse)
raj_member_reply <- read_csv("data/rajasthan/phone_survey_response/member_answered_phone.csv")
names(raj_member_reply) <- tolower(names(raj_member_reply))

raj_member_reply <- raj_member_reply %>%
     mutate_all(tolower) 

winner_ttest_clean <- winner_ttest_clean %>% 
     rename(key = key_2020)

winner_ttest_clean <- winner_ttest_clean %>% 
     filter(winner_ttest_clean$treat==1)

winner_ttest_clean <- winner_ttest_clean %>%
     mutate(member_reply_dummy = ifelse(key %in% raj_member_reply$key, 1, 0))

sum(winner_ttest_clean$member_reply_dummy, na.rm = TRUE) #should be 35



mean_values <- winner_ttest_clean %>%
     filter(member_reply_dummy == 1) %>%
     summarise(
          mean_age = mean(age, na.rm = TRUE),
          mean_total_children = mean(total_children, na.rm = TRUE),
          mean_grad_status = mean(grad_status, na.rm = TRUE),
          mean_unemployed = mean(unemployed, na.rm = TRUE),
          mean_assets = mean(log(win), na.rm = TRUE)
     )
mean_values


mean_values_table <- mean_values %>%
     kable("latex", 
           caption = "Summary Statistics for Phone Respondents \\label{tab:phone_reply_char}", 
           col.names = c("Mean Age", "Mean Total Children", "Graduation Status", "Unemployed Status", "Log of Assets"),
           digits = 2,  
           booktabs = TRUE) 


mean_values_table <- mean_values_table %>%
     add_footnote("\\begin{minipage}{\\linewidth}
                   \\scriptsize
                   \\emph{Notes:} Table shows characteristics -- age, number of children, total value of capital assets (winsorized at 10\%), and unemployment status -- of representatives who answered our phone calls. $\mathcal{N} = 35$.
                   \\end{minipage}",
                  escape = FALSE)  # Ensure LaTeX code is not escaped

mean_values_path <- "tables/mean_values_respondents.tex"
save_kable(mean_values_table, file = mean_values_path)



# panchayat_stats <- winner_ttest_clean %>%
#      filter(member_reply_dummy == 1) %>%
#      group_by(categoryofgrampanchayat) %>%
#      summarise(
#           count = n(),
#           proportion = count / nrow(winner_ttest_clean %>% filter(member_reply_dummy == 1))
#      )
# 
# panchayat_stats
