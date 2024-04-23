# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)

# Load dat
sp_2005 <- read_csv("data/sarpanch_election_data/sarpanch_2005.csv")
sp_2010 <- read_csv("data/sarpanch_election_data/sarpanch_2010.csv")
sp_2015 <- read_csv("data/sarpanch_election_data/sarpanch_2015.csv")
sp_2020 <- read_csv("data/sarpanch_election_data/sarpanch_2020_clean.csv")
names(sp_2020) <- c("dist_name", "samiti_name", "gp", "category")

# List of df
datasets <- list(sp_2005, sp_2010, sp_2015, sp_2020)

# Get some summary stats
compute_values <- function(dataset) {
  nrow_val <- nrow(dataset)
  unique_samiti_val <- length(unique(dataset$samiti_name))
  unique_dist_val <- length(unique(dataset$dist_name))
  
  data.frame("No. of GPs" = nrow_val, "No. of PS" = unique_samiti_val, "No. of Districts" = unique_dist_val)
}

results_list <- lapply(datasets, compute_values)
results_df <- bind_rows(results_list) %>% mutate(Year = as.integer(seq(2005, 2020, 5))) %>% relocate(Year)

print(xtable(results_df, 
      caption = "Summary of the GP Election Data", 
      label = "gp_summary"),
      include.rownames = FALSE,
      file = "tabs/gp_summary.tex")


### Join or Die

# Add Pratapgarh to 2005 to get crosswalk to 2010
sp_2005$dist_name_new <- ifelse(!is.na(sp_2005$dist_2010), sp_2005$dist_2010, sp_2005$dist_name_new)

sp_2005$key <- paste0(sp_2005$dist_name_new, sp_2005$samiti_name_new, sp_2005$gp_new)
sp_2010$key <- paste0(sp_2010$dist_name_new, sp_2010$samiti_name_new, sp_2010$gp_new)

sp_05_10 <- sp_2005 %>% 
        inner_join(sp_2010, 
                   by = 'key',
                   suffix = c("_2005", "_2010"))

# Fuzzy join is the way to go as you get everything
# Given stringdist_inner_join doesn't seem to support suffix
names(sp_2005) <- paste0(names(sp_2005), "_2005")
names(sp_2010) <- paste0(names(sp_2010), "_2010")

sp_05_10_f <- sp_2005 %>% 
              stringdist_inner_join(sp_2010, 
                       by = c('key_2005' = 'key_2010'),
                       ignore_case = TRUE,
                       distance_col = "dist")

write_csv(sp_05_10_f, file = "data/sarpanch_election_data/sp_2005_2010.csv")

# We manually review the file
# https://docs.google.com/spreadsheets/d/1B6tzxuuzEMb-TtdAI9inJoNHKWr-U88haT12Ys80y1g/edit#gid=382262682

sp_05_10_r <- read_csv("data/sarpanch_election_data/sp_2005_2010_manually_reviewed.csv")
sp_05_10_r2 <- sp_05_10_r %>% 
  filter(is.na(nuke) | !(nuke == 1)) %>%
  distinct(key_2005, .keep_all = TRUE)

# Let's create women var. 

sp_05_10_r2 <- sp_05_10_r2 %>%
  mutate(res_women_2005 = ifelse(grepl("W$", reservation_2005), "Women", "Open"),
         res_women_2010 = ifelse(grepl("W$", reservation_2010), "Women", "Open"),
         res_women_05_10 = paste(res_women_2005, res_women_2010, sep = "-"))


write.csv(sp_05_10_r2, file = "data/sarpanch_election_data/sp_2005_2010_fin.csv")

## Group by condition

sp_05_10_r2 %>% 
  group_by(res_women_05_10) %>% 
  summarise(mean_women_05 = mean(sex_2005 == "FEMALE", na.rm = T), 
            mean_women_10 = mean(sex_2010 == "F", na.rm = T))

## Transition Matrix

table(sp_05_10_r2$reservation_2005, sp_05_10_r2$reservation_2010)
table(sp_05_10_r2$res_women_2005, sp_05_10_r2$res_women_2010)

chisq.test(table(sp_05_10_r2$reservation_2005, sp_05_10_r2$reservation_2010))
chisq.test(table(sp_05_10_r2$res_women_2005, sp_05_10_r2$res_women_2010), correct = TRUE)

# Regression
summary(lm(I(res_women_2010 == "Women") ~ res_women_2005, data = sp_05_10_r2))


## Join 2015 to 2005_10

sp_05_10 <- read_csv("data/sarpanch_election_data/sp_2005_2010_fin.csv")[, -1]
names(sp_05_10)[names(sp_05_10) == "dist"] <- "dist_05_10"
names(sp_05_10)[names(sp_05_10) == "nuke"] <- "nuke_05_10"

sp_15    <- read_csv("data/sarpanch_election_data/sarpanch_2015.csv")
sp_15$key <- paste0(sp_15$dist_name_new, sp_15$samiti_name_new, sp_15$gp_new)
names(sp_15) <- paste0(names(sp_15), "_2015")

sp_05_10_15 <- sp_15 %>% 
  stringdist_left_join(sp_05_10,
             by = c('key_2015' = 'key_2010'), 
             method = "lv",
             distance_col = "dist",
             max_dist = 3,
             ignore_case = TRUE
             )

# Filter to keep only the best matching key2 for each key1
# Step 1: Filter out rows with all missing dist values for each key_2015
filtered_step1 <- sp_05_10_15 %>%
  group_by(key_2015) %>%
  filter(!all(is.na(dist)))

# Step 2: Find the minimum dist for each remaining key_2015
filtered_step2 <- filtered_step1 %>%
  group_by(key_2015) %>%
  filter(dist == min(dist, na.rm = TRUE)) %>%
  ungroup()

filtered_nas <- sp_05_10_15 %>%
  group_by(key_2015) %>%
  filter(all(is.na(dist)))

sp_05_10_15_best <- bind_rows(filtered_step2, filtered_nas)

## Write for manual filtration
write_csv(sp_05_10_15_best, file = "data/sarpanch_election_data/sp_05_10_15_best.csv")

## Post Manual filtration
sp_515 <- read_csv("data/sarpanch_election_data/sp_2005_2010_2015_best_manual.csv")
sp_515_fin <- sp_515 %>% 
  filter(nuke == 0)

sp_515_fin %>%
  write_csv("data/sarpanch_election_data/sp_2005_2010_2015_fin.csv")


### Add 2020

sp_20 <- read_csv("data/sarpanch_election_data/background/2020--2022/ContestingSarpanch.csv")

sp_20_r <- sp_20 %>%
  select(District, PanchayatSamiti, NameOfGramPanchayat, CategoryOfGramPanchayat) %>%
  distinct() %>%
  rename(district_2020 = District,
         ps_2020 = PanchayatSamiti,
         gp_2020 = NameOfGramPanchayat,
         reservation_2020 = CategoryOfGramPanchayat) %>%
  mutate(ps_2020 = gsub(" PANCHAYAT SAMITI", "", ps_2020)) %>%
  mutate_at(vars(district_2020, ps_2020, gp_2020), list(~ str_to_title(.)))

write_csv(sp_20_r, "data/sarpanch_election_data/sarpanch_2020.csv")

## Merge w/ 2020

sp_20_r$key_2020 <- with(sp_20_r, paste0(district_2020, ps_2020, gp_2020))

sp_all <- sp_20_r %>% 
  stringdist_left_join(sp_515_fin,
                       by = c('key_2020' = 'key_2015'), 
                       method = "lv",
                       distance_col = "dist_2020",
                       max_dist = 3,
                       ignore_case = TRUE
  )

# Filter to keep only the best matching key2 for each key1
# Step 1: Filter out rows with all missing dist values for each key_2020

filtered_step1 <- sp_all %>%
  group_by(key_2020) %>%
  filter(!all(is.na(dist)))

# Step 2: Find the minimum dist for each remaining key_2015
filtered_step2 <- filtered_step1 %>%
  group_by(key_2020) %>%
  filter(dist == min(dist, na.rm = TRUE)) %>%
  ungroup()

filtered_nas <- sp_all %>%
  group_by(key_2020) %>%
  filter(all(is.na(dist)))

sp_all_best <- bind_rows(filtered_step2, filtered_nas)

## Write for manual filtration
write_csv(sp_all_best, file = "data/sarpanch_election_data/sp_05_10_15_20_best.csv")

## Post Manual filtration
sp_520 <- read_csv("data/sarpanch_election_data/sp_05_10_15_20_best_manual.csv")
sp_520_fin <- sp_520 %>% 
  filter(nuke == 0)

df_with_count <- sp_520_fin %>%
  group_by(key_2020) %>%
  mutate(total_non_na_2015 = sum(!is.na(key_2015))) %>%
  ungroup()

filtered_df <- df_with_count %>%
  group_by(key_2020) %>%
  filter(ifelse(sum(total_non_na_2015) == 0, TRUE, !is.na(key_2015))) %>%
  slice(1) %>%
  select(-total_non_na_2015)

sp_520_fin %>%
  group_by(key_2020) %>% distinct(key_2020, .keep_all = TRUE) %>%
  write_csv("data/sarpanch_election_data/sp_2005_2010_2015_2020_fin.csv")


## Merge to Delim
delim_2014 <- read_csv("data/delim/gp_2014_delim_processed.csv")
unique_gps <- delim_2014 %>%
  select("district", "ps_goog_translate", "gp_goog_translate") %>%
  distinct()

write_csv(unique_gps, "data/delim/unique_gp_ps_d_2014.csv")




