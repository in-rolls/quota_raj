# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(kableExtra)
library(fixest)
library(progress)
library(fuzzyjoin)

normalize_string <- function(input_string) {
     # Remove diacritics and convert to lowercase
     normalized_string <- stri_trans_general(input_string, "Latin-ASCII")
     normalized_string <- stri_trans_tolower(normalized_string)
     return(normalized_string)
}

# Load dat
up_2005 <- read_parquet("data/up/up_gp_sarpanch_2005_fixed_with_transliteration.parquet")
up_2010 <- read_parquet("data/up/up_gp_sarpanch_2010_fixed_with_transliteration.parquet")
up_2015 <- read_parquet("data/up/up_gp_sarpanch_2015_fixed_with_transliteration.parquet")
up_2021 <- read_parquet("data/up/up_gp_sarpanch_2021_fixed_with_transliteration.parquet")

# Let's filter to winners for 2021
up_2021 <- up_2021 %>% filter(result == 'विजेता')

# See https://en.wikipedia.org/wiki/Kanpur_Dehat_district
up_2010$district_name     <- ifelse(up_2010$district_name == "रमाबाई नगर", "कानपुर देहात", up_2010$district_name)
up_2010$district_name_eng <- ifelse(up_2010$district_name_eng == "Ramabai Nagar", "Kanpur Dehat", up_2010$district_name_eng)

# Transform
up_2005_dedupe <- up_2005 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = paste(district_name, block_name, gp_name_fin),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            female_cand = I(cand_sex_fin == 'महिला')) %>%
     filter (!duplicated(key))
up_2010_dedupe <- up_2010 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = paste(district_name, block_name, gp_name_fin),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            female_cand = I(cand_sex_fin == 'महिला')) %>%
     filter (!duplicated(key))
up_2015_dedupe <- up_2015 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = paste(district_name, block_name, gp_name),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            female_cand = I(sex == 'महिला')) %>%
     filter (!duplicated(key))
up_2021_dedupe <- up_2021 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = paste(district_name, block_name, gp_name),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            female_cand = I(sex == 'महिला')) %>%
     filter (!duplicated(key))

# Join
up_05_10 <- inner_join(up_2005_dedupe, 
                       up_2010_dedupe, 
                       by = "key", 
                       suffix = c("_2005", "_2010"))
up_10_15 <- inner_join(up_2010_dedupe, 
                       up_2015_dedupe, 
                       by = "key", 
                       suffix = c("_2010", "_2015")) 
up_15_21 <- inner_join(up_2015_dedupe, 
                       up_2021_dedupe, 
                       by = "key", 
                       suffix = c("_2015", "_2021"))
up_all   <- inner_join(up_05_10, 
                       up_15_21, 
                       by = "key")

up_all$total_res <- with(up_all, rowSums(cbind(female_res_2005, female_res_2010, female_res_2015)))

save(up_all, file = "data/up/up_all_joined.RData")

# Fuzzy join

up_2005_dedupe_suff <- up_2005_dedupe %>%
     rename_with(~ paste0(., "_2005"))

up_2010_dedupe_suff <- up_2010_dedupe %>%
     rename_with(~ paste0(., "_2010"))

up_2015_dedupe_suff <- up_2015_dedupe %>%
     rename_with(~ paste0(., "_2015"))

up_2021_dedupe_suff <- up_2021_dedupe %>%
     rename_with(~ paste0(., "_2021"))

process_row <- function(row, 
                        df2, 
                        key1, 
                        key2, 
                        match_column1, 
                        match_column2, 
                        method = "jw", 
                        distance_col = "distance") {
     
     election_subset <- df2 %>%
          filter(tolower(!!sym(match_column2)) == tolower(row[[match_column1]]))
     
     # If no matching district found, return NULL
     if (nrow(election_subset) == 0) return(NULL)
     
     match_result <- stringdist_left_join(
          row,
          election_subset,
          by = setNames(key2, key1),
          method = method,
          ignore_case = TRUE,
          distance_col = distance_col
     ) %>%
          slice_min(!!sym(distance_col))
     
     return(match_result)
}

# Function to apply process_row_generalized across all rows with progress bar
apply_matching <- function(df1, 
                           df2, 
                           key1, 
                           key2, 
                           match_column1, 
                           match_column2, 
                           method = "jw", 
                           distance_col = "distance") {
     
     pb <- progress_bar$new(
          format = "[:bar] :current/:total (:percent) :elapsedfull",
          total = nrow(df1),
          clear = FALSE
     )
     
     processed_results <- df1 %>%
          split(seq(nrow(.))) %>%
          map_dfr(~ {
               pb$tick()
               process_row(.x, df2, key1, key2, match_column1, match_column2, method, distance_col)
          })
     
     return(processed_results)
}

process_matched_dataframe <- function(matched_df, 
                                      distance_threshold = 0.1, 
                                      distance_col = "distance", 
                                      group_by_cols_x, 
                                      group_by_cols_y) {
     matched_df %>%
          filter(!!sym(distance_col) < distance_threshold) %>%
          group_by(across(all_of(group_by_cols_x))) %>%
          mutate(dup_x = n() > 1) %>%
          ungroup() %>%
          group_by(across(all_of(group_by_cols_y))) %>%
          mutate(dup_y = n() > 1) %>%
          ungroup() %>%
          filter(!dup_x & !dup_y) %>%
          select(-dup_x, -dup_y)
}

up_05_10_f <- apply_matching(
     up_2005_dedupe_suff,
     up_2010_dedupe_suff,
     key1 = "key_2005",
     key2 = "key_2010",
     match_column1 = "district_name_2005",
     match_column2 = "district_name_2010",
     method = "jw",
     distance_col = "up_05_10_dist"
)

up_05_10_ff <- process_matched_dataframe(
     up_05_10_f,
     distance_threshold = 0.1,
     distance_col = "up_05_10_dist",
     group_by_cols_x = c("key_2005"),
     group_by_cols_y = c("key_2010")
)

up_10_15_f <- apply_matching(
     up_2010_dedupe_suff,
     up_2015_dedupe_suff,
     key1 = "key_2010",
     key2 = "key_2015",
     match_column1 = "district_name_2010",
     match_column2 = "district_name_2015",
     method = "jw",
     distance_col = "up_10_15_dist"
)

up_10_15_ff <- process_matched_dataframe(
     up_10_15_f,
     distance_threshold = 0.1,
     distance_col = "up_10_15_dist",
     group_by_cols_x = c("key_2010"),
     group_by_cols_y = c("key_2015")
)

write_parquet(up_05_10_ff, sink = "data/up/up_05_10_fuzzy.parquet")
write_parquet(up_10_15_ff, sink = "data/up/up_10_15_fuzzy.parquet")

