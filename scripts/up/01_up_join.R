# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(here)
library(progress)

# Load utils
source(here("scripts/00_utils.R"))

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

# Normalize string
normalize_string <- function(input_string) {
     # Remove diacritics and convert to lowercase
     normalized_string <- stri_trans_general(input_string, "Latin-ASCII")
     normalized_string <- stri_trans_tolower(normalized_string)
     return(normalized_string)
}

# Transform
up_2005_dedupe <- up_2005 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name_fin)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng))) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_res_status_fin_eng) & (gp_res_status_fin_eng != "Unknown"))

up_2010_dedupe <- up_2010 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name_fin)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng))) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_res_status_fin_eng) & (gp_res_status_fin_eng != "Unknown"))

up_2015_dedupe <- up_2015 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng))) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_reservation_status_eng) & (gp_reservation_status_eng != "Unknown"))

up_2021_dedupe <- up_2021 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng))) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_reservation_status_eng) & (gp_reservation_status_eng != "Unknown"))

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

up_all   <- inner_join(up_05_10, up_15_21, by = "key")

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

up_05_10_15_21_f <- apply_matching(
     up_05_10_ff,
     up_15_21,
     key1 = "eng_key_2010",
     key2 = "eng_key_2021",
     match_column1 = "district_name_2010",
     match_column2 = "district_name_2015",
     method = "jw",
     distance_col = "up_05_10_15_21_dist"
)

up_05_10_15_21_ff <- process_matched_dataframe(
     up_05_10_15_21_f,
     distance_threshold = 0.1,
     distance_col = "up_05_10_15_21_dist",
     group_by_cols_x = c("eng_key_2015"),
     group_by_cols_y = c("eng_key_2021")
)

write_parquet(up_05_10_ff, sink = "data/up/up_05_10_fuzzy.parquet")
write_parquet(up_10_15_ff, sink = "data/up/up_10_15_fuzzy.parquet")
write_parquet(up_05_10_15_21_ff,   sink = "data/up/up_all_fuzzy.parquet")
write_parquet(up_all, sink = "data/up/up_all.parquet")


jeff <- haven::read_dta((here("data/up/weaver_data_2.dta"))) 

jeff <- jeff %>%
     mutate(election = as.numeric(election)) %>% 
     mutate(election = recode(election, `-1` = 2010, `0` = 2015, `1` = 2020))

jeff_wide <- jeff %>%
     pivot_wider(
          id_cols = gp_id,
          names_from = election,
          values_from = c(
               reservation,
               reservation_female,
               winner_female,
               winner_votes,
               runnerup_votes,
               winner_total_assets,
               margin_of_victory,
               total_candidates_cancelled,
               total_candidates_returned,
               total_candidates,
               result_type,
               total_electorate,
               total_votes,
               total_votes_valid,
               total_votes_invalid,
               turnout,
               margin_of_victory_votes,
               total_candidates_sc,
               total_candidates_st,
               total_candidates_obc,
               total_candidates_gen,
               total_candidates_female,
               avg_education,
               avg_criminal_history,
               avg_total_assets_asinh,
               avg_zero_moveable,
               avg_zero_immoveable,
               avg_age,
               frac_candidates_sc,
               frac_candidates_st,
               frac_candidates_obc,
               frac_candidates_highcaste,
               frac_candidates_female,
               avg_m_education,
               avg_m_criminal_history,
               runnerup_female,
               winner_age,
               runnerup_age,
               winner_education,
               runnerup_education,
               winner_caste,
               runnerup_caste,
               winner_moveable_assets,
               runnerup_moveable_assets,
               winner_moveable_assets_asinh,
               runnerup_moveable_assets_asinh,
               winner_immoveable_assets,
               runnerup_immoveable_assets,
               winner_immoveable_assets_asinh,
               runnerup_immoveable_assets_asinh,
               winner_total_assets,
               runnerup_total_assets,
               winner_total_assets_asinh,
               runnerup_total_assets_asinh,
               winner_criminal_history,
               runnerup_criminal_history,
               winner_votes,
               runnerup_votes,
               winner_percent,
               runnerup_percent,
               winner_deposit,
               runnerup_deposit,
               winner_sc,
               runnerup_sc,
               winner_obc,
               runnerup_obc,
               winner_gen_caste,
               runnerup_gen_caste,
               winner_st,
               runnerup_st,
               winner_m_education,
               runnerup_m_education,
               winner_m_criminal_history,
               runnerup_m_criminal_history,
               winner_zero_moveable_assets,
               runnerup_zero_moveable_assets,
               winner_zero_immoveable_assets,
               runnerup_zero_moveable_assets
          )
     )

write_parquet(jeff_wide, here("data/up/jeff_wide.parquet"))

# Brief sanity check
summary(lm(winner_female_2020 ~ reservation_female_2010 + reservation_female_2015, 
           data = jeff_wide[jeff_wide$reservation_female_2020 == 0, ]))
