# Load libs
library(tidyverse)
library(readr)
library(stringdist)
library(fuzzyjoin)
library(magrittr)
library(here)
library(arrow)
library(readr)
library(progress)
library(janitor)

# Load dat
lgd_raj <- read_csv("data/lgd/raj_village_gp_mapping_2024.csv") %>% 
     clean_names() %>%
     mutate(state_id = "08") %>%
     group_by(district_name, subdistrict_name, local_body_name, village_name) %>%
     filter(n() == 1) %>%
     ungroup()

lgd_up <- read_csv("data/lgd/up_village_gp_mapping_2024.csv") %>% 
     clean_names() %>%
     mutate(state_id = "09") %>%
     group_by(district_name, subdistrict_name, local_body_name, village_name) %>%
     filter(n() == 1) %>%
     ungroup()

lgd <- bind_rows(lgd_raj, lgd_up)

# VD, PCA
sh_pca_01 <- read_csv("data/shrug/shrug-pca01-csv/pc01_pca_clean_shrid.csv")
sh_pca_01 <- sh_pca_01 %>%
     separate(shrid2, 
              into = c("census_yr", 
                       "state_id", 
                       "district_census_2011_code", 
                       "subdistrict_census_2011_code", 
                       "village_census_2011_code"), 
              sep = "-",
              remove = FALSE)

# We may want to remove cases where town is included: subdistrict = 00
# https://docs.devdatalab.org/SHRUG-Construction-Details/location-identifiers/town-and-village-identifiers/
sh_pca_01 <- sh_pca_01 %>% 
     filter(subdistrict_census_2011_code != "00") %>%
     filter(state_id == "08" | state_id == "09")

## Merge with LGD
shrug_lgd <- sh_pca_01 %>%
     inner_join(lgd, by = c("state_id", 
                            "district_census_2011_code", 
                            "subdistrict_census_2011_code", 
                            "village_census_2011_code")) %>%
     mutate(state = ifelse(state_id == "08", "Rajasthan", "Uttar Pradesh"),
            key = normalize_string(paste(district_name, subdistrict_name, local_body_name)),
            state_key = normalize_string(paste(state, district_name, subdistrict_name, local_body_name)))

## Merge to Elex/MNREGA
mnrega_elex_raj_05_10 <- read_parquet("data/raj/mnrega_elex_raj_05_10.parquet")
shrug_lgd_elex_strict <- shrug_lgd %>%
     inner_join(mnrega_elex_raj_05_10, by = c("key" = "match_name.x"))

# Process each row
process_row <- function(row) {

     election_subset <- mnrega_elex_raj_05_10 %>%
          filter(normalize_string(dist_name_new_2010) == normalize_string(row$district_name))
     
     # If no matching district found, return NULL
     if (nrow(election_subset) == 0) return(NULL)
     
     match_result <- stringdist_left_join(
          row,
          election_subset,
          by = c("key" = "match_name.x"),
          method = "jw",
          ignore_case = TRUE,
          distance_col = "dist_elex_lgd_match"
     ) %>%
          slice_min(dist_elex_lgd_match)
     
     return(match_result)
}

shrug_lgd_elex <- shrug_lgd[, c("district_name", "key")] %>%
     split(seq(nrow(.))) %>%
     map_dfr(~ process_row(.x))

# Remove duplicates and filter down to where  dist_mnrega_match < .05
shrug_lgd_elex <- shrug_lgd_elex %>%
     filter(dist_elex_lgd_match < 0.1) %>%
     group_by(key) %>%
     mutate(dup_x = n() > 1) %>%
     ungroup() %>%
     group_by(match_name) %>%
     mutate(dup_y = n() > 1) %>%
     ungroup() %>%
     filter(!dup_x & !dup_y) %>%
     select(-dup_x, -dup_y)

write_parquet(shrug_lgd_elex, sink = "data/raj/shrug_lgd_raj_elex_05_10.parquet")
