# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(arrow)
library(stringr)
library(here)
library(tidyr)
library(fixest)
library(kableExtra)
library(janitor)

# Load data ---------------------------------------------------------------
data_dir <- here("data/uttarakhand")
uk <- read_csv(here(data_dir, "uttarakhand-panchayat-elections.csv"))

# Rename cols
uk <- uk %>% 
     rename(
          year = `Year`,
          elected_position = `निर्वाचित पद`,
          district = `जनपद`,
          declared_result = `घोषित परिणाम`,
          unopposed_winner = `निर्विरोध निर्वाचित`,
          development_block = `विकास खण्‍ड`,
          gram_panchayat = `ग्राम पंचायत`,
          reservation_status = `आरक्षण स्थिति`,
          winner_name = `विजयी प्रत्‍याशी का नाम`,
          election_symbol = `चुनाव चिन्‍ह`,
          received_votes = `प्राप्‍त मत`,
          runner_up_name = `प्रथम रनर अप प्रत्‍याशी का नाम`,
          election_symbol_1 = `चुनाव चिन्‍ह.1`,
          secured_votes = `प्राप्‍त मत.1`,
          vote_difference = `मतों का अन्‍तर`,
          district_panchayat_ward = `क्षेत्र पंचायत वाडॅ`,
          zilla_panchayat_ward = `जिला पंचायत वाडॅ`,
          serial_number = `क्रमांक`,
          candidate_name = `अभ्‍यर्थी का नाम`,
          father_husband_name = `पिता/पति का नाम`,
          election_symbol_2 = `चुनाव चिन्‍ह.2`,
          received_votes_2 = `प्राप्‍त मत.2`
     )

# Translation
translation_dict <- c(
     "अनारक्षित" = "Unreserved",
     "महिला" = "Female",
     "अन्य पिछड़ा वर्ग महिला" = "Other Backward Class Female",
     "अनु0जाति" = "Scheduled Tribe",
     "अन्य पिछड़ा वर्ग" = "Other Backward Class",
     "अनु0जाति महिला" = "Scheduled Tribe Female",
     "अनु0जनजाति महिला" = "Scheduled Caste Female",
     "अनु0जनजाति" = "Scheduled Caste",
     "अनुसूचित जाति महिला" = "Scheduled Caste Female",
     "अनुसूचित जाति" = "Scheduled Caste",
     "अन्य पिछडा वर्ग महिला" = "Other Backward Class Female",
     "अन्य पिछडा वर्ग" = "Other Backward Class",
     "अनुसूचित जनजाति महिला" = "Scheduled Tribe Female",
     "अनुसूचित जनजाति" = "Scheduled Tribe",
     "अनु0जा0महिला" = "Scheduled Tribe Female",
     "अ0पि0वर्ग" = "General",
     "अ0पि0व0महिला" = "General Female",
     "अनु0ज0जा0महिला" = "Scheduled Tribe Female",
     "अनु0ज0जा0" = "Scheduled Tribe",
     "अनु0 जाति महिला" = "Scheduled Tribe Female",
     "म्हिला" = "Female",
     "अनु0 जाति" = "Scheduled Tribe",
     "अनु0 ज0जाति महिला" = "Scheduled Tribe Female",
     "अनु0ज0जाति" = "Scheduled Tribe"
)

uk <- uk %>%
     mutate(winner_name = trimws(winner_name, "both"),
            candidate_name = trimws(candidate_name, "both"),
            gram_panchayat = trimws(gram_panchayat, "both"),
            gram_panchayat = trimws(gsub("^[0-9]+-", "", gram_panchayat), "both"),
            district = trimws(district, "both"),
            development_block = trimws(development_block, "both"),
            key = paste0(district, gram_panchayat),
            reservation_status = case_when(
                 reservation_status %in% names(translation_dict) ~ translation_dict[reservation_status],
                 TRUE ~ reservation_status),
            treat = ifelse(reservation_status %in% c("Female",
                                                     "Other Backward Class Female",
                                                     "Scheduled Tribe Female", 
                                                     "Scheduled Caste Female",
                                                     "General Female"), 1, 0),
            dalit = ifelse(reservation_status %in% c("Scheduled Tribe Female",
                                                     "Scheduled Tribe",
                                                     "Scheduled Caste Female",
                                                     "Scheduled Caste"), 1, 0),
            obc =  ifelse(reservation_status %in% c("Other Backward Class",
                                                    "Other Backward Class Female"), 1, 0))

uk_gp <- uk %>% 
     filter(elected_position == "प्रधान (ग्राम पंचायत)")%>% 
     filter(winner_name == candidate_name) %>%
     group_by(year, key) %>%
     filter(n() == 1) %>%
     ungroup()

uk_gp_wide <- uk_gp[, c("key", "year", "reservation_status", "winner_name")] %>%
     pivot_wider(
          id_cols = key,
          names_from = year,
          values_from = c(reservation_status, winner_name),
          names_sep = "_",
          values_fill = list(reservation_status = NA, winner_name = NA)
     ) %>%
     filter(!is.na(reservation_status_2008) & !is.na(reservation_status_2014) & !is.na(reservation_status_2019))

# Fuzzy join
select_cols = c("key", "year", "reservation_status", "winner_name")

uk_gp_2008 <- uk_gp[uk_gp$year == 2008, select_cols] %>%
     rename_with(~ paste0(., "_2008"))
uk_gp_2014 <- uk_gp[uk_gp$year == 2014, select_cols ] %>%
     rename_with(~ paste0(., "_2014"))
uk_gp_2019 <- uk_gp[uk_gp$year == 2019, select_cols ] %>%
     rename_with(~ paste0(., "_2019"))

data_08_14_fuzzy <- uk_gp_2008 %>% 
     stringdist_inner_join(uk_gp_2014, 
                           by = c('key_2008' = 'key_2014'),
                           ignore_case = TRUE,
                           distance_col = "dist",
                           max_dist = 1)

data_08_14_fuzzy <- data_08_14_fuzzy %>%
     group_by(key_2008) %>%
     slice_min(order_by = dist, n = 1) %>%
     ungroup()

data_08_14_19_fuzzy <- data_08_14_fuzzy %>% 
     stringdist_inner_join(uk_gp_2019, 
                           by = c('key_2014' = 'key_2019'),
                           ignore_case = TRUE,
                           distance_col = "dist_14_19",
                           max_dist = 1)

data_08_14_19_fuzzy <- data_08_14_19_fuzzy %>%
     group_by(key_2014) %>%
     slice_min(order_by = dist_14_19, n = 1) %>%
     ungroup()

write_parquet(data_08_14_fuzzy, "data/uttarakhand/data_08_14_fuzzy.parquet")
write_parquet(data_08_14_19_fuzzy, "data/uttarakhand/data_08_14_19_fuzzy.parquet")
