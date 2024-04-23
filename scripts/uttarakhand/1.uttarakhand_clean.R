
library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(tidyr)
library(fixest)
library(kableExtra)
# Load data ---------------------------------------------------------------

data_dir <- here("..", "data/uttarakhand")
ukhand_files <- here(data_dir, "uttarakhand-panchayat-elections.csv")
ukhand_data <- readr::read_csv(ukhand_files)
colnames(ukhand_data) <- tolower(colnames(ukhand_data))


names(ukhand_data)
levels(as.factor(ukhand_data$Year))
# Clean raw data ---------------------------------------



year <- ukhand_data %>% 
     rename(
          year = `year`,
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


ukhand_data <- ukhand_data %>%
     mutate(reservation_status = case_when(
          reservation_status %in% names(translation_dict) ~ translation_dict[reservation_status],
          TRUE ~ reservation_status
     ))

unique(ukhand_data$reservation_status)



ukhand_data <- ukhand_data %>% 
     dplyr::mutate(treat = ifelse(ukhand_data$reservation_status %in% c("Female","Other Backward Class Female","Scheduled Tribe Female", 
                                                                           "Scheduled Caste Female", "General Female"), 1, 0),
                   dalit = ifelse(ukhand_data$reservation_status %in% c("Scheduled Tribe Female", "Scheduled Tribe",
                                                                       "Scheduled Caste Female","Scheduled Caste"), 1, 0),
                   obc =  ifelse(ukhand_data$reservation_status %in% c("Other Backward Class","Other Backward Class Female"), 1, 0))




