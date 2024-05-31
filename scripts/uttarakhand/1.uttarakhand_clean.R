
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
levels(as.factor(ukhand_data$year))
# Clean raw data ---------------------------------------



ukhand_data <- ukhand_data %>% 
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


ukhand_data <- ukhand_data %>% 
     filter(elected_position == "प्रधान (ग्राम पंचायत)")




ukhand_panch <- ukhand_data[ukhand_data$winner_name == ukhand_data$candidate_name, ] #remove duplicates

rm(ukhand_data)
# sum(is.na(ukhand_data$district_panchayat_ward)) == nrow(ukhand_data) #worthless column
# sum(is.na(ukhand_data$zilla_panchayat_ward)) == nrow(ukhand_data) ##worthless column


data_2008 <- filter(ukhand_panch, year == 2008)
data_2008$gram_panchayat <- trimws(data_2008$gram_panchayat, "both")


data_2008$key <- paste0(data_2008$district, data_2008$development_block, 
                        data_2008$gram_panchayat)#, data_2008$uniqueid)


# remove unnecessary columns for present analysis that is creating suplications (repeated rows due to father_husband_name)
data_2008 <- data_2008 %>% 
     select(-c(declared_result, unopposed_winner,
               runner_up_name, election_symbol_1, secured_votes, vote_difference, candidate_name, election_symbol_2,
               received_votes_2, district_panchayat_ward, zilla_panchayat_ward, serial_number, father_husband_name))


cleaned_data_2008 <- unique(data_2008)


# Check for duplicates in the key column
duplicated_keys <- cleaned_data_2008 %>% 
     group_by(key) %>% 
     summarise(count = n()) %>% 
     filter(count > 1) %>% 
     pull(key)

# Filter the original dataframe to extract rows with duplicated keys
duplicates_df <- cleaned_data_2008 %>% 
     filter(key %in% duplicated_keys)

# Remove the duplicated rows from the original dataframe
cleaned_data_2008 <- cleaned_data_2008 %>% 
     filter(!(key %in% duplicated_keys))







data_2014 <- filter(ukhand_panch, year == 2014)
data_2014$gram_panchayat_new <- gsub("^\\d+-", "", data_2014$gram_panchayat)
data_2014$gram_panchayat_new <- trimws(data_2014$gram_panchayat_new, "both")
data_2014$key <- paste0(data_2014$district, data_2014$development_block, 
                        data_2014$gram_panchayat_new)#, data_2014$uniqueid)



data_2014 <- data_2014 %>% 
     select(-c(declared_result, unopposed_winner,
               runner_up_name, election_symbol_1, secured_votes, vote_difference, candidate_name, election_symbol_2,
               received_votes_2, district_panchayat_ward, zilla_panchayat_ward, serial_number, father_husband_name, gram_panchayat))

cleaned_data_2014 <- unique(data_2014)

# Check for duplicates in the key column
duplicated_keys_14 <- cleaned_data_2014 %>% 
     group_by(key) %>% 
     summarise(count = n()) %>% 
     filter(count > 1) %>% 
     pull(key)

# Filter the original dataframe to extract rows with duplicated keys
duplicates_df_14 <- cleaned_data_2014 %>% 
     filter(key %in% duplicated_keys_14)

# Remove the duplicated rows from the original dataframe
cleaned_data_2014 <- cleaned_data_2014 %>% 
     filter(!(key %in% duplicated_keys_14))



names(cleaned_data_2008) <- paste0(names(cleaned_data_2008), "_2008")
names(cleaned_data_2014) <- paste0(names(data_2014), "_2014")


data_08_14_fuzzy <- cleaned_data_2008 %>% 
     stringdist_inner_join(cleaned_data_2014, 
                           by = c('key_2008' = 'key_2014'),
                           ignore_case = TRUE,
                           distance_col = "dist")


# test <- data_08_14_fuzzy %>% 
#      filter(dist == 0)

data_2019 <- filter(ukhand_panch, year == 2019)
data_2019$gram_panchayat <- trimws(data_2019$gram_panchayat, "both")

data_2019$key <- paste0(data_2019$district, data_2019$development_block, 
                        data_2019$gram_panchayat)#, data_2019$unique_id)


data_2019 <- data_2019 %>% 
     select(-c(declared_result, unopposed_winner,
               runner_up_name, election_symbol_1, secured_votes, vote_difference, candidate_name, election_symbol_2,
               received_votes_2, district_panchayat_ward, zilla_panchayat_ward, serial_number, father_husband_name))

cleaned_data_2019 <- unique(data_2019)

# Check for duplicates in the key column
duplicated_keys_19 <- cleaned_data_2019 %>% 
     group_by(key) %>% 
     summarise(count = n()) %>% 
     filter(count > 1) %>% 
     pull(key)

# Filter the original dataframe to extract rows with duplicated keys
duplicated_keys_19 <- cleaned_data_2019 %>% 
     filter(key %in% duplicated_keys_19)

# Remove the duplicated rows from the original dataframe
cleaned_data_2019 <- cleaned_data_2019 %>% 
     filter(!(key %in% duplicated_keys_19))



names(data_2019) <- paste0(names(data_2019), "_2019")



data_08_14_19_fuzzy <- data_08_14_fuzzy %>% 
     stringdist_inner_join(data_2019, 
                           by = c('key_2014' = 'key_2019'),
                           ignore_case = TRUE,
                           distance_col = "dist")





