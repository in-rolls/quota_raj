
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

data_dir <- here("..", "data/kerala")
kerala_files <- here(data_dir, "lsgi-election-kerala.csv")
kerala_data <- readr::read_csv(kerala_files)

# Clean raw data ---------------------------------------

colnames(kerala_data) <- tolower(colnames(kerala_data))

kerala_data <- kerala_data %>% 
     rename(lgi_type = 'lgi type',
            gram_panchayat = 'grama panchayat',
            ward_no = 'ward no.',
            ward_name = 'ward name',
            elected_members = 'elected members',
            name_of_member = 'name of member',
            gender = 'female/male',
            marital_status = 'marital status')

# only retain gram panchayat 
kerala_panch <- kerala_data %>% 
     dplyr::filter(lgi_type=="Grama Panchayat")

rm(kerala_data)

data_2010 <- filter(kerala_panch, year == 2010)
data_2010$key <- paste0(data_2010$district, data_2010$gram_panchayat, data_2010$ward_name, data_2010$ward_no)

data_2015 <- filter(kerala_panch, year == 2015)
data_2015$key <- paste0(data_2015$district, data_2015$gram_panchayat, data_2015$ward_name, data_2015$ward_no)

# #Simple Join #fail
# 
# simple_inner_10_15_<- data_2010 %>% 
#      inner_join(data_2015, 
#                 by = 'key',
#                 suffix = c("_2010", "_2015"))


#FuzzyJoin retains more! duh!

names(data_2010) <- paste0(names(data_2010), "_2010")
names(data_2015) <- paste0(names(data_2015), "_2015")

data_10_15_fuzzy <- data_2010 %>% 
     stringdist_inner_join(data_2015, 
                           by = c('key_2010' = 'key_2015'),
                           ignore_case = TRUE,
                           distance_col = "dist")

# write_csv(data_10_15_fuzzy, here(data_dir, "fuzze_join.csv") )

data_2020 <- filter(kerala_panch, year == 2020)
data_2020$key <- paste0(data_2020$district, data_2020$gram_panchayat, data_2020$ward_name, data_2020$ward_no)

names(data_2020) <- paste0(names(data_2020), "_2020")

# data_10_15_20_fuzzy <- data_2020 %>% 
#      stringdist_left_join(data_10_15_fuzzy,
#                           by = c('key_2020' = 'key_2015'), 
#                           method = "lv",
#                           distance_col = "dist",
#                           max_dist = 3,
#                           ignore_case = TRUE
#      )


data_10_15_20_fuzzy_inner <- data_2020 %>% 
     stringdist_inner_join(data_10_15_fuzzy,
                           by = c('key_2020' = 'key_2015'), 
                           ignore_case = TRUE,
                           distance_col = "dist")


kerala_wide <- data_10_15_20_fuzzy_inner

kerala_wide <- kerala_wide %>%
     mutate(treat_2010 = ifelse(kerala_wide$reservation_2010 %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            treat_2015 = ifelse(kerala_wide$reservation_2015 %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            treat_2020 = ifelse(kerala_wide$reservation_2020 %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            dalit_2010 = ifelse(kerala_wide$reservation_2010 %in% c("SC", "ST", "SC Woman", "ST Woman"), 1, 0),
            dalit_2015 = ifelse(kerala_wide$reservation_2015 %in% c("SC", "ST", "SC Woman", "ST Woman"), 1, 0),
            dalit_2020 = ifelse(kerala_wide$reservation_2020 %in% c("SC", "ST", "SC Woman", "ST Woman"), 1, 0),
            always_treated = ifelse(treat_2010 + treat_2015 == 2, 1, 0),   
            never_treated = ifelse(treat_2010 + treat_2015 == 0, 1, 0),    
            sometimes_treated = ifelse((treat_2010 | treat_2015 == 0), 1, 0),
            sometimes_treated1212 = ifelse(treat_2010 + treat_2015 == 1, 1, 0))


# write_csv(kerala_wide, here(data_dir, "kerala_wide.csv") )



# Transition Matrices -----------------------------------------------------

trans_10_15 <- table(kerala_wide$treat_2010, kerala_wide$treat_2015)
trans_15_20 <- table(kerala_wide$treat_2015, kerala_wide$treat_2020)

#comparison with 2010 - 2015

trans_10_15 <- table(kerala_wide$treat_2010, kerala_wide$treat_2015)
trans_10_20 <- table(kerala_wide$treat_2010, kerala_wide$treat_2020)

print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20,
     `2010-2015` = trans_10_15,
     `2010-2020` = trans_10_20
))


#write_to_latex(trans_matrices_store, here("..", "tables", "tran_matrices.tex"))

bin_trans_10_15 <- table(kerala_wide$treat_2010, kerala_wide$treat_2015)
bin_trans_15_20 <- table(kerala_wide$treat_2015, kerala_wide$treat_2020)
bin_trans_10_20 <- table(kerala_wide$treat_2010, kerala_wide$treat_2020)

bin_trans_matrices <- list(
     
     `2010-2015` = bin_trans_10_15,
     `2015-2020` = bin_trans_15_20,
     `2010-2020` = bin_trans_10_20
)



# Chi-Squared Test --------------------------------------------------------

chi_sq_test_10_15 <- chisq.test(bin_trans_10_15)
chi_sq_test_10_15 #done

chi_sq_test_15_20 <- chisq.test(bin_trans_15_20)
chi_sq_test_15_20#

chi_sq_test_10_20 <- chisq.test(bin_trans_10_20)
chi_sq_test_10_20#done


chi_squared_results <- data.frame(
     "Comparison" = c( "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = here("..", "tables", "kerala_chi_squared_results.tex"))



# Models ------------------------------------------------------------------

m_10_15 <- lm(I(gender_2015 == "Female") ~ treat_2010, data = filter(kerala_wide, treat_2015 == 0))
summary(m_10_15)

m_15_20 <- lm(I(gender_2020 == "Female") ~ treat_2015, data = filter(kerala_wide, treat_2020 == 0))
summary(m_15_20)

m_interact <- lm(I(gender_2020 == "Female") ~ treat_2010 * treat_2015, data = filter(kerala_wide, treat_2020 == 0))
summary(m_interact)

m_interact <- lm(I(gender_2020 == "Female") ~ treat_2010 * treat_2015, data = filter(kerala_wide, treat_2020 == 0))
summary(m_interact)

m_interact <- feols(I(gender_2020 == "Female") ~ treat_2010, data = filter(kerala_wide, treat_2020 == 0))
summary(m_interact)

levels(as.factor(kerala_wide$never_treated))
levels(as.factor(kerala_wide$always_treated))
levels(as.factor(kerala_wide$sometimes_treated))

mean(kerala_wide$sometimes_treated)# 0.52
mean(kerala_wide$always_treated) # 0.03568372
mean((kerala_wide$never_treated)) # 0.004740273


m_always <- feols(I(gender_2020 == "Female") ~ always_treated, data = filter(kerala_wide, treat_2020 == 0))
summary(m_always)

m_never <- feols(I(gender_2020 == "Female") ~ never_treated, data = filter(kerala_wide, treat_2020 == 0))
summary(m_never)


m_sometimes <- feols(I(gender_2020 == "Female") ~ sometimes_treated, data = filter(kerala_wide, treat_2020 == 0))
summary(m_sometimes)

# Legacy ------------------------------------------------------------------

# blind wide reshape
rm(list=ls())

# Load data ---------------------------------------------------------------

data_dir <- here("..", "data/kerala")
kerala_files <- here(data_dir, "lsgi-election-kerala.csv")
kerala_data <- readr::read_csv(kerala_files)

# Clean raw data ----------------------------------------------------------

colnames(kerala_data) <- tolower(colnames(kerala_data))
names(kerala_data)

kerala_data <- kerala_data %>% 
     rename(lgi_type = 'lgi type',
            gram_panchayat = 'grama panchayat',
            ward_no = 'ward no.',
            ward_name = 'ward name',
            elected_members = 'elected members',
            name_of_member = 'name of member',
            gender = 'female/male',
            marital_status = 'marital status')


# only retain gram panchayat 
kerala_panch <- kerala_data %>% 
     dplyr::filter(lgi_type=="Grama Panchayat")

rm(kerala_data)

levels(as.factor(kerala_panch$lgi_type))


kerala_panch <- kerala_panch %>% 
     select(-block, -municipality, -corporation, -lgi_type, -address,-`ward name.1`, -`ward no..1`)

kerala_panch <- kerala_panch %>%
     group_by(year) %>%
     mutate(unique_id = paste(year, row_number(), sep = "_")) %>%
     ungroup()

# paste(names(kerala_panch), collapse = ", ")

kerala_wide <- kerala_panch %>%
     pivot_wider(
          id_cols = "unique_id",
          names_from = year,
          values_from = c(
               district, gram_panchayat, ward_no, ward_name, elected_members, role, party, 
               reservation, name_of_member, phone, mobile, age, gender,
               marital_status , occupation, image))
