# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(tidyr)

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
levels(as.factor(kerala_panch$lgi_type))
kerala_panch <- kerala_panch %>% 
     select(-block, -municipality, -corporation, -lgi_type, -address,-`ward name.1`, -`ward no..1`)


names(kerala_panch)

# 
# kerala_wide <- reshape(
#      data = kerala_panch,
#      direction = "wide",
#      idvar = c("district", "gram_panchayat", "ward_no", "ward_name", "elected_members", "role", "party", "reservation", "name_of_member", "phone", "mobile", "age", "gender", "marital_status", "educational qualification", "occupation", "image"),
#      timevar = "year"
# )


kerala_wide_year <-kerala_panch %>%
     pivot_wider(
     names_from = year,
     values_from = c(
           age, district, 
          `educational qualification`, elected_members, gender, 
          gram_panchayat, marital_status, mobile, 
          name_of_member, occupation, party, 
          phone, reservation, role,  ward_name, 
          ward_no
     ),
     values_fn = list )


kerala_wide_year <- kerala_wide_year %>% select(-image)



# write.csv(test, "/Users/varun/Library/CloudStorage/Dropbox/USC/quota_raj/data/kerala/test.csv", row.names=FALSE)


# treatment  --------------------------------------------------------------


