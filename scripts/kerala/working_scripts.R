# Legacy to be purged after talk with Gaurav ------------------------------------------------------------------

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
