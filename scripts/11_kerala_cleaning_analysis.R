# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)


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

names(kerala_data)

# only retain gram panchayat 
kerala_panch <- kerala_data %>% 
     dplyr::filter(lgi_type=="Grama Panchayat")
levels(as.factor(kerala_panch$lgi_type))
kerala_panch <- kerala_panch %>% 
     select(-block, -municipality, -corporation)



# treatment  --------------------------------------------------------------

# Transition Matrces ------------------------------------------------------

# comparing wiht previous reservation status

trans_10_15 <- table(kerala_panch$prez_treat_2010, kerala_panch$prez_treat_2015)
trans_15_20 <- table(kerala_panch$prez_treat_2015, kerala_panch$prez_treat_2020)


print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20
))


#Ward Members

kerala_panch$treat_2010 <- NA
kerala_panch$treat_2015 <- NA
kerala_panch$treat_2020 <- NA
kerala_panch$dalit_2010 <- NA
kerala_panch$dalit_2015 <- NA
kerala_panch$dalit_2020 <- NA

kerala_panch <- kerala_panch %>% 
     mutate(treat = ifelse(kerala_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            treat_2010 = ifelse(reservation %in% c("Woman", "SC Woman", "ST Woman") & year == 2010, 1, 0),
            treat_2015 = ifelse(reservation %in% c("Woman", "SC Woman", "ST Woman") & year == 2015, 1, 0),
            treat_2020 = ifelse(reservation %in% c("Woman", "SC Woman", "ST Woman") & year == 2020, 1, 0),
            dalit_2010 =  ifelse(reservation %in% c("SC Woman", "ST Woman") & year == 2010, 1, 0),
            dalit_2015 =  ifelse(reservation %in% c("SC Woman", "ST Woman") & year == 2015, 1, 0),
            dalit_2020 =  ifelse(reservation %in% c("SC Woman", "ST Woman") & year == 2020, 1, 0),
            always_treated = ifelse(sum(treat_2010, treat_2015) == 2, 1, 0),
            never_treated = ifelse(sum(treat_2010, treat_2015) == 0, 1, 0),
            dalit_2010 = ifelse(reservation %in% c("SC", "ST", "SC Woman", "ST Woman") & year == 2010, 1, 0),
            dalit_2015 = ifelse(reservation %in% c("SC", "ST", "SC Woman", "ST Woman") & year == 2015, 1, 0),
            dalit_2020 = ifelse(reservation %in% c("SC", "ST", "SC Woman", "ST Woman") & year == 2020, 1, 0))

# levels(as.factor(kerala_panch$year)) # 2010, 15, 20
# levels(as.factor(kerala_panch$gender)) # Female male
# levels(as.factor(kerala_panch$reservation)) # "General"  "SC"       "SC Woman" "ST"       "ST Woman" "Woman" 


levels(as.factor(kerala_panch$always_treated))
levels(as.factor(kerala_panch$never_treated))
levels(as.factor(kerala_panch$sometimes_treated))
levels(as.factor(kerala_panch$twice_treated))


trans_10_15 <- table(kerala_panch$treat_2010, kerala_panch$treat_2015)
trans_15_20 <- table(kerala_panch$treat_2015, kerala_panch$treat_2020)

print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20
))


# Transition Matrices -----------------------------------------------------


bin_trans_10_15 <- table(kerala_panch$treat_2010, kerala_panch$treat_2015)
bin_trans_10_20 <- table(kerala_panch$treat_2010, kerala_panch$treat_2020)
bin_trans_15_20 <- table(kerala_panch$treat_2015, kerala_panch$treat_2020)


bin_trans_matrices <- list(
     
     `2010-2015` = bin_trans_10_15,
     `2010-2020` = bin_trans_10_20,
     `2015-2020` = bin_trans_15_20
)



# Chi-Squared Test --------------------------------------------------------

chi_sq_test_10_15 <- chisq.test(bin_trans_10_15)
chi_sq_test_10_15 #done


chi_sq_test_10_20 <- chisq.test(bin_trans_10_20)
chi_sq_test_10_20 #done

chi_sq_test_15_20 <- chisq.test(bin_trans_15_20)
chi_sq_test_15_20#


library(kableExtra)

chi_squared_results <- data.frame(
     "Comparison" = c( "2010-2015", "2010-2020", "2015-2020"),
     "Chi-Squared Test Statistic" = c(chi_sq_test_10_15$statistic, chi_sq_test_10_20$statistic, chi_sq_test_15_20$statistic),
     "Degrees of Freedom" = c(chi_sq_test_10_15$parameter, chi_sq_test_10_20$parameter, chi_sq_test_15_20$parameter),
     "P-Value" = format(c(chi_sq_test_10_15$p.value, chi_sq_test_10_20$p.value, chi_sq_test_15_20$p.value), digits = 4)
)

chi_squared_results_table <- kable(chi_squared_results, format = "latex", caption = "Chi-Squared Test Results", booktabs = TRUE)

cat(chi_squared_results_table, file = here("..", "tables", "kerala_chi_squared_results.tex"))



# Models ------------------------------------------------------------------
# Short-Term Regs ---------------------------------------------------------

library(fixest)

# Short Run Effects of Quotas (10-->15) -----------------------------------

m_10_15 <- feols((gender =="Female" & year==2015) ~ treat_2010 , data = filter(kerala_panch, year %in% c(2010, 2015), treat_2015 == 0))
summary(m_10_15)



m_10_15_dfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 | district, vcov = ~gram_panchayat, data = filter(kerala_panch, year %in% c(2010, 2015), treat_2015 == 0) )
summary(m_10_15_dfe)

m_10_15_gpfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 |  district + gram_panchayat, vcov = ~gram_panchayat,  data = filter(kerala_panch, year %in% c(2010, 2015),  treat_2015 == 0))
summary(m_10_15_gpfe)

m_10_15_wardfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 |  district +  gram_panchayat + ward_no ,  vcov = ~gram_panchayat,  data = filter(kerala_panch, year %in% c(2010, 2015), treat_2015 == 0))
summary(m_10_15_wardfe)

# Short Run Effects of Quotas (15-->20) -----------------------------------


m_15_20 <- feols((gender =="Female" & year==2020) ~ treat_2015, data = filter(kerala_panch, year %in% c(2015, 2020),  treat_2020 == 0))
summary(m_15_20)


m_15_20_dfe <- feols((gender =="Female" & year==2020) ~ treat_2015 | district, vcov = ~gram_panchayat, data = filter(kerala_panch, year %in% c(2015, 2020), treat_2020 == 0) )
summary(m_15_20_dfe)

m_15_20_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2015 |  district + gram_panchayat, vcov = ~gram_panchayat,  data = filter(kerala_panch, year %in% c(2015, 2020), treat_2020 == 0))
summary(m_15_20_gpfe)

m_15_20_wardfe <- feols((gender =="Female" & year==2020 ) ~ treat_2015 |  district +  gram_panchayat + ward_no ,  vcov = ~gram_panchayat,  data = filter(kerala_panch, year %in% c(2015, 2020), treat_2020 == 0))
summary(m_15_20_wardfe)

# Long-Term Regs ---------------------------------------------------------


# 2010 * 2015  full interaction #might be collinear

m_long_term <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015,   data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district, vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district + gram_panchayat , vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

m_long_term_wardfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district + gram_panchayat + ward_no , vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_wardfe)



# 2010 + 2015  full interaction

m_long_term <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015,   data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district, vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district + gram_panchayat , vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

m_long_term_wardfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district + gram_panchayat + ward_no , vcov = ~gram_panchayat, data = filter(kerala_panch, treat_2020 == 0))
summary(m_long_term_wardfe)

