# Load libs

library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)


# Load data ---------------------------------------------------------------

data_dir <- here("..", "data/up")
up_files <- here(data_dir, "up_gram_panchayat_pradhan_2021.csv")
up_data <- readr::read_csv(up_files)

# Clean raw data ----------------------------------------------------------

colnames(up_data) <- tolower(colnames(up_data))
names(up_data)

up_data <- up_data %>% 
     rename(lgi_type = 'lgi type',
            gram_panchayat = 'grama panchayat',
            ward_no = 'ward no.',
            ward_name = 'ward name',
            elected_members = 'elected members',
            name_of_member = 'name of member',
            gender = 'female/male',
            marital_status = 'marital status')

names(up_data)

# only retain gram panchayat 
up_panch <- up_data %>% 
     dplyr::filter(lgi_type=="Grama Panchayat")
levels(as.factor(up_panch$lgi_type))
up_panch <- up_panch %>% 
     select(-block, -municipality, -corporation)



# treatment  --------------------------------------------------------------

# Transition Matrces ------------------------------------------------------

# comparing wiht previous reservation status

trans_10_15 <- table(up_panch$prez_treat_2010, up_panch$prez_treat_2015)
trans_15_20 <- table(up_panch$prez_treat_2015, up_panch$prez_treat_2020)


print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20
))


#Ward Members

up_panch$treat_2010 <- NA
up_panch$treat_2015 <- NA
up_panch$treat_2020 <- NA
up_panch$dalit_2010 <- NA
up_panch$dalit_2015 <- NA
up_panch$dalit_2020 <- NA

up_panch <- up_panch %>% 
     mutate(treat = ifelse(up_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
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

# levels(as.factor(up_panch$year)) # 2010, 15, 20
# levels(as.factor(up_panch$gender)) # Female male
# levels(as.factor(up_panch$reservation)) # "General"  "SC"       "SC Woman" "ST"       "ST Woman" "Woman" 


levels(as.factor(up_panch$always_treated))
levels(as.factor(up_panch$never_treated))
levels(as.factor(up_panch$sometimes_treated))
levels(as.factor(up_panch$twice_treated))


trans_10_15 <- table(up_panch$treat_2010, up_panch$treat_2015)
trans_15_20 <- table(up_panch$treat_2015, up_panch$treat_2020)

print(trans_matrices <- list(
     `2010-2015` = trans_10_15,
     `2015-2020` = trans_15_20
))


# Transition Matrices -----------------------------------------------------


bin_trans_10_15 <- table(up_panch$treat_2010, up_panch$treat_2015)
bin_trans_10_20 <- table(up_panch$treat_2010, up_panch$treat_2020)
bin_trans_15_20 <- table(up_panch$treat_2015, up_panch$treat_2020)


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

cat(chi_squared_results_table, file = here("..", "tables", "up_chi_squared_results.tex"))



# Models ------------------------------------------------------------------
# Short-Term Regs ---------------------------------------------------------

library(fixest)

# Short Run Effects of Quotas (10-->15) -----------------------------------

m_10_15 <- feols((gender =="Female" & year==2015) ~ treat_2010 , data = filter(up_panch, year %in% c(2010, 2015), treat_2015 == 0))
summary(m_10_15)



m_10_15_dfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 | district, vcov = ~gram_panchayat, data = filter(up_panch, year %in% c(2010, 2015), treat_2015 == 0) )
summary(m_10_15_dfe)

m_10_15_gpfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 |  district + gram_panchayat, vcov = ~gram_panchayat,  data = filter(up_panch, year %in% c(2010, 2015),  treat_2015 == 0))
summary(m_10_15_gpfe)

m_10_15_wardfe <- feols((gender =="Female" & year==2015 ) ~ treat_2010 |  district +  gram_panchayat + ward_no ,  vcov = ~gram_panchayat,  data = filter(up_panch, year %in% c(2010, 2015), treat_2015 == 0))
summary(m_10_15_wardfe)

# Short Run Effects of Quotas (15-->20) -----------------------------------


m_15_20 <- feols((gender =="Female" & year==2020) ~ treat_2015, data = filter(up_panch, year %in% c(2015, 2020),  treat_2020 == 0))
summary(m_15_20)


m_15_20_dfe <- feols((gender =="Female" & year==2020) ~ treat_2015 | district, vcov = ~gram_panchayat, data = filter(up_panch, year %in% c(2015, 2020), treat_2020 == 0) )
summary(m_15_20_dfe)

m_15_20_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2015 |  district + gram_panchayat, vcov = ~gram_panchayat,  data = filter(up_panch, year %in% c(2015, 2020), treat_2020 == 0))
summary(m_15_20_gpfe)

m_15_20_wardfe <- feols((gender =="Female" & year==2020 ) ~ treat_2015 |  district +  gram_panchayat + ward_no ,  vcov = ~gram_panchayat,  data = filter(up_panch, year %in% c(2015, 2020), treat_2020 == 0))
summary(m_15_20_wardfe)

# Long-Term Regs ---------------------------------------------------------


# 2010 * 2015  full interaction #might be collinear

m_long_term <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015,   data = filter(up_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district, vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district + gram_panchayat , vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

m_long_term_wardfe <- feols((gender =="Female" & year==2020) ~ treat_2010 * treat_2015  | district + gram_panchayat + ward_no , vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_wardfe)



# 2010 + 2015  full interaction

m_long_term <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015,   data = filter(up_panch, treat_2020 == 0))
summary(m_long_term)

m_long_term_dfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district, vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_dfe)

m_long_term_gpfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district + gram_panchayat , vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_gpfe)

m_long_term_wardfe <- feols((gender =="Female" & year==2020) ~ treat_2010 + treat_2015  | district + gram_panchayat + ward_no , vcov = ~gram_panchayat, data = filter(up_panch, treat_2020 == 0))
summary(m_long_term_wardfe)



# Dalit Models ------------------------------------------------------------
# Presidents and VPs ------------------------------------------------------


#Where the offices of Presidents and Chairpersons is reserved for women, the offices of the Vice Presidents and
# Deputy Chairpersons will be unreserved and where the offices of the Presidents and Chairpersons not reserved for women, 
# the offices of the Vice Presidents and Deputy Chairpersons will be reserved for women.


levels(as.factor(up_panch$role))

up_panch <- up_panch %>% 
     mutate(president = ifelse(up_panch$role == 'President',1,0),
            vice_president = ifelse(up_panch$role == 'Vice President',1,0))

up_panch <- up_panch %>% 
     mutate(prez_treat = ifelse(president == 1 & up_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"), 1, 0),
            vp_treat = ifelse(vice_president == 1 & up_panch$reservation %in% c("Woman", "SC Woman", "ST Woman"),1,0),
            prez_treat_2010 = ifelse(prez_treat == 1 & year == 2010,1,0),
            prez_treat_2015 = ifelse(prez_treat == 1 & year == 2015,1,0),
            prez_treat_2020 = ifelse(prez_treat == 1 & year == 2020,1,0),
            vp_treat_2010 = ifelse(vice_president == 1 & year == 2010,1,0),
            vp_treat_2015 = ifelse(vice_president == 1 & year == 2015,1,0),
            vp_treat_2020 = ifelse(vice_president == 1 & year == 2020,1,0),
            prez_always = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 3,1,0),
            prez_never = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 0,1,0),
            prez_twice = ifelse(prez_treat_2010 + prez_treat_2015 + prez_treat_2010 == 2,1,0),
            vp_always = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 3,1,0),
            vp_never = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 0,1,0),
            vp_twice = ifelse(vp_treat_2010 + vp_treat_2015 + vp_treat_2010 == 2,1,0))


levels(as.factor(up_panch$prez_always))
levels(as.factor(up_panch$prez_never))
levels(as.factor(up_panch$prez_twice))

levels(as.factor(up_panch$vp_always))
levels(as.factor(up_panch$vp_never))
levels(as.factor(up_panch$vp_twice))



