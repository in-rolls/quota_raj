# Load libs
library(readr)
library(dplyr)
library(xtable)
library(fuzzyjoin)
library(stringr)
library(here)
library(kableExtra)
library(fixest)
library(arrow)
library(tidyverse)
library(stringi)
library(lfe)  # For clustered standard errors
library(ggplot2)
library(coefplot)
library(lmtest)
library(sandwich)
library(multiwayvcov)


# Pre-Processing for Balance Tests ----------------------------------------


# load("data/rajasthan/sarpanch_election_data/raj_panch.RData")
raj_lgd_shrug <- read_parquet("data/rajasthan/shrug_lgd_raj_elex_05_10.parquet")
shrug_vd <- read_csv("data/rajasthan/shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip")
raj_lgd_vd_merge <- merge(raj_lgd_shrug, shrug_vd, by = "shrid2") #amazing match

# Treatment Dummies --------------------------------------------------------
raj_lgd_vd_merge <- raj_lgd_vd_merge %>%
mutate(
treat_2005 = ifelse(grepl("W", reservation_2005, ignore.case = TRUE), 1, 0),
treat_2010 = ifelse(grepl("W", reservation_2010, ignore.case = TRUE), 1, 0), 

obc_2005 = ifelse(reservation_2005 == "OBC W", 1, 0),
obc_2010 = ifelse(reservation_2010 == "OBCW", 1, 0),

dalit_2005 = ifelse(reservation_2005 %in% c("SC W", "ST W"), 1, 0),
dalit_2010 = ifelse(reservation_2010 %in% c("SCW", "STW"), 1, 0),

all_sc_2005 = ifelse(grepl("SC|ST", reservation_2005, ignore.case = TRUE), 1, 0),
all_sc_2010 = ifelse(grepl("SC|ST", reservation_2010, ignore.case = TRUE), 1, 0),

all_obc_2005 = ifelse(grepl("OBC", reservation_2005, ignore.case = TRUE), 1, 0),
all_obc_2010 = ifelse(grepl("OBC", reservation_2010, ignore.case = TRUE), 1, 0)
)
     
     
raj_lgd_vd_merge$pc01_vd_bs_fac[is.na(raj_lgd_vd_merge$pc01_vd_bs_fac)] <- 0
raj_lgd_vd_merge$pc01_vd_rs_fac[is.na(raj_lgd_vd_merge$pc01_vd_rs_fac)] <- 0
raj_lgd_vd_merge$pc01_vd_nw_fac[is.na(raj_lgd_vd_merge$pc01_vd_nw_fac)] <- 0


# All Covariates -----------------------------------------------------------

# Variables Dictionary (Need this for sanity)

all_names <- names(raj_lgd_vd_merge)
pc01_vd_vars <- grep("^pc01_vd", all_names, value = TRUE)
print(pc01_vd_vars)

variable_names_mapping <- c(
     "Area_of_Village" = "pc01_vd_area",
     "Number_of_Households" = "pc01_vd_t_hh",
     "Total_Population" = "pc01_vd_t_p",
     "Total_Males" = "pc01_vd_t_m",
     "Total_Females" = "pc01_vd_t_f",
     "Scheduled_Castes_Population" = "pc01_vd_sc_p",
     "Scheduled_Castes_Males" = "pc01_vd_sc_m",
     "Scheduled_Castes_Females" = "pc01_vd_sc_f",
     "Scheduled_Tribes_Population" = "pc01_vd_st_p",
     "Scheduled_Tribes_Males" = "pc01_vd_st_m",
     "Scheduled_Tribes_Females" = "pc01_vd_st_f",
     "Educational_Facilities" = "pc01_vd_edu_fac",
     "Number_of_Primary_Schools" = "pc01_vd_p_sch",
     "Number_of_Middle_Schools" = "pc01_vd_m_sch",
     "Number_of_Secondary_Schools" = "pc01_vd_s_s_sch",
     "Number_of_Senior_Secondary_Schools" = "pc01_vd_s_s_sch",
     "Number_of_Colleges" = "pc01_vd_college",
     "Number_of_Industrial_Schools" = "pc01_vd_ind_sch",
     "Number_of_Training_Schools" = "pc01_vd_tr_sch",
     "Number_of_Adult_Literacy_Centres" = "pc01_vd_adlt_lt_ct",
     "Number_of_Other_Educational_Facilities" = "pc01_vd_oth_sch",
     "Medical_Facilities" = "pc01_vd_medi_fac",
     "Number_of_Hospitals" = "pc01_vd_hosp",
     "Number_of_Allopathic_Hospitals" = "pc01_vd_all_hosp",
     "Number_of_Ayurvedic_Hospitals" = "pc01_vd_ayu_hosp",
     "Number_of_Unani_Hospitals" = "pc01_vd_un_hosp",
     "Number_of_Homeopathic_Hospitals" = "pc01_vd_hom_hosp",
     "Number_of_Dispensaries" = "pc01_vd_disp_cntr",
     "Number_of_Allopathic_Dispensaries" = "pc01_vd_all_disp",
     "Number_of_Ayurvedic_Dispensaries" = "pc01_vd_ayu_disp",
     "Number_of_Unani_Dispensaries" = "pc01_vd_un_disp",
     "Number_of_Homeopathic_Dispensaries" = "pc01_vd_hom_disp",
     "Number_of_Maternity_and_Child_Welfare_Centres" = "pc01_vd_mcw_cntr",
     "Number_of_Maternity_Homes" = "pc01_vd_m_home",
     "Number_of_Child_Welfare_Centres" = "pc01_vd_cwc",
     "Number_of_Health_Centres" = "pc01_vd_h_cntr",
     "Number_of_Primary_Health_Centres" = "pc01_vd_ph_cntr",
     "Number_of_Primary_Health_Sub_Centres" = "pc01_vd_phs_cnt",
     "Number_of_Family_Welfare_Centres" = "pc01_vd_fwc_cntr",
     "Number_of_TB_Clinics" = "pc01_vd_tb_cln",
     "Number_of_Nursing_Homes" = "pc01_vd_n_home",
     "Number_of_Registered_Private_Medical_Practitioners" = "pc01_vd_rmp",
     "Number_of_Subsidised_Medical_Practitioners" = "pc01_vd_smp",
     "Number_of_Community_Health_Workers" = "pc01_vd_chw",
     "Number_of_Other_Medical_Facilities" = "pc01_vd_oth_cntr",
     "Tap_Water" = "pc01_vd_tap",
     "Well_Water" = "pc01_vd_well",
     "Tank_Water" = "pc01_vd_tank",
     "Tubewell_Water" = "pc01_vd_tubewell",
     "Handpump_Water" = "pc01_vd_handpump",
     "River_Water" = "pc01_vd_river",
     "Canal_Water" = "pc01_vd_canal",
     "Lake_Water" = "pc01_vd_lake",
     "Spring_Water" = "pc01_vd_spring",
     "Other_Drinking_Water_Sources" = "pc01_vd_other",
     "Post_Telegraph_Telephone_Facilities" = "pc01_vd_p_t_fac",
     "Number_of_Post_Offices" = "pc01_vd_post_off",
     "Number_of_Telegraph_Offices" = "pc01_vd_tele_off",
     "Number_of_Post_and_Telegraph_Offices" = "pc01_vd_post_tele",
     "Number_of_Telephone_Connections" = "pc01_vd_phone",
     "Communication_Facilities" = "pc01_vd_comm_fac",
     "Bus_Services" = "pc01_vd_bs_fac",
     "Railway_Services" = "pc01_vd_rs_fac",
     "Navigable_Waterways" = "pc01_vd_nw_fac",
     "Banking_Facilities" = "pc01_vd_bank_fac",
     "Number_of_Commercial_Banks" = "pc01_vd_comm_bank",
     "Number_of_Cooperative_Banks" = "pc01_vd_coop_bank",
     "Credit_Societies" = "pc01_vd_crsoc_fac",
     "Number_of_Agricultural_Credit_Societies" = "pc01_vd_ac_soc",
     "Number_of_Non_Agricultural_Credit_Societies" = "pc01_vd_nac_soc",
     "Number_of_Other_Credit_Societies" = "pc01_vd_other_soc",
     "Recreational_and_Cultural_Facilities" = "pc01_vd_rc_fac",
     "Number_of_Cinema_or_Video_Halls" = "pc01_vd_c_v_hall",
     "Number_of_Sports_Clubs" = "pc01_vd_sp_cl_fac",
     "Number_of_Stadiums_or_Auditoriums" = "pc01_vd_st_au_fac",
     "Paved_Road_Access" = "pc01_vd_app_pr",
     "Mud_Road_Access" = "pc01_vd_app_mr",
     "Footpath_Access" = "pc01_vd_app_fp",
     "Navigable_River_Access" = "pc01_vd_app_navriv",
     "Navigable_Canal_Access" = "pc01_vd_app_navcan",
     "Other_Navigable_Waterway_Access" = "pc01_vd_app_nw",
     "Distance_to_Nearest_Town" = "pc01_vd_dist_town",
     "Power_Supply" = "pc01_vd_power_supl",
     "Electricity_for_Domestic_Use" = "pc01_vd_power_dom",
     "Electricity_for_Agricultural_Use" = "pc01_vd_power_agr",
     "Electricity_for_Other_Purposes" = "pc01_vd_power_oth",
     "Electricity_for_All_Purposes" = "pc01_vd_power_all",
     "Newspaper_Magazine_Availability" = "pc01_vd_pap_mag",
     "Total_Income" = "pc01_vd_tot_inc",
     "Total_Expenditure" = "pc01_vd_tot_exp",
     "Forest_Land" = "pc01_vd_land_fores",
     "Government_Canal_Irrigation" = "pc01_vd_canal_govt",
     "Private_Canal_Irrigation" = "pc01_vd_canal_pvt",
     "Well_Without_Electricity_Irrigation" = "pc01_vd_well_wo_el",
     "Tube_Well_Without_Electricity_Irrigation" = "pc01_vd_tw_wo_el",
     "Tube_Well_With_Electricity_Irrigation" = "pc01_vd_tw_w_el",
     "Tank_Irrigation" = "pc01_vd_tank_irr",
     "River_Irrigation" = "pc01_vd_river_irr",
     "Lake_Irrigation" = "pc01_vd_lake_irr",
     "Waterfall_Irrigation" = "pc01_vd_w_fall",
     "Other_Irrigation" = "pc01_vd_oth_irr",
     "Total_Irrigated_Area" = "pc01_vd_tot_irr",
     "Unirrigated_Area" = "pc01_vd_un_irr",
     "Culturable_Waste_Area" = "pc01_vd_cult_waste",
     "Area_Not_Available_for_Cultivation" = "pc01_vd_area_na_cu", 
     "Residuals" = "Residuals",
     "(Intercept)" = "Intercept"
)

# Continuous Vars
covariate_families_cont <- list(
geography = c( "pc01_vd_river_irr", "pc01_vd_lake_irr", "pc01_vd_land_fores"), #pc01_vd_w_fall pc01_vd_area pc01_vd_tot_irr
#population_hh = c("pc01_vd_t_hh", "pc01_vd_t_m", "pc01_vd_t_p", "pc01_vd_sc_p", "pc01_vd_sc_m", "pc01_vd_st_p", "pc01_vd_st_m"), #"pc01_vd_sc_f", "pc01_vd_st_f"
water_sanitation = c("pc01_vd_tap", "pc01_vd_well", "pc01_vd_tank", "pc01_vd_tubewell", "pc01_vd_handpump", "pc01_vd_river", "pc01_vd_canal", "pc01_vd_other"), #pc01_vd_lake pc01_vd_spring
town_dist = c("pc01_vd_dist_town"))

# Dummy Vars
covariate_families_bin <- list(
education = c("pc01_vd_edu_fac"),
health = c( "pc01_vd_medi_fac", "pc01_vd_ph_cntr", "pc01_vd_phs_cnt", "pc01_vd_oth_cntr"),# "pc01_vd_tb_cln"),
communications = c("pc01_vd_p_t_fac", "pc01_vd_tele_off"),#"pc01_vd_post_tele"), pc01_vd_comm_fac
transport = c( "pc01_vd_rs_fac", "pc01_vd_nw_fac"), #pc01_vd_bs_fac
finance = c("pc01_vd_bank_fac", "pc01_vd_crsoc_fac"),
recreation = c("pc01_vd_rc_fac"),
roads = c("pc01_vd_app_pr", "pc01_vd_app_mr", "pc01_vd_app_fp"), # "pc01_vd_app_navriv"),
power = c("pc01_vd_power_supl", "pc01_vd_power_dom", "pc01_vd_power_agr", "pc01_vd_power_oth", "pc01_vd_power_all")
)

covariates <- c(
     covariate_families_cont$geography,
     covariate_families_cont$population_hh,
     covariate_families_cont$water_sanitation,
     covariate_families_cont$town_dist,
     covariate_families_bin$education,
     covariate_families_bin$health,
     covariate_families_bin$communications,
     covariate_families_bin$transport,
     covariate_families_bin$finance,
     covariate_families_bin$recreation,
     covariate_families_bin$roads,
     covariate_families_bin$power
)


# Treat Het
# m_05_10_psfe <- feols((sex_2010 =="F") ~ treat_2005 * pc01_vd_dist_town | I(paste0(dist_name_2010, samiti_name_2010)),  data = filter(raj_lgd_vd_merge, treat_2010 == 0))
# summary(m_05_10_psfe)


# T-Test ------------------------------------------------------------------
perform_t_tests <- function(vars, data) {
     results <- lapply(vars, function(var) {
          # Calculate sample sizes
          n_control <- sum(data$treat_2010 == 0, na.rm = TRUE)
          n_treatment <- sum(data$treat_2010 == 1, na.rm = TRUE)
          
          # Perform t-test
          t_test <- t.test(as.formula(paste(var, "~ treat_2010")), data = data)
          
          data.frame(
               Variable = var,
               Mean_Control = t_test$estimate[1],
               Mean_Treatment = t_test$estimate[2],
               t_Statistic = t_test$statistic,
               p_Value = t_test$p.value,
               Sample_Size_Control = n_control,
               Sample_Size_Treatment = n_treatment,
               p_Value_with_stars = cut(t_test$p.value,
                                        breaks = c(-Inf, 0.001, 0.01, 0.05, 1),
                                        labels = c("***", "**", "*", ""),
                                        right = FALSE)
          )
     })
     do.call(rbind, results)
}

results_by_family <- lapply(covariate_families_bin, perform_t_tests, data = raj_lgd_vd_merge)
results_combined <- do.call(rbind, results_by_family)

results_combined$Descriptive_Name <- sapply(results_combined$Variable, function(var) {
     name <- names(variable_names_mapping[variable_names_mapping == var])
     if (length(name) == 0) NA else name
})

results_combined$Family <- rep(names(covariate_families_bin), sapply(covariate_families_bin, length))

results_combined_for_latex <- results_combined[, c("Family", "Descriptive_Name", "Mean_Control", "Mean_Treatment", "Sample_Size_Control", "Sample_Size_Treatment", "t_Statistic", "p_Value")]
colnames(results_combined_for_latex) <- c("Family", "Variable", "Mean Control", "Mean Treatment", "Sample Size Control", "Sample Size Treatment", "t Statistic", "p Value")
latex_table <- xtable(results_combined_for_latex, 
                      caption = "T-Test Results by Covariate Family (Binary Variables)", 
                      label = "tab:t_tests_bin",
                      align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
file_path <- "tables/t_test_balance_bin.tex"
sink(file_path)
print(latex_table, 
      include.rownames = FALSE, 
      comment = FALSE, 
      caption.placement = "top", 
      floating = TRUE, 
      hline.after = c(-1, 0, nrow(results_combined_for_latex)), 
      sanitize.text.function = identity)
sink()
cat("\\footnotetext{\\textit{Note}: The p-values indicate whether there is a\\
    statistically significant difference between the treatment and control groups. \\
    Low p-values (typically <0.05) suggest a potential imbalance between groups, while high p-values indicate that the groups are more balanced with respect to the covariates. Significance levels: ***p < 0.001, **p < 0.01, *p < 0.05.}", 
    file = file_path, append = TRUE)



# Continuous --------------------------------------------------------------
results_cont <- lapply(covariate_families_cont, perform_t_tests, data = raj_lgd_vd_merge)
results_cont_combined <- do.call(rbind, results_cont)

results_cont_combined$Descriptive_Name <- sapply(results_cont_combined$Variable, function(var) {
     name <- names(variable_names_mapping[variable_names_mapping == var])
     if (length(name) == 0) NA else name
})

results_cont_combined$Family <- rep(names(covariate_families_cont), times = sapply(results_cont, nrow))

results_cont_combined_for_latex <- results_cont_combined[, c("Family", "Descriptive_Name", "Mean_Control", "Mean_Treatment", "Sample_Size_Control", "Sample_Size_Treatment", "t_Statistic", "p_Value")]
colnames(results_cont_combined_for_latex) <- c("Family", "Variable", "Mean Control", "Mean Treatment", "Sample_Size_Control", "Sample_Size_Treatment", "t_Statistic", "p_Value")
latex_table <- xtable(results_cont_combined_for_latex, 
                      caption = "T-Test Results by Covariate Family", 
                      label = "tab:t_tests",
                      align = c("l", "l", "r", "r", "r", "r", "r", "r","r"))

# Define the file path
file_path <- "tables/t_test_cont_balance.tex"
sink(file_path)
print(latex_table, 
      include.rownames = FALSE, 
      comment = FALSE, 
      caption.placement = "top", 
      floating = TRUE, 
      hline.after = c(-1, 0, nrow(results_cont_combined_for_latex)), 
      sanitize.text.function = identity)
sink()
cat("\\footnotetext{\\textit{Note}: The p-values indicate whether there is a\\
    statistically significant difference between the treatment and control groups. \\
    Low p-values (typically <0.05) suggest a potential imbalance between groups, while high p-values indicate that the groups are more balanced with respect to the covariates. Significance levels: ***p < 0.001, **p < 0.01, *p < 0.05.}", 
    file = file_path, append = TRUE)

# balance -----------------------------------------------------------------
balance_tests_cont <- lapply(covariate_families_cont, function(covars) {
     formula <- as.formula(paste("treat_2010 ~", paste(covars, collapse = " + ")))
     model <- feols(formula, gp_new_2010, data = raj_lgd_vd_merge)
     return(model)  
})
str(balance_tests_cont)


etable(balance_tests_cont, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       file = "tables/balance_tests_cont.tex",
       dict = c( "treat_2010" = "Quota in 2010",
                 "pc01_vd_area" = "Total Area",
                 "pc01_vd_river_irr" = "River Irrigation", "pc01_vd_lake_irr" = "Lake Irrigation",
                 "pc01_vd_w_fall" = "Waterfall Irrigation",
                 "pc01_vd_land_fores" = "Forest Land",
                 "pc01_vd_t_hh" = "Total Households",
                 "pc01_vd_t_m" = "Total_Males", "pc01_vd_t_p" = "Total_Population",
                 "pc01_vd_sc_p" = "SC Population", "pc01_vd_sc_m" = "SC Men", 
                 "pc01_vd_st_p" = "ST Population", "pc01_vd_st_m" = "ST Men", #"pc01_vd_st_f" "pc01_vd_sc_f"
                 "pc01_vd_tap" = "Tap Water", "pc01_vd_well" = "Well Water",
                 "pc01_vd_tank" = "Tank Water", "pc01_vd_tubewell" = "Tubewell Water",
                 "pc01_vd_handpump" = "Handpump Water", "pc01_vd_river" = "River Water",
                 "pc01_vd_canal" = "Canal Water", "pc01_vd_lake" = "Lake Water",
                 "pc01_vd_spring" = "Spring Water", "pc01_vd_other" = "Other Drinking Water Fac",
                 "pc01_vd_dist_town" = "Distance to Nearest Town",
                 "pc01_vd_tot_irr" = "Total Irrigated Area"),
       se.row=FALSE,        replace = TRUE)


# Regressions --------------------------------------------------------------

# Remove duplicates so that all are valid column names
covariates <- unique(covariates)
library(clubSandwich)
formula_2005 <- as.formula(paste("treat_2005 ~", paste(covariates, collapse = " + ")))
formula_2010 <- as.formula(paste("treat_2010 ~", paste(covariates, collapse = " + ")))
model_2005 <- lm(formula_2005, data = raj_lgd_vd_merge)
model_2010 <- lm(formula_2010, data = raj_lgd_vd_merge)

# clustered se
clustered_se_2005 <- coeftest(model_2005, vcov = vcovCL(model_2005, cluster = ~ gp_new_2010))
clustered_se_2010 <- coeftest(model_2010, vcov = vcovCL(model_2010, cluster = ~ gp_new_2010))

rownames(clustered_se_2005) <- names(variable_names_mapping)[match(rownames(clustered_se_2005), variable_names_mapping)]
rownames(clustered_se_2010) <- names(variable_names_mapping)[match(rownames(clustered_se_2010), variable_names_mapping)]

# plotting
plot_data_2005 <- data.frame(
     Term = rownames(clustered_se_2005),
     Estimate = clustered_se_2005[, "Estimate"],
     StdError = clustered_se_2005[, "Std. Error"]
)

# Plot 2005
coef_plot_2005 <- ggplot(plot_data_2005, aes(x = Estimate, y = reorder(Term, Estimate))) +
     geom_point() +
     geom_errorbarh(aes(xmin = Estimate - 1.96 * StdError, xmax = Estimate + 1.96 * StdError)) +
     theme_minimal() +
     geom_vline(xintercept = 0, color = "red", linetype = "solid") +
     
     theme(
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text = element_text(size = 10)
     ) +
     labs(title = "Coefficient Plot for Treatment 2005", x = "Estimate", y = "Covariate")

ggsave("plots/coefficient_plot_2005.png", plot = coef_plot_2005, width = 7, height = 7)

# plot 2010 
plot_data_2010 <- data.frame(
     Term = rownames(clustered_se_2010),
     Estimate = clustered_se_2010[, "Estimate"],
     StdError = clustered_se_2010[, "Std. Error"]
)

coef_plot_2010 <- ggplot(plot_data_2010, aes(x = Estimate, y = reorder(Term, Estimate))) +
     geom_point() +
     geom_errorbarh(aes(xmin = Estimate - 1.96 * StdError, xmax = Estimate + 1.96 * StdError)) +
     theme_minimal() +
     geom_vline(xintercept = 0, color = "red", linetype = "solid") +
     
     theme(
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text = element_text(size = 10)
     ) +
     labs(title = "Coefficient Plot for Treatment 2010", x = "Estimate", y = "Covariate")

ggsave("plots/coefficient_plot_2010.png", plot = coef_plot_2010, width = 7, height = 7)



# Randomization Inference -------------------------------------------------

# Run regression and extract F statistic

r_check_05 <- lm(treat_2005 ~ ., data = raj_lgd_vd_merge[, c("treat_2005", covariates)])
fstat <- summary(r_check_05)$fstatistic[1]
fstat

# Loop through randomized assignments of treatment and recalculate f-statistic
null <- raj_lgd_vd_merge
fstat_null <- vector(mode = "numeric", length = 500)

sum(null$treat_2005)
nrow(null)

for (i in seq_along(fstat_null)) {
     null$Z_sim_05 <- complete_ra(N = 5274, m = 1894)
     r_check <- lm(Z_sim_05 ~ ., data = null[, c("Z_sim_05", covariates)])
     fstat_null[[i]] <- summary(r_check)$fstatistic[1]
}

p <- sum(abs(fstat_null) >= fstat)/length(fstat_null) 
p

r_check_10 <- lm(treat_2010 ~ ., data = raj_lgd_vd_merge[, c("treat_2010", covariates)])
fstat <- summary(r_check_10)$fstatistic[1]
fstat

sum(null$treat_2010)

for (i in seq_along(fstat_null)) {
     null$Z_sim_10 <- complete_ra(N = 5274, m = 2562)
     r_check <- lm(Z_sim_10 ~ ., data = null[, c("Z_sim_10", covariates)])
     fstat_null[[i]] <- summary(r_check)$fstatistic[1]
}


# Calculate two sided p-value
p <- sum(abs(fstat_null) >= fstat)/length(fstat_null) 
p


# Check Multicollinearity
library(car)
vif_values <- vif(r_check)
vif_values

multi <- c()       # VIF > 10
mod_multi <- c()   # 5 < VIF <= 10
non_multi <- c()   # VIF <= 5

# Iterate through VIF values and classify variables
for (i in seq_along(vif_values)) {
     if (vif_values[i] > 10) {
          multi <- c(multi, names(vif_values)[i])
     } 
     else if (vif_values[i] > 5 & vif_values[i] <= 10) {
          mod_multi <- c(mod_multi, names(vif_values)[i])
     }
     else {
          non_multi <- c(non_multi, names(vif_values)[i])
     }
} 
multi
mod_multi
non_multi
