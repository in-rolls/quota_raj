# 02c_up_recode.R
# Consolidated UP panel creation script
# Input: up_*_fuzzy.parquet files (JW-matched, threshold < 0.1)
# Output: up_05_10.parquet, up_10_15.parquet, up_15_21.parquet, up_05_21.parquet

library(readr)
library(arrow)
library(dplyr)
library(stringi)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Creating UP Panels ===\n")

# ============================================================================
# Helper function to recode UP panels
# ============================================================================
recode_up_panel <- function(data, year1, year2) {
    y1 <- as.character(year1)
    y2 <- as.character(year2)

    res1 <- paste0("gp_res_status_fin_eng_", y1)
    res2 <- paste0("gp_res_status_fin_eng_", y2)
    dist2 <- paste0("district_name_eng_", y2)
    block2 <- paste0("block_name_eng_", y2)

    if (year2 >= 2015) {
        res2 <- paste0("gp_reservation_status_eng_", y2)
    }
    if (year1 >= 2015) {
        res1 <- paste0("gp_reservation_status_eng_", y1)
    }

    data <- data %>%
        mutate(
            !!paste0("treat_", y1) := ifelse(grepl("Female", !!sym(res1), ignore.case = TRUE), 1, 0),
            !!paste0("treat_", y2) := ifelse(grepl("Female", !!sym(res2), ignore.case = TRUE), 1, 0),
            case = paste0(!!sym(paste0("treat_", y1)), !!sym(paste0("treat_", y2))),
            !!paste0("obc_", y1) := ifelse(grepl("Other Backward Class", !!sym(res1), ignore.case = TRUE), 1, 0),
            !!paste0("obc_", y2) := ifelse(grepl("Other Backward Class", !!sym(res2), ignore.case = TRUE), 1, 0),
            !!paste0("dalit_", y1) := ifelse(grepl("Scheduled Caste|Scheduled Tribe", !!sym(res1), ignore.case = TRUE), 1, 0),
            !!paste0("dalit_", y2) := ifelse(grepl("Scheduled Caste|Scheduled Tribe", !!sym(res2), ignore.case = TRUE), 1, 0),
            !!paste0("dist_block_", y2) := paste0(!!sym(dist2), "_", !!sym(block2))
        )

    sex1 <- paste0("winner_sex_eng_", y1)
    sex2 <- paste0("winner_sex_eng_", y2)

    if (sex1 %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y1) := ifelse(!!sym(sex1) == "Female", 1, 0))
    }
    if (sex2 %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y2) := ifelse(!!sym(sex2) == "Female", 1, 0))
    }

    return(data)
}

# ============================================================================
# Panel 1: 2005-2010
# ============================================================================
cat("\n--- Creating 2005-2010 panel ---\n")

up_05_10 <- read_parquet(here("data/up/up_05_10_fuzzy.parquet"))
up_05_10 <- recode_up_panel(up_05_10, 2005, 2010)

cat("UP 05-10 panel N:", nrow(up_05_10), "\n")
write_parquet(up_05_10, here("data/up/up_05_10.parquet"))

# ============================================================================
# Panel 2: 2010-2015
# ============================================================================
cat("\n--- Creating 2010-2015 panel ---\n")

up_10_15 <- read_parquet(here("data/up/up_10_15_fuzzy.parquet"))
up_10_15 <- recode_up_panel(up_10_15, 2010, 2015)

cat("UP 10-15 panel N:", nrow(up_10_15), "\n")
write_parquet(up_10_15, here("data/up/up_10_15.parquet"))

# ============================================================================
# Panel 3: 2015-2021 - Create from raw data using exact key matching
# ============================================================================
cat("\n--- Creating 2015-2021 panel ---\n")

up_2015 <- read_parquet(here("data/up/up_gp_sarpanch_2015_fixed_with_transliteration.parquet"))
up_2021 <- read_parquet(here("data/up/up_gp_sarpanch_2021_fixed_with_transliteration.parquet"))

up_2021 <- up_2021 %>% filter(result == 'विजेता')

up_2015_dedupe <- up_2015 %>%
    mutate(key = normalize_string(paste(district_name, block_name, gp_name))) %>%
    filter(!duplicated(key) & !is.na(key)) %>%
    filter(!is.na(gp_reservation_status_eng) & gp_reservation_status_eng != "Unknown") %>%
    rename_with(~ paste0(., "_2015"))

up_2021_dedupe <- up_2021 %>%
    mutate(key = normalize_string(paste(district_name, block_name, gp_name))) %>%
    filter(!duplicated(key) & !is.na(key)) %>%
    filter(!is.na(gp_reservation_status_eng) & gp_reservation_status_eng != "Unknown") %>%
    rename_with(~ paste0(., "_2021"))

up_15_21 <- inner_join(up_2015_dedupe, up_2021_dedupe, by = c("key_2015" = "key_2021"))
up_15_21 <- recode_up_panel(up_15_21, 2015, 2021)

cat("UP 15-21 panel N:", nrow(up_15_21), "\n")
write_parquet(up_15_21, here("data/up/up_15_21.parquet"))

# ============================================================================
# Panel 4: Full 4-way panel (2005-2021) from existing fuzzy data
# ============================================================================
cat("\n--- Creating 4-way panel (2005-2021) ---\n")

up_all_fuzzy <- read_parquet(here("data/up/up_all_fuzzy.parquet"))

up_05_21 <- up_all_fuzzy %>%
    mutate(
        treat_2005 = ifelse(grepl("Female", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
        treat_2010 = ifelse(grepl("Female", gp_res_status_fin_eng_2010, ignore.case = TRUE), 1, 0),
        treat_2015 = ifelse(grepl("Female", gp_reservation_status_eng_2015, ignore.case = TRUE), 1, 0),
        treat_2021 = ifelse(grepl("Female", gp_reservation_status_eng_2021, ignore.case = TRUE), 1, 0),
        obc_2005 = ifelse(gp_res_status_fin_eng_2005 %in% c("Other Backward Class - Female"), 1, 0),
        obc_2010 = ifelse(gp_res_status_fin_eng_2010 %in% c("Other Backward Class - Female"), 1, 0),
        obc_2015 = ifelse(gp_reservation_status_eng_2015 %in% c("Other Backward Class Female"), 1, 0),
        obc_2021 = ifelse(gp_reservation_status_eng_2021 %in% c("Other Backward Class Female"), 1, 0),
        dalit_2005 = ifelse(gp_res_status_fin_eng_2005 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
        dalit_2010 = ifelse(gp_res_status_fin_eng_2010 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
        dalit_2015 = ifelse(gp_reservation_status_eng_2015 %in% c("Scheduled Caste - Female", "Scheduled Tribe - Female"), 1, 0),
        dalit_2021 = ifelse(gp_reservation_status_eng_2021 %in% c("Scheduled Caste Female", "Scheduled Tribe Female"), 1, 0),
        twice_treated = ifelse((treat_2005 + treat_2010) == 2, 1, 0),
        never_treated = as.integer(treat_2005 == 0 & treat_2010 == 0 & treat_2015 == 0),
        always_treated = as.integer(treat_2005 == 1 & treat_2010 == 1 & treat_2015 == 1),
        sometimes_treated = ifelse((treat_2005 + treat_2010) > 0, 1, 0),
        count_treated = treat_2005 + treat_2010 + treat_2015,
        once = ifelse((treat_2005 + treat_2010) == 1, 1, 0),
        inter_always_treated = ifelse((treat_2010 == 1) & (treat_2005 == 1), 1, 0),
        inter_sometimes_treated = ifelse((treat_2010 == 1) | (treat_2005 == 1), 1, 0),
        inter_never_treated = ifelse(treat_2005 + treat_2010 == 0, 1, 0),
        treat_all = paste(treat_2005, treat_2010, sep = "_"),
        all_sc_2005 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
        all_sc_2010 = ifelse(grepl("Scheduled", gp_res_status_fin_eng_2010, ignore.case = TRUE), 1, 0),
        all_sc_2015 = ifelse(grepl("Scheduled", gp_reservation_status_eng_2015, ignore.case = TRUE), 1, 0),
        all_sc_2021 = ifelse(grepl("Scheduled", gp_reservation_status_eng_2021, ignore.case = TRUE), 1, 0),
        all_obc_2005 = ifelse(grepl("Backward", gp_res_status_fin_eng_2005, ignore.case = TRUE), 1, 0),
        all_obc_2010 = ifelse(grepl("Backward", gp_res_status_fin_eng_2010, ignore.case = TRUE), 1, 0),
        all_obc_2015 = ifelse(grepl("Backward", gp_reservation_status_eng_2015, ignore.case = TRUE), 1, 0),
        all_obc_2021 = ifelse(grepl("Backward", gp_reservation_status_eng_2021, ignore.case = TRUE), 1, 0),
        fe_key_2010 = paste(district_name_eng_2010, block_name_eng_2010),
        cluster_key_2010 = paste(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010),
        fe_key_2015 = paste(district_name_eng_2015, block_name_eng_2015),
        cluster_key_2015 = paste(district_name_eng_2015, block_name_eng_2015, gp_name_eng_2015),
        fe_key_2021 = paste(district_name_eng_2021, block_name_eng_2021),
        cluster_key_2021 = paste(district_name_eng_2021, block_name_eng_2021, gp_name_eng_2021),
        dist_block_2021 = paste0(district_name_eng_2021, "_", block_name_eng_2021),
        dist_block_2015 = paste0(district_name_eng_2015, "_", block_name_eng_2015),
        dist_block_2010 = paste0(district_name_eng_2010, "_", block_name_eng_2010),
        female_winner_2005 = as.integer(cand_sex_fin_2005 == "महिला"),
        female_winner_2010 = as.integer(cand_sex_fin_2010 == "महिला"),
        female_winner_2015 = as.integer(sex_2015 == "महिला"),
        female_winner_2021 = as.integer(sex_2021 == "महिला")
    )

cat("UP 05-21 (4-way) panel N:", nrow(up_05_21), "\n")
write_parquet(up_05_21, here("data/up/up_05_21.parquet"))

# ============================================================================
# Summary
# ============================================================================
cat("\n=== Summary ===\n")
cat("up_05_10.parquet:", nrow(up_05_10), "GPs\n")
cat("up_10_15.parquet:", nrow(up_10_15), "GPs\n")
cat("up_15_21.parquet:", nrow(up_15_21), "GPs\n")
cat("up_05_21.parquet:", nrow(up_05_21), "GPs (4-way)\n")
cat("=== UP Panels Complete ===\n")
