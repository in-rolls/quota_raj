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

message("=== Creating UP Panels ===")

diag_rows <- list()

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

    sex1_eng <- paste0("winner_sex_eng_", y1)
    sex2_eng <- paste0("winner_sex_eng_", y2)
    sex1_hindi <- if (y1 < 2015) paste0("cand_sex_fin_", y1) else paste0("sex_", y1)
    sex2_hindi <- if (y2 < 2015) paste0("cand_sex_fin_", y2) else paste0("sex_", y2)

    if (sex1_eng %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y1) := ifelse(!!sym(sex1_eng) == "Female", 1, 0))
    } else if (sex1_hindi %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y1) := as.integer(!!sym(sex1_hindi) == "महिला"))
    }
    if (sex2_eng %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y2) := ifelse(!!sym(sex2_eng) == "Female", 1, 0))
    } else if (sex2_hindi %in% names(data)) {
        data <- data %>%
            mutate(!!paste0("female_winner_", y2) := as.integer(!!sym(sex2_hindi) == "महिला"))
    }

    return(data)
}

# ============================================================================
# Panel 1: 2005-2010
# ============================================================================
message("\n--- Creating 2005-2010 panel ---")

up_05_10 <- read_parquet(here("data/up/up_05_10_fuzzy.parquet")) %>%
    mutate(match_key = make_match_key(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010))
message("  Input from fuzzy file: ", nrow(up_05_10))

if ("key_2010" %in% names(up_05_10)) {
    dup_2010 <- up_05_10 %>%
        count(key_2010, name = "n") %>%
        filter(!is.na(key_2010), n > 1)
    diag_rows[[length(diag_rows) + 1]] <- tibble(
        panel = "up_05_10",
        metric = "duplicate_key_2010_rows",
        value = sum(dup_2010$n)
    )
}

up_05_10 <- recode_up_panel(up_05_10, 2005, 2010)

message("  After recode: ", nrow(up_05_10))
message("  Final panel N: ", nrow(up_05_10))
write_parquet(up_05_10, here("data/up/up_05_10.parquet"))

# ============================================================================
# Panel 2: 2010-2015
# ============================================================================
message("\n--- Creating 2010-2015 panel ---")

up_10_15 <- read_parquet(here("data/up/up_10_15_fuzzy.parquet")) %>%
    mutate(match_key = make_match_key(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010))
message("  Input from fuzzy file: ", nrow(up_10_15))

if ("key_2010" %in% names(up_10_15)) {
    dup_2010_1015 <- up_10_15 %>%
        count(key_2010, name = "n") %>%
        filter(!is.na(key_2010), n > 1)
    diag_rows[[length(diag_rows) + 1]] <- tibble(
        panel = "up_10_15",
        metric = "duplicate_key_2010_rows",
        value = sum(dup_2010_1015$n)
    )
}

up_10_15 <- recode_up_panel(up_10_15, 2010, 2015)

message("  After recode: ", nrow(up_10_15))
message("  Final panel N: ", nrow(up_10_15))
write_parquet(up_10_15, here("data/up/up_10_15.parquet"))

# ============================================================================
# Panel 3: 2015-2021 - Create from raw data using exact key matching
# ============================================================================
message("\n--- Creating 2015-2021 panel ---")

up_2015 <- read_parquet(here("data/up/up_gp_sarpanch_2015_fixed_with_transliteration.parquet"))
up_2021 <- read_parquet(here("data/up/up_gp_sarpanch_2021_fixed_with_transliteration.parquet"))
message("  Input: 2015 raw =", nrow(up_2015), ", 2021 raw =", nrow(up_2021))

up_2021 <- up_2021 %>% filter(result == 'विजेता')
message("  2021 after winner filter: ", nrow(up_2021))

up_2015_dedupe <- up_2015 %>%
    mutate(match_key = make_match_key(district_name, block_name, gp_name)) %>%
    filter(!is.na(match_key)) %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    filter(!is.na(gp_reservation_status_eng) & gp_reservation_status_eng != "Unknown") %>%
    rename_with(~ paste0(., "_2015"))

n_2015_dropped <- nrow(up_2015) - nrow(up_2015_dedupe)
message("  2015 after dedup + filter: ", nrow(up_2015_dedupe), "(dropped", n_2015_dropped, ")")

up_2021_dedupe <- up_2021 %>%
    mutate(match_key = make_match_key(district_name, block_name, gp_name)) %>%
    filter(!is.na(match_key)) %>%
    group_by(match_key) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    filter(!is.na(gp_reservation_status_eng) & gp_reservation_status_eng != "Unknown") %>%
    rename_with(~ paste0(., "_2021"))

n_2021_dropped <- nrow(up_2021) - nrow(up_2021_dedupe)
message("  2021 after dedup + filter: ", nrow(up_2021_dedupe), "(dropped", n_2021_dropped, ")")

up_15_21 <- inner_join(up_2015_dedupe, up_2021_dedupe, by = c("match_key_2015" = "match_key_2021")) %>%
    rename(match_key = match_key_2015)
message("  After inner_join: ", nrow(up_15_21))

dup_2015_raw <- up_2015 %>%
    mutate(match_key = make_match_key(district_name, block_name, gp_name)) %>%
    count(match_key, name = "n") %>%
    filter(!is.na(match_key), n > 1)

dup_2021_raw <- up_2021 %>%
    mutate(match_key = make_match_key(district_name, block_name, gp_name)) %>%
    count(match_key, name = "n") %>%
    filter(!is.na(match_key), n > 1)

unmatched_2015 <- up_2015_dedupe %>%
    anti_join(up_2021_dedupe, by = c("match_key_2015" = "match_key_2021")) %>%
    nrow()

unmatched_2021 <- up_2021_dedupe %>%
    anti_join(up_2015_dedupe, by = c("match_key_2021" = "match_key_2015")) %>%
    nrow()

diag_rows[[length(diag_rows) + 1]] <- tibble(
    panel = "up_15_21",
    metric = c(
        "raw_rows_2015", "raw_rows_2021_winners",
        "duplicate_match_key_rows_2015_raw", "duplicate_match_key_rows_2021_raw",
        "rows_2015_after_dedupe_filter", "rows_2021_after_dedupe_filter",
        "dropped_rows_2015", "dropped_rows_2021",
        "inner_join_rows",
        "unmatched_2015_after_dedupe", "unmatched_2021_after_dedupe"
    ),
    value = c(
        nrow(up_2015), nrow(up_2021),
        sum(dup_2015_raw$n), sum(dup_2021_raw$n),
        nrow(up_2015_dedupe), nrow(up_2021_dedupe),
        n_2015_dropped, n_2021_dropped,
        nrow(up_15_21),
        unmatched_2015, unmatched_2021
    )
)

up_15_21 <- recode_up_panel(up_15_21, 2015, 2021)

message("  Final panel N: ", nrow(up_15_21))
write_parquet(up_15_21, here("data/up/up_15_21.parquet"))

# ============================================================================
# Panel 4: Full 4-way panel (2005-2021) from existing fuzzy data
# ============================================================================
message("\n--- Creating 4-way panel (2005-2021) ---")

up_all_fuzzy <- read_parquet(here("data/up/up_all_fuzzy.parquet")) %>%
    mutate(match_key = make_match_key(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010))
message("  Input from fuzzy file: ", nrow(up_all_fuzzy))

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
        twice_treated = as.integer((treat_2005 + treat_2010 + treat_2015) == 2),
        never_treated = as.integer(treat_2005 == 0 & treat_2010 == 0 & treat_2015 == 0),
        always_treated = as.integer(treat_2005 == 1 & treat_2010 == 1 & treat_2015 == 1),
        sometimes_treated = as.integer((treat_2005 + treat_2010 + treat_2015) > 0),
        count_treated = treat_2005 + treat_2010 + treat_2015,
        once = as.integer((treat_2005 + treat_2010 + treat_2015) == 1),
        thrice_treated = as.integer((treat_2005 + treat_2010 + treat_2015) == 3),
        inter_always_treated_05_10 = as.integer((treat_2010 == 1) & (treat_2005 == 1)),
        inter_sometimes_treated_05_10 = as.integer((treat_2010 == 1) | (treat_2005 == 1)),
        inter_never_treated_05_10 = as.integer(treat_2005 + treat_2010 == 0),
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

message("  Final panel N: ", nrow(up_05_21))
write_parquet(up_05_21, here("data/up/up_05_21.parquet"))

dir.create(here("data/crosswalks/audit"), showWarnings = FALSE, recursive = TRUE)
diag_df <- bind_rows(diag_rows)
write_csv(diag_df, here("data/crosswalks/audit/02b_up_panel_join_diagnostics.csv"))
message("Saved: data/crosswalks/audit/02b_up_panel_join_diagnostics.csv")

# ============================================================================
# Summary
# ============================================================================
message("\n=== Summary ===")
message("up_05_10.parquet: ", nrow(up_05_10), " GPs")
message("up_10_15.parquet: ", nrow(up_10_15), " GPs")
message("up_15_21.parquet: ", nrow(up_15_21), " GPs")
message("up_05_21.parquet: ", nrow(up_05_21), "GPs (4-way)")
message("=== UP Panels Complete ===")
