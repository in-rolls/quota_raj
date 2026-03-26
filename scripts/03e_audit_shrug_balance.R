# 03e_audit_shrug_balance.R
# Audit SHRUG match balance: check if unmatched GPs differ systematically
# Output: tabs/shrug_match_balance.tex

library(dplyr)
library(arrow)
library(here)
library(kableExtra)

source(here("scripts/00_config.R"))

message("=== SHRUG Match Balance Audit ===")

# =============================================================================
# Load Data
# =============================================================================

raj <- read_parquet(here("data/raj/shrug_gp_raj_05_10_block.parquet"))
up <- read_parquet(here("data/up/shrug_gp_up_05_10_block.parquet"))

raj$matched <- !is.na(raj$shrid2)
up$matched <- !is.na(up$shrid2)

message("\n--- Overall Match Rates ---")
message("Rajasthan: ", round(100 * mean(raj$matched), 1), "% matched (N =", nrow(raj), ")")
message("UP: ", round(100 * mean(up$matched), 1), "% matched (N =", nrow(up), ")")

# =============================================================================
# Balance Tests
# =============================================================================

results <- list()

# Helper function for chi-squared test
run_chisq <- function(matched, var, var_name, state) {
    tbl <- table(matched, var, useNA = "no")
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
        return(data.frame(
            state = state,
            variable = var_name,
            test = "chi-sq",
            statistic = NA_real_,
            df = NA_real_,
            p_value = NA_real_,
            n = sum(!is.na(var)),
            matched_n = sum(matched & !is.na(var)),
            unmatched_n = sum(!matched & !is.na(var)),
            stringsAsFactors = FALSE,
            row.names = NULL
        ))
    }
    test <- chisq.test(tbl)
    data.frame(
        state = state,
        variable = var_name,
        test = "chi-sq",
        statistic = round(as.numeric(test$statistic), 2),
        df = as.numeric(test$parameter),
        p_value = round(test$p.value, 4),
        n = sum(!is.na(var)),
        matched_n = sum(matched & !is.na(var)),
        unmatched_n = sum(!matched & !is.na(var)),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

# Helper function for t-test
run_ttest <- function(matched, var, var_name, state) {
    if (sum(!is.na(var[matched])) < 2 || sum(!is.na(var[!matched])) < 2) {
        return(data.frame(
            state = state,
            variable = var_name,
            test = "t-test",
            statistic = NA_real_,
            df = NA_real_,
            p_value = NA_real_,
            n = sum(!is.na(var)),
            matched_mean = NA_real_,
            unmatched_mean = NA_real_,
            stringsAsFactors = FALSE,
            row.names = NULL
        ))
    }
    test <- t.test(var ~ matched, na.action = na.omit)
    data.frame(
        state = state,
        variable = var_name,
        test = "t-test",
        statistic = round(as.numeric(test$statistic), 2),
        df = round(as.numeric(test$parameter), 1),
        p_value = round(test$p.value, 4),
        n = sum(!is.na(var)),
        matched_mean = round(mean(var[matched], na.rm = TRUE), 3),
        unmatched_mean = round(mean(var[!matched], na.rm = TRUE), 3),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

message("\n--- Rajasthan Balance Tests ---")

# Reservation status (chi-sq)
res <- run_chisq(raj$matched, raj$treat_2010, "Quota (2010)", "Rajasthan")
message("Quota 2010: chi-sq =", res$statistic, ", p =", res$p_value)
results$raj_res <- res

# Caste category (chi-sq)
res <- run_chisq(raj$matched, raj$caste_category_2005, "Caste Category (2005)", "Rajasthan")
message("Caste Category 2005: chi-sq =", res$statistic, ", p =", res$p_value)
results$raj_caste <- res

# Prior female winner (t-test)
res <- run_ttest(raj$matched, raj$female_winner_2005, "Female Winner (2005)", "Rajasthan")
message("Female Winner 2005: t =", res$statistic, ", p =", res$p_value)
message("  Matched mean: ", res$matched_mean, ", Unmatched mean:", res$unmatched_mean)
results$raj_winner <- res

# District clustering
raj_dist <- raj %>%
    group_by(district_std_2010) %>%
    summarize(
        match_rate = mean(matched),
        n = n(),
        .groups = "drop"
    ) %>%
    arrange(match_rate)

message("\nDistricts with lowest match rates:")
print(head(raj_dist, 5))

message("\n--- UP Balance Tests ---")

# Reservation status (chi-sq)
res <- run_chisq(up$matched, up$treat_2010, "Quota (2010)", "UP")
message("Quota 2010: chi-sq =", res$statistic, ", p =", res$p_value)
results$up_res <- res

# OBC reservation (chi-sq)
res <- run_chisq(up$matched, up$obc_2005, "OBC Reserved (2005)", "UP")
message("OBC 2005: chi-sq =", res$statistic, ", p =", res$p_value)
results$up_obc <- res

# SC/Dalit reservation (chi-sq)
res <- run_chisq(up$matched, up$dalit_2005, "SC/Dalit Reserved (2005)", "UP")
message("Dalit 2005: chi-sq =", res$statistic, ", p =", res$p_value)
results$up_dalit <- res

# Prior female winner (t-test) - using cand_sex_fin_2005
up$female_winner_2005 <- ifelse(up$cand_sex_fin_2005 == "female", 1, 0)
res <- run_ttest(up$matched, up$female_winner_2005, "Female Winner (2005)", "UP")
message("Female Winner 2005: t =", res$statistic, ", p =", res$p_value)
message("  Matched mean: ", res$matched_mean, ", Unmatched mean:", res$unmatched_mean)
results$up_winner <- res

# District clustering
up_dist <- up %>%
    group_by(district_name_eng_2010) %>%
    summarize(
        match_rate = mean(matched),
        n = n(),
        .groups = "drop"
    ) %>%
    arrange(match_rate)

message("\nDistricts with lowest match rates:")
print(head(up_dist, 10))

message("\nDistricts with 0% match rate:")
zero_match <- up_dist %>% filter(match_rate == 0)
print(zero_match)
message("Number of districts with 0% SHRUG coverage: ", nrow(zero_match))

# =============================================================================
# Create Summary Table
# =============================================================================

message("\n--- Generating LaTeX Table ---")

# Combine all results
all_tests <- bind_rows(
    results$raj_res %>% select(state, variable, statistic, df, p_value),
    results$raj_caste %>% select(state, variable, statistic, df, p_value),
    results$raj_winner %>% select(state, variable, statistic, df, p_value),
    results$up_res %>% select(state, variable, statistic, df, p_value),
    results$up_obc %>% select(state, variable, statistic, df, p_value),
    results$up_dalit %>% select(state, variable, statistic, df, p_value),
    results$up_winner %>% select(state, variable, statistic, df, p_value)
)

# Format p-values and statistics
all_tests <- all_tests %>%
    mutate(
        p_value_fmt = case_when(
            is.na(p_value) ~ "-",
            p_value < 0.001 ~ "$<$0.001",
            p_value < 0.01 ~ sprintf("%.3f", p_value),
            TRUE ~ sprintf("%.2f", p_value)
        ),
        sig = case_when(
            is.na(p_value) ~ "",
            p_value < 0.01 ~ "***",
            p_value < 0.05 ~ "**",
            p_value < 0.10 ~ "*",
            TRUE ~ ""
        ),
        Statistic = ifelse(is.na(statistic), "-", as.character(statistic)),
        P_value = ifelse(is.na(p_value), "-", paste0(p_value_fmt, sig))
    )

tbl_df <- all_tests %>%
    select(Variable = variable, Statistic, `P-value` = P_value)

kbl(tbl_df, format = "latex", booktabs = TRUE, escape = FALSE,
    align = c("l", "r", "r"),
    caption = "Balance Test: Matched vs. Unmatched GPs",
    row.names = FALSE) %>%
    pack_rows("Rajasthan", 1, 3, italic = TRUE) %>%
    pack_rows("Uttar Pradesh", 4, 7, italic = TRUE) %>%
    footnote(
        general = paste0(
            "Chi-squared tests for categorical variables, t-tests for continuous. ",
            "Matched = GP linked to SHRUG. ",
            "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1."
        ),
        escape = FALSE
    ) %>%
    save_kable(here("tabs/shrug_match_balance.tex"))

message("Saved: tabs/shrug_match_balance.tex")

# =============================================================================
# Summary Statistics
# =============================================================================

message("\n--- Summary ---")

message("\nRajasthan:")
message("  Total GPs: ", nrow(raj))
message("  Matched: ", sum(raj$matched), "(", round(100 * mean(raj$matched), 1), "%)")
message("  Unmatched: ", sum(!raj$matched), "(", round(100 * (1 - mean(raj$matched)), 1), "%)")

message("\nUP:")
message("  Total GPs: ", nrow(up))
message("  Matched: ", sum(up$matched), "(", round(100 * mean(up$matched), 1), "%)")
message("  Unmatched: ", sum(!up$matched), "(", round(100 * (1 - mean(up$matched)), 1), "%)")

message("\n=== Key Finding ===")
message("For UP, SHRUG coverage is geographic (", nrow(zero_match), " districts with 0% coverage).")
message("Check if female reservation status differs between matched/unmatched.")
message("If chi-sq p > 0.05 for female reservation, unmatched GPs are unlikely to bias treatment effects.")

message("\n=== Done ===")
