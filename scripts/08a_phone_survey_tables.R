# 07c_phone_survey_tables.R
# Generate Phone Survey Response Distribution Tables
#
# Section 1: Rural Survey Tables
#   - tabs/phone_survey.tex (Panel A: quota seats)
#   - tabs/phone_survey_openseats.tex (Panel B: open seats)
#
# Section 2: Jaipur Urban Survey Tables
#   - tabs/jaipur_urban_phone_survey_quota.tex (Panel A: quota seats)
#   - tabs/jaipur_urban_phone_survey_open.tex (Panel B: open seats)

library(dplyr)
library(here)
library(readxl)
library(kableExtra)

source(here("scripts/00_config.R"))

message("=== Phone Survey Tables ===")

# =============================================================================
# Shared Helper
# =============================================================================

fmt_n_pct <- function(n, total) {
    paste0(n, " (", round(100 * n / total, 1), ")")
}

# =============================================================================
# Section 1: Rural Survey Tables
# =============================================================================

message("\n### Section 1: Rural Survey Tables ###")

generate_rural_quota_table <- function(output_file) {
    message("\n--- Rural Panel A: Gender-Quota Seats ---")

    d <- read_excel(
        here("data/raj/source/phone_survey_response/sampled_nos_full_analysis.xlsx"),
        sheet = "Sheet1"
    )

    total <- nrow(d)
    answered <- d %>% filter(phone_answered == "yes")
    n_ans <- nrow(answered)
    no_answer <- total - n_ans

    message("  Total: ", total)
    message("  Answered: ", n_ans, "(", round(100 * n_ans / total, 1), "%)")

    member <- sum(answered$phone_answered_by == "member", na.rm = TRUE)
    non_member <- answered %>% filter(phone_answered_by == "non_member")
    n_non_member <- nrow(non_member)

    male_rel <- sum(non_member$respondent_gender == "male", na.rm = TRUE)
    spouse <- sum(non_member$relationship == "spouse", na.rm = TRUE)
    son <- sum(non_member$relationship == "child" &
               non_member$respondent_gender == "male", na.rm = TRUE)
    other_male <- male_rel - spouse - son

    female_rel <- sum(non_member$respondent_gender == "female", na.rm = TRUE)

    unknown <- sum(answered$phone_answered_by == "unknown", na.rm = TRUE) +
               sum(is.na(answered$phone_answered_by)) +
               sum(is.na(non_member$respondent_gender))

    transferred <- sum(non_member$did_transfer == "yes", na.rm = TRUE)
    refused <- sum(non_member$did_transfer == "no", na.rm = TRUE)
    n_transfer_relevant <- n_non_member

    message("  Member: ", member, " Non-member: ", n_non_member, " Unknown: ", unknown)
    message("  Male relatives: ", male_rel, " (spouse: ", spouse, " son: ", son, " other: ", other_male, ")")
    message("  Female relatives: ", female_rel)
    message("  Transfer - yes: ", transferred, " no: ", refused)

    df <- data.frame(
        Category = c(
            "Answered", "No Answer",
            "Elected Representative", "Male Relative",
            "\\hspace{1em}Spouse", "\\hspace{1em}Son", "\\hspace{1em}Other Male",
            "Female Relative", "Unknown/Blank",
            "Transferred to Member", "Refused to Transfer"
        ),
        Value = c(
            fmt_n_pct(n_ans, total), fmt_n_pct(no_answer, total),
            fmt_n_pct(member, n_ans), fmt_n_pct(male_rel, n_ans),
            fmt_n_pct(spouse, n_ans), fmt_n_pct(son, n_ans), fmt_n_pct(other_male, n_ans),
            fmt_n_pct(female_rel, n_ans), fmt_n_pct(unknown, n_ans),
            fmt_n_pct(transferred, n_transfer_relevant), fmt_n_pct(refused, n_transfer_relevant)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("\\textbf{Response Category}", "\\textbf{N (\\%)}"),
        align = c("l", "r")) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        pack_rows(paste0("Among Answered (N = ", n_ans, ")"), 3, 9, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        pack_rows(paste0("Call Transfer Requested (N = ", n_transfer_relevant, ")"), 10, 11, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        save_kable(output_file)

    message("  Saved: ", output_file)
}

generate_rural_open_table <- function(output_file) {
    message("\n--- Rural Panel B: Open Seats ---")

    d <- read_excel(
        here("data/raj/source/phone_survey_response/sampled_mobile_nos_open_seats.xlsx"),
        sheet = "Sheet1"
    )

    total <- nrow(d)
    answered <- d %>% filter(phone_answered == "yes")
    n_ans <- nrow(answered)
    no_answer <- total - n_ans

    message("  Total: ", total)
    message("  Answered: ", n_ans, "(", round(100 * n_ans / total, 1), "%)")

    member <- sum(answered$phone_answered_by == "member", na.rm = TRUE)
    non_member <- answered %>% filter(phone_answered_by %in% c("non_member", "non-member"))
    n_non_member <- nrow(non_member)

    male_rel <- sum(non_member$relative_gender == "male", na.rm = TRUE)
    female_rel <- sum(non_member$relative_gender == "female", na.rm = TRUE)

    unknown <- sum(answered$phone_answered_by == "unknown", na.rm = TRUE) +
               sum(is.na(answered$phone_answered_by))

    message("  Member: ", member, " Non-member: ", n_non_member, " Unknown: ", unknown)
    message("  Male relatives: ", male_rel, " Female relatives: ", female_rel)

    df <- data.frame(
        Category = c(
            "Answered", "No Answer",
            "Elected Representative", "Male Relative", "Female Relative", "Unknown/Blank"
        ),
        Value = c(
            fmt_n_pct(n_ans, total), fmt_n_pct(no_answer, total),
            fmt_n_pct(member, n_ans), fmt_n_pct(male_rel, n_ans),
            fmt_n_pct(female_rel, n_ans), fmt_n_pct(unknown, n_ans)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("\\textbf{Response Category}", "\\textbf{N (\\%)}"),
        align = c("l", "r")) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        pack_rows(paste0("Among Answered (N = ", n_ans, ")"), 3, 6, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        save_kable(output_file)

    message("  Saved: ", output_file)
}

generate_rural_quota_table(here("tabs/phone_survey.tex"))
generate_rural_open_table(here("tabs/phone_survey_openseats.tex"))

# =============================================================================
# Section 2: Jaipur Urban Survey Tables
# =============================================================================

message("\n### Section 2: Jaipur Urban Survey Tables ###")

survey <- read_excel(
    here("data/raj/source/phone_survey_response/jaipur_audit.xlsx"),
    sheet = "Sheet1"
) %>%
    filter(!is.na(attempt_to_reach))

generate_urban_quota_table <- function(data, output_file) {
    message("\n--- Urban Panel A: Gender-Quota Seats ---")

    total <- nrow(data)
    answered <- sum(data$phone_responded == 1, na.rm = TRUE)
    no_answer <- total - answered

    ans_data <- data %>% filter(phone_responded == 1)
    member <- sum(ans_data$respondent == "member", na.rm = TRUE)
    non_member <- sum(ans_data$respondent == "non_member", na.rm = TRUE)
    unknown <- sum(ans_data$respondent == "unknown", na.rm = TRUE)

    nm_data <- ans_data %>% filter(respondent == "non_member")
    nm_male <- sum(nm_data$respondent_gender == "male", na.rm = TRUE)
    nm_female <- sum(nm_data$respondent_gender == "female", na.rm = TRUE)

    message("  Total: ", total)
    message("  Answered: ", answered, " (", round(100 * answered / total, 1), "%)")
    message("  Member: ", member, " Non-member: ", non_member, " Unknown: ", unknown)
    message("  Non-member male: ", nm_male, " female: ", nm_female)

    df <- data.frame(
        Category = c(
            "Answered", "No Answer",
            "Elected Representative", "Non-Member",
            "\\hspace{1em}Male", "\\hspace{1em}Female",
            "Unknown"
        ),
        Value = c(
            fmt_n_pct(answered, total), fmt_n_pct(no_answer, total),
            fmt_n_pct(member, answered), fmt_n_pct(non_member, answered),
            fmt_n_pct(nm_male, non_member), fmt_n_pct(nm_female, non_member),
            fmt_n_pct(unknown, answered)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Response Category", "N (\\%)"),
        align = c("l", "r")) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE) %>%
        pack_rows(paste0("Among Answered (N = ", answered, ")"), 3, 7, italic = TRUE) %>%
        save_kable(output_file)

    message("  Saved: ", output_file)
}

generate_urban_open_table <- function(data, output_file) {
    message("\n--- Urban Panel B: Open Seats ---")

    total <- nrow(data)
    answered <- sum(data$phone_responded == 1, na.rm = TRUE)
    no_answer <- total - answered

    ans_data <- data %>% filter(phone_responded == 1)
    member <- sum(ans_data$respondent == "member", na.rm = TRUE)
    unknown <- sum(ans_data$respondent == "unknown", na.rm = TRUE)

    spouse_rel <- sum(
        ans_data$respondent == "non_member" &
            ans_data$relationship %in% c("spouse", "child", "family_member"),
        na.rm = TRUE
    )

    other_nonmember <- sum(ans_data$respondent == "non_member", na.rm = TRUE) - spouse_rel
    na_respondent <- sum(is.na(ans_data$respondent))

    message("  Total: ", total)
    message("  Answered: ", answered, " (", round(100 * answered / total, 1), "%)")
    message("  Member: ", member, " Spouse/Relative: ", spouse_rel,
        " Other non-member: ", other_nonmember, " Unknown: ", unknown,
        " NA respondent: ", na_respondent)

    df <- data.frame(
        Category = c(
            "Answered", "No Answer",
            "Elected Representative", "Spouse/Relative", "Other Non-Member", "Unknown"
        ),
        Value = c(
            fmt_n_pct(answered, total), fmt_n_pct(no_answer, total),
            fmt_n_pct(member, answered), fmt_n_pct(spouse_rel, answered),
            fmt_n_pct(other_nonmember + na_respondent, answered),
            fmt_n_pct(unknown, answered)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Response Category", "N (\\%)"),
        align = c("l", "r")) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE) %>%
        pack_rows(paste0("Among Answered (N = ", answered, ")"), 3, 6, italic = TRUE) %>%
        save_kable(output_file)

    message("  Saved: ", output_file)
}

quota <- survey %>% filter(treat_status == 1)
open <- survey %>% filter(treat_status == 0)

generate_urban_quota_table(quota, here("tabs/jaipur_urban_phone_survey_quota.tex"))
generate_urban_open_table(open, here("tabs/jaipur_urban_phone_survey_open.tex"))

message("\n=== Done ===")
