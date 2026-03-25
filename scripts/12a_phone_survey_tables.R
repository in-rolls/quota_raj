# 12a_phone_survey_tables.R
# Generate Phone Survey Response Distribution Tables
# Output: tabs/phone_survey.tex (Panel A: quota seats)
#         tabs/phone_survey_openseats.tex (Panel B: open seats)

library(dplyr)
library(here)
library(readxl)
library(kableExtra)

source(here("scripts/00_config.R"))

cat("=== Phone Survey Tables ===\n")

fmt_n_pct <- function(n, total) {
    paste0(n, " (", round(100 * n / total, 1), ")")
}


# =============================================================================
# Panel A: Quota Seats
# =============================================================================

generate_quota_table <- function(output_file) {
    cat("\n--- Panel A: Gender-Quota Seats ---\n")

    d <- read_excel(
        here("data/raj/source/phone_survey_response/sampled_nos_full_analysis.xlsx"),
        sheet = "Sheet1"
    )

    total <- nrow(d)
    answered <- d %>% filter(phone_answered == "yes")
    n_ans <- nrow(answered)
    no_answer <- total - n_ans

    cat("  Total:", total, "\n")
    cat("  Answered:", n_ans, "(", round(100 * n_ans / total, 1), "%)\n")

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
               sum(is.na(answered$phone_answered_by))

    transferred <- sum(non_member$did_transfer == "yes", na.rm = TRUE)
    refused <- sum(non_member$did_transfer == "no", na.rm = TRUE)

    cat("  Member:", member, "Non-member:", n_non_member, "Unknown:", unknown, "\n")
    cat("  Male relatives:", male_rel, "(spouse:", spouse, "son:", son, "other:", other_male, ")\n")
    cat("  Female relatives:", female_rel, "\n")
    cat("  Transfer - yes:", transferred, "no:", refused, "\n")

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
            fmt_n_pct(transferred, n_ans), fmt_n_pct(refused, n_ans)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("\\textbf{Response Category}", "\\textbf{N (\\%)}"),
        align = c("l", "r")) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        pack_rows(paste0("Among Answered (N = ", n_ans, ")"), 3, 9, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        pack_rows("Call Transfer Requested", 10, 11, italic = TRUE, bold = FALSE, indent = FALSE) %>%
        save_kable(output_file)

    cat("  Saved:", output_file, "\n")
}

# =============================================================================
# Panel B: Open Seats
# =============================================================================

generate_open_table <- function(output_file) {
    cat("\n--- Panel B: Open Seats ---\n")

    d <- read_excel(
        here("data/raj/source/phone_survey_response/sampled_mobile_nos_open_seats.xlsx"),
        sheet = "Sheet1"
    )

    total <- nrow(d)
    answered <- d %>% filter(phone_answered == "yes")
    n_ans <- nrow(answered)
    no_answer <- total - n_ans

    cat("  Total:", total, "\n")
    cat("  Answered:", n_ans, "(", round(100 * n_ans / total, 1), "%)\n")

    member <- sum(answered$phone_answered_by == "member", na.rm = TRUE)
    non_member <- answered %>% filter(phone_answered_by %in% c("non_member", "non-member"))
    n_non_member <- nrow(non_member)

    male_rel <- sum(non_member$relative_gender == "male", na.rm = TRUE)
    female_rel <- sum(non_member$relative_gender == "female", na.rm = TRUE)

    unknown <- sum(answered$phone_answered_by == "unknown", na.rm = TRUE) +
               sum(is.na(answered$phone_answered_by))

    cat("  Member:", member, "Non-member:", n_non_member, "Unknown:", unknown, "\n")
    cat("  Male relatives:", male_rel, "Female relatives:", female_rel, "\n")

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

    cat("  Saved:", output_file, "\n")
}

# =============================================================================
# Generate tables
# =============================================================================

generate_quota_table(here("tabs/phone_survey.tex"))
generate_open_table(here("tabs/phone_survey_openseats.tex"))

cat("\n=== Done ===\n")
