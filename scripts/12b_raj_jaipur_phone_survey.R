# 12b_raj_jaipur_phone_survey.R
# Generate Table G.2: Urban Phone Survey Response Distribution (Jaipur)
# Output: tabs/jaipur_urban_phone_survey_quota.tex (Panel A)
#         tabs/jaipur_urban_phone_survey_open.tex (Panel B)

library(dplyr)
library(here)
library(readxl)
library(kableExtra)

source(here("scripts/00_config.R"))

cat("=== Jaipur Urban Phone Survey Table ===\n")

# Load and filter data
survey <- read_excel(
    here("data/raj/source/phone_survey_response/jaipur_audit.xlsx"),
    sheet = "Sheet1"
) %>%
    filter(!is.na(attempt_to_reach))

# Helper: format N (%)
fmt_n_pct <- function(n, total) {
    paste0(n, " (", round(100 * n / total, 1), ")")
}

# Function to generate LaTeX table for quota seats (Panel A)
generate_quota_table <- function(data, output_file) {
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

    cat("  Total:", total, "\n")
    cat("  Answered:", answered, "(", round(100 * answered / total, 1), "%)\n")
    cat("  Member:", member, "Non-member:", non_member, "Unknown:", unknown, "\n")
    cat("  Non-member male:", nm_male, "female:", nm_female, "\n")

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
        kable_styling(font_size = 8) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE) %>%
        pack_rows(paste0("Among Answered (N = ", answered, ")"), 3, 7, italic = TRUE) %>%
        save_kable(output_file)

    cat("  Saved:", output_file, "\n")
}

# Function to generate LaTeX table for open seats (Panel B)
generate_open_table <- function(data, output_file) {
    total <- nrow(data)
    answered <- sum(data$phone_responded == 1, na.rm = TRUE)
    no_answer <- total - answered

    ans_data <- data %>% filter(phone_responded == 1)
    member <- sum(ans_data$respondent == "member", na.rm = TRUE)
    unknown <- sum(ans_data$respondent == "unknown", na.rm = TRUE)

    # Spouse/relative: non-member with relationship spouse, child, or family_member
    spouse_rel <- sum(
        ans_data$respondent == "non_member" &
            ans_data$relationship %in% c("spouse", "child", "family_member"),
        na.rm = TRUE
    )

    cat("  Total:", total, "\n")
    cat("  Answered:", answered, "(", round(100 * answered / total, 1), "%)\n")
    cat("  Member:", member, "Spouse/Relative:", spouse_rel, "Unknown:", unknown, "\n")

    df <- data.frame(
        Category = c(
            "Answered", "No Answer",
            "Elected Representative", "Spouse/Relative", "Unknown"
        ),
        Value = c(
            fmt_n_pct(answered, total), fmt_n_pct(no_answer, total),
            fmt_n_pct(member, answered), fmt_n_pct(spouse_rel, answered),
            fmt_n_pct(unknown, answered)
        )
    )

    kbl(df, format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Response Category", "N (\\%)"),
        align = c("l", "r")) %>%
        kable_styling(font_size = 8) %>%
        pack_rows(paste0("Initial Contact (N = ", total, ")"), 1, 2, italic = TRUE) %>%
        pack_rows(paste0("Among Answered (N = ", answered, ")"), 3, 5, italic = TRUE) %>%
        save_kable(output_file)

    cat("  Saved:", output_file, "\n")
}

# Generate tables
quota <- survey %>% filter(treat_status == 1)
open <- survey %>% filter(treat_status == 0)

cat("\n--- Panel A: Gender-Quota Seats ---\n")
generate_quota_table(quota, here("tabs/jaipur_urban_phone_survey_quota.tex"))

cat("\n--- Panel B: Open Seats ---\n")
generate_open_table(open, here("tabs/jaipur_urban_phone_survey_open.tex"))

cat("\n=== Done ===\n")
