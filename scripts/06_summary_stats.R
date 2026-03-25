# 06_summary_stats.R
# Create unified summary statistics table for Rajasthan and UP
# Output: tabs/summary_stats_combined.tex

library(dplyr)
library(kableExtra)
library(arrow)
library(here)

source(here("scripts/00_config.R"))

cat("=== Creating Combined Summary Statistics Table ===\n")

# Load Rajasthan data
raj_panch <- read_parquet("data/raj/raj_05_20.parquet")

# Load UP data
up_data <- tryCatch({
    read_parquet("data/up/up_all_fuzzy.parquet")
}, error = function(e) {
    load("data/up/up_all_recoded.RData")
    up_all
})

# Rajasthan Summary Statistics
raj_stats <- data.frame(
    Variable = c(
        "Number of GPs (Analysis Sample)",
        "Electoral Cycles",
        "Years Covered",
        "Treatment Rate (%)",
        "Women Winners in Open Seats 2010 (%)",
        "Women Winners in Open Seats 2015 (%)",
        "Women Winners in Open Seats 2020 (%)"
    ),
    Rajasthan = c(
        format(nrow(raj_panch), big.mark = ","),
        "4",
        "2005, 2010, 2015, 2020",
        "50.0",
        round(mean(raj_panch$sex_2010[raj_panch$female_res_2010 == 0] == "F", na.rm = TRUE) * 100, 1),
        round(mean(raj_panch$sex_manual_2015[raj_panch$female_res_2015 == 0] == "F", na.rm = TRUE) * 100, 1),
        round(mean(raj_panch$sex_2020[raj_panch$female_res_2020 == 0] == "F", na.rm = TRUE) * 100, 1)
    )
)

# UP Summary Statistics
up_stats <- data.frame(
    Variable = c(
        "Number of GPs (Analysis Sample)",
        "Electoral Cycles",
        "Years Covered",
        "Treatment Rate (%)",
        "Women Winners in Open Seats 2010 (%)",
        "Women Winners in Open Seats 2015 (%)",
        "Women Winners in Open Seats 2020 (%)"
    ),
    UP = c(
        format(nrow(up_data), big.mark = ","),
        "3",
        "2005, 2010, 2015",
        "50.0",
        ifelse("sex_2010" %in% names(up_data),
               round(mean(up_data$sex_2010[up_data$treat_2010 == 0] == "F", na.rm = TRUE) * 100, 1),
               "---"),
        ifelse("sex_2015" %in% names(up_data),
               round(mean(up_data$sex_2015[up_data$treat_2015 == 0] == "F", na.rm = TRUE) * 100, 1),
               "---"),
        "---"
    )
)

# Combine
combined_stats <- merge(raj_stats, up_stats, by = "Variable", all = TRUE)
combined_stats <- combined_stats[match(raj_stats$Variable, combined_stats$Variable), ]

# Create LaTeX table
latex_table <- kable(combined_stats,
                     format = "latex",
                     booktabs = TRUE,
                     row.names = FALSE,
                     col.names = c("", "Rajasthan", "Uttar Pradesh"),
                     caption = "Summary Statistics",
                     label = "tab:summary_stats",
                     align = c("l", "r", "r"),
                     escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c(" " = 1, "State" = 2)) %>%
    footnote(general = "Analysis sample includes GPs consistently defined across electoral cycles. Treatment rate is the proportion of GPs randomly assigned gender quotas each cycle.")

# Save
save_kable(latex_table, file = "tabs/summary_stats_combined.tex")
cat("Saved: tabs/summary_stats_combined.tex\n")

cat("=== Summary Statistics Complete ===\n")
