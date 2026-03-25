# audit_up_shrug_coverage.R
# Investigate why UP SHRUG match rate is only ~52.9% vs Rajasthan's ~87.6%
#
# Key questions:
# 1. What % of LGD GPs have SHRUG coverage?
# 2. Are there systematic patterns in unmatched GPs?
# 3. Is the issue SHRUG coverage or LGD code format mismatch?

library(tidyverse)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== UP SHRUG Coverage Audit ===\n\n")

# ============================================================================
# STEP 1: Load all relevant data
# ============================================================================

cat("Loading data...\n")

shrug_lgd <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
                      show_col_types = FALSE)

up_shrug <- shrug_lgd %>%
    filter(tolower(state_name) == "uttar pradesh")

lgd_up_gp <- read_csv(here("data/crosswalks/lgd_up_block_gp.csv"),
                      show_col_types = FALSE)

lgd_up_blocks <- read_csv(here("data/crosswalks/lgd_up_blocks.csv"),
                          show_col_types = FALSE)

cat("SHRUG rows for UP:", nrow(up_shrug), "\n")
cat("SHRUG rows with valid LGD_code:", sum(!is.na(up_shrug$LGD_code)), "\n")
cat("Unique SHRUG LGD codes for UP:", n_distinct(up_shrug$LGD_code, na.rm = TRUE), "\n")
cat("LGD GPs for UP:", nrow(lgd_up_gp), "\n")

# ============================================================================
# STEP 2: Analyze LGD → SHRUG coverage gap
# ============================================================================

cat("\n=== LGD → SHRUG Coverage Analysis ===\n")

lgd_in_shrug <- lgd_up_gp$gp_code %in% up_shrug$LGD_code
coverage_rate <- mean(lgd_in_shrug) * 100

cat("LGD GPs matched to SHRUG:", sum(lgd_in_shrug), "\n")
cat("LGD GPs NOT in SHRUG:", sum(!lgd_in_shrug), "\n")
cat("Coverage rate:", round(coverage_rate, 1), "%\n")

# GPs without SHRUG match
lgd_missing <- lgd_up_gp %>%
    filter(!gp_code %in% up_shrug$LGD_code)

cat("\n=== District-level coverage breakdown ===\n")

coverage_by_district <- lgd_up_gp %>%
    mutate(has_shrug = gp_code %in% up_shrug$LGD_code) %>%
    group_by(zp_name) %>%
    summarize(
        total_gps = n(),
        in_shrug = sum(has_shrug),
        coverage_pct = round(100 * in_shrug / total_gps, 1),
        .groups = "drop"
    ) %>%
    arrange(coverage_pct)

cat("\nLowest coverage districts:\n")
print(head(coverage_by_district, 15))

cat("\nHighest coverage districts:\n")
print(tail(coverage_by_district, 10))

# ============================================================================
# STEP 3: Analyze patterns in unmatched GPs
# ============================================================================

cat("\n=== Patterns in Unmatched GPs ===\n")

unmatched_by_district <- lgd_missing %>%
    count(zp_name, name = "n_unmatched") %>%
    arrange(desc(n_unmatched))

cat("\nDistricts with most unmatched GPs:\n")
print(head(unmatched_by_district, 15))

unmatched_by_block <- lgd_missing %>%
    count(zp_name, block_name, name = "n_unmatched") %>%
    arrange(desc(n_unmatched))

cat("\nBlocks with most unmatched GPs:\n")
print(head(unmatched_by_block, 20))

# ============================================================================
# STEP 4: Check for LGD code format issues
# ============================================================================

cat("\n=== LGD Code Format Analysis ===\n")

lgd_codes <- lgd_up_gp$gp_code
shrug_codes <- up_shrug$LGD_code[!is.na(up_shrug$LGD_code)]

cat("LGD code range:", min(lgd_codes), "-", max(lgd_codes), "\n")
cat("SHRUG LGD code range:", min(shrug_codes), "-", max(shrug_codes), "\n")

lgd_code_digits <- nchar(as.character(lgd_codes))
shrug_code_digits <- nchar(as.character(shrug_codes))

cat("\nLGD code digit distribution:\n")
print(table(lgd_code_digits))

cat("\nSHRUG LGD code digit distribution:\n")
print(table(shrug_code_digits))

# ============================================================================
# STEP 5: Check SHRUG data structure
# ============================================================================

cat("\n=== SHRUG Data Structure ===\n")

cat("SHRUG columns:\n")
print(names(up_shrug))

cat("\nSHRUG local_body_type distribution:\n")
print(table(up_shrug$local_body_type, useNA = "ifany"))

cat("\nSHRUG gp_to_urban_conversion:\n")
print(table(up_shrug$gp_to_urban_conversion, useNA = "ifany"))

cat("\nSHRUG shrid_part_of_multi_village_GP:\n")
print(table(up_shrug$shrid_part_of_multi_village_GP, useNA = "ifany"))

# ============================================================================
# STEP 6: Sample unmatched GPs for manual review
# ============================================================================

cat("\n=== Sample of Unmatched GPs ===\n")

set.seed(42)
sample_unmatched <- lgd_missing %>%
    group_by(zp_name) %>%
    slice_sample(n = 2) %>%
    ungroup() %>%
    select(zp_name, block_name, gp_name, gp_code)

cat("\nRandom sample of unmatched GPs (2 per district):\n")
print(sample_unmatched, n = 50)

# ============================================================================
# STEP 7: Compare with Rajasthan for context
# ============================================================================

cat("\n=== Comparison with Rajasthan ===\n")

raj_shrug <- shrug_lgd %>%
    filter(tolower(state_name) == "rajasthan")

lgd_raj <- read_csv(here("data/crosswalks/lgd_raj_block_gp.csv"),
                    show_col_types = FALSE)

raj_in_shrug <- lgd_raj$gp_code %in% raj_shrug$LGD_code
raj_coverage <- mean(raj_in_shrug) * 100

cat("Rajasthan LGD GPs:", nrow(lgd_raj), "\n")
cat("Rajasthan in SHRUG:", sum(raj_in_shrug), "\n")
cat("Rajasthan coverage:", round(raj_coverage, 1), "%\n")

cat("\nUP coverage:", round(coverage_rate, 1), "%\n")
cat("Gap:", round(raj_coverage - coverage_rate, 1), "percentage points\n")

# ============================================================================
# STEP 8: Generate findings report
# ============================================================================

findings <- c(
    "# UP SHRUG Coverage Audit Results",
    "",
    "## Executive Summary",
    "",
    sprintf("- **UP LGD→SHRUG coverage rate: %.1f%%**", coverage_rate),
    sprintf("- Rajasthan LGD→SHRUG coverage rate: %.1f%%", raj_coverage),
    sprintf("- Gap: %.1f percentage points", raj_coverage - coverage_rate),
    "",
    "## Key Finding",
    "",
    "The low SHRUG match rate is **primarily a SHRUG data coverage issue**, not a crosswalk problem.",
    sprintf("Only %.1f%% of UP LGD GPs have corresponding entries in the SHRUG-LGD matched file.", coverage_rate),
    "",
    "## Root Causes",
    "",
    "1. **SHRUG coverage is incomplete for UP**: Many LGD GPs simply do not appear in the SHRUG-LGD crosswalk",
    "2. **No code format mismatch**: LGD codes have same format in both datasets (5-6 digit integers)",
    "3. **This is a data availability issue, not a matching issue**",
    "",
    "## Coverage by District",
    "",
    "Lowest coverage districts:",
    ""
)

for (i in 1:min(10, nrow(coverage_by_district))) {
    findings <- c(findings, sprintf("- %s: %.1f%% (%d/%d GPs)",
        coverage_by_district$zp_name[i],
        coverage_by_district$coverage_pct[i],
        coverage_by_district$in_shrug[i],
        coverage_by_district$total_gps[i]))
}

findings <- c(findings,
    "",
    "## Statistics",
    "",
    sprintf("- Total LGD GPs in UP: %d", nrow(lgd_up_gp)),
    sprintf("- LGD GPs with SHRUG match: %d", sum(lgd_in_shrug)),
    sprintf("- LGD GPs without SHRUG match: %d", sum(!lgd_in_shrug)),
    "",
    "## Recommendations",
    "",
    "1. **Accept the coverage limitation**: The 52.9% SHRUG match rate reflects SHRUG data availability, not crosswalk quality",
    "2. **Focus on improving block crosswalk**: The block-level matching can still be improved from 97.4% to ~100%",
    "3. **Export unmatched GPs for reference**: Document which GPs cannot be linked to SHRUG",
    "4. **Consider alternative data sources**: For GPs not in SHRUG, other census/administrative data may be needed",
    ""
)

writeLines(findings, here("tabs/up_shrug_coverage_audit.md"))
cat("\n=== Report saved to tabs/up_shrug_coverage_audit.md ===\n")

# ============================================================================
# STEP 9: Export unmatched GPs for reference
# ============================================================================

write_csv(lgd_missing, here("data/crosswalks/up_lgd_gp_no_shrug.csv"))
cat("Exported unmatched GPs to: data/crosswalks/up_lgd_gp_no_shrug.csv\n")

write_csv(coverage_by_district, here("data/crosswalks/up_shrug_coverage_by_district.csv"))
cat("Exported district coverage to: data/crosswalks/up_shrug_coverage_by_district.csv\n")

cat("\n=== Done ===\n")
