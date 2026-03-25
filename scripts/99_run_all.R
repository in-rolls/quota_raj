# 99_run_all.R
# Run the full analysis pipeline
# Execute from project root: Rscript scripts/99_run_all.R

library(here)

cat("=======================================================\n")
cat("   QUOTA_RAJ: Full Analysis Pipeline\n")
cat("=======================================================\n\n")

run_script <- function(script_name) {
    cat("\n-------------------------------------------------------\n")
    cat("Running:", script_name, "\n")
    cat("-------------------------------------------------------\n")
    tryCatch({
        source(here("scripts", script_name))
        cat("SUCCESS:", script_name, "\n")
    }, error = function(e) {
        cat("ERROR in", script_name, ":", e$message, "\n")
    })
}

# =============================================================================
# Phase 1: Data Extraction (Crosswalks)
# =============================================================================
cat("\n### PHASE 1: DATA EXTRACTION ###\n")

run_script("01_extract_up_lgd.R")
run_script("01a_raj_standardize_source.R")
run_script("01d_raj_standardize_samiti_names.R")
run_script("01e_raj_create_samiti_crosswalk.R")

# =============================================================================
# Phase 2: Create Panels
# =============================================================================
cat("\n### PHASE 2: CREATE PANELS ###\n")

run_script("02b_raj_recode.R")
run_script("02c_up_recode.R")

# =============================================================================
# Phase 3: Link to SHRUG
# =============================================================================
cat("\n### PHASE 3: LINK TO SHRUG ###\n")

run_script("03a_raj_shrug_lgd_block.R")
run_script("03a_up_shrug_lgd_block.R")

# =============================================================================
# Phase 4: Balance Tests
# =============================================================================
cat("\n### PHASE 4: BALANCE TESTS ###\n")

run_script("04_balance.R")
run_script("04a_balance_electoral.R")

# =============================================================================
# Phase 5: Transition Matrices
# =============================================================================
cat("\n### PHASE 5: TRANSITION MATRICES ###\n")

run_script("05_transition_matrices.R")

# =============================================================================
# Phase 7: Short-Term Effects
# =============================================================================
cat("\n### PHASE 7: SHORT-TERM EFFECTS ###\n")

run_script("07_short_term.R")
run_script("07b_treatment_rotation.R")
run_script("07c_placebo.R")
run_script("07d_shrug_covariates.R")
run_script("07e_het_effects.R")

# =============================================================================
# Phase 8: Long-Term Effects
# =============================================================================
cat("\n### PHASE 8: LONG-TERM EFFECTS ###\n")

run_script("08_long_term.R")

# =============================================================================
# Phase 9: Descriptive Tables
# =============================================================================
cat("\n### PHASE 9: DESCRIPTIVE TABLES ###\n")

run_script("02_descriptive_tables.R")

# =============================================================================
# Phase 10: Candidate Quality
# =============================================================================
cat("\n### PHASE 10: CANDIDATE QUALITY ###\n")

run_script("11_candidate_quality.R")

# =============================================================================
# Phase 11: Phone Surveys
# =============================================================================
cat("\n### PHASE 11: PHONE SURVEYS ###\n")

run_script("12a_raj_phone_survey.R")
run_script("12b_raj_jaipur_phone_survey.R")

# =============================================================================
# Phase 12: Weaver Replication
# =============================================================================
cat("\n### PHASE 12: WEAVER REPLICATION ###\n")

run_script("13_weaver_replication.R")

# =============================================================================
# Phase 13: Power Analysis
# =============================================================================
cat("\n### PHASE 13: POWER ANALYSIS ###\n")

run_script("14_power_analysis.R")

# =============================================================================
# Summary
# =============================================================================
cat("\n=======================================================\n")
cat("   PIPELINE COMPLETE\n")
cat("=======================================================\n")
cat("Check individual script output above for files created.\n")
