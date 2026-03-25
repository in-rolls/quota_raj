# 99_run_all.R
# Run the full analysis pipeline
# Execute from project root: Rscript scripts/99_run_all.R

library(here)

cat("=======================================================\n")
cat("   QUOTA_RAJ: Full Analysis Pipeline\n")
cat("=======================================================\n\n")

# =============================================================================
# Logging Setup
# =============================================================================

dir.create(here("logs"), showWarnings = FALSE)
log_file <- here("logs", paste0("pipeline_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

log_msg <- function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf("[%s] %s: %s", timestamp, level, msg)
    cat(line, "\n")
    cat(line, "\n", file = log_file, append = TRUE)
}

log_msg("Pipeline started")
log_msg(paste("Log file:", log_file))

results <- list()

run_script <- function(script_name) {
    cat("\n-------------------------------------------------------\n")
    cat("Running:", script_name, "\n")
    cat("-------------------------------------------------------\n")

    log_msg(paste("Starting:", script_name))
    start_time <- Sys.time()

    result <- tryCatch({
        source(here("scripts", script_name))
        elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
        log_msg(paste("SUCCESS:", script_name, "(", elapsed, "s)"))
        "success"
    }, warning = function(w) {
        elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
        log_msg(paste("WARNING in", script_name, ":", w$message), "WARN")
        cat("WARNING:", w$message, "\n")
        "warning"
    }, error = function(e) {
        elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
        log_msg(paste("ERROR in", script_name, ":", e$message), "ERROR")
        cat("ERROR:", e$message, "\n")
        "error"
    })

    results[[script_name]] <<- result
    return(result)
}

# =============================================================================
# Phase 1: Data Extraction (Crosswalks)
# =============================================================================
cat("\n### PHASE 1: DATA EXTRACTION ###\n")
log_msg("=== PHASE 1: DATA EXTRACTION ===")

run_script("01_extract_up_lgd.R")
run_script("01a_raj_standardize_source.R")
run_script("01d_raj_standardize_samiti_names.R")
run_script("01e_raj_create_samiti_crosswalk.R")

# =============================================================================
# Phase 2: Create Panels
# =============================================================================
cat("\n### PHASE 2: CREATE PANELS ###\n")
log_msg("=== PHASE 2: CREATE PANELS ===")

run_script("02b_raj_recode.R")
run_script("02c_up_recode.R")

# =============================================================================
# Phase 3: Link to SHRUG
# =============================================================================
cat("\n### PHASE 3: LINK TO SHRUG ###\n")
log_msg("=== PHASE 3: LINK TO SHRUG ===")

run_script("03a_raj_shrug_lgd_block.R")
run_script("03a_up_shrug_lgd_block.R")

# =============================================================================
# Phase 4: Balance Tests
# =============================================================================
cat("\n### PHASE 4: BALANCE TESTS ###\n")
log_msg("=== PHASE 4: BALANCE TESTS ===")

run_script("04_balance.R")
run_script("04a_balance_electoral.R")

# =============================================================================
# Phase 5: Transition Matrices
# =============================================================================
cat("\n### PHASE 5: TRANSITION MATRICES ###\n")
log_msg("=== PHASE 5: TRANSITION MATRICES ===")

run_script("05_transition_matrices.R")

# =============================================================================
# Phase 7: Short-Term Effects
# =============================================================================
cat("\n### PHASE 7: SHORT-TERM EFFECTS ###\n")
log_msg("=== PHASE 7: SHORT-TERM EFFECTS ===")

run_script("07_short_term.R")
run_script("07b_treatment_rotation.R")
run_script("07c_placebo.R")
run_script("07d_shrug_covariates.R")
run_script("07e_het_effects.R")

# =============================================================================
# Phase 8: Long-Term Effects
# =============================================================================
cat("\n### PHASE 8: LONG-TERM EFFECTS ###\n")
log_msg("=== PHASE 8: LONG-TERM EFFECTS ===")

run_script("08_long_term.R")

# =============================================================================
# Phase 9: Descriptive Tables
# =============================================================================
cat("\n### PHASE 9: DESCRIPTIVE TABLES ###\n")
log_msg("=== PHASE 9: DESCRIPTIVE TABLES ===")

run_script("02_descriptive_tables.R")

# =============================================================================
# Phase 10: Candidate Quality
# =============================================================================
cat("\n### PHASE 10: CANDIDATE QUALITY ###\n")
log_msg("=== PHASE 10: CANDIDATE QUALITY ===")

run_script("11_candidate_quality.R")

# =============================================================================
# Phase 11: Phone Surveys
# =============================================================================
cat("\n### PHASE 11: PHONE SURVEYS ###\n")
log_msg("=== PHASE 11: PHONE SURVEYS ===")

run_script("12a_raj_phone_survey.R")
run_script("12b_raj_jaipur_phone_survey.R")

# =============================================================================
# Phase 12: Weaver Replication
# =============================================================================
cat("\n### PHASE 12: WEAVER REPLICATION ###\n")
log_msg("=== PHASE 12: WEAVER REPLICATION ===")

run_script("13_weaver_replication.R")

# =============================================================================
# Phase 13: Power Analysis
# =============================================================================
cat("\n### PHASE 13: POWER ANALYSIS ###\n")
log_msg("=== PHASE 13: POWER ANALYSIS ===")

run_script("14_power_analysis.R")

# =============================================================================
# Summary
# =============================================================================
cat("\n=======================================================\n")
cat("   PIPELINE COMPLETE\n")
cat("=======================================================\n")

n_success <- sum(unlist(results) == "success")
n_warning <- sum(unlist(results) == "warning")
n_error <- sum(unlist(results) == "error")

log_msg("=== PIPELINE SUMMARY ===")
log_msg(paste("Successes:", n_success))
log_msg(paste("Warnings:", n_warning))
log_msg(paste("Errors:", n_error))
log_msg(paste("Log saved to:", log_file))

cat("\nSummary:\n")
cat("  Successes:", n_success, "\n")
cat("  Warnings:", n_warning, "\n")
cat("  Errors:", n_error, "\n")
cat("\nLog saved to:", log_file, "\n")
cat("Check individual script output above for files created.\n")
