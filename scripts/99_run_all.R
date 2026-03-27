# 99_run_all.R
# Run the full analysis pipeline
# Execute from project root: Rscript scripts/99_run_all.R

library(here)

message("=======================================================")
message("   QUOTA_RAJ: Full Analysis Pipeline")
message("=======================================================")

# =============================================================================
# Logging Setup
# =============================================================================

dir.create(here("logs"), showWarnings = FALSE)
log_file <- here("logs", paste0("pipeline_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

log_msg <- function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf("[%s] %s: %s", timestamp, level, msg)
    message(line)
    cat(line, "\n", file = log_file, append = TRUE)
}

log_msg("Pipeline started")
log_msg(paste("Log file:", log_file))

results <- list()
warning_count <- 0L

run_script <- function(script_name) {
    message("\n-------------------------------------------------------")
    message("Running: ", script_name)
    message("-------------------------------------------------------")

    log_msg(paste("Starting:", script_name))
    start_time <- Sys.time()

    had_warning <- FALSE
    warning_messages <- character()

    tryCatch({
        withCallingHandlers({
            source(here("scripts", script_name))
        }, warning = function(w) {
            had_warning <<- TRUE
            warning_messages <<- c(warning_messages, conditionMessage(w))
            log_msg(paste("WARNING in", script_name, ":", conditionMessage(w)), "WARN")
            invokeRestart("muffleWarning")
        })

        elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)

        if (had_warning) {
            warning_count <<- warning_count + length(warning_messages)
            log_msg(paste("SUCCESS WITH WARNINGS:", script_name, "(", elapsed, "s)"), "WARN")
            results[[script_name]] <<- "warning"
            return("warning")
        }

        log_msg(paste("SUCCESS:", script_name, "(", elapsed, "s)"))
        results[[script_name]] <<- "success"
        return("success")
    }, error = function(e) {
        elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
        log_msg(paste("ERROR in", script_name, ":", e$message), "ERROR")
        results[[script_name]] <<- "error"
        stop(sprintf("Pipeline halted at %s after %ss: %s", script_name, elapsed, e$message))
    })
}

# =============================================================================
# Phase 1: Data Extraction (Crosswalks + Standardization)
# =============================================================================
message("\n### PHASE 1: DATA EXTRACTION ###")
log_msg("=== PHASE 1: DATA EXTRACTION ===")

run_script("01d_up_extract_lgd.R")
run_script("01a_raj_standardize_source.R")
run_script("01c_raj_create_samiti_xwalk.R")

# =============================================================================
# Phase 2: Create Panels + UP Crosswalks (depends on refreshed UP panels)
# =============================================================================
message("\n### PHASE 2: CREATE PANELS ###")
log_msg("=== PHASE 2: CREATE PANELS ===")

run_script("02a_raj_recode.R")
run_script("02b_up_recode.R")
run_script("01e_up_create_district_xwalk.R")
run_script("01f_up_create_block_xwalk.R")
run_script("01g_audit_crosswalk_provenance.R")

# =============================================================================
# Phase 3: Link to SHRUG
# =============================================================================
message("\n### PHASE 3: LINK TO SHRUG ###")
log_msg("=== PHASE 3: LINK TO SHRUG ===")

run_script("03a_raj_shrug_match.R")
run_script("03b_up_shrug_match.R")

# =============================================================================
# Phase 4: Balance Tests
# =============================================================================
message("\n### PHASE 4: BALANCE TESTS ###")
log_msg("=== PHASE 4: BALANCE TESTS ===")

run_script("04d_balance.R")
run_script("04e_balance_electoral.R")

# =============================================================================
# Phase 5: Transition Matrices
# =============================================================================
message("\n### PHASE 5: TRANSITION MATRICES ###")
log_msg("=== PHASE 5: TRANSITION MATRICES ===")

run_script("04c_transition_matrices.R")

# =============================================================================
# Phase 7: Short-Term Effects
# =============================================================================
message("\n### PHASE 7: SHORT-TERM EFFECTS ###")
log_msg("=== PHASE 7: SHORT-TERM EFFECTS ===")

run_script("05a_short_term_main.R")
run_script("05c_short_term_treatment_rotation.R")
run_script("05d_short_term_placebo.R")
run_script("05e_short_term_covariates.R")
run_script("05f_short_term_het_effects.R")

# =============================================================================
# Phase 8: Long-Term Effects
# =============================================================================
message("\n### PHASE 8: LONG-TERM EFFECTS ###")
log_msg("=== PHASE 8: LONG-TERM EFFECTS ===")

run_script("06a_long_term_main.R")

# =============================================================================
# Phase 9: Descriptive Tables
# =============================================================================
message("\n### PHASE 9: DESCRIPTIVE TABLES ###")
log_msg("=== PHASE 9: DESCRIPTIVE TABLES ===")

run_script("04a_descriptive_tables.R")

# =============================================================================
# Phase 10: Candidate Quality
# =============================================================================
message("\n### PHASE 10: CANDIDATE QUALITY ###")
log_msg("=== PHASE 10: CANDIDATE QUALITY ===")

run_script("07b_candidate_quality.R")

# =============================================================================
# Phase 11: Phone Surveys
# =============================================================================
message("\n### PHASE 11: PHONE SURVEYS ###")
log_msg("=== PHASE 11: PHONE SURVEYS ===")

run_script("08a_phone_survey_tables.R")
run_script("08b_raj_phone_survey.R")

# =============================================================================
# Phase 12: Weaver Replication
# =============================================================================
message("\n### PHASE 12: WEAVER REPLICATION ###")
log_msg("=== PHASE 12: WEAVER REPLICATION ===")

run_script("09_weaver_replication.R")

# =============================================================================
# Phase 13: Power Analysis
# =============================================================================
message("\n### PHASE 13: POWER ANALYSIS ###")
log_msg("=== PHASE 13: POWER ANALYSIS ===")

run_script("04f_power_analysis.R")

# =============================================================================
# Summary
# =============================================================================
message("\n=======================================================")
message("   PIPELINE COMPLETE")
message("=======================================================")

n_success <- sum(unlist(results) == "success")
n_warning <- sum(unlist(results) == "warning")
n_error <- sum(unlist(results) == "error")

log_msg("=== PIPELINE SUMMARY ===")
log_msg(paste("Successes:", n_success))
log_msg(paste("Warnings:", n_warning, "(warning events:", warning_count, ")"))
log_msg(paste("Errors:", n_error))
log_msg(paste("Log saved to:", log_file))

message("\nSummary:")
message("  Successes: ", n_success)
message("  Warnings: ", n_warning, " (events: ", warning_count, ")")
message("  Errors: ", n_error)
message("\nLog saved to: ", log_file)
message("Check individual script output above for files created.")
