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
# Phase 11: Candidate Quality
# =============================================================================
cat("\n### PHASE 11: CANDIDATE QUALITY ###\n")

run_script("11_candidate_quality.R")

# =============================================================================
# Summary
# =============================================================================
cat("\n=======================================================\n")
cat("   PIPELINE COMPLETE\n")
cat("=======================================================\n\n")

cat("Data files created:\n")
cat("  data/crosswalks/lgd_up_block_gp.csv\n")
cat("  data/crosswalks/lgd_up_blocks.csv\n")
cat("  data/raj/raj_05_10.parquet\n")
cat("  data/raj/raj_10_15.parquet\n")
cat("  data/raj/raj_15_20.parquet\n")
cat("  data/raj/raj_05_20.parquet\n")
cat("  data/raj/shrug_gp_raj_05_10_block.parquet\n")
cat("  data/raj/shrug_gp_raj_10_15_block.parquet\n")
cat("  data/raj/shrug_gp_raj_15_20_block.parquet\n")
cat("  data/raj/shrug_gp_raj_05_20_block.parquet\n")
cat("  data/up/up_05_10.parquet\n")
cat("  data/up/up_10_15.parquet\n")
cat("  data/up/up_15_21.parquet\n")
cat("  data/up/up_05_21.parquet\n")
cat("  data/up/shrug_gp_up_05_10_block.parquet\n")
cat("  data/up/shrug_gp_up_10_15_block.parquet\n")
cat("  data/up/shrug_gp_up_15_21_block.parquet\n")
cat("  data/up/shrug_gp_up_05_21_block.parquet\n")

cat("\nTables created:\n")
cat("  tabs/balance_electoral.tex\n")
cat("  tabs/balance_combined.tex\n")
cat("  tabs/reserved_or_not_chi_sq_raj.tex\n")
cat("  tabs/up_chi_squared_results.tex\n")
cat("  tabs/raj_treatment_reg.tex\n")
cat("  tabs/up_treatment_reg.tex\n")
cat("  tabs/short_term_combined.tex\n")
cat("  tabs/short_term_full_sample.tex\n")
cat("  tabs/treatment_rotation.tex\n")
cat("  tabs/placebo_combined.tex\n")
cat("  tabs/short_term_shrug_covariates.tex\n")
cat("  tabs/het_effects_town.tex\n")
cat("  tabs/het_effects_infra.tex\n")
cat("  tabs/het_effects_power.tex\n")
cat("  tabs/long_term_combined.tex\n")
cat("  tabs/dosage_combined.tex\n")
cat("  tabs/cand_characteristics_combined.tex\n")
cat("  tabs/mean_values_respondents.tex\n")
cat("  tabs/up_cand_characteristics.tex\n")

cat("\n=======================================================\n")
