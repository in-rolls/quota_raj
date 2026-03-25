# 08b_long_term_random_rotation.R
# Long-term effects analysis restricted to "random rotation" districts
# Districts where chi-square test fails to reject independence (p > 0.05)
# for BOTH 05→10 AND 10→15 transitions
# Output: tabs/long_term_random_rotation.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Long-Term Effects: Random Rotation Districts ===\n")

# =============================================================================
# Load 4-way panels
# =============================================================================

raj_05_20 <- read_parquet(here("data/raj/raj_05_20.parquet"))
up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

cat("\nLoaded 4-way panels:\n")
cat("Raj 05-20:", nrow(raj_05_20), "GPs\n")
cat("UP 05-21:", nrow(up_05_21), "GPs\n")

# =============================================================================
# Prepare UP data - add female_winner variable for 2015
# =============================================================================

up_05_21 <- up_05_21 %>%
    mutate(
        female_winner_2015 = as.integer(sex_2015 == "महिला"),
        female_winner_2021 = as.integer(sex_2021 == "महिला")
    )

# =============================================================================
# Function to compute chi-square p-value per district
# =============================================================================

compute_district_chisq <- function(data, treat_t1, treat_t2, district_var) {
    results <- data %>%
        group_by(!!sym(district_var)) %>%
        summarise(
            n = n(),
            n_00 = sum(!!sym(treat_t1) == 0 & !!sym(treat_t2) == 0, na.rm = TRUE),
            n_01 = sum(!!sym(treat_t1) == 0 & !!sym(treat_t2) == 1, na.rm = TRUE),
            n_10 = sum(!!sym(treat_t1) == 1 & !!sym(treat_t2) == 0, na.rm = TRUE),
            n_11 = sum(!!sym(treat_t1) == 1 & !!sym(treat_t2) == 1, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
            chisq_p = {
                tbl <- matrix(c(n_00, n_01, n_10, n_11), nrow = 2)
                if (any(rowSums(tbl) == 0) || any(colSums(tbl) == 0)) {
                    NA_real_
                } else {
                    tryCatch(
                        chisq.test(tbl, correct = FALSE)$p.value,
                        error = function(e) NA_real_
                    )
                }
            }
        ) %>%
        ungroup()

    return(results)
}

# =============================================================================
# Compute chi-square tests for 05→10 and 10→15 transitions
# =============================================================================

cat("\n=== Computing Chi-Square Tests by District ===\n")

# Rajasthan: use district_std_2015 for both (consistent district identifier across panel)
chisq_raj_0510 <- compute_district_chisq(raj_05_20, "treat_2005", "female_res_2010", "district_std_2015")
chisq_raj_1015 <- compute_district_chisq(raj_05_20, "treat_2010", "female_res_2015", "district_std_2015")

# UP: use district_name_eng_2015 for consistency
chisq_up_0510 <- compute_district_chisq(up_05_21, "treat_2005", "treat_2010", "district_name_eng_2015")
chisq_up_1015 <- compute_district_chisq(up_05_21, "treat_2010", "treat_2015", "district_name_eng_2015")

# Chi-square tests for 15→20/21 transitions (for 15-year analysis)
chisq_raj_1520 <- compute_district_chisq(raj_05_20, "treat_2015", "female_res_2020", "district_std_2015")
chisq_up_1521 <- compute_district_chisq(up_05_21, "treat_2015", "treat_2021", "district_name_eng_2015")

# Districts where p > 0.05 for BOTH transitions
random_raj_0510 <- chisq_raj_0510 %>% filter(chisq_p > 0.05) %>% pull(district_std_2015)
random_raj_1015 <- chisq_raj_1015 %>% filter(chisq_p > 0.05) %>% pull(district_std_2015)
random_raj_both <- intersect(random_raj_0510, random_raj_1015)

random_up_0510 <- chisq_up_0510 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2015)
random_up_1015 <- chisq_up_1015 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2015)
random_up_both <- intersect(random_up_0510, random_up_1015)

# Triple-intersection districts (for 15-year analysis)
random_raj_1520 <- chisq_raj_1520 %>% filter(chisq_p > 0.05) %>% pull(district_std_2015)
random_raj_all <- Reduce(intersect, list(random_raj_0510, random_raj_1015, random_raj_1520))

random_up_1521 <- chisq_up_1521 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2015)
random_up_all <- Reduce(intersect, list(random_up_0510, random_up_1015, random_up_1521))

cat("\nRandom-rotation districts (p > 0.05 for each transition):\n")
cat("Raj 05-10:", length(random_raj_0510), "of", nrow(chisq_raj_0510), "districts\n")
cat("Raj 10-15:", length(random_raj_1015), "of", nrow(chisq_raj_1015), "districts\n")
cat("Raj BOTH:", length(random_raj_both), "of", nrow(chisq_raj_0510), "districts\n")
cat("Raj 15-20:", length(random_raj_1520), "of", nrow(chisq_raj_1520), "districts\n")
cat("Raj ALL THREE:", length(random_raj_all), "of", nrow(chisq_raj_0510), "districts\n")
cat("UP 05-10:", length(random_up_0510), "of", nrow(chisq_up_0510), "districts\n")
cat("UP 10-15:", length(random_up_1015), "of", nrow(chisq_up_1015), "districts\n")
cat("UP BOTH:", length(random_up_both), "of", nrow(chisq_up_0510), "districts\n")
cat("UP 15-21:", length(random_up_1521), "of", nrow(chisq_up_1521), "districts\n")
cat("UP ALL THREE:", length(random_up_all), "of", nrow(chisq_up_0510), "districts\n")

# =============================================================================
# Rajasthan: Long-term effects (open seats in 2015, random-rotation districts)
# =============================================================================
cat("\n--- Rajasthan Long-Term (2005→2015, open seats) ---\n")

raj_lt_rr <- raj_05_20 %>%
    filter(district_std_2015 %in% random_raj_both,
           female_res_2015 == 0,
           !is.na(female_winner_2015))
cat("Open seats in 2015 in random-rotation districts:", nrow(raj_lt_rr), "\n")

if (nrow(raj_lt_rr) > 0) {
    m_raj_lt_nofe <- feols(female_winner_2015 ~ treat_2005 * treat_2010, data = raj_lt_rr)
    m_raj_lt_fe <- feols(female_winner_2015 ~ treat_2005 * treat_2010 | dist_samiti_2015, data = raj_lt_rr)
} else {
    m_raj_lt_nofe <- NULL
    m_raj_lt_fe <- NULL
}

# =============================================================================
# UP: Long-term effects (open seats in 2015, random-rotation districts)
# =============================================================================
cat("\n--- UP Long-Term (2005→2015, open seats) ---\n")

up_lt_rr <- up_05_21 %>%
    filter(district_name_eng_2015 %in% random_up_both,
           treat_2015 == 0,
           !is.na(female_winner_2015))
cat("Open seats in 2015 in random-rotation districts:", nrow(up_lt_rr), "\n")

if (nrow(up_lt_rr) > 0) {
    m_up_lt_nofe <- feols(female_winner_2015 ~ treat_2005 * treat_2010, data = up_lt_rr)
    m_up_lt_fe <- feols(female_winner_2015 ~ treat_2005 * treat_2010 | dist_block_2015, data = up_lt_rr)
} else {
    m_up_lt_nofe <- NULL
    m_up_lt_fe <- NULL
}

# =============================================================================
# 15-YEAR ANALYSIS: 2005→2020/2021 (matching main 08_long_term.R specification)
# =============================================================================
cat("\n=== 15-Year Long-Term Analysis (Random Rotation Districts) ===\n")

# Rajasthan 15-year
raj_lt_full <- raj_05_20 %>%
    filter(district_std_2015 %in% random_raj_all,
           treat_2020 == 0,
           !is.na(female_winner_2020))
cat("Raj open seats in 2020 in random-rotation districts:", nrow(raj_lt_full), "\n")

if (nrow(raj_lt_full) > 0) {
    m_raj_full_nofe <- feols(female_winner_2020 ~ treat_2005 * treat_2010 * treat_2015, data = raj_lt_full)
    m_raj_full_fe <- feols(female_winner_2020 ~ treat_2005 * treat_2010 * treat_2015 | dist_samiti_2020, data = raj_lt_full)
} else {
    m_raj_full_nofe <- NULL
    m_raj_full_fe <- NULL
    cat("WARNING: No Rajasthan districts pass all three chi-square tests\n")
}

# UP 15-year
up_lt_full <- up_05_21 %>%
    filter(district_name_eng_2015 %in% random_up_all,
           treat_2021 == 0,
           !is.na(female_winner_2021))
cat("UP open seats in 2021 in random-rotation districts:", nrow(up_lt_full), "\n")

if (nrow(up_lt_full) > 0) {
    m_up_full_nofe <- feols(female_winner_2021 ~ treat_2005 * treat_2010 * treat_2015, data = up_lt_full)
    m_up_full_fe <- feols(female_winner_2021 ~ treat_2005 * treat_2010 * treat_2015 | dist_block_2021, data = up_lt_full)
} else {
    m_up_full_nofe <- NULL
    m_up_full_fe <- NULL
}

# =============================================================================
# Build output table
# =============================================================================
cat("\n=== Generating output table ===\n")

# Helper functions for manual table generation
get_model_stats <- function(m, treat_vars) {
    if (is.null(m)) return(NULL)
    coefs <- coef(m)
    se <- sqrt(diag(vcov(m)))

    stats_list <- lapply(treat_vars, function(v) {
        if (v %in% names(coefs)) {
            coef_val <- coefs[v]
            se_val <- se[v]
            pval <- 2 * pnorm(-abs(coef_val / se_val))
            list(coef = coef_val, se = se_val, pval = pval)
        } else {
            NULL
        }
    })
    names(stats_list) <- treat_vars

    r2 <- fitstat(m, "r2")$r2
    n <- m$nobs

    list(vars = stats_list, r2 = r2, n = n)
}

format_coef <- function(coef, pval) {
    if (is.null(coef) || is.na(coef)) return("---")
    stars <- ""
    if (!is.na(pval)) {
        if (pval < 0.01) stars <- "$^{***}$"
        else if (pval < 0.05) stars <- "$^{**}$"
        else if (pval < 0.1) stars <- "$^{*}$"
    }
    paste0(sprintf("%.2f", coef), stars)
}

format_se <- function(se) {
    if (is.null(se) || is.na(se)) return("")
    paste0("(", sprintf("%.2f", se), ")")
}

format_r2 <- function(r2) {
    if (is.null(r2) || is.na(r2)) return("---")
    sprintf("%.2f", r2)
}

format_n <- function(n) {
    if (is.null(n) || is.na(n)) return("---")
    format(n, big.mark = ",")
}

# Get stats for all models
treat_vars <- c("treat_2005", "treat_2010", "treat_2005:treat_2010")
stats_raj_nofe <- get_model_stats(m_raj_lt_nofe, treat_vars)
stats_raj_fe <- get_model_stats(m_raj_lt_fe, treat_vars)
stats_up_nofe <- get_model_stats(m_up_lt_nofe, treat_vars)
stats_up_fe <- get_model_stats(m_up_lt_fe, treat_vars)

# Build 4-column table manually
notes_text <- paste0(
    "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
    "Outcome: woman elected in open seat (2015). ",
    "Sample restricted to districts where chi-square test fails to reject independence of quota assignment ",
    "for BOTH 05$\\rightarrow$10 AND 10$\\rightarrow$15 transitions (p $>$ 0.05). ",
    "Districts retained: Raj: ", length(random_raj_both), "/", nrow(chisq_raj_0510), "; ",
    "UP: ", length(random_up_both), "/", nrow(chisq_up_0510), ". ",
    "Heteroskedasticity-robust standard errors."
)

tex_lines <- c(
    "{\\centering\\scriptsize",
    "\\begingroup",
    "\\begin{tabular}{@{}lcc@{\\hspace{1em}}cc@{}}",
    "   \\toprule",
    "    & \\multicolumn{2}{c}{Rajasthan} & \\multicolumn{2}{c}{Uttar Pradesh} \\\\ ",
    "   \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "    & \\multicolumn{2}{c}{05$\\rightarrow$15} & \\multicolumn{2}{c}{05$\\rightarrow$15} \\\\",
    "                                    & No FE   & FE            & No FE        & FE \\\\  ",
    "                                    & [i]     & [ii]          & [iii]        & [iv]\\\\  ",
    "   \\midrule "
)

# Quota 2005 row
if (!is.null(stats_raj_nofe) && !is.null(stats_raj_nofe$vars$treat_2005)) {
    q05_raj_nofe <- stats_raj_nofe$vars$treat_2005
    q05_raj_fe <- stats_raj_fe$vars$treat_2005
} else {
    q05_raj_nofe <- list(coef = NA, se = NA, pval = NA)
    q05_raj_fe <- list(coef = NA, se = NA, pval = NA)
}
if (!is.null(stats_up_nofe) && !is.null(stats_up_nofe$vars$treat_2005)) {
    q05_up_nofe <- stats_up_nofe$vars$treat_2005
    q05_up_fe <- stats_up_fe$vars$treat_2005
} else {
    q05_up_nofe <- list(coef = NA, se = NA, pval = NA)
    q05_up_fe <- list(coef = NA, se = NA, pval = NA)
}

coef_q05_row <- paste0(
    "   $\\text{Quota}_{2005}$           & ",
    format_coef(q05_raj_nofe$coef, q05_raj_nofe$pval), " & ",
    format_coef(q05_raj_fe$coef, q05_raj_fe$pval), " & ",
    format_coef(q05_up_nofe$coef, q05_up_nofe$pval), " & ",
    format_coef(q05_up_fe$coef, q05_up_fe$pval), "\\\\  "
)
se_q05_row <- paste0(
    "                                    & ",
    format_se(q05_raj_nofe$se), " & ",
    format_se(q05_raj_fe$se), " & ",
    format_se(q05_up_nofe$se), " & ",
    format_se(q05_up_fe$se), "\\\\  "
)

# Quota 2010 row
if (!is.null(stats_raj_nofe) && !is.null(stats_raj_nofe$vars$treat_2010)) {
    q10_raj_nofe <- stats_raj_nofe$vars$treat_2010
    q10_raj_fe <- stats_raj_fe$vars$treat_2010
} else {
    q10_raj_nofe <- list(coef = NA, se = NA, pval = NA)
    q10_raj_fe <- list(coef = NA, se = NA, pval = NA)
}
if (!is.null(stats_up_nofe) && !is.null(stats_up_nofe$vars$treat_2010)) {
    q10_up_nofe <- stats_up_nofe$vars$treat_2010
    q10_up_fe <- stats_up_fe$vars$treat_2010
} else {
    q10_up_nofe <- list(coef = NA, se = NA, pval = NA)
    q10_up_fe <- list(coef = NA, se = NA, pval = NA)
}

coef_q10_row <- paste0(
    "   $\\text{Quota}_{2010}$           & ",
    format_coef(q10_raj_nofe$coef, q10_raj_nofe$pval), " & ",
    format_coef(q10_raj_fe$coef, q10_raj_fe$pval), " & ",
    format_coef(q10_up_nofe$coef, q10_up_nofe$pval), " & ",
    format_coef(q10_up_fe$coef, q10_up_fe$pval), "\\\\  "
)
se_q10_row <- paste0(
    "                                    & ",
    format_se(q10_raj_nofe$se), " & ",
    format_se(q10_raj_fe$se), " & ",
    format_se(q10_up_nofe$se), " & ",
    format_se(q10_up_fe$se), "\\\\  "
)

# Interaction row
if (!is.null(stats_raj_nofe) && !is.null(stats_raj_nofe$vars$`treat_2005:treat_2010`)) {
    int_raj_nofe <- stats_raj_nofe$vars$`treat_2005:treat_2010`
    int_raj_fe <- stats_raj_fe$vars$`treat_2005:treat_2010`
} else {
    int_raj_nofe <- list(coef = NA, se = NA, pval = NA)
    int_raj_fe <- list(coef = NA, se = NA, pval = NA)
}
if (!is.null(stats_up_nofe) && !is.null(stats_up_nofe$vars$`treat_2005:treat_2010`)) {
    int_up_nofe <- stats_up_nofe$vars$`treat_2005:treat_2010`
    int_up_fe <- stats_up_fe$vars$`treat_2005:treat_2010`
} else {
    int_up_nofe <- list(coef = NA, se = NA, pval = NA)
    int_up_fe <- list(coef = NA, se = NA, pval = NA)
}

coef_int_row <- paste0(
    "   $\\text{Quota}_{2005} \\times \\text{Quota}_{2010}$ & ",
    format_coef(int_raj_nofe$coef, int_raj_nofe$pval), " & ",
    format_coef(int_raj_fe$coef, int_raj_fe$pval), " & ",
    format_coef(int_up_nofe$coef, int_up_nofe$pval), " & ",
    format_coef(int_up_fe$coef, int_up_fe$pval), "\\\\  "
)
se_int_row <- paste0(
    "                                    & ",
    format_se(int_raj_nofe$se), " & ",
    format_se(int_raj_fe$se), " & ",
    format_se(int_up_nofe$se), " & ",
    format_se(int_up_fe$se), "\\\\  "
)

# R2 row
r2_row <- paste0(
    "   R$^2$                            & ",
    format_r2(stats_raj_nofe$r2), " & ",
    format_r2(stats_raj_fe$r2), " & ",
    format_r2(stats_up_nofe$r2), " & ",
    format_r2(stats_up_fe$r2), "\\\\  "
)

# Observations row
n_row <- paste0(
    "   Observations                     & ",
    format_n(stats_raj_nofe$n), " & ",
    format_n(stats_raj_fe$n), " & ",
    format_n(stats_up_nofe$n), " & ",
    format_n(stats_up_fe$n), "\\\\  "
)

# FE row
fe_row <- paste0(
    "   (District, Samiti) FE            & ",
    " & ", if (!is.null(m_raj_lt_fe)) "$\\checkmark$" else "", " & ",
    " & ", if (!is.null(m_up_lt_fe)) "$\\checkmark$" else "", "\\\\  "
)

tex_lines <- c(tex_lines,
    coef_q05_row, se_q05_row,
    coef_q10_row, se_q10_row,
    coef_int_row, se_int_row,
    r2_row, n_row, fe_row)

tex_lines <- c(tex_lines,
    "   \\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes_text, "}")
)

writeLines(tex_lines, here("tabs", "long_term_random_rotation.tex"))
cat("Created: tabs/long_term_random_rotation.tex\n")

# =============================================================================
# Build 15-year output table (long_term_random_rotation_full.tex)
# =============================================================================
cat("\n=== Generating 15-year output table ===\n")

treat_vars_full <- c("treat_2005", "treat_2010", "treat_2015",
                     "treat_2005:treat_2010", "treat_2005:treat_2015",
                     "treat_2010:treat_2015", "treat_2005:treat_2010:treat_2015")

stats_raj_full_nofe <- get_model_stats(m_raj_full_nofe, treat_vars_full)
stats_raj_full_fe <- get_model_stats(m_raj_full_fe, treat_vars_full)
stats_up_full_nofe <- get_model_stats(m_up_full_nofe, treat_vars_full)
stats_up_full_fe <- get_model_stats(m_up_full_fe, treat_vars_full)

notes_text_full <- paste0(
    "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
    "Outcome: woman elected in open seat (2020/2021). ",
    "Sample restricted to districts where chi-square test fails to reject independence of quota assignment ",
    "for ALL THREE transitions: 05$\\rightarrow$10, 10$\\rightarrow$15, AND 15$\\rightarrow$20/21 (p $>$ 0.05). ",
    "Districts retained: Raj: ", length(random_raj_all), "/", nrow(chisq_raj_0510), "; ",
    "UP: ", length(random_up_all), "/", nrow(chisq_up_0510), ". ",
    "Heteroskedasticity-robust standard errors."
)

tex_lines_full <- c(
    "{\\centering\\scriptsize",
    "\\begingroup",
    "\\begin{tabular}{@{}lcc@{\\hspace{1em}}cc@{}}",
    "   \\toprule",
    "    & \\multicolumn{2}{c}{Rajasthan} & \\multicolumn{2}{c}{Uttar Pradesh} \\\\ ",
    "   \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "    & \\multicolumn{2}{c}{05$\\rightarrow$20} & \\multicolumn{2}{c}{05$\\rightarrow$21} \\\\",
    "                                    & No FE   & FE            & No FE        & FE \\\\  ",
    "                                    & [i]     & [ii]          & [iii]        & [iv]\\\\  ",
    "   \\midrule "
)

# Helper to extract stats for a variable
get_var_stats <- function(stats_nofe, stats_fe, var_name) {
    if (!is.null(stats_nofe) && !is.null(stats_nofe$vars[[var_name]])) {
        nofe <- stats_nofe$vars[[var_name]]
    } else {
        nofe <- list(coef = NA, se = NA, pval = NA)
    }
    if (!is.null(stats_fe) && !is.null(stats_fe$vars[[var_name]])) {
        fe <- stats_fe$vars[[var_name]]
    } else {
        fe <- list(coef = NA, se = NA, pval = NA)
    }
    list(nofe = nofe, fe = fe)
}

# Quota 2005 row
q05_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2005")
q05_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2005")
coef_q05_full_row <- paste0(
    "   $\\text{Quota}_{2005}$           & ",
    format_coef(q05_raj$nofe$coef, q05_raj$nofe$pval), " & ",
    format_coef(q05_raj$fe$coef, q05_raj$fe$pval), " & ",
    format_coef(q05_up$nofe$coef, q05_up$nofe$pval), " & ",
    format_coef(q05_up$fe$coef, q05_up$fe$pval), "\\\\  "
)
se_q05_full_row <- paste0(
    "                                    & ",
    format_se(q05_raj$nofe$se), " & ",
    format_se(q05_raj$fe$se), " & ",
    format_se(q05_up$nofe$se), " & ",
    format_se(q05_up$fe$se), "\\\\  "
)

# Quota 2010 row
q10_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2010")
q10_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2010")
coef_q10_full_row <- paste0(
    "   $\\text{Quota}_{2010}$           & ",
    format_coef(q10_raj$nofe$coef, q10_raj$nofe$pval), " & ",
    format_coef(q10_raj$fe$coef, q10_raj$fe$pval), " & ",
    format_coef(q10_up$nofe$coef, q10_up$nofe$pval), " & ",
    format_coef(q10_up$fe$coef, q10_up$fe$pval), "\\\\  "
)
se_q10_full_row <- paste0(
    "                                    & ",
    format_se(q10_raj$nofe$se), " & ",
    format_se(q10_raj$fe$se), " & ",
    format_se(q10_up$nofe$se), " & ",
    format_se(q10_up$fe$se), "\\\\  "
)

# Quota 2015 row
q15_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2015")
q15_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2015")
coef_q15_full_row <- paste0(
    "   $\\text{Quota}_{2015}$           & ",
    format_coef(q15_raj$nofe$coef, q15_raj$nofe$pval), " & ",
    format_coef(q15_raj$fe$coef, q15_raj$fe$pval), " & ",
    format_coef(q15_up$nofe$coef, q15_up$nofe$pval), " & ",
    format_coef(q15_up$fe$coef, q15_up$fe$pval), "\\\\  "
)
se_q15_full_row <- paste0(
    "                                    & ",
    format_se(q15_raj$nofe$se), " & ",
    format_se(q15_raj$fe$se), " & ",
    format_se(q15_up$nofe$se), " & ",
    format_se(q15_up$fe$se), "\\\\  "
)

# Quota 2005 x Quota 2010 row
int_0510_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2005:treat_2010")
int_0510_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2005:treat_2010")
coef_int_0510_row <- paste0(
    "   $\\text{Quota}_{2005} \\times \\text{Quota}_{2010}$ & ",
    format_coef(int_0510_raj$nofe$coef, int_0510_raj$nofe$pval), " & ",
    format_coef(int_0510_raj$fe$coef, int_0510_raj$fe$pval), " & ",
    format_coef(int_0510_up$nofe$coef, int_0510_up$nofe$pval), " & ",
    format_coef(int_0510_up$fe$coef, int_0510_up$fe$pval), "\\\\  "
)
se_int_0510_row <- paste0(
    "                                    & ",
    format_se(int_0510_raj$nofe$se), " & ",
    format_se(int_0510_raj$fe$se), " & ",
    format_se(int_0510_up$nofe$se), " & ",
    format_se(int_0510_up$fe$se), "\\\\  "
)

# Quota 2005 x Quota 2015 row
int_0515_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2005:treat_2015")
int_0515_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2005:treat_2015")
coef_int_0515_row <- paste0(
    "   $\\text{Quota}_{2005} \\times \\text{Quota}_{2015}$ & ",
    format_coef(int_0515_raj$nofe$coef, int_0515_raj$nofe$pval), " & ",
    format_coef(int_0515_raj$fe$coef, int_0515_raj$fe$pval), " & ",
    format_coef(int_0515_up$nofe$coef, int_0515_up$nofe$pval), " & ",
    format_coef(int_0515_up$fe$coef, int_0515_up$fe$pval), "\\\\  "
)
se_int_0515_row <- paste0(
    "                                    & ",
    format_se(int_0515_raj$nofe$se), " & ",
    format_se(int_0515_raj$fe$se), " & ",
    format_se(int_0515_up$nofe$se), " & ",
    format_se(int_0515_up$fe$se), "\\\\  "
)

# Quota 2010 x Quota 2015 row
int_1015_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2010:treat_2015")
int_1015_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2010:treat_2015")
coef_int_1015_row <- paste0(
    "   $\\text{Quota}_{2010} \\times \\text{Quota}_{2015}$ & ",
    format_coef(int_1015_raj$nofe$coef, int_1015_raj$nofe$pval), " & ",
    format_coef(int_1015_raj$fe$coef, int_1015_raj$fe$pval), " & ",
    format_coef(int_1015_up$nofe$coef, int_1015_up$nofe$pval), " & ",
    format_coef(int_1015_up$fe$coef, int_1015_up$fe$pval), "\\\\  "
)
se_int_1015_row <- paste0(
    "                                    & ",
    format_se(int_1015_raj$nofe$se), " & ",
    format_se(int_1015_raj$fe$se), " & ",
    format_se(int_1015_up$nofe$se), " & ",
    format_se(int_1015_up$fe$se), "\\\\  "
)

# Triple interaction row
int_triple_raj <- get_var_stats(stats_raj_full_nofe, stats_raj_full_fe, "treat_2005:treat_2010:treat_2015")
int_triple_up <- get_var_stats(stats_up_full_nofe, stats_up_full_fe, "treat_2005:treat_2010:treat_2015")
coef_triple_row <- paste0(
    "   $\\text{Quota}_{2005} \\times \\text{Quota}_{2010} \\times \\text{Quota}_{2015}$ & ",
    format_coef(int_triple_raj$nofe$coef, int_triple_raj$nofe$pval), " & ",
    format_coef(int_triple_raj$fe$coef, int_triple_raj$fe$pval), " & ",
    format_coef(int_triple_up$nofe$coef, int_triple_up$nofe$pval), " & ",
    format_coef(int_triple_up$fe$coef, int_triple_up$fe$pval), "\\\\  "
)
se_triple_row <- paste0(
    "                                    & ",
    format_se(int_triple_raj$nofe$se), " & ",
    format_se(int_triple_raj$fe$se), " & ",
    format_se(int_triple_up$nofe$se), " & ",
    format_se(int_triple_up$fe$se), "\\\\  "
)

# R2 row
r2_full_row <- paste0(
    "   R$^2$                            & ",
    format_r2(stats_raj_full_nofe$r2), " & ",
    format_r2(stats_raj_full_fe$r2), " & ",
    format_r2(stats_up_full_nofe$r2), " & ",
    format_r2(stats_up_full_fe$r2), "\\\\  "
)

# Observations row
n_full_row <- paste0(
    "   Observations                     & ",
    format_n(stats_raj_full_nofe$n), " & ",
    format_n(stats_raj_full_fe$n), " & ",
    format_n(stats_up_full_nofe$n), " & ",
    format_n(stats_up_full_fe$n), "\\\\  "
)

# FE row
fe_full_row <- paste0(
    "   (District, Samiti) FE            & ",
    " & ", if (!is.null(m_raj_full_fe)) "$\\checkmark$" else "", " & ",
    " & ", if (!is.null(m_up_full_fe)) "$\\checkmark$" else "", "\\\\  "
)

tex_lines_full <- c(tex_lines_full,
    coef_q05_full_row, se_q05_full_row,
    coef_q10_full_row, se_q10_full_row,
    coef_q15_full_row, se_q15_full_row,
    coef_int_0510_row, se_int_0510_row,
    coef_int_0515_row, se_int_0515_row,
    coef_int_1015_row, se_int_1015_row,
    coef_triple_row, se_triple_row,
    r2_full_row, n_full_row, fe_full_row)

tex_lines_full <- c(tex_lines_full,
    "   \\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes_text_full, "}")
)

writeLines(tex_lines_full, here("tabs", "long_term_random_rotation_full.tex"))
cat("Created: tabs/long_term_random_rotation_full.tex\n")

cat("\n=== Long-Term Random Rotation Analysis Complete ===\n")
