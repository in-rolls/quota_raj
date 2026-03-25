# 07a_short_term_random_rotation.R
# Short-term effects analysis restricted to "random rotation" districts
# Districts where chi-square test fails to reject independence (p > 0.05)
# Output: tabs/short_term_random_rotation.tex

library(dplyr)
library(arrow)
library(fixest)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Short-Term Effects: Random Rotation Districts ===\n")

# =============================================================================
# Load all panels
# =============================================================================

raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet"))
raj_10_15 <- read_parquet(here("data/raj/raj_10_15.parquet"))
raj_15_20 <- read_parquet(here("data/raj/raj_15_20.parquet"))

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

cat("\nLoaded panels:\n")
cat("Raj 05-10:", nrow(raj_05_10), "GPs\n")
cat("Raj 10-15:", nrow(raj_10_15), "GPs\n")
cat("Raj 15-20:", nrow(raj_15_20), "GPs\n")
cat("UP 05-10:", nrow(up_05_10), "GPs\n")
cat("UP 10-15:", nrow(up_10_15), "GPs\n")
cat("UP 15-21:", nrow(up_15_21), "GPs\n")

# =============================================================================
# Create female_winner variables for UP
# Hindi: महिला = Female, पुरुष = Male
# =============================================================================

up_05_10 <- up_05_10 %>%
    mutate(
        female_winner_2010 = as.integer(cand_sex_fin_2010 == "महिला")
    )

up_10_15 <- up_10_15 %>%
    mutate(
        female_winner_2015 = as.integer(sex_2015 == "महिला")
    )

up_15_21 <- up_15_21 %>%
    mutate(
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
# Compute chi-square tests for each panel and identify random-rotation districts
# =============================================================================

cat("\n=== Computing Chi-Square Tests by District ===\n")

# Rajasthan panels
chisq_raj_0510 <- compute_district_chisq(raj_05_10, "treat_2005", "female_res_2010", "district_std_2010")
chisq_raj_1015 <- compute_district_chisq(raj_10_15, "treat_2010", "female_res_2015", "district_std_2015")
chisq_raj_1520 <- compute_district_chisq(raj_15_20, "treat_2015", "female_res_2020", "district_std_2020")

# UP panels
chisq_up_0510 <- compute_district_chisq(up_05_10, "treat_2005", "treat_2010", "district_name_eng_2010")
chisq_up_1015 <- compute_district_chisq(up_10_15, "treat_2010", "treat_2015", "district_name_eng_2015")
chisq_up_1521 <- compute_district_chisq(up_15_21, "treat_2015", "treat_2021", "district_name_eng_2021")

# Districts where p > 0.05 (fail to reject independence)
random_raj_0510 <- chisq_raj_0510 %>% filter(chisq_p > 0.05) %>% pull(district_std_2010)
random_raj_1015 <- chisq_raj_1015 %>% filter(chisq_p > 0.05) %>% pull(district_std_2015)
random_raj_1520 <- chisq_raj_1520 %>% filter(chisq_p > 0.05) %>% pull(district_std_2020)

random_up_0510 <- chisq_up_0510 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2010)
random_up_1015 <- chisq_up_1015 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2015)
random_up_1521 <- chisq_up_1521 %>% filter(chisq_p > 0.05) %>% pull(district_name_eng_2021)

cat("\nRandom-rotation districts retained:\n")
cat("Raj 05-10:", length(random_raj_0510), "of", nrow(chisq_raj_0510), "districts\n")
cat("Raj 10-15:", length(random_raj_1015), "of", nrow(chisq_raj_1015), "districts\n")
cat("Raj 15-20:", length(random_raj_1520), "of", nrow(chisq_raj_1520), "districts\n")
cat("UP 05-10:", length(random_up_0510), "of", nrow(chisq_up_0510), "districts\n")
cat("UP 10-15:", length(random_up_1015), "of", nrow(chisq_up_1015), "districts\n")
cat("UP 15-21:", length(random_up_1521), "of", nrow(chisq_up_1521), "districts\n")

# =============================================================================
# Rajasthan 2005 → 2010 (Open seats in 2010, random-rotation districts)
# =============================================================================
cat("\n--- Rajasthan 2005 → 2010 ---\n")

raj_05_10_rr <- raj_05_10 %>%
    filter(district_std_2010 %in% random_raj_0510, female_res_2010 == 0)
cat("Open seats in random-rotation districts:", nrow(raj_05_10_rr), "\n")

if (nrow(raj_05_10_rr) > 0) {
    m_raj_0510_nofe <- feols(female_winner_2010 ~ treat_2005, data = raj_05_10_rr)
    m_raj_0510_fe <- feols(female_winner_2010 ~ treat_2005 | dist_samiti_2010, data = raj_05_10_rr)
} else {
    m_raj_0510_nofe <- NULL
    m_raj_0510_fe <- NULL
}

# =============================================================================
# Rajasthan 2010 → 2015 (Open seats in 2015, random-rotation districts)
# =============================================================================
cat("\n--- Rajasthan 2010 → 2015 ---\n")

raj_10_15_rr <- raj_10_15 %>%
    filter(district_std_2015 %in% random_raj_1015, female_res_2015 == 0, !is.na(female_winner_2015))
cat("Open seats in random-rotation districts:", nrow(raj_10_15_rr), "\n")

if (nrow(raj_10_15_rr) > 0) {
    m_raj_1015_nofe <- feols(female_winner_2015 ~ treat_2010, data = raj_10_15_rr)
    m_raj_1015_fe <- feols(female_winner_2015 ~ treat_2010 | dist_samiti_2015, data = raj_10_15_rr)
} else {
    m_raj_1015_nofe <- NULL
    m_raj_1015_fe <- NULL
}

# =============================================================================
# Rajasthan 2015 → 2020 (Open seats in 2020, random-rotation districts)
# =============================================================================
cat("\n--- Rajasthan 2015 → 2020 ---\n")

raj_15_20_rr <- raj_15_20 %>%
    filter(district_std_2020 %in% random_raj_1520, female_res_2020 == 0, !is.na(female_winner_2020))
cat("Open seats in random-rotation districts:", nrow(raj_15_20_rr), "\n")

if (nrow(raj_15_20_rr) > 0) {
    m_raj_1520_nofe <- feols(female_winner_2020 ~ treat_2015, data = raj_15_20_rr)
    m_raj_1520_fe <- feols(female_winner_2020 ~ treat_2015 | dist_samiti_2020, data = raj_15_20_rr)
} else {
    m_raj_1520_nofe <- NULL
    m_raj_1520_fe <- NULL
}

# =============================================================================
# UP 2005 → 2010 (Open seats in 2010, random-rotation districts)
# =============================================================================
cat("\n--- UP 2005 → 2010 ---\n")

up_05_10_rr <- up_05_10 %>%
    filter(district_name_eng_2010 %in% random_up_0510, treat_2010 == 0)
cat("Open seats in random-rotation districts:", nrow(up_05_10_rr), "\n")

if (nrow(up_05_10_rr) > 0) {
    m_up_0510_nofe <- feols(female_winner_2010 ~ treat_2005, data = up_05_10_rr)
    m_up_0510_fe <- feols(female_winner_2010 ~ treat_2005 | dist_block_2010, data = up_05_10_rr)
} else {
    m_up_0510_nofe <- NULL
    m_up_0510_fe <- NULL
}

# =============================================================================
# UP 2010 → 2015 (Open seats in 2015, random-rotation districts)
# =============================================================================
cat("\n--- UP 2010 → 2015 ---\n")

up_10_15_rr <- up_10_15 %>%
    filter(district_name_eng_2015 %in% random_up_1015, treat_2015 == 0)
cat("Open seats in random-rotation districts:", nrow(up_10_15_rr), "\n")

if (nrow(up_10_15_rr) > 0) {
    m_up_1015_nofe <- feols(female_winner_2015 ~ treat_2010, data = up_10_15_rr)
    m_up_1015_fe <- feols(female_winner_2015 ~ treat_2010 | dist_block_2015, data = up_10_15_rr)
} else {
    m_up_1015_nofe <- NULL
    m_up_1015_fe <- NULL
}

# =============================================================================
# UP 2015 → 2021 (Open seats in 2021, random-rotation districts)
# =============================================================================
cat("\n--- UP 2015 → 2021 ---\n")

up_15_21_rr <- up_15_21 %>%
    filter(district_name_eng_2021 %in% random_up_1521, treat_2021 == 0)
cat("Open seats in random-rotation districts:", nrow(up_15_21_rr), "\n")

if (nrow(up_15_21_rr) > 0) {
    m_up_1521_nofe <- feols(female_winner_2021 ~ treat_2015, data = up_15_21_rr)
    m_up_1521_fe <- feols(female_winner_2021 ~ treat_2015 | dist_block_2021, data = up_15_21_rr)
} else {
    m_up_1521_nofe <- NULL
    m_up_1521_fe <- NULL
}

# =============================================================================
# Combined Output Table: All panels, NO FE and FE
# Always include all 12 columns to match short_term_combined.tex
# =============================================================================
cat("\n=== Generating output table ===\n")

# Track which panels have models
panels_info <- list(
    raj_0510 = list(nofe = m_raj_0510_nofe, fe = m_raj_0510_fe, label = "05$\\rightarrow$10", state = "raj"),
    raj_1015 = list(nofe = m_raj_1015_nofe, fe = m_raj_1015_fe, label = "10$\\rightarrow$15", state = "raj"),
    raj_1520 = list(nofe = m_raj_1520_nofe, fe = m_raj_1520_fe, label = "15$\\rightarrow$20", state = "raj"),
    up_0510 = list(nofe = m_up_0510_nofe, fe = m_up_0510_fe, label = "05$\\rightarrow$10", state = "up"),
    up_1015 = list(nofe = m_up_1015_nofe, fe = m_up_1015_fe, label = "10$\\rightarrow$15", state = "up"),
    up_1521 = list(nofe = m_up_1521_nofe, fe = m_up_1521_fe, label = "15$\\rightarrow$21", state = "up")
)

# Check which panels have valid models
valid_panels <- panels_info[sapply(panels_info, function(x) !is.null(x$nofe) && !is.null(x$fe))]
missing_panels <- panels_info[sapply(panels_info, function(x) is.null(x$nofe) || is.null(x$fe))]

if (length(valid_panels) == 0) {
    cat("ERROR: No models could be estimated. Check data availability.\n")
    quit(status = 1)
}

cat("Panels with valid models:", paste(names(valid_panels), collapse = ", "), "\n")
if (length(missing_panels) > 0) {
    cat("Panels without valid models (will show placeholders):", paste(names(missing_panels), collapse = ", "), "\n")
}

# Build model list from valid panels only
all_models <- unlist(lapply(valid_panels, function(x) list(x$nofe, x$fe)), recursive = FALSE)

# Generate table using etable for valid models, then manually add placeholder columns
dict_combined <- c(
    "female_winner_2010" = "Woman Elected",
    "female_winner_2015" = "Woman Elected",
    "female_winner_2020" = "Woman Elected",
    "female_winner_2021" = "Woman Elected",
    "treat_2005" = "$\\text{Quota}_{t-1}$",
    "treat_2010" = "$\\text{Quota}_{t-1}$",
    "treat_2015" = "$\\text{Quota}_{t-1}$",
    "(Intercept)" = "Intercept",
    "dist_samiti_2010" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_block_2010" = "(District, Block)",
    "dist_block_2015" = "(District, Block)",
    "dist_block_2021" = "(District, Block)"
)

notes_text <- paste0(
    "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1. ",
    "Outcome: woman elected in open seat. ",
    "Sample restricted to districts where chi-square test fails to reject independence of quota assignment across periods (p $>$ 0.05). ",
    "Districts retained: Raj 05$\\rightarrow$10: ", length(random_raj_0510), "/", nrow(chisq_raj_0510), "; ",
    "10$\\rightarrow$15: ", length(random_raj_1015), "/", nrow(chisq_raj_1015), "; ",
    "15$\\rightarrow$20: ", length(random_raj_1520), "/", nrow(chisq_raj_1520), "; ",
    "UP 05$\\rightarrow$10: ", length(random_up_0510), "/", nrow(chisq_up_0510), "; ",
    "10$\\rightarrow$15: ", length(random_up_1015), "/", nrow(chisq_up_1015), "; ",
    "15$\\rightarrow$21: ", length(random_up_1521), "/", nrow(chisq_up_1521), ". ",
    "Heteroskedasticity-robust standard errors."
)

# Generate fixed 12-column table manually to ensure consistency
# Extract coefficients and stats from valid models

get_model_stats <- function(m) {
    if (is.null(m)) return(NULL)
    coef_val <- coef(m)["treat_2005"]
    if (is.na(coef_val)) coef_val <- coef(m)["treat_2010"]
    if (is.na(coef_val)) coef_val <- coef(m)["treat_2015"]
    se_val <- sqrt(diag(vcov(m)))[names(coef_val)]
    pval <- 2 * pt(-abs(coef_val / se_val), df = m$nobs - length(coef(m)))
    r2 <- fitstat(m, "r2")$r2
    n <- m$nobs
    list(coef = coef_val, se = se_val, pval = pval, r2 = r2, n = n)
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

# Get stats for all panels
stats_raj_0510_nofe <- get_model_stats(m_raj_0510_nofe)
stats_raj_0510_fe <- get_model_stats(m_raj_0510_fe)
stats_raj_1015_nofe <- get_model_stats(m_raj_1015_nofe)
stats_raj_1015_fe <- get_model_stats(m_raj_1015_fe)
stats_raj_1520_nofe <- get_model_stats(m_raj_1520_nofe)
stats_raj_1520_fe <- get_model_stats(m_raj_1520_fe)
stats_up_0510_nofe <- get_model_stats(m_up_0510_nofe)
stats_up_0510_fe <- get_model_stats(m_up_0510_fe)
stats_up_1015_nofe <- get_model_stats(m_up_1015_nofe)
stats_up_1015_fe <- get_model_stats(m_up_1015_fe)
stats_up_1521_nofe <- get_model_stats(m_up_1521_nofe)
stats_up_1521_fe <- get_model_stats(m_up_1521_fe)

# Build 12-column table manually
tex_lines <- c(
    "{\\centering\\scriptsize",
    "\\begingroup",
    "\\begin{tabular}{@{}lcccccc@{\\hspace{1em}}cccccc@{}}",
    "   \\toprule",
    "    & \\multicolumn{6}{c}{Rajasthan} & \\multicolumn{6}{c}{Uttar Pradesh} \\\\ ",
    "   \\cmidrule(lr){2-7} \\cmidrule(lr){8-13}",
    "    & \\multicolumn{2}{c}{05$\\rightarrow$10} & \\multicolumn{2}{c}{10$\\rightarrow$15} & \\multicolumn{2}{c}{15$\\rightarrow$20} & \\multicolumn{2}{c}{05$\\rightarrow$10} & \\multicolumn{2}{c}{10$\\rightarrow$15} & \\multicolumn{2}{c}{15$\\rightarrow$21} \\\\ ",
    "                                    & No FE   & FE            & No FE        & FE            & No FE  & FE            & No FE        & FE            & No FE        & FE            & No FE        & FE \\\\  ",
    "                                    & [i]     & [ii]          & [iii]        & [iv]          & [v]    & [vi]          & [vii]        & [viii]        & [ix]         & [x]           & [xi]         & [xii]\\\\  ",
    "   \\midrule "
)

# Coefficient row
coef_row <- paste0(
    "   $\\text{Quota}_{t-1}$             & ",
    format_coef(stats_raj_0510_nofe$coef, stats_raj_0510_nofe$pval), "    & ",
    format_coef(stats_raj_0510_fe$coef, stats_raj_0510_fe$pval), "          & ",
    format_coef(stats_raj_1015_nofe$coef, stats_raj_1015_nofe$pval), " & ",
    format_coef(stats_raj_1015_fe$coef, stats_raj_1015_fe$pval), "  & ",
    format_coef(stats_raj_1520_nofe$coef, stats_raj_1520_nofe$pval), "   & ",
    format_coef(stats_raj_1520_fe$coef, stats_raj_1520_fe$pval), "          & ",
    format_coef(stats_up_0510_nofe$coef, stats_up_0510_nofe$pval), " & ",
    format_coef(stats_up_0510_fe$coef, stats_up_0510_fe$pval), "  & ",
    format_coef(stats_up_1015_nofe$coef, stats_up_1015_nofe$pval), "              & ",
    format_coef(stats_up_1015_fe$coef, stats_up_1015_fe$pval), " & ",
    format_coef(stats_up_1521_nofe$coef, stats_up_1521_nofe$pval), "        & ",
    format_coef(stats_up_1521_fe$coef, stats_up_1521_fe$pval), "\\\\  "
)

# SE row
se_row <- paste0(
    "                                    & ",
    format_se(stats_raj_0510_nofe$se), " & ",
    format_se(stats_raj_0510_fe$se), "       & ",
    format_se(stats_raj_1015_nofe$se), " & ",
    format_se(stats_raj_1015_fe$se), "        & ",
    format_se(stats_raj_1520_nofe$se), " & ",
    format_se(stats_raj_1520_fe$se), "        & ",
    format_se(stats_up_0510_nofe$se), "      & ",
    format_se(stats_up_0510_fe$se), "       & ",
    format_se(stats_up_1015_nofe$se), "               & ",
    format_se(stats_up_1015_fe$se), " & ",
    format_se(stats_up_1521_nofe$se), "        & ",
    format_se(stats_up_1521_fe$se), "\\\\  "
)

# R2 row
r2_row <- paste0(
    "   R$^2$                            & ",
    format_r2(stats_raj_0510_nofe$r2), "  & ",
    format_r2(stats_raj_0510_fe$r2), "          & ",
    format_r2(stats_raj_1015_nofe$r2), "  & ",
    format_r2(stats_raj_1015_fe$r2), "          & ",
    format_r2(stats_raj_1520_nofe$r2), "  & ",
    format_r2(stats_raj_1520_fe$r2), "          & ",
    format_r2(stats_up_0510_nofe$r2), "       & ",
    format_r2(stats_up_0510_fe$r2), "          & ",
    format_r2(stats_up_1015_nofe$r2), "  & ",
    format_r2(stats_up_1015_fe$r2), " & ",
    format_r2(stats_up_1521_nofe$r2), "        & ",
    format_r2(stats_up_1521_fe$r2), "\\\\  "
)

# Observations row
n_row <- paste0(
    "   Observations                     & ",
    format_n(stats_raj_0510_nofe$n), "   & ",
    format_n(stats_raj_0510_fe$n), "         & ",
    format_n(stats_raj_1015_nofe$n), "    & ",
    format_n(stats_raj_1015_fe$n), "           & ",
    format_n(stats_raj_1520_nofe$n), "  & ",
    format_n(stats_raj_1520_fe$n), "         & ",
    format_n(stats_up_0510_nofe$n), "       & ",
    format_n(stats_up_0510_fe$n), "        & ",
    format_n(stats_up_1015_nofe$n), "                & ",
    format_n(stats_up_1015_fe$n), " & ",
    format_n(stats_up_1521_nofe$n), "        & ",
    format_n(stats_up_1521_fe$n), "\\\\  "
)

# FE rows - check which models have which FE type
has_raj_fe <- !is.null(m_raj_0510_fe) || !is.null(m_raj_1015_fe) || !is.null(m_raj_1520_fe)
has_up_fe <- !is.null(m_up_0510_fe) || !is.null(m_up_1015_fe) || !is.null(m_up_1521_fe)

fe_samiti_row <- paste0(
    "   (District, Samiti) FE & ",
    " & ", if (!is.null(m_raj_0510_fe)) "$\\checkmark$" else "", "  & ",
    " & ", if (!is.null(m_raj_1015_fe)) "$\\checkmark$" else "", "  & ",
    " & ", if (!is.null(m_raj_1520_fe)) "$\\checkmark$" else "", "  & ",
    " & ", if (!is.null(m_up_0510_fe)) "$\\checkmark$" else "", "  & ",
    " & ", if (!is.null(m_up_1015_fe)) "$\\checkmark$" else "", "  & ",
    " & ", if (!is.null(m_up_1521_fe)) "$\\checkmark$" else "", "\\\\  "
)

tex_lines <- c(tex_lines, coef_row, se_row, r2_row, n_row, fe_samiti_row)

tex_lines <- c(tex_lines,
    "   \\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes_text, "}")
)

writeLines(tex_lines, here("tabs", "short_term_random_rotation.tex"))
cat("Created: tabs/short_term_random_rotation.tex\n")

cat("\n=== Random Rotation Analysis Complete ===\n")
