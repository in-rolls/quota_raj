# 04a_balance_electoral.R
# Balance tests on electoral variables (full sample, no SHRUG filtering)
# Tests whether 2010 treatment is balanced on 2005 characteristics

library(dplyr)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

cat("=== Electoral Balance Tests ===\n")

# Load panels
raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet"))
up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

cat("Rajasthan N:", nrow(raj_05_10), "\n")
cat("UP N:", nrow(up_05_10), "\n")

# ===========================================================================
# Rajasthan Balance Tests
# ===========================================================================
cat("\n--- Rajasthan ---\n")

# Prior female winner
raj_fem_winner_test <- t.test(female_winner_2005 ~ treat_2010, data = raj_05_10)
cat("Prior female winner p-value:", round(raj_fem_winner_test$p.value, 4), "\n")

# Caste category (chi-squared)
raj_caste_tab <- table(raj_05_10$caste_category_2005, raj_05_10$treat_2010)
raj_caste_test <- chisq.test(raj_caste_tab)
cat("Caste category chi-sq p-value:", round(raj_caste_test$p.value, 4), "\n")

# ===========================================================================
# UP Balance Tests
# ===========================================================================
cat("\n--- Uttar Pradesh ---\n")

# Create female winner indicator
up_05_10 <- up_05_10 %>%
    mutate(female_winner_2005 = as.integer(cand_sex_fin_2005 == "\u092e\u0939\u093f\u0932\u093e"))

# Prior female winner
up_fem_winner_test <- t.test(female_winner_2005 ~ treat_2010, data = up_05_10)
cat("Prior female winner p-value:", round(up_fem_winner_test$p.value, 4), "\n")

# OBC status
up_obc_test <- t.test(obc_2005 ~ treat_2010, data = up_05_10)
cat("OBC status p-value:", round(up_obc_test$p.value, 4), "\n")

# Dalit status
up_dalit_test <- t.test(dalit_2005 ~ treat_2010, data = up_05_10)
cat("Dalit status p-value:", round(up_dalit_test$p.value, 4), "\n")

# ===========================================================================
# Generate LaTeX Table
# ===========================================================================
cat("\n--- Creating table ---\n")

fmt <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
fmt_int <- function(x) format(as.integer(x), big.mark = ",")

# Helper to get means by treatment
get_means <- function(data, var, treat_var = "treat_2010") {
    means <- tapply(data[[var]], data[[treat_var]], mean, na.rm = TRUE)
    c(open = unname(means["0"]), quota = unname(means["1"]))
}

# Rajasthan results
raj_fem <- get_means(raj_05_10, "female_winner_2005")
raj_caste_props <- prop.table(raj_caste_tab, 2)

# UP results
up_fem <- get_means(up_05_10, "female_winner_2005")
up_obc <- get_means(up_05_10, "obc_2005")
up_dalit <- get_means(up_05_10, "dalit_2005")

# Build table
tex_lines <- c(
    "{\\centering\\scriptsize",
    "\\begin{tabular}{@{}lrrrcrrrr@{}}",
    "\\toprule",
    "& \\multicolumn{3}{c}{Rajasthan} & & \\multicolumn{3}{c}{Uttar Pradesh} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){6-8}",
    "Variable & Open & Quota & p-value & & Open & Quota & p-value \\\\",
    "\\midrule",
    paste0("Prior female winner & ", fmt(raj_fem["open"]), " & ", fmt(raj_fem["quota"]), " & ",
           fmt(raj_fem_winner_test$p.value, 3), " & & ",
           fmt(up_fem["open"]), " & ", fmt(up_fem["quota"]), " & ",
           fmt(up_fem_winner_test$p.value, 3), " \\\\"),
    paste0("OBC reservation (2005) & -- & -- & -- & & ",
           fmt(up_obc["open"]), " & ", fmt(up_obc["quota"]), " & ",
           fmt(up_obc_test$p.value, 3), " \\\\"),
    paste0("SC/ST reservation (2005) & -- & -- & -- & & ",
           fmt(up_dalit["open"]), " & ", fmt(up_dalit["quota"]), " & ",
           fmt(up_dalit_test$p.value, 3), " \\\\"),
    "\\addlinespace",
    paste0("Caste category ($\\chi^2$) & \\multicolumn{2}{c}{(4 categories)} & ",
           fmt(raj_caste_test$p.value, 3), " & & -- & -- & -- \\\\"),
    "\\midrule",
    paste0("N & \\multicolumn{3}{c}{", fmt_int(nrow(raj_05_10)), "} & & ",
           "\\multicolumn{3}{c}{", fmt_int(nrow(up_05_10)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par}",
    "",
    "\\vspace{0.5ex}",
    paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes:} Balance tests for 2010 quota assignment ",
           "on 2005 electoral characteristics. ``Open'' = GPs not reserved for women in 2010; ",
           "``Quota'' = GPs reserved for women in 2010. This table uses the full electoral panel ",
           "(not SHRUG-filtered). p-values from t-tests except caste category ($\\chi^2$ test).}")
)

writeLines(tex_lines, here("tabs/balance_electoral.tex"))
cat("Created: tabs/balance_electoral.tex\n")

cat("\n=== Electoral Balance Tests Complete ===\n")
