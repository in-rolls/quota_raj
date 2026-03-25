# 00_config.R
# Central configuration for quota_raj project
# Variable dictionaries, helper functions, and shared settings
# Note: File paths use here() directly in scripts rather than centralized PATHS

library(here)
library(dplyr)
library(arrow)

# =============================================================================
# VARIABLE DICTIONARIES (for aer_etable)
# =============================================================================

DICT_RAJ_TREAT <- c(
    "treat_2005" = "$\\text{Quota}_{2005}$",
    "treat_2010" = "$\\text{Quota}_{2010}$",
    "treat_2015" = "$\\text{Quota}_{2015}$",
    "treat_2020" = "$\\text{Quota}_{2020}$",
    "female_res_2005" = "$\\text{Quota}_{2005}$",
    "female_res_2010" = "$\\text{Quota}_{2010}$",
    "female_res_2015" = "$\\text{Quota}_{2015}$",
    "female_res_2020" = "$\\text{Quota}_{2020}$"
)

DICT_UP_TREAT <- c(
    "treat_2005" = "$\\text{Quota}_{2005}$",
    "treat_2010" = "$\\text{Quota}_{2010}$",
    "treat_2015" = "$\\text{Quota}_{2015}$",
    "treat_2021" = "$\\text{Quota}_{2021}$"
)

DICT_FE_RAJ <- c(
    "dist_samiti_2020" = "(District, Samiti)",
    "dist_samiti_2015" = "(District, Samiti)",
    "dist_samiti_2010" = "(District, Samiti)"
)

DICT_FE_UP <- c(
    "dist_block_2021" = "(District, Samiti)",
    "dist_block_2015" = "(District, Samiti)",
    "dist_block_2010" = "(District, Samiti)"
)

DICT_DOSAGE <- c(
    "never_treated" = "Never Treated",
    "always_treated" = "Always Treated",
    "sometimes_treated" = "Sometimes Treated",
    "treatment_categoryAlways Treated" = "Always vs Never Treated",
    "treatment_categorySometimes Treated" = "Sometimes Treated",
    "count_treated" = "Num. Prior Quotas"
)

# Combined dictionaries for convenience
DICT_RAJ <- c(DICT_RAJ_TREAT, DICT_FE_RAJ, DICT_DOSAGE)
DICT_UP <- c(DICT_UP_TREAT, DICT_FE_UP, DICT_DOSAGE)


# =============================================================================
# NOTES TEMPLATES
# =============================================================================

NOTES_SIGNIF <- "$^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1."

NOTES_SHORT_TERM <- paste0(
    NOTES_SIGNIF,
    " The dependent variable is whether a woman was elected in an open seat. ",
    "The sample is restricted to GPs where the seat was open (not reserved for women) ",
    "in the outcome year. Heteroskedasticity-robust standard errors."
)

NOTES_LONG_TERM <- paste0(
    NOTES_SIGNIF,
    " The dependent variable is whether a woman was elected in an open seat. ",
    "Models include full interactions of treatment indicators across election cycles. ",
    "Heteroskedasticity-robust standard errors."
)

# =============================================================================
# PUBLICATION-QUALITY FIGURE SETTINGS
# =============================================================================

# Publication-quality ggplot2 theme (Jackman-inspired)
theme_pub <- function(base_size = 11, base_family = "") {
    ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "gray30", fill = NA, linewidth = 0.5),
        axis.ticks = ggplot2::element_line(color = "gray30", linewidth = 0.3),
        axis.text = ggplot2::element_text(color = "gray20"),
        axis.title = ggplot2::element_text(color = "gray10", face = "plain"),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.9)),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold", hjust = 0),
        plot.margin = ggplot2::margin(10, 10, 10, 10),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.1)),
        plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0)
    )
}

# Academic color palette
COLORS_PUB <- c(
    primary = "#2C3E50",
    secondary = "#7F8C8D",
    accent = "#C0392B",
    highlight = "#27AE60",
    light = "#BDC3C7"
)

# Standard figure dimensions (inches)
FIG_WIDTH_FULL <- 6.5
FIG_WIDTH_HALF <- 3.25
FIG_HEIGHT <- 4.5
