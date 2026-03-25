# fig_01_coefplots.R
# Generate publication-quality coefficient plots for main results
# Output: figs/short_term_coefplot.pdf, figs/short_term_coefplot_up.pdf,
#         figs/short_term_coefplot_combined.pdf, figs/long_term_coefplot.pdf

library(dplyr)
library(tidyr)
library(fixest)
library(arrow)
library(here)
library(ggplot2)
library(broom)

source(here("scripts/00_config.R"))

cat("=== Generating Coefficient Plots ===\n")

if (!dir.exists(here("figs"))) dir.create(here("figs"))

# =============================================================================
# RAJASTHAN SHORT-TERM MODELS
# =============================================================================

raj_panch <- read_parquet(here("data/raj/raj_05_20.parquet"))

cat("\n--- Rajasthan Short-term Effects ---\n")
m_raj_05_10 <- feols((sex_2010 == "F") ~ female_res_2005 | dist_samiti_2010,
                  data = filter(raj_panch, female_res_2010 == 0))
m_raj_10_15 <- feols((sex_manual_2015 == "F") ~ female_res_2010 | dist_samiti_2015,
                  data = filter(raj_panch, female_res_2015 == 0))
m_raj_15_20 <- feols((sex_2020 == "F") ~ female_res_2015 | dist_samiti_2020,
                  data = filter(raj_panch, female_res_2020 == 0))

tidy_model <- function(model, label, state = NULL) {
    df <- broom::tidy(model, conf.int = TRUE) %>%
        filter(!grepl("Intercept|dist_samiti|dist_block", term)) %>%
        mutate(model = label)
    if (!is.null(state)) df$state <- state
    df
}

coef_raj <- bind_rows(
    tidy_model(m_raj_05_10, "2005 → 2010", "Rajasthan"),
    tidy_model(m_raj_10_15, "2010 → 2015", "Rajasthan"),
    tidy_model(m_raj_15_20, "2015 → 2020", "Rajasthan")
)

coef_raj$model <- factor(coef_raj$model, levels = rev(c(
    "2005 → 2010",
    "2010 → 2015",
    "2015 → 2020"
)))

p_short_raj <- ggplot(coef_raj, aes(x = estimate, y = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0,
        color = COLORS_PUB["secondary"],
        linewidth = 0.6
    ) +
    geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
    labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
    theme_pub() +
    theme(axis.text.y = element_text(hjust = 0))

ggsave(
    here("figs", "short_term_coefplot.pdf"),
    p_short_raj,
    width = FIG_WIDTH_FULL,
    height = 3.5,
    device = cairo_pdf
)
cat("Saved: figs/short_term_coefplot.pdf\n")

# =============================================================================
# UP SHORT-TERM MODELS
# =============================================================================

cat("\n--- UP Short-term Effects ---\n")

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
up_all <- read_parquet(here("data/up/up_05_21.parquet"))

m_up_05_10 <- feols((cand_sex_fin_2010 == "महिला") ~ treat_2005 | dist_block_2010,
                    data = filter(up_05_10, treat_2010 == 0))
m_up_10_15 <- feols((sex_2015 == "महिला") ~ treat_2010 | I(paste0(district_name_eng_2015, "_", block_name_eng_2015)),
                    data = filter(up_all, treat_2015 == 0))

coef_up <- bind_rows(
    tidy_model(m_up_05_10, "2005 → 2010", "UP"),
    tidy_model(m_up_10_15, "2010 → 2015", "UP")
)

coef_up$model <- factor(coef_up$model, levels = rev(c(
    "2005 → 2010",
    "2010 → 2015"
)))

p_short_up <- ggplot(coef_up, aes(x = estimate, y = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0,
        color = COLORS_PUB["secondary"],
        linewidth = 0.6
    ) +
    geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
    labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
    theme_pub() +
    theme(axis.text.y = element_text(hjust = 0))

ggsave(
    here("figs", "short_term_coefplot_up.pdf"),
    p_short_up,
    width = FIG_WIDTH_FULL,
    height = 3,
    device = cairo_pdf
)
cat("Saved: figs/short_term_coefplot_up.pdf\n")

# =============================================================================
# COMBINED SHORT-TERM PLOT (FACETED BY STATE)
# =============================================================================

cat("\n--- Combined Short-term Effects ---\n")

coef_combined <- bind_rows(
    coef_raj %>% mutate(state = "Rajasthan"),
    coef_up %>% mutate(state = "Uttar Pradesh")
)

coef_combined$model <- factor(coef_combined$model, levels = rev(c(
    "2005 → 2010",
    "2010 → 2015",
    "2015 → 2020"
)))

coef_combined$state <- factor(coef_combined$state, levels = c("Rajasthan", "Uttar Pradesh"))

p_short_combined <- ggplot(coef_combined, aes(x = estimate, y = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0,
        color = COLORS_PUB["secondary"],
        linewidth = 0.6
    ) +
    geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
    facet_wrap(~state, ncol = 2) +
    labs(x = "Effect on Pr(Woman Elected)", y = NULL) +
    theme_pub() +
    theme(
        axis.text.y = element_text(hjust = 0),
        strip.text = element_text(face = "bold", size = 11)
    )

ggsave(
    here("figs", "short_term_coefplot_combined.pdf"),
    p_short_combined,
    width = FIG_WIDTH_FULL * 1.5,
    height = 3.5,
    device = cairo_pdf
)
cat("Saved: figs/short_term_coefplot_combined.pdf\n")

# =============================================================================
# RAJASTHAN LONG-TERM MODEL
# =============================================================================

cat("\n--- Long-term Effects ---\n")
m_long_term <- feols(
    (sex_2020 == "F") ~ treat_2015 * treat_2010 * treat_2005 | dist_samiti_2020,
    data = filter(raj_panch, treat_2020 == 0)
)

coef_long <- broom::tidy(m_long_term, conf.int = TRUE) %>%
    filter(!grepl("Intercept|dist_samiti", term)) %>%
    mutate(
        label = case_when(
            term == "treat_2015" ~ "Quota 2015",
            term == "treat_2010" ~ "Quota 2010",
            term == "treat_2005" ~ "Quota 2005",
            term == "treat_2015:treat_2010" ~ "2015 × 2010",
            term == "treat_2015:treat_2005" ~ "2015 × 2005",
            term == "treat_2010:treat_2005" ~ "2010 × 2005",
            term == "treat_2015:treat_2010:treat_2005" ~ "2015 × 2010 × 2005",
            TRUE ~ term
        )
    )

coef_long$label <- factor(coef_long$label, levels = rev(c(
    "Quota 2005",
    "Quota 2010",
    "Quota 2015",
    "2010 × 2005",
    "2015 × 2005",
    "2015 × 2010",
    "2015 × 2010 × 2005"
)))

p_long <- ggplot(coef_long, aes(x = estimate, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0,
        color = COLORS_PUB["secondary"],
        linewidth = 0.6
    ) +
    geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
    labs(x = "Effect on Pr(Woman Elected in 2020)", y = NULL) +
    theme_pub() +
    theme(axis.text.y = element_text(hjust = 0))

ggsave(
    here("figs", "long_term_coefplot.pdf"),
    p_long,
    width = FIG_WIDTH_FULL,
    height = 4.5,
    device = cairo_pdf
)
cat("Saved: figs/long_term_coefplot.pdf\n")

# =============================================================================
# TREATMENT PERSISTENCE PLOT
# =============================================================================

cat("\n--- Treatment Persistence ---\n")

persistence_data <- raj_panch %>%
    mutate(
        persist_05_10 = ifelse(female_res_2005 == 1, female_res_2010, NA),
        persist_10_15 = ifelse(female_res_2010 == 1, female_res_2015, NA),
        persist_15_20 = ifelse(female_res_2015 == 1, treat_2020, NA)
    ) %>%
    summarise(
        trans_05_10 = mean(persist_05_10, na.rm = TRUE),
        trans_10_15 = mean(persist_10_15, na.rm = TRUE),
        trans_15_20 = mean(persist_15_20, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "transition", values_to = "proportion") %>%
    mutate(
        transition = case_when(
            transition == "trans_05_10" ~ "2005 -> 2010",
            transition == "trans_10_15" ~ "2010 -> 2015",
            transition == "trans_15_20" ~ "2015 -> 2020"
        )
    )

persistence_data$transition <- factor(
    persistence_data$transition,
    levels = c("2005 -> 2010", "2010 -> 2015", "2015 -> 2020")
)

p_persist <- ggplot(persistence_data, aes(x = transition, y = proportion)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    geom_col(fill = COLORS_PUB["primary"], width = 0.6) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
        x = "Electoral Transition",
        y = "Proportion Remaining Quota"
    ) +
    annotate(
        "text",
        x = 0.6, y = 0.53,
        label = "Random (50%)",
        color = "gray50",
        hjust = 0,
        size = 3
    ) +
    theme_pub()

ggsave(
    here("figs", "treatment_persistence.pdf"),
    p_persist,
    width = FIG_WIDTH_FULL,
    height = FIG_HEIGHT,
    device = cairo_pdf
)
cat("Saved: figs/treatment_persistence.pdf\n")

# =============================================================================
# DOSAGE EFFECTS PLOT
# =============================================================================

cat("\n--- Dosage Effects ---\n")

raj_panch <- raj_panch %>%
    mutate(count_treated_label = factor(
        count_treated,
        levels = c(0, 1, 2, 3),
        labels = c("Never", "Once", "Twice", "Always")
    ))

m_dosage <- feols((sex_2020 == "F") ~ count_treated_label,
                  data = filter(raj_panch, treat_2020 == 0))

coef_dosage <- broom::tidy(m_dosage, conf.int = TRUE) %>%
    filter(!grepl("Intercept", term)) %>%
    mutate(
        label = case_when(
            term == "count_treated_labelOnce" ~ "Once",
            term == "count_treated_labelTwice" ~ "Twice",
            term == "count_treated_labelAlways" ~ "Always",
            TRUE ~ term
        )
    )

coef_dosage$label <- factor(coef_dosage$label,
                            levels = c("Once", "Twice", "Always"))

p_dosage <- ggplot(coef_dosage, aes(x = estimate, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0,
        color = COLORS_PUB["secondary"],
        linewidth = 0.6
    ) +
    geom_point(size = 2.5, color = COLORS_PUB["primary"]) +
    labs(
        x = "Effect on Pr(Woman Elected in 2020)",
        y = "Prior Quota Exposure",
        subtitle = "Reference: Never treated"
    ) +
    theme_pub() +
    theme(axis.text.y = element_text(hjust = 0))

ggsave(
    here("figs", "dosage_coefplot.pdf"),
    p_dosage,
    width = FIG_WIDTH_FULL,
    height = 3.5,
    device = cairo_pdf
)
cat("Saved: figs/dosage_coefplot.pdf\n")

cat("\n=== Coefficient Plots Complete ===\n")
