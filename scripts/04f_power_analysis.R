library(dplyr)
library(ggplot2)
library(here)
source(here("scripts/00_config.R"))

inference <- read.csv(here("tabs/model_inference.csv"))
power <- inference %>% filter(fixed_effects, variance == "cluster", sample == "full")
write.csv(power, here("tabs/power_analysis.csv"), row.names = FALSE)
display <- power %>% transmute(
  State = state, Period = period, Contrast = contrast, N = n,
  `Estimate (pp)` = 100 * estimate, `Cluster SE (pp)` = 100 * standard_error,
  `95% CI (pp)` = sprintf("[%.2f, %.2f]", 100 * conf_low, 100 * conf_high),
  `Approx. MDE (pp)` = 100 * mde_80
)
tex <- knitr::kable(display,
  format = "latex", booktabs = TRUE, digits = 2,
  caption = "Precision of the fitted short-run models and cumulative contrasts", label = "power_analysis"
) %>%
  kableExtra::kable_styling(font_size = 8, latex_options = "scale_down")
writeLines(
  c(
    as.character(tex),
    "\\par\\smallskip{\\footnotesize Normal-approximation MDEs use 80\\% power and two-sided 5\\% size, computed from each fitted model or full-covariance linear contrast. They are precision summaries, not evidence of a zero effect or exact bootstrap power.}"
  ),
  here("tabs/power_analysis.tex")
)
plot <- ggplot(power, aes(x = 100 * mde_80, y = interaction(state, period, contrast, sep = " "))) +
  geom_point() +
  labs(x = "Approximate minimum detectable effect (percentage points)", y = NULL) +
  theme_pub()
ggsave(here("figs/mde_plot.pdf"), plot, width = 8, height = 6)
