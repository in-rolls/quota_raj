library(tidyverse)
library(here)

inference <- read_csv(here("tabs/model_inference.csv"), show_col_types = FALSE) |>
  filter(fixed_effects, variance == "cluster")
numbers <- character()
for (state in c("raj", "up")) {
  periods <- if (state == "raj") c("05_10", "10_15", "15_20") else c("05_10", "10_15", "15_21")
  for (i in seq_along(periods)) {
    row <- inference |> filter(.data$state == .env$state, period == periods[[i]], sample == "full")
    prefix <- paste0(state, "Short", c("One", "Two", "Three")[[i]])
    numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
    numbers[paste0(prefix, "Low")] <- sprintf("%.1f", 100 * row$conf_low)
    numbers[paste0(prefix, "High")] <- sprintf("%.1f", 100 * row$conf_high)
  }
  for (sample in c("full", "restricted")) {
    rows <- inference |> filter(
      .data$state == .env$state, .data$sample == .env$sample,
      contrast %in% c("111-000", "111-001")
    )
    for (contrast in c("111-000", "111-001")) {
      row <- rows |> filter(.data$contrast == .env$contrast)
      prefix <- paste0(
        state, if (sample == "full") "Full" else "Restricted",
        if (contrast == "111-000") "Always" else "Extra"
      )
      numbers[prefix] <- sprintf("%.1f", 100 * row$estimate)
      numbers[paste0(prefix, "Low")] <- sprintf("%.1f", 100 * row$bootstrap_low)
      numbers[paste0(prefix, "High")] <- sprintf("%.1f", 100 * row$bootstrap_high)
      numbers[paste0(prefix, "N")] <- format(row$n, big.mark = ",", scientific = FALSE, trim = TRUE)
      numbers[paste0(prefix, "LeftN")] <- format(row$left_n, big.mark = ",", trim = TRUE)
      numbers[paste0(prefix, "RightN")] <- format(row$right_n, big.mark = ",", trim = TRUE)
      numbers[paste0(prefix, "SharedClusters")] <- as.character(row$common_clusters)
    }
  }
}
short <- inference |> filter(sample == "full", !str_detect(contrast, "-"))
numbers["shortMdeMin"] <- sprintf("%.1f", 100 * min(short$mde_80))
numbers["shortMdeMax"] <- sprintf("%.1f", 100 * max(short$mde_80))
long <- inference |> filter(sample == "full", contrast == "111-000")
numbers["longMdeMin"] <- sprintf("%.1f", 100 * min(long$mde_80))
numbers["longMdeMax"] <- sprintf("%.1f", 100 * max(long$mde_80))
quality <- read_csv(here("tabs/candidate_characteristics.csv"), show_col_types = FALSE)
for (sample in c("Winners", "Women winners")) {
  for (variable in c("Age", "Graduate", "Unemployed", "Log assets")) {
    row <- quality |> filter(.data$sample == .env$sample, .data$variable == .env$variable)
    prefix <- paste0(if (sample == "Winners") "all" else "women", str_remove_all(variable, " "))
    factor <- if (variable %in% c("Graduate", "Unemployed")) 100 else 1
    numbers[paste0(prefix, "Open")] <- sprintf("%.1f", factor * row$open)
    numbers[paste0(prefix, "Quota")] <- sprintf("%.1f", factor * row$quota)
  }
}
stopifnot(length(numbers) > 0, !anyNA(numbers), !anyDuplicated(names(numbers)))
writeLines(paste0("\\newcommand{\\", names(numbers), "}{", numbers, "}"), here("tabs/numbers.tex"))
