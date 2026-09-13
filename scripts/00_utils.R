# Gen. Util. Functions

# Create match key from district, block/samiti, and GP components
make_match_key <- function(district, block, gp) {
  paste(tolower(trimws(district)), tolower(trimws(block)), tolower(trimws(gp)), sep = "_")
}

# =============================================================================
# TRANSITION MATRIX FUNCTIONS
# =============================================================================

#' Create transition matrix using tidyverse
#' @param data Data frame with treatment indicators
#' @param from Unquoted column name for "from" period
#' @param to Unquoted column name for "to" period
#' @return Matrix with from values as row names, to values as column names
make_transition_matrix <- function(data, from, to) {
  data %>%
    dplyr::count({{ from }}, {{ to }}) %>%
    tidyr::pivot_wider(
      names_from = {{ to }},
      values_from = n,
      values_fill = list(n = 0)
    ) %>%
    tibble::column_to_rownames(var = rlang::as_label(rlang::enquo(from))) %>%
    as.matrix()
}

# =============================================================================
# AER-STYLE TABLE FUNCTIONS
# =============================================================================

## AER-Style etable wrapper for fixest
library(fixest)

aer_etable <- function(models, file, dict = NULL, digits = 2, notes = NULL,
                       title = NULL, label = NULL, placement = "htbp",
                       headers = NULL, ...) {
  do.call(fixest::etable, c(list(
    models,
    tex = TRUE, file = file, replace = TRUE,
    caption = title, label = label, placement = placement,
    float = !is.null(title) || !is.null(label),
    style.tex = fixest::style.tex(
      "aer",
      model.format = "[i]", depvar.style = "*",
      fixef.suffix = " FE", fontsize = "scriptsize"
    ),
    digits = digits, digits.stats = digits, powerBelow = -10,
    fitstat = ~ r2 + n, se.row = FALSE, dict = dict,
    headers = if (is.null(headers)) list("auto") else headers,
    depvar = is.null(headers), notes = notes, tpt = TRUE, adjustbox = "max width=\\linewidth"
  ), list(...)))
}
