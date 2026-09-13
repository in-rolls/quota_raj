# Gen. Util. Functions

library(stringi)
library(stringdist)

# Create match key from district, block/samiti, and GP components
make_match_key <- function(district, block, gp) {
  paste(tolower(trimws(district)), tolower(trimws(block)), tolower(trimws(gp)), sep = "_")
}

# Remove diacritics, convert to lowercase, convert to single ws, trim extra ws, remove punct
normalize_string <- function(input_string) {
  normalized_string <- stri_trans_general(input_string, "Latin-ASCII")
  normalized_string <- stri_trans_tolower(normalized_string)
  normalized_string <- gsub("\\s+", " ", normalized_string)
  normalized_string <- trimws(normalized_string)
  normalized_string <- gsub("[[:punct:]]", "", normalized_string)
  return(normalized_string)
}

# =============================================================================
# FUZZY MATCHING FUNCTIONS
# =============================================================================

#' Fuzzy match GP names within a block
#' @param elex_row Single row from election data with elex_gp_std column
#' @param lgd_block_gps Data frame of LGD GPs in the same block with gp_name_std column
#' @param threshold Max JW distance for match (default 0.30)
#' @param id_col Name of the ID column in elex_row (e.g., "sl_no_2010" or "key_2010")
#' @param gp_col Name of the GP name column in elex_row (e.g., "gp_new_2010" or "gp_name_eng_2010")
#' @return tibble with match info or NULL if no match
fuzzy_match_within_block <- function(elex_row, lgd_block_gps, threshold = 0.30,
                                     id_col = "id", gp_col = "gp_name") {
  if (nrow(lgd_block_gps) == 0) {
    return(NULL)
  }

  distances <- stringdist::stringdist(elex_row$elex_gp_std,
    lgd_block_gps$gp_name_std,
    method = "jw"
  )

  if (all(is.na(distances))) {
    return(NULL)
  }
  best_dist <- min(distances, na.rm = TRUE)
  if (best_dist > threshold) {
    return(NULL)
  }

  tied_indices <- which(distances == best_dist)
  match_confidence <- "unique"
  tie_count <- length(tied_indices)

  if (tie_count != 1) {
    return(NULL)
  }
  best_idx <- tied_indices[[1]]

  # Check for numeric mismatch
  elex_numbers <- gsub("[^0-9]", "", elex_row$elex_gp_std)
  lgd_numbers <- gsub("[^0-9]", "", lgd_block_gps$gp_name_std[best_idx])

  if (nchar(elex_numbers) > 0 && nchar(lgd_numbers) > 0 && elex_numbers != lgd_numbers) {
    return(NULL)
  }

  result <- tibble::tibble(
    lgd_gp_code = lgd_block_gps$gp_code[best_idx],
    lgd_gp_name = lgd_block_gps$gp_name[best_idx],
    match_distance = best_dist,
    match_confidence = match_confidence,
    tie_count = tie_count
  )
  result[[id_col]] <- elex_row[[id_col]]
  result[[gp_col]] <- elex_row[[gp_col]]
  return(result)
}

#' Normalize string (strict version - removes ALL whitespace)
#' Use for block/GP matching where spaces should be ignored
#' @param input_string Character vector to normalize
#' @return Normalized string with no spaces
normalize_string_strict <- function(input_string) {
  normalized_string <- stri_trans_general(input_string, "Latin-ASCII")
  normalized_string <- stri_trans_tolower(normalized_string)
  normalized_string <- gsub("[[:space:]]+", "", normalized_string)
  normalized_string <- gsub("[[:punct:]]", "", normalized_string)
  return(normalized_string)
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
