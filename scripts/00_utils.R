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

## Custom Stargazer

library(stargazer)

convert_sci_to_decimal <- function(text, digits = 2) {
    pattern <- "\\$(-?[0-9]+\\.?[0-9]*)\\\\times 10\\^\\{(-?[0-9]+)\\}\\$"
    matches <- gregexpr(pattern, text, perl = TRUE)
    if (matches[[1]][1] == -1) return(text)

    result <- text
    for (match_info in regmatches(text, matches)[[1]]) {
        parts <- regmatches(match_info, regexec(pattern, match_info, perl = TRUE))[[1]]
        mantissa <- as.numeric(parts[2])
        exponent <- as.numeric(parts[3])
        value <- mantissa * (10 ^ exponent)
        formatted <- sprintf(paste0("%.", digits, "f"), value)
        result <- sub(match_info, formatted, result, fixed = TRUE)
    }
    return(result)
}

aggressive_round <- function(x, digits = 2) {
    x <- sapply(x, convert_sci_to_decimal, digits = digits)
    x <- gsub("([0-9])\\.([0-9]{2})[0-9]+", "\\1.\\2", x)
    x <- gsub(" \\.([0-9]{2})[0-9]+", " .\\2", x)
    return(x)
}

# Create a combined balance/comparison table from multiple data frames
# Each df should have columns: Variable, and then comparison columns (e.g., Open, Quota, Diff)
make_balance_table <- function(dfs, group_names, notes = NULL, out = NULL,
                               colsep = "1em", var_col = "Variable") {
    if (length(dfs) != length(group_names)) {
        stop("Number of data frames must match number of group names")
    }

    # Get variable names from first df
    variables <- dfs[[1]][[var_col]]
    n_vars <- length(variables)

    # Get comparison column names (everything except Variable)
    comp_cols <- setdiff(names(dfs[[1]]), var_col)
    n_comp <- length(comp_cols)
    n_groups <- length(dfs)

    # Build column spec with spacing between groups
    col_spec <- paste0("@{}l", paste(rep(paste0(paste(rep("c", n_comp), collapse = ""), "@{\\hspace{", colsep, "}}"), n_groups - 1), collapse = ""), paste(rep("c", n_comp), collapse = ""), "@{}")

    # Build header rows
    group_header <- paste0("    & ", paste(sapply(seq_along(group_names), function(i) {
        paste0("\\multicolumn{", n_comp, "}{c}{", group_names[i], "}")
    }), collapse = " & "), " \\\\")

    cmidrule_row <- paste0("   ", paste(sapply(seq_along(group_names), function(i) {
        start <- 2 + (i - 1) * n_comp
        end <- start + n_comp - 1
        paste0("\\cmidrule(lr){", start, "-", end, "}")
    }), collapse = " "))

    col_header <- paste0("   ", var_col, " & ", paste(rep(comp_cols, n_groups), collapse = " & "), " \\\\")

    # Build data rows
    data_rows <- sapply(1:n_vars, function(i) {
        values <- sapply(dfs, function(df) {
            sapply(comp_cols, function(col) {
                val <- df[[col]][i]
                if (is.numeric(val)) sprintf("%.2f", val) else as.character(val)
            })
        })
        paste0("   ", variables[i], " & ", paste(as.vector(values), collapse = " & "), "\\\\")
    })

    # Assemble table
    tex_lines <- c(
        "{\\centering\\scriptsize",
        "\\begingroup",
        paste0("\\begin{tabular}{", col_spec, "}"),
        "   \\toprule",
        group_header,
        cmidrule_row,
        col_header,
        "   \\midrule",
        data_rows,
        "   \\bottomrule",
        "\\end{tabular}",
        "\\par\\endgroup",
        "\\par}"
    )

    if (!is.null(notes)) {
        tex_lines <- c(tex_lines, "", "\\vspace{0.5ex}",
            paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes, "}"))
    }

    if (!is.null(out)) {
        writeLines(tex_lines, out)
    }
    invisible(tex_lines)
}

custom_stargazer <- function(..., notes = NULL, digits = 2, out = NULL,
                             title = NULL, label = NULL, placement = "htbp",
                             colnames = NULL) {
     st_output <- capture.output(
          stargazer(...,
                    type = "latex",
                    digits = digits,
                    header = FALSE,
                    floating = FALSE,
                    notes.append = FALSE)
     )

     st_output <- aggressive_round(st_output)

     # Escape underscores in text (not in math mode or LaTeX commands)
     # This handles underscores in variable names like "Total_Population"
     st_output <- gsub("([^\\\\])_", "\\1\\\\_", st_output)

     # Convert stargazer's \textasteriskcentered to proper superscript notation
     st_output <- gsub("\\\\textasteriskcentered \\\\textasteriskcentered \\\\textasteriskcentered", "$^{***}$", st_output)
     st_output <- gsub("\\\\textasteriskcentered \\\\textasteriskcentered", "$^{**}$", st_output)
     st_output <- gsub("\\\\textasteriskcentered", "$^{*}$", st_output)

     # Clean up stargazer noise
     st_output <- st_output[!grepl("\\\\textit\\{Note:", st_output)]
     st_output <- st_output[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\centering", st_output)]
     st_output <- st_output[!grepl("\\\\caption\\{", st_output)]
     st_output <- st_output[!grepl("\\\\label\\{", st_output)]

     # Keep only the first tabular environment (stargazer bug creates spurious ones)
     begin_idx <- grep("\\\\begin\\{tabular\\}", st_output)
     end_idx <- grep("\\\\end\\{tabular\\}", st_output)
     if (length(begin_idx) > 1 && length(end_idx) >= 1) {
          st_output <- st_output[1:end_idx[1]]
     }

     # Replace \hline with booktabs
     hline_indices <- grep("\\\\hline", st_output)
     if (length(hline_indices) >= 2) {
          st_output[hline_indices[1]] <- gsub("\\\\hline", "\\\\toprule", st_output[hline_indices[1]])
          st_output[hline_indices[length(hline_indices)]] <- gsub("\\\\hline", "\\\\bottomrule", st_output[hline_indices[length(hline_indices)]])
          if (length(hline_indices) > 2) {
               for (i in 2:(length(hline_indices)-1)) {
                    st_output[hline_indices[i]] <- gsub("\\\\hline", "\\\\midrule", st_output[hline_indices[i]])
               }
          }
     }

     # Remove any existing scriptsize
     st_output <- st_output[!grepl("\\\\scriptsize", st_output)]

     # Insert column headers after \toprule if colnames is provided
     if (!is.null(colnames)) {
         toprule_idx <- grep("\\\\toprule", st_output)
         if (length(toprule_idx) > 0) {
             header_row <- paste0(paste(colnames, collapse = " & "), " \\\\ ")
             st_output <- c(st_output[1:toprule_idx[1]], header_row, st_output[(toprule_idx[1]+1):length(st_output)])
         }
     }

     # Build output with optional table wrapper
     if (!is.null(title) || !is.null(label)) {
         wrapped_output <- c(
             paste0("\\begin{table}[", placement, "]"),
             "\\centering"
         )
         if (!is.null(title)) {
             if (!is.null(label)) {
                 wrapped_output <- c(wrapped_output,
                     paste0("\\caption{\\label{", label, "}", title, "}"))
             } else {
                 wrapped_output <- c(wrapped_output,
                     paste0("\\caption{", title, "}"))
             }
         } else if (!is.null(label)) {
             wrapped_output <- c(wrapped_output,
                 paste0("\\label{", label, "}"))
         }
         wrapped_output <- c(wrapped_output, "{\\scriptsize", st_output, "}")
         if (!is.null(notes)) {
             wrapped_output <- c(wrapped_output,
                 "\\vspace{0.5ex}",
                 paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes, "}"))
         }
         wrapped_output <- c(wrapped_output, "\\end{table}")
     } else {
         wrapped_output <- c("{\\scriptsize", st_output, "}")
         if (!is.null(notes)) {
             wrapped_output <- c(wrapped_output,
                 "\\vspace{0.5ex}",
                 paste0("{\\scriptsize \\emph{Notes: } ", notes, "}"))
         }
     }

     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     }
     invisible(wrapped_output)
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
    if (nrow(lgd_block_gps) == 0) return(NULL)

    distances <- stringdist::stringdist(elex_row$elex_gp_std,
                                        lgd_block_gps$gp_name_std,
                                        method = "jw")

    best_dist <- min(distances)
    if (best_dist > threshold) return(NULL)

    tied_indices <- which(distances == best_dist)
    match_confidence <- "unique"
    tie_count <- length(tied_indices)

    if (tie_count > 1) {
        elex_first_char <- substr(elex_row$elex_gp_std, 1, 1)
        lgd_first_chars <- substr(lgd_block_gps$gp_name_std[tied_indices], 1, 1)
        first_char_matches <- tied_indices[lgd_first_chars == elex_first_char]

        if (length(first_char_matches) >= 1) {
            tied_indices <- first_char_matches
        }

        if (length(tied_indices) > 1) {
            tied_names <- lgd_block_gps$gp_name_std[tied_indices]
            best_idx <- tied_indices[order(tied_names)[1]]
        } else {
            best_idx <- tied_indices[1]
        }
        match_confidence <- "tie_resolved"
    } else {
        best_idx <- tied_indices[1]
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
# T-TEST HELPER FUNCTIONS
# =============================================================================

#' Run t-tests comparing treatment vs control groups
#' @param data Data frame with treatment indicator and outcome variables
#' @param vars Character vector of variable names to test
#' @param labels Character vector of display labels for variables
#' @param treat_var Name of the treatment indicator variable (default "treat")
#' @param na_string String to return when test cannot be run (default "--")
#' @param digits Number of decimal places (default 2)
#' @return tibble with Variable, Open (mean), Quota (mean), Diff. (with stars)
run_t_tests <- function(data, vars, labels, treat_var = "treat",
                        na_string = "--", digits = 2) {
    fmt <- paste0("%.", digits, "f")
    purrr::map2_dfr(vars, labels, function(var, label) {
        d <- data %>% dplyr::filter(!is.na(.data[[var]]))

        if (nrow(d) == 0 || length(unique(d[[treat_var]])) < 2) {
            return(tibble::tibble(
                Variable = label,
                Open = na_string,
                Quota = na_string,
                `Diff.` = na_string
            ))
        }

        fml <- stats::reformulate(treat_var, var)
        test <- stats::t.test(fml, data = d)
        stars <- dplyr::case_when(
            test$p.value < 0.01 ~ "$^{***}$",
            test$p.value < 0.05 ~ "$^{**}$",
            test$p.value < 0.1 ~ "$^{*}$",
            TRUE ~ ""
        )
        tibble::tibble(
            Variable = label,
            Open = sprintf(fmt, test$estimate[1]),
            Quota = sprintf(fmt, test$estimate[2]),
            `Diff.` = paste0(sprintf(fmt, test$estimate[1] - test$estimate[2]), stars)
        )
    })
}

#' Create a simple mean table for summary statistics (no comparison)
#' @param data Data frame
#' @param vars Character vector of variable names
#' @param labels Character vector of display labels
#' @param digits Number of decimal places (default 2)
#' @return tibble with Variable and Mean columns
make_mean_table <- function(data, vars, labels, digits = 2) {
    fmt <- paste0("%.", digits, "f")
    tibble::tibble(
        Variable = labels,
        Mean = sapply(vars, function(v) sprintf(fmt, mean(data[[v]], na.rm = TRUE)))
    )
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

# =============================================================================
# TABLE FORMATTING HELPERS (for manual LaTeX table construction)
# =============================================================================

format_coef_stars <- function(coef, pval, digits = 2) {
    if (is.null(coef) || is.na(coef)) return("---")
    stars <- ""
    if (!is.na(pval)) {
        if (pval < 0.01) stars <- "$^{***}$"
        else if (pval < 0.05) stars <- "$^{**}$"
        else if (pval < 0.1) stars <- "$^{*}$"
    }
    paste0(sprintf(paste0("%.", digits, "f"), coef), stars)
}

format_se_parens <- function(se, digits = 2) {
    if (is.null(se) || is.na(se)) return("")
    paste0("(", sprintf(paste0("%.", digits, "f"), se), ")")
}

format_r2_val <- function(r2, digits = 2) {
    if (is.null(r2) || is.na(r2)) return("---")
    sprintf(paste0("%.", digits, "f"), r2)
}

format_n_comma <- function(n) {
    if (is.null(n) || is.na(n)) return("---")
    format(n, big.mark = ",")
}

aer_etable <- function(models, file, dict = NULL, digits = 2, notes = NULL,
                       title = NULL, label = NULL, placement = "htbp",
                       cmidrules = NULL, colsep = NULL, headers = NULL, ...) {
    args <- list(models = models,
           tex = TRUE,
           style.tex = style.tex("aer", model.format = "[i]", depvar.style = "*"),
           digits = digits,
           digits.stats = digits,
           fitstat = ~ r2 + n,
           se.row = FALSE,
           dict = dict,
           interaction.combine = " $\\times $ ",
           ...)

    if (!is.null(headers)) {
        args$headers <- headers
    }

    res <- do.call(etable, args)
    res <- aggressive_round(res)

    # Replace "fixed effects" with "FE" for cleaner output
    res <- gsub(" fixed effects", " FE", res)

    # Collapse duplicate FE rows (same label, different columns get checkmarks)
    # Find rows with FE labels and merge them
    fe_pattern <- "^\\s*\\(.*\\) FE\\s*&"
    fe_indices <- grep(fe_pattern, res)
    if (length(fe_indices) > 1) {
        # Extract FE labels
        fe_labels <- sub("^\\s*(\\([^)]+\\) FE).*", "\\1", res[fe_indices])
        # Track which rows to remove (by content, not index)
        rows_to_remove <- c()
        # Group by label
        for (fe_label in unique(fe_labels)) {
            label_indices <- fe_indices[fe_labels == fe_label]
            if (length(label_indices) > 1) {
                # Merge rows: combine checkmarks from all rows with same label
                first_row <- res[label_indices[1]]
                for (idx in label_indices[-1]) {
                    # Replace empty cells with checkmarks from other rows
                    first_row <- gsub("&\\s*&", "& PLACEHOLDER &", first_row)
                    other_row <- res[idx]
                    rows_to_remove <- c(rows_to_remove, idx)
                    # Extract checkmarks positions from other row
                    if (grepl("\\$\\\\checkmark\\$", other_row)) {
                        # Find column positions with checkmarks in other row
                        parts_other <- strsplit(other_row, "&")[[1]]
                        parts_first <- strsplit(first_row, "&")[[1]]
                        for (i in seq_along(parts_other)) {
                            if (grepl("\\$\\\\checkmark\\$", parts_other[i]) &&
                                !grepl("\\$\\\\checkmark\\$", parts_first[i])) {
                                parts_first[i] <- parts_other[i]
                            }
                        }
                        first_row <- paste(parts_first, collapse = "&")
                    }
                }
                first_row <- gsub("PLACEHOLDER", "", first_row)
                res[label_indices[1]] <- first_row
            }
        }
        # Remove duplicate rows after all merging is done
        if (length(rows_to_remove) > 0) {
            res <- res[-rows_to_remove]
        }
    }

    # Strip everything except the tabular content
    res <- res[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\centering|\\\\caption|\\\\label", res)]

    # Remove any existing scriptsize
    res <- res[!grepl("\\\\scriptsize", res)]

    # Remove blank rows (lines that are just whitespace followed by \\)
    res <- res[!grepl("^\\s*\\\\\\\\\\s*$", res)]

    # When headers are provided, remove duplicate multicolumn rows (depvar row)
    # fixest generates both headers row and depvar row; keep only the first
    if (!is.null(headers)) {
        toprule_idx <- grep("\\\\toprule", res)
        midrule_idx <- grep("\\\\midrule", res)
        if (length(toprule_idx) > 0 && length(midrule_idx) > 0) {
            header_region <- (toprule_idx[1] + 1):(midrule_idx[1] - 1)
            multicolumn_rows <- which(grepl("\\\\multicolumn", res))
            multicolumn_in_header <- multicolumn_rows[multicolumn_rows %in% header_region]
            if (length(multicolumn_in_header) > 1) {
                # Remove depvar row (first), keep custom headers (second)
                rows_to_remove <- multicolumn_in_header[1]
                res <- res[-rows_to_remove]
            }
        }
    }

    # Fix column alignment: ensure all coefficient columns are properly centered
    # Replace tabular spec to add centering extracolsep
    res <- gsub("\\\\begin\\{tabular\\}\\{l(c+)\\}",
                "\\\\begin{tabular}{@{}l\\1@{}}", res)

    # Add column separator spacing if specified (e.g., colsep = list(after = 7, space = "1em"))
    if (!is.null(colsep)) {
        # Find the tabular spec and insert spacing
        tabular_line <- grep("\\\\begin\\{tabular\\}", res)
        if (length(tabular_line) > 0) {
            spec <- res[tabular_line[1]]
            # Count 'c's and insert @{\hspace{...}} after the specified column
            # Pattern: @{}l followed by c's, then @{}
            # We need to insert @{\hspace{Xem}} after column N (where N = colsep$after)
            n_cols <- colsep$after
            space <- if (!is.null(colsep$space)) colsep$space else "1em"
            # Build replacement: @{}l + n c's + @{\hspace{...}} + remaining c's + @{}
            pattern <- paste0("(\\\\begin\\{tabular\\}\\{@\\{\\}l", paste(rep("c", n_cols), collapse = ""), ")(c+)(@\\{\\}\\})")
            replacement <- paste0("\\1@{\\\\hspace{", space, "}}\\2\\3")
            res[tabular_line[1]] <- gsub(pattern, replacement, spec)
        }
    }

    # Ensure \end{tabular} is present (safety check)
    if (!any(grepl("\\\\end\\{tabular\\}", res))) {
        # Find bottomrule and add \end{tabular} after it
        bottomrule_idx <- grep("\\\\bottomrule", res)
        if (length(bottomrule_idx) > 0) {
            res <- c(res[1:bottomrule_idx[length(bottomrule_idx)]],
                     "\\end{tabular}",
                     if (bottomrule_idx[length(bottomrule_idx)] < length(res)) res[(bottomrule_idx[length(bottomrule_idx)]+1):length(res)] else NULL)
        }
    }

    # Add cmidrules if specified (e.g., cmidrules = list(after = 1, rules = c("2-7", "8-11")))
    if (!is.null(cmidrules)) {
        # Find lines after toprule that contain multicolumn (header rows)
        toprule_idx <- grep("\\\\toprule", res)
        if (length(toprule_idx) > 0) {
            # Count header rows after toprule (lines with multicolumn before midrule)
            midrule_idx <- grep("\\\\midrule", res)
            if (length(midrule_idx) > 0) {
                header_rows <- which(grepl("\\\\multicolumn", res) & seq_along(res) > toprule_idx[1] & seq_along(res) < midrule_idx[1])
                if (length(header_rows) >= cmidrules$after) {
                    insert_after <- header_rows[cmidrules$after]
                    cmidrule_line <- paste0("   ", paste(sapply(cmidrules$rules, function(r) paste0("\\cmidrule(lr){", r, "}")), collapse = " "))
                    res <- c(res[1:insert_after], cmidrule_line, res[(insert_after+1):length(res)])
                }
            }
        }
    }

    # Build output with optional table wrapper
    if (!is.null(title) || !is.null(label)) {
        # Full table environment with caption and label
        wrapped_output <- c(
            paste0("\\begin{table}[", placement, "]"),
            "\\centering"
        )
        if (!is.null(title)) {
            if (!is.null(label)) {
                wrapped_output <- c(wrapped_output,
                    paste0("\\caption{\\label{", label, "}", title, "}"))
            } else {
                wrapped_output <- c(wrapped_output,
                    paste0("\\caption{", title, "}"))
            }
        } else if (!is.null(label)) {
            wrapped_output <- c(wrapped_output,
                paste0("\\label{", label, "}"))
        }
        wrapped_output <- c(wrapped_output,
            "{\\centering\\scriptsize",
            res,
            "\\par}"
        )
        if (!is.null(notes)) {
            wrapped_output <- c(wrapped_output,
                "",
                "\\vspace{0.5ex}",
                paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes, "}")
            )
        }
        wrapped_output <- c(wrapped_output, "\\end{table}")
    } else {
        # Just tabular content wrapped in scriptsize and centered
        wrapped_output <- c(
            "{\\centering\\scriptsize",
            res,
            "\\par}"
        )
        if (!is.null(notes)) {
            wrapped_output <- c(wrapped_output,
                "",
                "\\vspace{0.5ex}",
                paste0("\\parbox{\\linewidth}{\\scriptsize \\emph{Notes: } ", notes, "}")
            )
        }
    }

    if (!is.null(file)) {
        writeLines(wrapped_output, con = file)
    }
    invisible(wrapped_output)
}
