# Gen. Util. Functions

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

# Constant term 
cons_term <- "Statistical significance symbols for the constant terms are suppressed."

custom_stargazer <- function(models, notes, digits = 2, float.env = "table", ..., out = NULL) {
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE,
               type = "latex",
               model.names = FALSE,
               omit.stat = c("rsq", "ser", "f"),
               digits = digits,
               column.sep.width = "0pt",
               dep.var.caption = "",
               dep.var.labels.include = FALSE,
               star.cutoffs = c(0.05, 0.01, 0.001),
               report = "vc*s",
               no.space = TRUE,
               single.row = FALSE,
               font.size = "scriptsize",
               notes.append = FALSE,
               notes = NULL,
               notes.align = "l",
               ...
          )
     )
     
     repeat {
          new_output <- gsub(
               pattern = "(Constant.*?)(\\$.*?\\$)",
               replacement = "\\1",
               x = stargazer_output,
               perl = TRUE
          )
          if(identical(new_output, stargazer_output)) break
          stargazer_output <- new_output
     }
     
     
     
     # Remove any existing table or sidewaystable environments
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\begin\\{sidewaystable\\}|\\\\end\\{sidewaystable\\}", stargazer_output)
     ]
     
     # Generate the output for `table`
     if (float.env == "table") {
          wrapped_output <- paste0(
               "\\begin{table}[!htbp]\n",
               "\\centering\n",
               "\\begin{threeparttable}\n",
               paste(stargazer_output, collapse = "\n"),
               "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
               paste0("\\item[] ", notes, collapse = "\n"),
               "\n\\end{tablenotes}\n",
               "\\end{threeparttable}\n",
               "\\end{table}"
          )
     }
     # Generate the output for `sidewaystable`
     else if (float.env == "sidewaystable") {
          wrapped_output <- paste0(
               "\\begin{sidewaystable}[!htbp]\n",
               "\\centering\n",
               "\\begin{threeparttable}\n",
               paste(stargazer_output, collapse = "\n"),
               "\n\\begin{tablenotes}[flushleft]\n\\setlength{\\itemindent}{0em}\n\\scriptsize\n",
               paste0("\\item[] ", notes, collapse = "\n"), # Add custom multiline notes
               "\n\\end{tablenotes}\n",
               "\\end{threeparttable}\n",
               "\\end{sidewaystable}"
          )
     } else {
          stop("Invalid float_env. Use 'table' or 'sidewaystable'.")
     }
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}