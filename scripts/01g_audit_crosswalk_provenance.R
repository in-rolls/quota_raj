# 01g_audit_crosswalk_provenance.R
# Audit crosswalk provenance, key integrity, and LGD validity.
# Output:
#   - data/crosswalks/audit/01g_crosswalk_provenance_audit.csv
#   - data/crosswalks/audit/01g_crosswalk_provenance_audit.md

library(readr)
library(dplyr)
library(tidyr)
library(here)

message("=== Crosswalk Provenance Audit ===")

dir.create(here("tabs"), showWarnings = FALSE)
dir.create(here("data/crosswalks/audit"), showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(here("data/crosswalks/active"))) {
    stop("Expected data/crosswalks/active directory to exist; manual crosswalk inputs should be provisioned before audit.")
}

required_inputs <- c(
    here("data/crosswalks/active/raj_district_xwalk.csv"),
    here("data/crosswalks/active/raj_samiti_std.csv"),
    here("data/crosswalks/active/raj_samiti_xwalk.csv"),
    here("data/crosswalks/active/up_district_xwalk.csv"),
    here("data/crosswalks/active/up_block_xwalk.csv"),
    here("data/lgd/processed/lgd_up_blocks.csv"),
    here("data/lgd/processed/lgd_up_block_gp.csv"),
    here("data/lgd/processed/lgd_raj_block_gp.csv")
)

missing_required <- required_inputs[!file.exists(required_inputs)]
if (length(missing_required) > 0) {
    stop(
        "Missing required crosswalk inputs:\n",
        paste(missing_required, collapse = "\n")
    )
}

crosswalk_catalog <- tibble::tribble(
    ~file, ~producer_script, ~consumer_scripts, ~key_cols,
    "data/lgd/processed/lgd_up_blocks.csv", "01d_up_extract_lgd.R", "01e_up_create_district_xwalk.R;01f_up_create_block_xwalk.R;03b_up_shrug_match.R", "block_code",
    "data/lgd/processed/lgd_up_block_gp.csv", "01d_up_extract_lgd.R", "03b_up_shrug_match.R", "gp_code",
    "data/crosswalks/active/up_district_xwalk.csv", "01e_up_create_district_xwalk.R", "01f_up_create_block_xwalk.R", "elex_district",
    "data/crosswalks/active/up_block_xwalk.csv", "01f_up_create_block_xwalk.R", "03b_up_shrug_match.R", "elex_district,elex_block",
    "data/crosswalks/active/raj_district_xwalk.csv", "01b_raj_create_district_xwalk.R", "01c_raj_create_samiti_xwalk.R;02a_raj_recode.R;03c_shrug_all_panels.R", "elex_district_raw",
    "data/crosswalks/active/raj_samiti_std.csv", "01c_raj_create_samiti_xwalk.R", "02a_raj_recode.R;03c_shrug_all_panels.R", "district_std,samiti_raw",
    "data/crosswalks/active/raj_samiti_xwalk.csv", "01c_raj_create_samiti_xwalk.R", "03a_raj_shrug_match.R", "elex_district,elex_samiti",
    "data/lgd/processed/lgd_raj_block_gp.csv", "LGD extraction/manual import", "01c_raj_create_samiti_xwalk.R;03a_raj_shrug_match.R", "gp_code"
)

read_any_csv <- function(path) {
    read_csv(here(path), show_col_types = FALSE)
}

safe_n_unique_keys <- function(df, key_cols) {
    keys <- trimws(unlist(strsplit(key_cols, ",")))
    if (length(keys) == 0 || any(!keys %in% names(df))) return(NA_integer_)
    nrow(distinct(df, across(all_of(keys))))
}

safe_key_unique <- function(df, key_cols) {
    keys <- trimws(unlist(strsplit(key_cols, ",")))
    if (length(keys) == 0 || any(!keys %in% names(df))) return(NA)
    nrow(df) == nrow(distinct(df, across(all_of(keys))))
}

file_stats <- crosswalk_catalog %>%
    rowwise() %>%
    mutate(
        abs_path = here(file),
        exists = file.exists(abs_path),
        modified_time = if (exists) as.character(file.info(abs_path)$mtime) else NA_character_,
        n_rows = if (exists) nrow(read_any_csv(file)) else NA_integer_,
        n_unique_keys = if (exists) safe_n_unique_keys(read_any_csv(file), key_cols) else NA_integer_,
        key_is_unique = if (exists) safe_key_unique(read_any_csv(file), key_cols) else NA
    ) %>%
    ungroup()

# LGD validity checks for active matching crosswalks
lgd_up_blocks <- read_any_csv("data/lgd/processed/lgd_up_blocks.csv")
lgd_raj_block_gp <- read_any_csv("data/lgd/processed/lgd_raj_block_gp.csv")

up_block_xwalk <- read_any_csv("data/crosswalks/active/up_block_xwalk.csv")
up_district_xwalk <- read_any_csv("data/crosswalks/active/up_district_xwalk.csv")
raj_samiti_xwalk <- read_any_csv("data/crosswalks/active/raj_samiti_xwalk.csv")

up_block_invalid <- up_block_xwalk %>%
    anti_join(lgd_up_blocks %>% distinct(block_code), by = c("lgd_block_code" = "block_code")) %>%
    nrow()

up_district_name_invalid <- up_district_xwalk %>%
    anti_join(lgd_up_blocks %>% distinct(zp_name), by = c("lgd_district" = "zp_name")) %>%
    nrow()

up_district_code_invalid <- up_district_xwalk %>%
    anti_join(lgd_up_blocks %>% distinct(zp_code), by = c("lgd_zp_code" = "zp_code")) %>%
    nrow()

raj_block_invalid <- raj_samiti_xwalk %>%
    anti_join(lgd_raj_block_gp %>% distinct(block_code), by = c("lgd_block_code" = "block_code")) %>%
    nrow()

validity <- tibble::tribble(
    ~file, ~validity_check, ~invalid_rows,
    "data/crosswalks/active/up_block_xwalk.csv", "lgd_block_code in lgd_up_blocks$block_code", up_block_invalid,
    "data/crosswalks/active/up_district_xwalk.csv", "lgd_district in lgd_up_blocks$zp_name", up_district_name_invalid,
    "data/crosswalks/active/up_district_xwalk.csv", "lgd_zp_code in lgd_up_blocks$zp_code", up_district_code_invalid,
    "data/crosswalks/active/raj_samiti_xwalk.csv", "lgd_block_code in lgd_raj_block_gp$block_code", raj_block_invalid
)

audit <- file_stats %>%
    left_join(
        validity %>% group_by(file) %>% summarize(invalid_rows = sum(invalid_rows), .groups = "drop"),
        by = "file"
    ) %>%
    mutate(invalid_rows = coalesce(invalid_rows, 0L))

write_csv(audit, here("data/crosswalks/audit/01g_crosswalk_provenance_audit.csv"))
message("Saved: data/crosswalks/audit/01g_crosswalk_provenance_audit.csv")

# Markdown summary
summary_lines <- c(
    "# Crosswalk Provenance Audit",
    "",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Integrity Summary",
    sprintf("- Files audited: %d", nrow(audit)),
    sprintf("- Missing files: %d", sum(!audit$exists, na.rm = TRUE)),
    sprintf("- Files with non-unique keys: %d", sum(audit$key_is_unique == FALSE, na.rm = TRUE)),
    sprintf("- Files with LGD validity failures: %d", sum(audit$invalid_rows > 0, na.rm = TRUE)),
    "",
    "## LGD Validity Checks",
    "",
    "| File | Check | Invalid Rows |",
    "|---|---|---:|"
)

for (i in seq_len(nrow(validity))) {
    summary_lines <- c(
        summary_lines,
        sprintf("| %s | %s | %d |",
                validity$file[i], validity$validity_check[i], validity$invalid_rows[i])
    )
}

summary_lines <- c(
    summary_lines,
    "",
    "## Notes",
    "- `data/crosswalks/active/raj_district_xwalk.csv` and `data/crosswalks/active/raj_samiti_std.csv` are the active standardization crosswalks used for Rajasthan panel keys.",
    "- `data/crosswalks/active/raj_samiti_xwalk.csv` is the active LGD block matching crosswalk for Rajasthan.",
    "- `data/crosswalks/active/up_block_xwalk.csv` is the active LGD block matching crosswalk for UP.",
    "- LGD hierarchy reference files are read from `data/lgd/processed/`."
)

writeLines(summary_lines, here("data/crosswalks/audit/01g_crosswalk_provenance_audit.md"))
message("Saved: data/crosswalks/audit/01g_crosswalk_provenance_audit.md")

message("=== Crosswalk Provenance Audit Complete ===")
