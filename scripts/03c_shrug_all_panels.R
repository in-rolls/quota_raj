# 03c_shrug_all_panels.R
# Create SHRUG-linked panels using LGD Block Panchayat matching.
#
# Strategy:
#   - Use 05-10 block-matched outputs as anchor mappings to LGD GP code
#   - Propagate LGD mapping to later panels via stable cross-year keys
#   - Aggregate SHRUG covariates at LGD GP level (all villages per LGD code)
#
# Output:
#   - data/raj/shrug_gp_raj_05_10_block.parquet
#   - data/raj/shrug_gp_raj_10_15_block.parquet
#   - data/raj/shrug_gp_raj_15_20_block.parquet
#   - data/raj/shrug_gp_raj_05_20_block.parquet
#   - data/up/shrug_gp_up_05_10_block.parquet
#   - data/up/shrug_gp_up_10_15_block.parquet
#   - data/up/shrug_gp_up_15_21_block.parquet
#   - data/up/shrug_gp_up_05_21_block.parquet
#   - data/crosswalks/audit/03c_shrug_lgd_aggregation_summary.csv
#   - data/crosswalks/audit/03c_cross_year_mapping_audit.csv
#   - data/crosswalks/audit/03c_shrug_lgd_aggregation_audit.md

library(tidyverse)
library(arrow)
library(here)

source(here("scripts/00_config.R"))
source(here("scripts/00_utils.R"))

message("=== Creating SHRUG-Linked Panels from Block Matching ===")

if (!dir.exists(here("data/crosswalks/active"))) {
    stop("Expected data/crosswalks/active directory with manual crosswalk inputs before running 03c.")
}
dir.create(here("data/crosswalks/audit"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("tabs"), showWarnings = FALSE)

required_manual_inputs <- c(
    here("data/crosswalks/active/raj_district_xwalk.csv"),
    here("data/crosswalks/active/raj_samiti_std.csv")
)

missing_manual <- required_manual_inputs[!file.exists(required_manual_inputs)]
if (length(missing_manual) > 0) {
    stop(
        "Missing required manual Rajasthan crosswalk inputs:\n",
        paste(missing_manual, collapse = "\n")
    )
}

# =============================================================================
# Helpers
# =============================================================================

sum_or_na <- function(x) {
    if (all(is.na(x))) {
        return(NA_real_)
    }
    sum(x, na.rm = TRUE)
}

weighted_or_mean <- function(x, w) {
    valid <- !is.na(x) & !is.na(w) & w > 0
    if (sum(valid) > 0) {
        return(stats::weighted.mean(x[valid], w[valid]))
    }
    if (all(is.na(x))) {
        return(NA_real_)
    }
    mean(x, na.rm = TRUE)
}

assert_single_target <- function(df, from_cols, to_col, label, outfile, strict = TRUE) {
    mapping <- df %>%
        distinct(across(all_of(c(from_cols, to_col))))

    collisions <- mapping %>%
        count(across(all_of(from_cols)), name = "n_targets") %>%
        filter(n_targets > 1)

    if (nrow(collisions) > 0) {
        details <- mapping %>%
            inner_join(collisions, by = from_cols) %>%
            arrange(desc(n_targets))
        write_csv(details, outfile)
        msg <- sprintf("%s has %d conflicting mappings. See %s", label, nrow(collisions), outfile)
        if (strict) {
            stop(msg)
        }
        warning(msg)
    }

    invisible(nrow(collisions))
}

# =============================================================================
# Load SHRUG Census data and LGD crosswalk
# =============================================================================

message("\nLoading SHRUG Census data...")
shrug_pca <- read_csv(here("data/shrug/shrug-pca01-csv/pc01_pca_clean_shrid.csv.zip"),
                      show_col_types = FALSE)
shrug_vd <- read_csv(here("data/shrug/shrug-vd01-csv/pc01_vd_clean_shrid.csv.zip"),
                     show_col_types = FALSE)

message("Loading SHRUG-LGD linkage...")
shrug_lgd_link <- read_csv(here("data/shrug_gp_xwalk/data/shrug_LGD_matched.csv"),
                           show_col_types = FALSE) %>%
    filter(!is.na(LGD_code), !is.na(shrid2)) %>%
    transmute(
        state_name = tolower(state_name),
        shrid2 = shrid2,
        LGD_code = as.numeric(LGD_code)
    )

# Build village-level covariate table with LGD code
shrug_village_covars <- shrug_lgd_link %>%
    left_join(shrug_pca, by = "shrid2") %>%
    left_join(shrug_vd, by = "shrid2")

num_cols <- names(shrug_village_covars)[sapply(shrug_village_covars, is.numeric)]
num_cols <- setdiff(num_cols, c("LGD_code", "shrid2"))

share_cols <- num_cols[str_detect(num_cols, "share|ratio|rate|pct|prop")]
count_cols <- setdiff(num_cols, share_cols)

if (!"pc01_pca_tot_p" %in% names(shrug_village_covars)) {
    stop("pc01_pca_tot_p is required for weighted share aggregation")
}

message("Aggregating SHRUG covariates to LGD GP level...")

agg_counts <- shrug_village_covars %>%
    group_by(state_name, LGD_code) %>%
    summarize(
        n_villages = n(),
        shrid2 = first(shrid2),
        across(all_of(count_cols), sum_or_na),
        .groups = "drop"
    )

if (length(share_cols) > 0) {
    agg_shares <- shrug_village_covars %>%
        group_by(state_name, LGD_code) %>%
        summarize(
            across(
                all_of(share_cols),
                ~ weighted_or_mean(.x, pc01_pca_tot_p)
            ),
            .groups = "drop"
        )

    shrug_lgd_covars <- agg_counts %>%
        left_join(agg_shares, by = c("state_name", "LGD_code"))
} else {
    shrug_lgd_covars <- agg_counts
}

aggregation_summary <- shrug_lgd_covars %>%
    group_by(state_name) %>%
    summarize(
        total_lgd_codes = n(),
        total_villages = sum(n_villages),
        multi_village_lgd_codes = sum(n_villages > 1),
        pct_multi_village = round(100 * multi_village_lgd_codes / total_lgd_codes, 1),
        median_villages = median(n_villages),
        max_villages = max(n_villages),
        .groups = "drop"
    ) %>%
    arrange(state_name)

write_csv(aggregation_summary, here("data/crosswalks/audit/03c_shrug_lgd_aggregation_summary.csv"))
message("Saved: data/crosswalks/audit/03c_shrug_lgd_aggregation_summary.csv")

summary_lines <- c(
    "# SHRUG-LGD Aggregation Audit",
    "",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "| State | LGD Codes | Villages | Multi-village LGD | % Multi-village | Median villages | Max villages |",
    "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(aggregation_summary))) {
    row <- aggregation_summary[i, ]
    summary_lines <- c(
        summary_lines,
        sprintf(
            "| %s | %d | %d | %d | %.1f | %.1f | %d |",
            row$state_name,
            row$total_lgd_codes,
            row$total_villages,
            row$multi_village_lgd_codes,
            row$pct_multi_village,
            row$median_villages,
            row$max_villages
        )
    )
}

summary_lines <- c(
    summary_lines,
    "",
    "Aggregation policy:",
    "- Count-like numeric variables: sum across villages in the LGD GP.",
    "- Share/rate variables (name pattern contains share/ratio/rate/pct/prop): weighted mean using `pc01_pca_tot_p`, fallback to unweighted mean."
)

writeLines(summary_lines, here("data/crosswalks/audit/03c_shrug_lgd_aggregation_audit.md"))
message("Saved: data/crosswalks/audit/03c_shrug_lgd_aggregation_audit.md")

raj_covars <- shrug_lgd_covars %>%
    filter(state_name == "rajasthan")

up_covars <- shrug_lgd_covars %>%
    filter(state_name == "uttar pradesh")

# =============================================================================
# Rajasthan mappings and panels
# =============================================================================

message("\n========================================")
message("=== RAJASTHAN SHRUG Panels ===")
message("========================================")

crosswalk_district <- read_csv(here("data/crosswalks/active/raj_district_xwalk.csv"), show_col_types = FALSE) %>%
    select(elex_district_raw, shrug_district) %>%
    rename(district_raw = elex_district_raw, district_std = shrug_district) %>%
    mutate(district_std = toupper(district_std))
crosswalk_samiti <- read_csv(here("data/crosswalks/active/raj_samiti_std.csv"), show_col_types = FALSE)

raj_block_05_10 <- read_parquet(here("data/raj/shrug_lgd_raj_elex_05_10_block.parquet"))
message("Block-matched 05-10 rows: ", nrow(raj_block_05_10))

raj_block_05_10 <- raj_block_05_10 %>%
    left_join(crosswalk_district, by = c("dist_name_new_2010" = "district_raw")) %>%
    mutate(district_std_2010 = ifelse(is.na(district_std), dist_name_new_2010, district_std)) %>%
    select(-district_std) %>%
    left_join(
        crosswalk_samiti %>% select(district_std, samiti_raw, samiti_std),
        by = c("district_std_2010" = "district_std", "samiti_name_new_2010" = "samiti_raw")
    ) %>%
    mutate(
        samiti_std_2010 = ifelse(is.na(samiti_std), samiti_name_new_2010, samiti_std),
        gp_std_2010 = normalize_string(gp_new_2010)
    ) %>%
    select(-samiti_std)

raj_shrug_mapping <- raj_block_05_10 %>%
    filter(!is.na(lgd_gp_code)) %>%
    mutate(match_key_2010 = make_match_key(district_std_2010, samiti_std_2010, gp_std_2010)) %>%
    select(
        match_key_2010,
        shrid2,
        lgd_gp_code, lgd_gp_name,
        lgd_block_code, lgd_block_name,
        lgd_district, match_type
    )

assert_single_target(
    raj_shrug_mapping,
    from_cols = c("match_key_2010"),
    to_col = "lgd_gp_code",
    label = "Rajasthan 2010 key -> LGD code",
    outfile = here("data/crosswalks/audit/03c_raj_key_mapping_collisions.csv")
)

raj_shrug_mapping <- raj_shrug_mapping %>%
    distinct(match_key_2010, .keep_all = TRUE)

message("Rajasthan SHRUG mapping rows: ", nrow(raj_shrug_mapping))

mapping_audit <- list(
    tibble(check = "raj_match_key_2010_to_lgd", conflicts = 0L)
)

# Panel 1: 05-10
raj_05_10 <- read_parquet(here("data/raj/raj_05_10.parquet")) %>%
    mutate(match_key_2010 = make_match_key(district_std_2010, samiti_std_2010, gp_std_2010)) %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(raj_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(raj_05_10, here("data/raj/shrug_gp_raj_05_10_block.parquet"))
message("Saved: data/raj/shrug_gp_raj_05_10_block.parquet")

# Panel 2: 10-15
raj_10_15 <- read_parquet(here("data/raj/raj_10_15.parquet")) %>%
    mutate(match_key_2010 = make_match_key(district_std_2010, samiti_std_2010, gp_std_2010)) %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(raj_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(raj_10_15, here("data/raj/shrug_gp_raj_10_15_block.parquet"))
message("Saved: data/raj/shrug_gp_raj_10_15_block.parquet")

# Panel 3: 15-20 (via 2015 -> 2010 key map)
raj_10_15_key_map <- raj_10_15 %>%
    mutate(match_key_2015 = make_match_key(district_std_2015, samiti_std_2015, gp_std_2015)) %>%
    select(match_key_2015, match_key_2010)

assert_single_target(
    raj_10_15_key_map,
    from_cols = c("match_key_2015"),
    to_col = "match_key_2010",
    label = "Rajasthan 2015 key -> 2010 key",
    outfile = here("data/crosswalks/audit/03c_raj_2015_to_2010_key_collisions.csv")
)

raj_10_15_key_map <- raj_10_15_key_map %>%
    distinct(match_key_2015, .keep_all = TRUE)

mapping_audit[[length(mapping_audit) + 1]] <- tibble(check = "raj_match_key_2015_to_2010", conflicts = 0L)

raj_15_20 <- read_parquet(here("data/raj/raj_15_20.parquet")) %>%
    mutate(match_key_2015 = make_match_key(district_std_2015, samiti_std_2015, gp_std_2015)) %>%
    left_join(raj_10_15_key_map, by = "match_key_2015") %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(raj_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(raj_15_20, here("data/raj/shrug_gp_raj_15_20_block.parquet"))
message("Saved: data/raj/shrug_gp_raj_15_20_block.parquet")

# Panel 4: 05-20 long-term
raj_05_20 <- read_parquet(here("data/raj/raj_05_20.parquet"))

if ("district_std_2010" %in% names(raj_05_20)) {
    raj_05_20 <- raj_05_20 %>%
        mutate(match_key_2010 = make_match_key(district_std_2010, samiti_std_2010, gp_std_2010))
} else if ("dist_name_new_2010" %in% names(raj_05_20)) {
    raj_05_20 <- raj_05_20 %>%
        mutate(match_key_2010 = make_match_key(dist_name_new_2010, samiti_name_new_2010, gp_new_2010))
} else if ("match_key" %in% names(raj_05_20)) {
    raj_05_20 <- raj_05_20 %>%
        mutate(match_key_2010 = match_key)
} else {
    stop("Could not derive match_key_2010 for raj_05_20")
}

raj_05_20 <- raj_05_20 %>%
    left_join(raj_shrug_mapping, by = "match_key_2010") %>%
    left_join(raj_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(raj_05_20, here("data/raj/shrug_gp_raj_05_20_block.parquet"))
message("Saved: data/raj/shrug_gp_raj_05_20_block.parquet")

# =============================================================================
# Uttar Pradesh mappings and panels
# =============================================================================

message("\n========================================")
message("=== UTTAR PRADESH SHRUG Panels ===")
message("========================================")

up_block_05_10 <- read_parquet(here("data/up/shrug_lgd_up_elex_05_10_block.parquet"))
message("UP block-matched 05-10 rows: ", nrow(up_block_05_10))

up_shrug_mapping <- up_block_05_10 %>%
    filter(!is.na(lgd_gp_code)) %>%
    select(
        key_2010,
        shrid2,
        lgd_gp_code, lgd_gp_name,
        lgd_block_code, lgd_block_name,
        gp_match_type
    ) %>%
    rename(match_type = gp_match_type)

assert_single_target(
    up_shrug_mapping,
    from_cols = c("key_2010"),
    to_col = "lgd_gp_code",
    label = "UP 2010 key -> LGD code",
    outfile = here("data/crosswalks/audit/03c_up_key_mapping_collisions.csv")
)

up_shrug_mapping <- up_shrug_mapping %>%
    distinct(key_2010, .keep_all = TRUE)

mapping_audit[[length(mapping_audit) + 1]] <- tibble(check = "up_key_2010_to_lgd", conflicts = 0L)

# UP Panel 1: 05-10
up_05_10 <- read_parquet(here("data/up/up_05_10.parquet")) %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(up_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(up_05_10, here("data/up/shrug_gp_up_05_10_block.parquet"))
message("Saved: data/up/shrug_gp_up_05_10_block.parquet")

# UP Panel 2: 10-15 (via normalized 2010 name triplets)
up_10_15 <- read_parquet(here("data/up/up_10_15.parquet"))

up_05_10_key_map <- up_05_10 %>%
    filter(!is.na(key_2010)) %>%
    mutate(
        district_eng_std_2010 = normalize_string(district_name_eng_2010),
        block_eng_std_2010 = normalize_string(block_name_eng_2010),
        gp_eng_std_2010 = normalize_string(gp_name_eng_2010)
    ) %>%
    select(key_2010_map = key_2010, district_eng_std_2010, block_eng_std_2010, gp_eng_std_2010)

conflicts_up_triplet_2010 <- assert_single_target(
    up_05_10_key_map,
    from_cols = c("district_eng_std_2010", "block_eng_std_2010", "gp_eng_std_2010"),
    to_col = "key_2010_map",
    label = "UP normalized 2010 name triplet -> key_2010",
    outfile = here("data/crosswalks/audit/03c_up_2010_triplet_key_collisions.csv"),
    strict = FALSE
)

up_05_10_key_map <- up_05_10_key_map %>%
    group_by(district_eng_std_2010, block_eng_std_2010, gp_eng_std_2010) %>%
    filter(n() == 1) %>%
    ungroup()

mapping_audit[[length(mapping_audit) + 1]] <- tibble(
    check = "up_triplet2010_to_key2010",
    conflicts = as.integer(conflicts_up_triplet_2010)
)

up_10_15 <- up_10_15 %>%
    mutate(
        district_eng_std_2010 = normalize_string(district_name_eng_2010),
        block_eng_std_2010 = normalize_string(block_name_eng_2010),
        gp_eng_std_2010 = normalize_string(gp_name_eng_2010)
    ) %>%
    left_join(up_05_10_key_map, by = c("district_eng_std_2010", "block_eng_std_2010", "gp_eng_std_2010")) %>%
    mutate(key_2010 = coalesce(key_2010, key_2010_map)) %>%
    select(-district_eng_std_2010, -block_eng_std_2010, -gp_eng_std_2010) %>%
    select(-key_2010_map) %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(up_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(up_10_15, here("data/up/shrug_gp_up_10_15_block.parquet"))
message("Saved: data/up/shrug_gp_up_10_15_block.parquet")

# UP Panel 3: 15-21 (via normalized 2015 name triplets -> key_2010)
up_15_21 <- read_parquet(here("data/up/up_15_21.parquet"))

up_10_15_mapping <- up_10_15 %>%
    filter(!is.na(key_2010)) %>%
    mutate(
        district_eng_std_2015 = normalize_string(district_name_eng_2015),
        block_eng_std_2015 = normalize_string(block_name_eng_2015),
        gp_eng_std_2015 = normalize_string(gp_name_eng_2015)
    ) %>%
    select(district_eng_std_2015, block_eng_std_2015, gp_eng_std_2015, key_2010_map = key_2010)

conflicts_up_triplet_2015 <- assert_single_target(
    up_10_15_mapping,
    from_cols = c("district_eng_std_2015", "block_eng_std_2015", "gp_eng_std_2015"),
    to_col = "key_2010_map",
    label = "UP normalized 2015 name triplet -> key_2010",
    outfile = here("data/crosswalks/audit/03c_up_2015_triplet_key_collisions.csv"),
    strict = FALSE
)

up_10_15_mapping <- up_10_15_mapping %>%
    group_by(district_eng_std_2015, block_eng_std_2015, gp_eng_std_2015) %>%
    filter(n() == 1) %>%
    ungroup()

mapping_audit[[length(mapping_audit) + 1]] <- tibble(
    check = "up_triplet2015_to_key2010",
    conflicts = as.integer(conflicts_up_triplet_2015)
)

up_15_21 <- up_15_21 %>%
    mutate(
        district_eng_std_2015 = normalize_string(district_name_eng_2015),
        block_eng_std_2015 = normalize_string(block_name_eng_2015),
        gp_eng_std_2015 = normalize_string(gp_name_eng_2015)
    ) %>%
    left_join(up_10_15_mapping, by = c("district_eng_std_2015", "block_eng_std_2015", "gp_eng_std_2015")) %>%
    rename(key_2010 = key_2010_map) %>%
    select(-district_eng_std_2015, -block_eng_std_2015, -gp_eng_std_2015) %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(up_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(up_15_21, here("data/up/shrug_gp_up_15_21_block.parquet"))
message("Saved: data/up/shrug_gp_up_15_21_block.parquet")

# UP Panel 4: 05-21
up_05_21 <- read_parquet(here("data/up/up_05_21.parquet")) %>%
    left_join(up_shrug_mapping, by = "key_2010") %>%
    left_join(up_covars, by = c("lgd_gp_code" = "LGD_code"))

write_parquet(up_05_21, here("data/up/shrug_gp_up_05_21_block.parquet"))
message("Saved: data/up/shrug_gp_up_05_21_block.parquet")

# =============================================================================
# Mapping audit output
# =============================================================================

mapping_audit_df <- bind_rows(mapping_audit)
write_csv(mapping_audit_df, here("data/crosswalks/audit/03c_cross_year_mapping_audit.csv"))
message("Saved: data/crosswalks/audit/03c_cross_year_mapping_audit.csv")

message("\n=== Summary ===")
message("Created SHRUG-matched panel files using LGD-level aggregated covariates.")
message("Rajasthan and Uttar Pradesh panels refreshed.")
message("=== Done ===")
