# 01d_raj_standardize_samiti_names.R
# Generate samiti crosswalk from standardized source files
# Input: data/raj/source_YYYY_std.parquet, data/raj/crosswalk_district.csv
# Output: data/raj/crosswalk_samiti.csv

library(dplyr)
library(readr)
library(arrow)
library(here)

source(here("scripts/00_config.R"))

cat("=== Creating Samiti Crosswalk ===\n")

# =============================================================================
# Load source data and extract unique samitis
# =============================================================================

src_2005 <- read_parquet(here("data/raj/source_2005_std.parquet"))
src_2010 <- read_parquet(here("data/raj/source_2010_std.parquet"))
src_2015 <- read_parquet(here("data/raj/source_2015_std.parquet"))
src_2020 <- read_parquet(here("data/raj/source_2020_std.parquet"))

samitis <- bind_rows(
    src_2005 %>% select(district_raw, samiti_raw),
    src_2010 %>% select(district_raw, samiti_raw),
    src_2015 %>% select(district_raw, samiti_raw),
    src_2020 %>% select(district_raw, samiti_raw)
) %>%
    distinct()

cat("Unique (district_raw, samiti_raw) pairs:", nrow(samitis), "\n")

# =============================================================================
# Load district crosswalk and join
# =============================================================================

dist_xwalk <- read_csv(here("data/raj/crosswalk_district.csv"), show_col_types = FALSE)

samitis <- samitis %>%
    left_join(dist_xwalk, by = "district_raw") %>%
    filter(!is.na(district_std))

cat("After joining district crosswalk:", nrow(samitis), "\n")

# =============================================================================
# Samiti standardization function
# =============================================================================

standardize_samiti <- function(raw) {
    std <- toupper(trimws(raw))
    std <- gsub("\\s+", "", std)

    replacements <- list(
        c("ARAIN$", "ARAI"),
        c("BHINAI$", "BHINAY"),
        c("PISANGAN$", "PEESANGAN"),
        c("SHRINAGAR$", "SRINAGAR"),
        c("^SAWAR$", "SARWAD"),
        c("REINI$|RENI$", "RAINI"),
        c("MUNDAWAR$|MANDAWAR$", "MUNDAWAR"),
        c("UMRAIN$|UMREN$", "UMRAIN"),
        c("ANADPURI$", "ANANDPURI"),
        c("GADHI$", "GARHI"),
        c("TALWADA$|TALWARA$", "TALWADA"),
        c("ANTA$|ANTAH$", "ANTA"),
        c("ATARU$", "ATRU"),
        c("SHAHABAD$|SHAHBAD$", "SHAHBAD"),
        c("BAYATU$|BAYTOO$|BAITU$", "BAYTU"),
        c("CHAUHATAN$|CHOHTAN$|CHOHTTAN$", "CHAUHATAN"),
        c("DHORIMANA$", "DHORIMANNA"),
        c("ROOPWAS$|RUPBAS$", "ROOPWAS"),
        c("VAIR$|WEIR$", "VAIR"),
        c("SAHADA$|SAHARA$", "SAHARA"),
        c("LOONKARANSAR$|LUNKARANSAR$", "LUNKARANSAR"),
        c("KPATAN$|K\\.PATAN$|KESHORAIPATAN$", "KESHORAIPATAN"),
        c("BADISADRI$|BARISADRI$", "BARISADRI"),
        c("BHENSRODGARH$|BHAINSRORGARH$", "BHAINSRORGARH"),
        c("BHOPALSAGAR$|BHUPALSAGAR$", "BHOPALSAGAR"),
        c("CHOTISADARI$|CHHOTISADARI$|CHHOTISADRI$|CHOTISADRI$", "CHHOTISADRI"),
        c("CHITTORGARH$|CHHITORGARH$", "CHITTORGARH"),
        c("DOONGLA$|DUNGLA$", "DUNGLA"),
        c("LACHHMANGARH$|LAXMANGARH$", "LAXMANGARH"),
        c("BICHCHIWARA$|BICHHIWARA$|BICHIWARA$", "BICHIWARA"),
        c("DUNGARPUR$|DUNGERPUR$", "DUNGARPUR"),
        c("ANOOPGARH$", "ANUPGARH"),
        c("RAISINGHNAGAR$|RAISNGHNAGAR$", "RAISINGHNAGAR"),
        c("SADULSAHAR$|SADULSHAHAR$", "SADULSHAHAR"),
        c("BHADRA$|HBHADRA$", "BHADRA"),
        c("HANUMANGARH$|HHANUMANGARH$", "HANUMANGARH"),
        c("NOHAR$|HNOHAR$", "NOHAR"),
        c("PILIBANGA$|HPILIBANGA$", "PILIBANGA"),
        c("RAWATSAR$|HRAWATSAR$", "RAWATSAR"),
        c("SANGARIYA$|HSANGARIYA$", "SANGARIYA"),
        c("TIBBI$|TIBI$|HTIBBI$", "TIBBI"),
        c("AMBER$", "AMER"),
        c("JALASU$|JALSU$", "JALSU"),
        c("SAMBHAR$|SAMBHARLAKE$|SAMBHER$", "SAMBHARLAKE"),
        c("SANKARA$|SANKDA$|SANKRA$", "SANKRA"),
        c("BHEENMAL$|BHINMAL$", "BHINMAL"),
        c("SAYALA$|SAYLA$", "SAYLA"),
        c("AKALERA$", "AKLERA"),
        c("DAG$", "DUG"),
        c("PIDAWA$|PIRAWA$", "PIRAWA"),
        c("MANDOR$|MANDORE$", "MANDORE"),
        c("OSIAN$|OSIYAN$", "OSIYAN"),
        c("PIPARSAHAR$|PIPARSHAHAR$", "PIPARSHAHAR"),
        c("KARAULI$|KAURALI$", "KARAULI"),
        c("KHAIRABAD$|KHERABAD$", "KHAIRABAD"),
        c("DEEDWANA$|DIDWANA$", "DIDWANA"),
        c("KUCHAMAN$|KUCHAMANCITY$", "KUCHAMAN"),
        c("LADNU$|LADNUN$", "LADNU"),
        c("MUNDAWA$|MUNDWA$", "MUNDWA"),
        c("KHARCHI\\(M\\.JUNC\\)$|MARWARJN\\.$|MARWARJUN$", "MARWARJUNCTION"),
        c("RANI$|RANISTATION$", "RANI"),
        c("SOJAT$|SOJATCITY$", "SOJAT"),
        c("DHARIYAWAD$|DHARIAWAD$|DHRIABAD$", "DHARIAWAD"),
        c("PEEPALKHOONT$|PIPALKHUNT$|PEEPALKHUNT$", "PIPALKHUNT"),
        c("KHAMNOR$|KHAMNORE$", "KHAMNOR"),
        c("GANGAPUR$|GANGAPURCITY$", "GANGAPUR"),
        c("SMADHOPUR$|S\\.MADHOPUR$", "SAWAIMADHOPUR"),
        c("DHOD$|DHOND$", "DHOD"),
        c("SHEOGANJ$|SHIVGANJ$", "SHIVGANJ"),
        c("NEWAI$|NIWAI$", "NIWAI"),
        c("UNIARA$|UNIYARA$", "UNIARA"),
        c("BADGAON$|BARGAON$", "BADGAON"),
        c("BHINDAR$|BHINDER$", "BHINDER"),
        c("KHERW?ADA$|KHERW?ARA$", "KHERWARA"),
        c("KOTDA$|KOTRA$", "KOTDA"),
        c("LASADIA$|LASADIYA$", "LASADIYA"),
        c("MAVLI$|MAWALI$", "MAVLI"),
        c("SALUMBAR$|SALUMBER$", "SALUMBER"),
        c("MAHWA$", "MAHUWA"),
        c("AANDHII$", "AANDHI")
    )

    for (r in replacements) {
        if (grepl(r[1], std, ignore.case = TRUE)) {
            std <- r[2]
            break
        }
    }

    std
}

# =============================================================================
# Apply standardization
# =============================================================================

crosswalk <- samitis %>%
    select(district_std, samiti_raw) %>%
    mutate(
        samiti_std = sapply(samiti_raw, function(x) {
            cleaned <- gsub("\\s+", "", toupper(trimws(x)))
            standardize_samiti(cleaned)
        })
    )

# Consolidate duplicates
crosswalk <- crosswalk %>%
    group_by(district_std, samiti_std) %>%
    mutate(samiti_std = first(samiti_std)) %>%
    ungroup()

# Report variants
counts <- crosswalk %>%
    group_by(district_std, samiti_std) %>%
    summarize(
        n_variants = n(),
        variants = paste(unique(samiti_raw), collapse = ", "),
        .groups = "drop"
    ) %>%
    filter(n_variants > 1)

cat("\n--- Samiti variants that map to same standardized name ---\n")
if (nrow(counts) > 0) print(counts, n = 50)

# =============================================================================
# Save output
# =============================================================================

crosswalk <- distinct(crosswalk)
write_csv(crosswalk, here("data/raj/crosswalk_samiti.csv"))
cat("\nSaved: data/raj/crosswalk_samiti.csv\n")

unique_mappings <- crosswalk %>% distinct(district_std, samiti_std)
cat("Unique (district_std, samiti_std) pairs:", nrow(unique_mappings), "\n")

cat("=== Samiti Crosswalk Complete ===\n")
