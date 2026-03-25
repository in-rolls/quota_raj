# Extract UP LGD Block Panchayat -> GP hierarchy from XML Excel file
# LGD exports Excel files as XML (Excel 2003 XML format)

library(xml2)
library(tidyverse)
library(here)

source(here("scripts/00_config.R"))

cat("=== Extracting UP LGD Block-GP Hierarchy ===\n")

# Parse the XML Excel file
xml_file <- here("data/lgd/up_lgd/up_block_panchayat.xls")
cat("Reading:", xml_file, "\n")

doc <- read_xml(xml_file)

# Define namespace
ns <- xml_ns(doc)

# Get all rows
rows <- xml_find_all(doc, ".//ss:Row", ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet"))
cat("Total rows:", length(rows), "\n")

# Extract cell data from each row
extract_row_data <- function(row) {
    cells <- xml_find_all(row, ".//ss:Cell/ss:Data",
                          ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet"))
    values <- xml_text(cells)
    return(values)
}

# Get header (should be around row 3-4 based on the structure)
# Skip title rows
all_data <- map(rows, extract_row_data)

# Find header row (contains "S.No." or "Localbody Type Code")
header_idx <- which(sapply(all_data, function(x) any(grepl("^S\\.No\\.$", x))))
if (length(header_idx) == 0) {
    header_idx <- 3
}
cat("Header at row:", header_idx[1], "\n")

# Get data starting after header
data_rows <- all_data[(header_idx[1]+1):length(all_data)]

# Convert to data frame
df <- map_dfr(data_rows, function(row) {
    if (length(row) >= 7) {
        tibble(
            sno = row[1],
            lb_type_code = row[2],
            lb_type_name = row[3],
            lb_code = row[4],
            lb_version = row[5],
            lb_name_eng = row[6],
            lb_name_local = row[7],
            parent_code = if(length(row) >= 8) row[8] else NA
        )
    } else {
        NULL
    }
})

cat("Extracted rows:", nrow(df), "\n")

# Clean data
df <- df %>%
    filter(!is.na(lb_type_code), lb_type_code != "") %>%
    mutate(
        lb_type_code = as.integer(lb_type_code),
        lb_code = as.integer(lb_code),
        parent_code = as.integer(parent_code)
    )

# Check local body types
cat("\nLocal body types:\n")
print(table(df$lb_type_name))

# Separate by type:
# Type 1 = Zilla Panchayat (District)
# Type 2 = Block Panchayat (Kshetra/Samiti)
# Type 3 = Gram Panchayat (Village)

zp <- df %>% filter(lb_type_code == 1) %>%
    select(zp_code = lb_code, zp_name = lb_name_eng)

bp <- df %>% filter(lb_type_code == 2) %>%
    select(block_code = lb_code, block_name = lb_name_eng, zp_code = parent_code)

gp <- df %>% filter(lb_type_code == 3) %>%
    select(gp_code = lb_code, gp_name = lb_name_eng, block_code = parent_code)

cat("\nZilla Panchayats:", nrow(zp), "\n")
cat("Block Panchayats:", nrow(bp), "\n")
cat("Gram Panchayats:", nrow(gp), "\n")

# Join to create hierarchy: GP -> Block -> District
lgd_up_gp <- gp %>%
    left_join(bp, by = "block_code") %>%
    left_join(zp, by = "zp_code")

cat("\nGPs with full hierarchy:", sum(!is.na(lgd_up_gp$block_name)), "\n")

# Save
write_csv(lgd_up_gp, here("data/crosswalks/lgd_up_block_gp.csv"))
cat("\nSaved: data/crosswalks/lgd_up_block_gp.csv\n")

# Also save block list for crosswalk
lgd_up_blocks <- bp %>%
    left_join(zp, by = "zp_code")

write_csv(lgd_up_blocks, here("data/crosswalks/lgd_up_blocks.csv"))
cat("Saved: data/crosswalks/lgd_up_blocks.csv\n")

cat("\n=== Done ===\n")
