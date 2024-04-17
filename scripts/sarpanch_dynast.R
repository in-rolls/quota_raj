## Load libs

library(readr)
library(stringr)
library(stringdist)
library(dplyr)
library(stargazer)

# Load dat
sp <- read_csv("data/sarpanch_election_data/sp_2005_2010_fin.csv")

# Names may have parentheticals that you can plausibly remove like (BAI), (CHAMAR) (GOSWAMI), etc.

# Normalize names
sp$cl_name_2005 <- tolower(gsub("\\.", "", sp$name_2005))
sp$cl_name_2010 <- tolower(gsub("\\.", "", sp$name_2010))

# Recode
sp$ln_05   <- ifelse(str_count(sp$name_2005, "\\s+") > 0, word(sp$cl_name_2005, -1), NA)
sp$ln_10   <- ifelse(str_count(sp$name_2010, "\\s+") > 0, word(sp$cl_name_2010, -1), NA)

sp$same_name <- sp$cl_name_2005 == sp$cl_name_2010
sp$same_ln   <- sp$ln_05 == sp$ln_10

are_names_similar <- function(name1, name2) {
  stringdist::stringdist(name1, name2, method = "lv") <= 1
}

# Apply the function to create a new variable
sp$same_name_1 <- mapply(are_names_similar, sp$cl_name_2005, sp$cl_name_2010)
sp$same_ln_1 <- mapply(are_names_similar, sp$ln_05, sp$ln_10)

sp$w_05 <- ifelse(grepl("W", sp$reservation_2005), 1, 0)
sp$w_10 <- ifelse(grepl("W", sp$reservation_2010), 1, 0)

table(sp$w_05, sp$same_name_1)
table(sp$w_05, sp$same_ln_1)

sp$sex_2005n <- ifelse(sp$sex_2005 == "FEMALE", 1, 0) 
sp$sex_2010n <- ifelse(sp$sex_2010 == "F", 1, 0) 

summary(lm(sex_2010n ~ w_05, data = sp[sp$w_10 == 0, ]))

# 2005-2010--2015
sp <- read_csv("data/sarpanch_election_data/sp_2005_2010_2015_best_manual.csv")

sp$w_05 <- ifelse(grepl("W", sp$reservation_2005), 1, 0)
sp$w_10 <- ifelse(grepl("W", sp$reservation_2010), 1, 0)
sp$w_15 <- ifelse(grepl("W", sp$reservation_2015), 1, 0)

sp$sex_2005n <- ifelse(sp$sex_2005 == "FEMALE", 1, 0) 
sp$sex_2010n <- ifelse(sp$sex_2010 == "F", 1, 0) 
sp$sex_2015n <- ifelse(grepl("W", sp$category_2015), 1, 0)

sp_small <- sp %>%
  distinct(key_2005, .keep_all = TRUE) %>%
  distinct(key_2010, .keep_all = TRUE) %>%
  distinct(key_2015, .keep_all = TRUE)

summary(lm(sex_2010n ~ w_05, data = sp[sp$w_10 == 0, ]))
summary(lm(sex_2015n ~ w_05 + w_10, data = sp[sp$w_15 == 0, ]))

mod1 <- lm(sex_2010n ~ w_05, data = sp_small[sp_small$w_10 == 0, ])
mod2 <- lm(sex_2015n ~ w_05 + w_10, data = sp_small[sp_small$w_15 == 0, ])

# Generate LaTeX table using stargazer
stargazer(mod1, mod2, 
          title = "Effect of Reserving the Seat for Women on the Probability of Electing a Woman When the Seat is Unreserved",
          type = "latex",
          dep.var.labels = c("2010", "2015"),
          covariate.labels = c("Reserved in 2005", "Reserved in 2010"),
          header = FALSE,
          label = "tab:seat_win",
          align = TRUE,
          omit.stat = c("f", "ser", "adj.rsq"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          out = "tables/seat_outcome.tex"
)

# 2005--2020
sp <- read_csv("data/sarpanch_election_data/sp_2005_2010_2015_2020_fin2.csv")

sp$w_05 <- ifelse(grepl("W", sp$reservation_2005), 1, 0)
sp$w_10 <- ifelse(grepl("W", sp$reservation_2010), 1, 0)
sp$w_15 <- ifelse(grepl("W", sp$reservation_2015), 1, 0)
sp$w_20 <- ifelse(grepl("W", sp$reservation_2020), 1, 0)

sp$sex_2005n <- ifelse(sp$sex_2005 == "FEMALE", 1, 0) 
sp$sex_2010n <- ifelse(sp$sex_2010 == "F", 1, 0) 
sp$sex_2015n <- ifelse(grepl("W", sp$category_2015.x), 1, 0)
sp$sex_2020n <- ifelse(sp$sex_2020 == "F", 1, 0) 

sp_small <- sp %>%
  distinct(key_2005, .keep_all = TRUE) %>%
  distinct(key_2010, .keep_all = TRUE) %>%
  distinct(key_2015, .keep_all = TRUE) %>%
  distinct(key_2020, .keep_all = TRUE)

mod1 <- lm(sex_2010n ~ w_05, data = sp_small[sp_small$w_10 == 0, ])
mod2 <- lm(sex_2015n ~ w_05 + w_10, data = sp_small[sp_small$w_15 == 0, ])
mod3 <- lm(sex_2020n ~ w_05 + w_10 + w_15, data = sp_small[sp_small$w_20 == 0, ])
