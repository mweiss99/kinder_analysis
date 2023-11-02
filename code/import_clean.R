# ==================================================================
# I: Load packages, import data and define variables of interest
# ==================================================================

# clear global environment and set working directory 
rm(list = ls())
setwd("/Users/markweiss/Documents/Research/Projects/kinder_ra/data")

# install packages
install_pack <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "readr", "tidycensus", "knitr", "haven")
install_pack(packages)

# import data
mydata <- read_dta("nes1986.dta")

# rename important vars
mydata <- mydata %>% 
rename("form" = V860046, 
       "sex" = V860755, 
       "race" = V860756, 
       "hispanic" = V860757, 
       "education" = V860599, 
       "rent_own" = V860754, 
       "try_harder" = V860579, 
       "slvry_blame" = V860580, 
       "less_deserve" = V860566, 
       "overc_prej" = V860568, 
       "know_bush" = V860343,
       "know_weinberger" = V860344,
       "know_rehnquist" = V860345, 
       "know_volker" = V860346, 
       "know_dole" = V860347, 
       "know_oneill" = V860348,
       "interest_1" = V860102, 
       "interest_2" = V860059,
       "party_id" = V860300, 
       "govt_services" = V860448, 
       "coop_russia" = V860487, 
       "import_restr" = V860520, 
       "defense_spend" = V860405, 
       "interv_ca" = V860428
      )

# define demographic/socioeconomic variables
mydata$white <- rep(NA, nrow(mydata))
mydata$black <- rep(NA, nrow(mydata))

mydata$nhs <- rep(NA, nrow(mydata))
mydata$hs <- rep(NA, nrow(mydata))
mydata$somecol <- rep(NA, nrow(mydata))
mydata$college <- rep(NA, nrow(mydata))
mydata$own <- rep(NA, nrow(mydata))

mydata$op_govt_services <- rep(NA, nrow(mydata))
mydata$op_coop_russia <- rep(NA, nrow(mydata))
mydata$op_defense_spend <- rep(NA, nrow(mydata))
mydata$op_interv_ca <- rep(NA, nrow(mydata))
mydata$op_import_restr <- rep(NA, nrow(mydata))


# ==================================================================
# II: Code outcomes and covariates of interest
# ==================================================================

# code demographic vars
mydata <- mydata %>%
  mutate(
    white = ifelse(between(race, 1, 4) & race != 99, 0, white),
    white = ifelse(race == 1 & hispanic == 5 & race != 99, 1, white),
    black = ifelse(race == 2, 1, ifelse(race == 1 | race > 2, 0, black)),
    female = ifelse(sex == 2, 0, 1),
    sex = ifelse(sex == 2, 0, ifelse(sex == 1, 1, NA))
) 

# code socioeconomic vars
mydata <- mydata %>%
  mutate(nhs = case_when(
    education < 12 ~ 1,
    education >= 12 & education != 99 ~ 0,
    TRUE ~ NA_real_
  ),
  hs = case_when(
    education == 12 ~ 1,
    education < 12 | (education > 12 & education != 99) ~ 0,
    TRUE ~ NA_real_
  ),
  somecol = case_when(
    between(education, 13, 15) ~ 1,
    education < 12 | (education > 15 & education != 99) ~ 0,
    TRUE ~ NA_real_
  ),
  college = case_when(
    education == 16 ~ 1,
    education < 16 | (education > 16 & education != 99) ~ 0,
    TRUE ~ NA_real_
  ))

mydata <- mydata %>%
  mutate(own = case_when(
    rent_own == 1 ~ 1,
    rent_own %in% 2:7 ~ 0,
    TRUE ~ NA_real_
  ))

# identify political figure vars
names <- c("bush", "weinberger", "rehnquist", "volker", "dole", "oneill")

mydata <- mydata %>%
mutate(across(starts_with("know"), ~ ifelse(. == 1, 1, ifelse(. %in% c(5, 8), 0, NA)), .names = "ident_{.col}")) %>%
  select(-starts_with("know")) %>%
  rename(
  "ident_bush" = ident_know_bush, 
  "ident_weinberger" = ident_know_weinberger, 
  "ident_rehnquist" = ident_know_rehnquist, 
  "ident_volker" = ident_know_volker, 
  "ident_dole" = ident_know_dole,
  "ident_oneill" = ident_know_oneill
)

mydata$knowledge <- rowMeans(mydata[, c("ident_bush", "ident_weinberger",
"ident_rehnquist", "ident_volker", "ident_dole", "ident_oneill")])

# interest in politics variables
mydata <- mydata %>%
    mutate(interest_1 = case_when(
    interest_1 == 1 ~ 0.5,
    interest_1 == 2 ~ 0.334,
    interest_1 == 4 ~ 0.167,
    interest_1 == 5 ~ 0,
    interest_1 %in% c(8, 9) ~ NA_real_,
    TRUE ~ interest_1
  )) %>%
  mutate(interest_2 = case_when(
    interest_2 == 1 ~ 0.5,
    interest_2 == 3 ~ 0.334,
    interest_2 == 5 ~ 0.167,
    interest_2 == 9 ~ NA_real_,
    TRUE ~ interest_2
  ))

mydata$taste <- rowSums((mydata[, c("interest_1", "interest_2")]))

# modern racism variable
# overc_prej, slvr_blame, less_deserve, try_harder

mydata <- mydata %>%
    mutate(overc_prej = case_when( 
      overc_prej %in% c(0, 8, 9) ~ NA_real_,
      overc_prej == 1 ~ 1, 
      overc_prej == 2 ~ 0.5, 
      overc_prej == 3 ~ 0,
      overc_prej == 4 ~ -0.5, 
      overc_prej == 5 ~ -1,
    )) %>%
    mutate(try_harder = case_when(
      try_harder %in% c(0, 8, 9) ~ NA_real_,
      try_harder == 1 ~ 1, 
      try_harder == 2 ~ 0.5, 
      try_harder == 3 ~ 0, 
      try_harder == 4 ~ -0.5,
      try_harder == 5 ~ -1
    )) %>%
    mutate(slvry_blame = case_when(
      slvry_blame %in% c(0, 8, 9) ~ NA_real_, 
      slvry_blame == 1 ~ -1, 
      slvry_blame == 2 ~ -0.5, 
      slvry_blame == 3 ~ 0, 
      slvry_blame == 4 ~ 0.5, 
      slvry_blame == 5 ~ 1 
    )) %>%
    mutate(less_deserve = case_when(
      less_deserve %in% c(0, 8, 9) ~ NA_real_, 
      less_deserve == 1 ~ -1, 
      less_deserve == 2 ~ -0.5, 
      less_deserve == 3 ~ 0, 
      less_deserve == 4 ~ 0.5, 
      less_deserve == 5 ~ 1
    ))

mydata$racism <- rowMeans(mydata[, c("overc_prej", "try_harder", "slvry_blame", "less_deserve")])

# partisanship variable
mydata <- mydata %>%
    mutate(party_id = case_when( 
      party_id %in% c(7:9) ~ NA_real_,
      party_id == 0 ~ -1, 
      party_id == 1 ~ -0.666, 
      party_id == 2 ~ -0.333, 
      party_id == 3 ~ 0, 
      party_id == 4 ~ 0.333, 
      party_id == 5 ~ 0.666, 
      party_id == 6 ~ 1))

# takes opinion on alternate dep. variables
mydata <- mydata %>%
    mutate(op_govt_services = case_when( 
      govt_services == 9 ~ NA_real_, 
      govt_services %in% c(0,8) ~ 0, 
      govt_services %in% c(1:7) ~ 1
    )) %>%
    mutate(op_import_restr = ifelse(mydata$import_restr == 1, 1, 
    ifelse(mydata$import_restr %in% c(5, 8), 0, NA))) %>%
    mutate(op_coop_russia = case_when(
      coop_russia == 9 ~ NA_real_,
      coop_russia %in% c(0,8) ~ 0, 
      coop_russia %in% c(1:7) ~ 1
    )) %>%
    mutate(op_defense_spend = case_when(
      defense_spend == 9 ~ NA_real_,
      defense_spend %in% c(0,8) ~ 0, 
      defense_spend %in% c(1:7) ~ 1
    )) %>%
    mutate(op_interv_ca = case_when( 
      interv_ca == 9 ~ NA_real_,
      interv_ca %in% c(0,8) ~ 0, 
      interv_ca %in% c(1:7) ~ 1
    ))
