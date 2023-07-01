########################################################################
### FILE DESCRIPTION: Cleaning Raw Canadian Census Data
### PRIMARY OBJECTIVE: Standardizing Primary Variables, Creating Single Multi-Year Dataframe
### CREATED BY: Amy Kim
### CREATED ON: Sep 29 2022
### LAST MODIFIED: Apr 18 2023
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(MatchIt)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

########################################################################
### IMPORTING DATA
########################################################################
## canadian census data from 1852-1921
years <- c(1852, seq(1871, 1921, 10))
#raw1852 <- read.spss(glue("{dbox}/census1852.sav")) %>% as.data.frame() # 20% sample
#raw1871 <- read.spss(glue("{dbox}/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
#raw1881 <- read.spss(glue("{dbox}/census1881.sav")) %>% as.data.frame() # 100% sample
#raw1891 <- read.spss(glue("{dbox}/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 4% sample

# using helper functions
source(glue("{git}/code/helper.R"))

########################################################################
### CLEANING DATA
########################################################################
clean1901 <- raw1901 %>% rename(BPL = bpl, SEX = sex, MARST = marst, AGE = ageyr, YRIMM = immyr, PROPOWNR = propownr, NATL = natl, OCC = occ,
                                CANREAD = canread, LASTNAME = indlnm, FIRSTNAME = indfnm, PROVINCE = province) %>%
  mutate(BORNCHI = ifelse(bpl2 == 50000, 1, 0),
         BORNJAP = ifelse(str_detect(BPL, "JAP"), 1, 0),
         OCCSTR = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{3}")))), #extracts first three digits of occupation code (IPUMS)
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         HOUSEOWN = ifelse(AGE >= 18, ifelse(PROPOWNR == "Yes", 1, 0), NA),
         YRIMM = as.numeric(YRIMM),
         IMM = ifelse(bpl2 < 16000 & bpl2 >= 15000, 0, 1),
         LABOR = ifelse(AGE >= 18, ifelse(str_detect(OCC, "LAB"), 1, 0), NA),
         OCCGRP = case_when(occ2 == "Labourer" | str_detect(OCC, "LAB") ~ "Laborer",
                            OCCSTR >= 711 & OCCSTR < 719 ~ "Farmer",
                            OCCSTR < 614 ~ "Skilled",
                            OCCSTR >= 812 & OCCSTR < 955 ~ "Skilled", #blacksmiths etc., with laborers already removed
                            !is.na(OCCSTR) ~ "Unskilled",
                            TRUE ~ NA_character_),
         PROVINCE = case_when(PROVINCE == "AT" | PROVINCE == "KE" | PROVINCE == "MC" | PROVINCE == "UN" | PROVINCE == "YU" ~ "UT",
                              PROVINCE == "QU" ~ "QC",
                              TRUE ~ PROVINCE),
         RURAL = ifelse(urbplace == 999, 1, 0),
         EARN = ifelse(AGE >= 18, earnings, NA)) %>%
  select(c(MALE, AGE, BORNCHI, BORNJAP, MAR, YRIMM, OCCGRP, EARN, CANREAD, IMM, PROVINCE, HOUSEOWN, LABOR)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

bplcanadastrings <- "(Ontario)|(Quebec)|(Nova Scotia)|(New Brunswick)|(Manitoba)|(Saskatchewan)|(Prince Edward Island)|(British Columbia)|(Alberta)|(Canada)|(Newfoundland)|(Northwest Territories)|(Yukon)|(Cape Breton)|(Labrador)"

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(EARN = ifelse(AGE >= 18, 
                       ifelse(!grepl("[0-9]+", EARNINGS_AT_CHIEF_OCC), 
                              NA_real_, 
                              suppressWarnings(as.numeric(as.character(EARNINGS_AT_CHIEF_OCC)))), NA),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), suppressWarnings(as.numeric(as.character(YRIMM))), NA_real_),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 4),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCCIND = str_to_lower(OCCUPATION_CHIEF_OCC_IND_CL),
         LABOR = ifelse(AGE >= 18, ifelse(str_detect(OCCIND, "(L|l)abour") | str_detect(OCCIND, "(L|l)abor"), 1, 0), NA),
         OCCGRP = case_when(occ1 == "Agriculture" ~ "Farmer",
                            OCC == "Laborers (n.e.c.)" |  str_detect(OCCIND, "Labour") ~ "Laborer",
                            OCC == "Managers, officials, and proprietors (n.e.c.)" ~ "Skilled",
                            occ1 == "Forestry and lumbering" | occ1 == "Mining" | occ1 == "Fisheries and hunting" | str_starts(occ1, "Domestic")~ "Unskilled",
                            str_starts(occ1, "Manufactures") | occ1 == "Trade and Merchandising" | occ1 == "Transportation" | occ1 == "Building trades" ~ "Skilled",
                            occ1 == "Civil and municipal service" | occ1 == "Professional pursuits" ~ "Skilled"),
         RURAL = ifelse(str_detect(CCRI_URBAN_RURAL_1911,"Rural"), 1, 0),
         PROVINCE = str_trim(PR_1911),
         HOUSEOWN = ifelse(AGE >= 18, ifelse(RELATIONSHIP == "Head", 1, 0), NA)) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, LABOR, RURAL, PROVINCE, HOUSEOWN)) %>% 
  mutate(YEAR = 1911, BORNCHI = ifelse(BPL == "China", 1, 0), 
         BORNJAP = ifelse(BPL == "Japan", 1, 0))

clean1921 <- raw1921 %>% rename(AGE = Derived_Age_In_Years, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = CHIEFOCCUP, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME, PROVINCE = Province) %>%
  mutate(EARN = ifelse(AGE >= 18, ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), suppressWarnings(as.numeric(as.character(ANNUAL_EARNING_AMOUNT))), NA_real_), NA),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), suppressWarnings(as.numeric(as.character(YRIMM))), NA_real_),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 5),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCC = str_to_lower(OCC),
         LABOR = ifelse(AGE >= 18, ifelse(str_detect(OCC, "labor") | str_detect(OCC, "labour"), 1, 0), NA),
         OCCGRP = case_when(str_detect(CHIEF_OCCUPATION, "Farm") ~ "Farmer",
                            str_detect(CHIEF_OCCUPATION, "Labor") | str_detect(CHIEF_OCCUPATION, "labor") ~ "Laborer",
                            str_detect(CHIEF_OCCUPATION, "Mine") | CHIEF_OCCUPATION == "Private household workers (n.e.c.)" | str_detect(CHIEF_OCCUPATION, "Lumbermen") | CHIEF_OCCUPATION == "Carpenters" | CHIEF_OCCUPATION == "Housekeepers, private household" | CHIEF_OCCUPATION == "Fishermen and oystermen" ~ "Unskilled",
                            str_detect(CHIEF_OCCUPATION, "home") | (str_detect(CHIEF_OCCUPATION, "house") & !str_detect(CHIEF_OCCUPATION, "except")) ~ "Unskilled",
                            CHIEF_OCCUPATION != "Blank" & CHIEF_OCCUPATION != "Unemployed/ without occupation" & CHIEF_OCCUPATION != "Illegible_duplicated_99999003" ~ "Skilled",
                            TRUE ~ NA_character_),
         HOUSEOWN = ifelse(AGE >= 18, ifelse(str_detect(HOME_OWNED_OR_RENTED, "Owned"), 1, 0), NA),
         RURAL = ifelse(str_detect(CCRI_URBAN_RURAL_1921,"Rural"), 1, 0),) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, LABOR, PROVINCE, RURAL, HOUSEOWN)) %>% 
  mutate(YEAR = 1921, BORNCHI = ifelse(BPL == "China", 1, 0), 
         BORNJAP = ifelse(BPL == "Japan", 1, 0))

clean_all <- bind_rows(clean1901, clean1911) %>%
  bind_rows(clean1921) %>%
  mutate(AGE = ifelse(AGE > 200, NA, AGE),
         YRIMM = ifelse(YRIMM < 1500 | YRIMM > YEAR, NA, YRIMM),
         IDNUM = row_number())

clean_imm <- clean_all %>%   
  filter(IMM == 1)

write_csv(clean_all, glue("{dbox}/cleaned/can_clean.csv"))
write_csv(clean_imm, glue("{dbox}/cleaned/can_clean_imm.csv"))

# precomputing summary stats for all of canada
can_all_summ <- summstats(clean_all %>% mutate(source = "CA Census", group = "All", WEIGHT = 1), c("MALE", "MAR", "AGE", "CANREAD", "LABOR", "EARN"))
write_csv(can_all_summ, glue("{dbox}/cleaned/can_all_summ.csv"))
