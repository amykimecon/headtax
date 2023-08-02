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
raw1871 <- read.spss(glue("{dbox}/raw/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/raw/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/raw/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 4% sample

# using helper functions
source(glue("{git}/code/helper.R"))

########################################################################
### CLEANING DATA
########################################################################
countries <- c("Canada", "Ireland", "England", "Scotland", "US", "Germany", "France", "Wales", "Italy",
               "Netherlands", "Norway", "Denmark", "Australia and NZ", "Switzerland", "Spain", "Poland", "Sweden",
               "Austria and Hungary", "Greece", "Russia", "Belgium", "China", "Japan", "India", "Iceland", "Chile", "Romania",
               "West Indies", "Bermuda", "Mexico", "Finland", "Portugal", "East Indies", "Turkey", "Brazil")

clean1901 <- raw1901 %>% rename(SEX = sex, MARST = marst, AGE = ageyr, YRIMM = immyr, PROPOWNR = propownr, NATL = natl, OCC = occ,
                                CANREAD = canread, LASTNAME = indlnm, FIRSTNAME = indfnm, PROVINCE = province) %>%
  mutate(OCCSTR = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{3}")))), #extracts first three digits of occupation code (IPUMS)
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
         EARN = ifelse(AGE >= 18 & !is.na(earnings), earnings + ifelse(is.na(exearn), 0, exearn), NA),
         BPL = case_when(bpl2 < 10000 ~ "US",
                         bpl2 == 40000 ~ "Denmark",
                         bpl2 == 40100 ~ "Finland",
                         bpl2 == 40200 ~ "Iceland",
                         bpl2 == 40400 ~ "Norway",
                         bpl2 == 40500 ~ "Sweden",
                         bpl2 >= 41000 & bpl2 < 41010 ~ "England",
                         bpl2 >= 41100 & bpl2 < 41200 ~ "Scotland",
                         bpl2 == 41200 ~ "Wales",
                         bpl2 >= 41400 & bpl2 < 42000 ~ "Ireland",
                         bpl2 == 42000 | bpl2 == 45600 | bpl2 == 42010 ~ "Austria and Hungary",
                         bpl2 == 42100 ~ "Belgium",
                         bpl2 >= 42200 & bpl2 < 42600 ~ "France",
                         bpl2 == 42600 ~ "Netherlands",
                         bpl2 == 42700 ~ "Switzerland",
                         bpl2 == 43300 ~ "Greece",
                         bpl2 == 43500 ~ "Italy",
                         bpl2 == 43700 ~ "Portugal",
                         bpl2 == 44100 ~ "Spain",
                         bpl2 >= 45300 & bpl2 <= 45360 ~ "Germany",
                         bpl2 == 45700 |bpl2 == 45711 ~ "Poland",
                         bpl2 == 47100 ~ "Romania",
                         bpl2 >= 48900 & bpl2 < 50000 ~ "Russia",
                         bpl2 == 50000 ~ "China",
                         bpl2 == 50400 ~ "Japan",
                         bpl2 >= 52100 & bpl2 <= 52105 ~ "India",
                         bpl2 == 70010 | bpl2 == 70020 | bpl2 == 70015 ~ "Australia and NZ",
                         bpl2 >= 25000 & bpl2 < 27000 ~ "West Indies",
                         bpl2 == 54600 ~ "Turkey",
                         bpl2 == 16010 ~ "Bermuda",
                         bpl2 >= 51200 & bpl2 < 53000 ~ "East Indies",
                         bpl2 == 30020 ~ "Chile",
                         bpl2 == 20000 ~ "Mexico",
                         str_detect(bpl, "(BUROKOVIA|BUK)") ~ "Romania",
                         bpl2 >= 15000 & bpl2 < 16000 ~ "Canada",
                         TRUE ~ "Other")) %>%
  select(c(MALE, AGE, BPL, MAR, YRIMM, OCCGRP, EARN, CANREAD, IMM, PROVINCE, HOUSEOWN, LABOR)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, 
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% countries ~ bplstring,
                         bplstring %in% c("Galicia", "Russian Poland", "Austrian Poland", "Austrian-Galicia", "German Poland") ~ "Poland",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Hungary") | str_detect(bplstring, "Austria") ~ "Austria and Hungary",
                         bplstring %in% c("Australia", "New Zealand") ~ "Australia and NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas") | str_detect(bplstring, "West Indies") ~ "West Indies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Brittany") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         bplstring %in% c("Alsace", "Lorraine", "Normandy") ~ "France",
                         bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "East Indies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick)") ~ "Canada",
                         TRUE ~ "Other"),
         EARN = ifelse(AGE >= 18 & grepl("[0-9]+", EARNINGS_AT_CHIEF_OCC),
                       suppressWarnings(as.numeric(as.character(EARNINGS_AT_CHIEF_OCC))) + 
                         ifelse(grepl("[0-9]+", EARNINGS_AT_OTHER_OCC), suppressWarnings(as.numeric(as.character(EARNINGS_AT_OTHER_OCC))), 0),
                       NA),
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
         IMM = ifelse(BPL == "Canada", 0, 1),
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
  select(c(MALE, AGE, MAR, CANREAD, BPL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, LABOR, RURAL, PROVINCE, HOUSEOWN)) %>% 
  mutate(YEAR = 1911)

clean1921 <- raw1921 %>% rename(AGE = Derived_Age_In_Years, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION,
                                CANREAD = CAN_READ_INDICATOR, OCC = CHIEFOCCUP, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME, PROVINCE = Province) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% countries ~ bplstring,
                         bplstring %in% c("Galicia", "Russian Poland", "Austrian Poland", "Austrian-Galicia", "German Poland") ~ "Poland",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Hungary") | str_detect(bplstring, "Austria") ~ "Austria and Hungary",
                         bplstring %in% c("Australia", "New Zealand") ~ "Australia and NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas", "Haiti", "Trinidad and Tobago") | str_detect(bplstring, "West Indies") ~ "West Indies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Hesse", "Hamburg", 
                                          "Rhine Province", "Schleswig-Holstein", "Westphalia", "Wurtemberg", "Holstein", "Baden", 
                                          "Brandenburg", "Saxe-Weimar-Eisenach", "Thuringian State", "Pomerania", 'Posen', "West Prussia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         bplstring %in% c("Alsace", "Lorraine", "Alsace-Lorraine", "Brittany") ~ "France",
                         str_detect(bplstring, "East Indies") | bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "East Indies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick|Saskatchewan)") | 
                           bplstring %in% c("St. Pamphile de l'Islet") ~ "Canada",
                         TRUE ~ "Other"),
         EARN = ifelse(AGE >= 18, ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), suppressWarnings(as.numeric(as.character(ANNUAL_EARNING_AMOUNT))), NA_real_), NA),
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
         IMM = ifelse(BPL == "Canada", 0, 1),
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
  select(c(MALE, AGE, MAR, CANREAD, BPL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, LABOR, PROVINCE, RURAL, HOUSEOWN)) %>% 
  mutate(YEAR = 1921)

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


#### BIRTHPLACE TOTALS
countries <- c("Canada", "Ireland", "England", "Scotland", "US", "Germany", "France", "Wales", "Italy",
               "Netherlands", "Norway", "Denmark", "Australia and NZ", "Switzerland", "Spain", "Poland", "Sweden",
               "Austria and Hungary", "Greece", "Russia", "Belgium", "China", "Japan", "India", "Iceland", "Chile", "Romania",
               "West Indies", "Bermuda", "Mexico", "Finland", "Portugal", "East Indies", "Turkey", "Brazil")
# note: germany includes prussia, bavaria, berlin
# austria includes hungary 
# australia includes new zealand
# poland includes russian poland, prussian poland, galicia
# romania includes bukovina
# west indies includes haiti, jamaica, british west indies, antigua barbuda, bahamas, barbados, trinidad and tobago, 
#   cuba, dominica, dominican republic, grenada, st kitts and nevis, st lucia, st. vincent and grenadines
# france includes alsace, lorraine

bpl1871 <- raw1871 %>% 
  mutate(BPL = case_when(birthpl %in% c("ON", "QU", "NS", "NB", "CA", "PE", "NE") | bpcod == "Prince edward island_duplicated_3200" ~ "Canada",
                         birthpl %in% c("IR", "EN", "SC", "FR", "WA", "IT", "NO", "DE", "ST", "SP", "PO", "SW") ~ as.character(bpcod),
                         birthpl == "PR" | birthpl == "GE" | birthpl == "BA" ~ "Germany",
                         bpcod == "East india" | bpcod == "Ceylon" ~ "East Indies",
                         birthpl == "DU" ~ "Netherlands",
                         bpcod %in% c("West indies", "Trinidad", "Jamaica") ~ "West Indies",
                         bpcod == "Australia" ~ "Australia and NZ",
                         birthpl == "US" ~ "US",
                         bpcod %in% countries ~ as.character(bpcod),
                         TRUE ~ "Other")) %>% #filter(BPL == "Other") %>% group_by(bpcod) %>% summarize(POP = sum(popwgt))
  group_by(BPL) %>% summarize(POP = sum(popwgt)) %>% mutate(YEAR = 1871)

us_states <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[1:82]))
canadian <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[84:1130]))

bpl1881 <- raw1881 %>% 
  mutate(bplstring = trimws(as.character(birthplace_ccri)),
         BPL = case_when(bplstring %in% countries ~ bplstring, 
                         bplstring %in% c("Prussia", "Bavaria", "Berlin", "Hamburg", "Wurtemberg", "Hesse", "Mecklenburg", "Hanover", "Saxony", "Holstein") ~ "Germany",
                         bplstring == "Hungary" | bplstring == "Austria-Vienna" | bplstring == "Austria" ~ "Austria and Hungary",
                         bplstring == "Russian Poland" | bplstring == "German Poland" ~ "Poland",
                         bplstring == "Australia and New Zealand" | bplstring == "New Zealand" | bplstring == "Australia" ~ "Australia and NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Trinidad and Tobago", "Bahamas", 
                                          "British West Indies", "St. Vincent (West Indies)", "St. Kitts-Nevis", "Dominica", "Dominican Republic",
                                          "Grenada", "Haiti", "St. Lucia") ~ "West Indies",
                         bplstring == "Alsace" | bplstring == "Lorraine" ~ "France",
                         bplstring %in% c("Malaysia", "Sri Lanka (Ceylon)", "Burma (Myanmar)", "Singapore") ~ "East Indies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian ~ "Canada",
                         TRUE ~ "Other")) %>% #filter(BPL == "Other") %>% group_by(bplstring) %>% summarize(POP = n())
  group_by(BPL) %>% summarize(POP = n()) %>% mutate(YEAR = 1881)

bpl1891 <- raw1891 %>% mutate(WEIGHT = case_when(samplesize == 5 ~ 20, samplesize == 10 ~ 10, samplesize == 100 ~ 1)) %>%
  mutate(BPL = case_when(bplcode < 10000 ~ "US",
                             bplcode == 40000 ~ "Denmark",
                             bplcode == 40100 ~ "Finland",
                             bplcode == 40400 ~ "Norway",
                             bplcode == 40500 ~ "Sweden",
                             bplcode == 41000 ~ "England",
                             bplcode == 41100 ~ "Scotland",
                             bplcode == 41200 ~ "Wales",
                             bplcode == 41400 ~ "Ireland",
                             bplcode == 42000 ~ "Belgium",
                             bplcode == 42100 | bplcode == 42111 ~ "France",
                             bplcode == 42500 ~ "Netherlands",
                             bplcode == 42600 ~ "Switzerland",
                             bplcode == 43300 ~ "Greece",
                             bplcode == 43400 ~ "Italy",
                             bplcode == 43600 ~ "Portugal",
                             bplcode == 43800 ~ "Spain",
                             bplcode == 45000 | bplcode == 45400 | bplcode == 45010 ~ "Austria and Hungary",
                             bplcode >= 45300 & bplcode <= 45360 | BPL == "MECKLENBURGH" ~ "Germany",
                             bplcode >= 45500 & bplcode < 45600 ~ "Poland",
                             bplcode == 45600 ~ "Romania",
                             bplcode >= 46000 & bplcode < 49900 ~ "Russia",
                             bplcode == 50000 | BPL == "CANTON" ~ "China",
                             bplcode == 50100 ~ "Japan",
                             bplcode == 52100 ~ "India",
                             bplcode >= 70010 & bplcode <= 70020 ~ "Australia and NZ",
                             bplcode >= 25000 & bplcode < 27000 ~ "West Indies",
                             bplcode == 16010 ~ "Bermuda",
                             bplcode == 26030 ~ "Jamaica",
                             bplcode == 20000 ~ "Mexico",
                             bplcode == 30015 ~ "Brazil",
                             bplcode == 30020 ~ "Chile",
                             bplcode == 54200 ~ "Turkey",
                             bplcode >= 51200 & bplcode < 53000 ~ "East Indies",
                             bplcode >= 15000 & bplcode < 16000 ~ "Canada",
                             TRUE ~ "Other")) %>% #filter(BPL2 == "Other") %>% group_by(bplcode, BPL) %>% summarize(POP = sum(WEIGHT))
  group_by(BPL) %>% summarize(POP = sum(WEIGHT))  %>% mutate(YEAR = 1891)

bpl1901 <- raw1901 %>% mutate(WEIGHT = 20,
                              BPL = case_when(bpl2 < 10000 ~ "US",
                                                  bpl2 == 40000 ~ "Denmark",
                                                  bpl2 == 40100 ~ "Finland",
                                                  bpl2 == 40200 ~ "Iceland",
                                                  bpl2 == 40400 ~ "Norway",
                                                  bpl2 == 40500 ~ "Sweden",
                                                  bpl2 >= 41000 & bpl2 < 41010 ~ "England",
                                                  bpl2 >= 41100 & bpl2 < 41200 ~ "Scotland",
                                                  bpl2 == 41200 ~ "Wales",
                                                  bpl2 >= 41400 & bpl2 < 42000 ~ "Ireland",
                                                  bpl2 == 42000 | bpl2 == 45600 | bpl2 == 42010 ~ "Austria and Hungary",
                                                  bpl2 == 42100 ~ "Belgium",
                                                  bpl2 >= 42200 & bpl2 < 42600 ~ "France",
                                                  bpl2 == 42600 ~ "Netherlands",
                                                  bpl2 == 42700 ~ "Switzerland",
                                                  bpl2 == 43300 ~ "Greece",
                                                  bpl2 == 43500 ~ "Italy",
                                                  bpl2 == 43700 ~ "Portugal",
                                                  bpl2 == 44100 ~ "Spain",
                                                  bpl2 >= 45300 & bpl2 <= 45360 ~ "Germany",
                                                  bpl2 == 45700 |bpl2 == 45711 ~ "Poland",
                                                  bpl2 == 47100 ~ "Romania",
                                                  bpl2 >= 48900 & bpl2 < 50000 ~ "Russia",
                                                  bpl2 == 50000 ~ "China",
                                                  bpl2 == 50400 ~ "Japan",
                                                  bpl2 >= 52100 & bpl2 <= 52105 ~ "India",
                                                  bpl2 == 70010 | bpl2 == 70020 | bpl2 == 70015 ~ "Australia and NZ",
                                                  bpl2 >= 25000 & bpl2 < 27000 ~ "West Indies",
                                                  bpl2 == 54600 ~ "Turkey",
                                                  bpl2 == 16010 ~ "Bermuda",
                                                  bpl2 >= 51200 & bpl2 < 53000 ~ "East Indies",
                                                  bpl2 == 30020 ~ "Chile",
                                                  bpl2 == 20000 ~ "Mexico",
                                                  str_detect(bpl, "(BUROKOVIA|BUK)") ~ "Romania",
                                                  bpl2 >= 15000 & bpl2 < 16000 ~ "Canada",
                                                  TRUE ~ "Other")) %>% #filter(BPL == "Other") %>% group_by(bpl, bpl2) %>% summarize(POP = sum(WEIGHT))
                                group_by(BPL) %>% summarize(POP = sum(WEIGHT))  %>% mutate(YEAR = 1901)
                              
bpl1911 <- raw1911 %>% mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                                                 str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                                                 str_starts(Dwelling_Unit_Type, "MU") ~ 4)) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% countries ~ bplstring,
                         bplstring %in% c("Galicia", "Russian Poland", "Austrian Poland", "Austrian-Galicia", "German Poland") ~ "Poland",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Hungary") | str_detect(bplstring, "Austria") ~ "Austria and Hungary",
                         bplstring %in% c("Australia", "New Zealand") ~ "Australia and NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas") | str_detect(bplstring, "West Indies") ~ "West Indies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Brittany") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         bplstring %in% c("Alsace", "Lorraine", "Normandy") ~ "France",
                         bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "East Indies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick)") ~ "Canada",
                         TRUE ~ "Other")) %>% #filter(BPL == "Other") %>% group_by(INDIVIDUAL_BIRTH_COUNTRY) %>% summarize(POP = sum(WEIGHT))
  group_by(BPL) %>% summarize(POP = sum(WEIGHT)) %>% mutate(YEAR = 1911)

bpl1921 <- raw1921 %>% mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                                                 str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                                                 str_starts(Dwelling_Unit_Type, "MU") ~ 5)) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% countries ~ bplstring,
                         bplstring %in% c("Galicia", "Russian Poland", "Austrian Poland", "Austrian-Galicia", "German Poland") ~ "Poland",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Hungary") | str_detect(bplstring, "Austria") ~ "Austria and Hungary",
                         bplstring %in% c("Australia", "New Zealand") ~ "Australia and NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas", "Haiti", "Trinidad and Tobago") | str_detect(bplstring, "West Indies") ~ "West Indies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Hesse", "Hamburg", 
                                          "Rhine Province", "Schleswig-Holstein", "Westphalia", "Wurtemberg", "Holstein", "Baden", 
                                          "Brandenburg", "Saxe-Weimar-Eisenach", "Thuringian State", "Pomerania", 'Posen', "West Prussia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         bplstring %in% c("Alsace", "Lorraine", "Alsace-Lorraine", "Brittany") ~ "France",
                         str_detect(bplstring, "East Indies") | bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "East Indies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick|Saskatchewan)") | 
                           bplstring %in% c("St. Pamphile de l'Islet") ~ "Canada",
                         TRUE ~ "Other")) %>% # filter(BPL == "Other") %>% group_by(INDIVIDUAL_BIRTH_COUNTRY) %>% summarize(POP = sum(WEIGHT))
  group_by(BPL) %>% summarize(POP = sum(WEIGHT))  %>% mutate(YEAR = 1921)


## combining all years
bplall <- list(bpl1871, bpl1881, bpl1891, bpl1901, bpl1911, bpl1921) %>% bind_rows()
bplwide <- bplall %>% pivot_wider(id_cols = BPL, names_from = YEAR, values_from = POP)

bplfrac <- bplall %>% group_by(BPL) %>% mutate(basepop = ifelse(YEAR == 1881, POP, NA)) %>%
  fill(basepop, .direction = "updown") %>% mutate(POPFRAC = log(POP/basepop))

ggplot(bplall %>% filter(!(BPL %in% c("Canada", "US", "England", "Scotland", "Ireland", "Wales"))), 
       aes(x = YEAR, y = POP, color = BPL)) + geom_line()

ggplot(bplall %>% filter(!(BPL %in% c("Canada", "US", "England", "Scotland", "Ireland", "Wales"))), 
       aes(x = YEAR, y = POP)) + geom_line() + facet_wrap(~BPL)

# eastern europe
ggplot(bplfrac %>% filter((BPL %in% c("Austria and Hungary", "Poland", "Romania", "Russia"))), 
       aes(x = YEAR, y = POPFRAC)) + geom_line() + facet_wrap(~BPL)

# scandinavia
ggplot(bplfrac %>% filter((BPL %in% c("Denmark", "Finland", "Norway", "Sweden"))), 
       aes(x = YEAR, y = POPFRAC)) + geom_line() + facet_wrap(~BPL)

# western/southern europe
ggplot(bplfrac %>% filter((BPL %in% c("Belgium", "France", "Germany", "Italy", "Netherlands", "Switzerland"))), 
       aes(x = YEAR, y = POPFRAC)) + geom_line() + facet_wrap(~BPL)

# americas
ggplot(bplfrac %>% filter((BPL %in% c("Bermuda", "Brazil", "Chile", "West Indies", "Mexico"))), 
       aes(x = YEAR, y = POPFRAC)) + geom_line() + facet_wrap(~BPL)

# eastern hem
ggplot(bplfrac %>% filter((BPL %in% c("Australia and NZ", "China", "East Indies", "India", "Japan", "Turkey"))), 
       aes(x = YEAR, y = POPFRAC)) + geom_line() + facet_wrap(~BPL)

ggplot(bplfrac %>% filter(!(BPL %in% c("Canada"))), aes(x = YEAR, y = POPFRAC, color = BPL)) + geom_line()




