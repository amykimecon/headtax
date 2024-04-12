#_____________________________________________________________
# FILE DESCRIPTION: Cleaning Canadian Census Data, Consolidating Years
# CREATED BY: Amy Kim
# CREATED ON: Sep 2022
# LAST MODIFIED: April 2024
#_____________________________________________________________

library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)

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
us_states <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[1:82]))
canadian <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[84:1130]))
uk <- c("England", "Ireland", "Scotland", "Wales", "Jersey", "Isle of Man", "Gibraltar", "Great Britain", "Channel Islands",
        "Guernsey", "United Kingdom, n.s", "Northern Ireland") 

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
                         bpl2 >= 41000 & bpl2 < 41500 ~ "UK",
                         bpl2 == 42000 | bpl2 == 45600 | bpl2 == 42010 | bpl2 == 45700 | bpl2 == 45711~ "AustriaHungary",
                         bpl2 == 42100 ~ "Belgium",
                         bpl2 >= 42200 & bpl2 < 42600 ~ "France",
                         bpl2 == 42600 ~ "Netherlands",
                         bpl2 == 42700 ~ "Switzerland",
                         bpl2 == 43300 ~ "Greece",
                         bpl2 == 43500 ~ "Italy",
                         bpl2 == 43700 ~ "Portugal",
                         bpl2 == 44100 ~ "Spain",
                         bpl2 >= 45300 & bpl2 <= 45360 ~ "Germany",
                         bpl2 == 45700 ~ "Poland",
                         bpl2 == 47100 ~ "Romania",
                         bpl2 >= 48900 & bpl2 < 50000 ~ "Russia",
                         bpl2 == 50000 ~ "China",
                         bpl2 == 50400 ~ "Japan",
                         bpl2 >= 52100 & bpl2 <= 52105 ~ "India",
                         bpl2 == 70010 | bpl2 == 70015 ~ "Australia",
                         bpl2 == 70020 ~ "NZ",
                         bpl2 >= 25000 & bpl2 < 27000 ~ "WIndies",
                         bpl2 == 54600 ~ "Turkey",
                         bpl2 == 16010 ~ "Bermuda",
                         bpl2 >= 51200 & bpl2 < 53000 ~ "EIndies",
                         bpl2 == 30020 ~ "Chile",
                         bpl2 == 20000 ~ "Mexico",
                         bpl2 == 30005 ~ "Argentina",
                         bpl2 == 60011 ~ "Algeria",
                         bpl2 == 60012 ~ "Egypt",
                         bpl2 == 60090 ~ "SAfrica",
                         str_detect(bpl, "(BUROKOVIA|BUK)") ~ "Romania",
                         bpl2 >= 15000 & bpl2 < 16000 ~ "Canada",
                         bpl2 == 99999 ~ "Unknown",
                         TRUE ~ "Other")) %>%
  select(c(MALE, AGE, BPL, MAR, YRIMM, OCCGRP, EARN, CANREAD, IMM, PROVINCE, HOUSEOWN, LABOR)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, 
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian Poland", "Austrian-Galicia") | str_detect(bplstring, "Austria") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland") ~ "Russia",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Brittany","German Poland", "Bohemia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Normandy") ~ "France",
                         bplstring %in% c("East Indies", "Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick)") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", "Not Given", "At Sea") ~ "Unknown",
                         TRUE ~ bplstring),
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
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian Poland", "Austrian-Galicia") | str_detect(bplstring, "Austria") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland") ~ "Russia",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas", "Haiti", "Trinidad and Tobago") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Hesse", "Hamburg", 
                                          "Rhine Province", "Schleswig-Holstein", "Westphalia", "Wurtemberg", "Holstein", "Baden", 
                                          "Brandenburg", "Saxe-Weimar-Eisenach", "Thuringian State", "Pomerania", 'Posen', "West Prussia", "German Poland") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Alsace-Lorraine", "Brittany") ~ "France",
                         str_detect(bplstring, "East Indies") | bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick|Saskatchewan)") | 
                           bplstring %in% c("St. Pamphile de l'Islet") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", "Not Given", "At Sea") ~ "Unknown",
                         TRUE ~ bplstring),
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
# note: germany includes prussia, bavaria, berlin
# austria includes hungary 
# australia includes new zealand
# poland includes russian poland, prussian poland, galicia
# romania includes bukovina
# west indies includes haiti, jamaica, british west indies, antigua barbuda, bahamas, barbados, trinidad and tobago, 
#   cuba, dominica, dominican republic, grenada, st kitts and nevis, st lucia, st. vincent and grenadines
# france includes alsace, lorraine

bpl1871 <- raw1871 %>% 
  mutate(BPL = case_when(birthp13 %in% c("Ontario", "Quebec", "Nova scotia", "New brunswick", "Other canada") ~ "Canada",
                         birthpl %in% c("ON", "QU", "NS", "NB", "CA", "PE", "NE") ~ "Canada",
                         birthp13 %in% c("England", "Ireland", "Scotland") ~ "UK",
                         birthp13 == "Us" ~ "US",
                         birthpl == "PR" | birthpl == "GE" | birthpl == "BA" ~ "Germany",
                         bpcod == "East india" | bpcod == "Ceylon" ~ "EIndies",
                         birthpl == "DU" ~ "Netherlands",
                         bpcod %in% c("West indies", "Trinidad", "Jamaica") ~ "WIndies",
                         birthp13 == "Unknown" ~ "Unknown",
                         TRUE ~ as.character(bpcod))) %>% #filter(BPL == "Other") %>% group_by(bpcod) %>% summarize(POP = sum(popwgt))
  group_by(BPL) %>% summarize(POP = sum(popwgt)) %>% mutate(YEAR = 1871)

bpl1881 <- raw1881 %>% 
  mutate(bplstring = trimws(as.character(birthplace_ccri)),
         BPL = case_when(bplstring %in% c("German Poland","Prussia", "Bavaria", "Berlin", "Hamburg", "Wurtemberg", "Hesse", "Mecklenburg", "Hanover", "Saxony", "Holstein", "Brunswick") ~ "Germany",
                         bplstring == "Russian Poland" ~ "Russia",
                         bplstring %in% c("Hungary", "Austria-Vienna", "Austria") ~ "AustriaHungary",
                         bplstring == "Australia and New Zealand" | bplstring == "Australia" ~ "Australia",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Trinidad and Tobago", "Bahamas", 
                                          "British West Indies", "St. Vincent (West Indies)", "St. Kitts-Nevis", "Dominica", "Dominican Republic",
                                          "Grenada", "Haiti", "St. Lucia") ~ "WIndies",
                         bplstring == "Alsace" | bplstring == "Lorraine" ~ "France",
                         bplstring == "Southern Africa" ~ "SAfrica",
                         bplstring %in% c("Malaysia", "Sri Lanka (Ceylon)", "Burma (Myanmar)", "Singapore", "East Indies") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian ~ "Canada",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Illegible", "Not Mapped", "At Sea", "Invalid Value", "Blank", "Unknown") ~ "Unknown",
                         TRUE ~ bplstring)) %>% #filter(BPL == "Other") %>% group_by(bplstring) %>% summarize(POP = n())
  group_by(BPL) %>% summarize(POP = n()) %>% mutate(YEAR = 1881)

bpl1891 <- raw1891 %>% mutate(WEIGHT = case_when(samplesize == 5 ~ 20, samplesize == 10 ~ 10, samplesize == 100 ~ 1)) %>%
  mutate(BPL = case_when(bplcode < 10000 ~ "US",
                             bplcode == 40000 ~ "Denmark",
                             bplcode == 40100 ~ "Finland",
                             bplcode == 40400 ~ "Norway",
                             bplcode == 40500 ~ "Sweden",
                             bplcode >= 41000 & bplcode < 42000 ~ "UK",
                             bplcode == 42000 ~ "Belgium",
                             bplcode == 42100 | bplcode == 42111 ~ "France",
                             bplcode == 42500 ~ "Netherlands",
                             bplcode == 42600 ~ "Switzerland",
                             bplcode == 43300 ~ "Greece",
                             bplcode == 43400 ~ "Italy",
                             bplcode == 43600 ~ "Portugal",
                             bplcode == 43800 ~ "Spain",
                             bplcode == 45000 | bplcode == 45400 | bplcode == 45010 | bplcode == 45510 | bplcode == 45511 ~ "AustriaHungary",
                             bplcode >= 45300 & bplcode <= 45360 | BPL == "MECKLENBURGH" | (bplcode >= 45520 & bplcode < 45530) ~ "Germany",
                             bplcode == 45600 ~ "Romania",
                             (bplcode >= 46000 & bplcode < 49900) | bplcode == 45530 ~ "Russia",
                             bplcode == 50000 | BPL == "CANTON" ~ "China",
                             bplcode == 50100 ~ "Japan",
                             bplcode == 52100 ~ "India",
                             bplcode >= 70010 & bplcode < 70020 ~ "Australia",
                             bplcode == 70020 ~ "New Zealand",
                             bplcode >= 25000 & bplcode < 27000 ~ "WIndies",
                             bplcode == 16010 ~ "Bermuda",
                             bplcode == 26030 ~ "Jamaica",
                             bplcode == 20000 ~ "Mexico",
                             bplcode == 30005 ~ "Argentina",
                             bplcode == 30015 ~ "Brazil",
                             bplcode == 30020 ~ "Chile",
                             bplcode == 30050 ~ "Peru",
                             bplcode == 30060 ~ "Uruguay",
                             bplcode == 30065 ~ "Venezuela",
                             bplcode == 54200 ~ "Turkey",
                             bplcode == 60011 ~ "Algeria",
                             bplcode >= 51200 & bplcode < 53000 ~ "EIndies",
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
                                                  bpl2 >= 41000 & bpl2 < 41500 ~ "UK",
                                                  bpl2 == 42000 | bpl2 == 45600 | bpl2 == 42010 | bpl2 == 45700 | bpl2 == 45711~ "AustriaHungary",
                                                  bpl2 == 42100 ~ "Belgium",
                                                  bpl2 >= 42200 & bpl2 < 42600 ~ "France",
                                                  bpl2 == 42600 ~ "Netherlands",
                                                  bpl2 == 42700 ~ "Switzerland",
                                                  bpl2 == 43300 ~ "Greece",
                                                  bpl2 == 43500 ~ "Italy",
                                                  bpl2 == 43700 ~ "Portugal",
                                                  bpl2 == 44100 ~ "Spain",
                                                  bpl2 >= 45300 & bpl2 <= 45360 ~ "Germany",
                                                  bpl2 == 45700 ~ "Poland",
                                                  bpl2 == 47100 ~ "Romania",
                                                  bpl2 >= 48900 & bpl2 < 50000 ~ "Russia",
                                                  bpl2 == 50000 ~ "China",
                                                  bpl2 == 50400 ~ "Japan",
                                                  bpl2 >= 52100 & bpl2 <= 52105 ~ "India",
                                                  bpl2 == 70010 | bpl2 == 70015 ~ "Australia",
                                                  bpl2 == 70020 ~ "NZ",
                                                  bpl2 >= 25000 & bpl2 < 27000 ~ "WIndies",
                                                  bpl2 == 54600 ~ "Turkey",
                                                  bpl2 == 16010 ~ "Bermuda",
                                                  bpl2 >= 51200 & bpl2 < 53000 ~ "EIndies",
                                                  bpl2 == 30020 ~ "Chile",
                                                  bpl2 == 20000 ~ "Mexico",
                                                  bpl2 == 30005 ~ "Argentina",
                                                  bpl2 == 60011 ~ "Algeria",
                                                  bpl2 == 60012 ~ "Egypt",
                                                  bpl2 == 60090 ~ "SAfrica",
                                                  str_detect(bpl, "(BUROKOVIA|BUK)") ~ "Romania",
                                                  bpl2 >= 15000 & bpl2 < 16000 ~ "Canada",
                                                  bpl2 == 99999 ~ "Unknown",
                                                  TRUE ~ "Other")) %>% #filter(BPL == "Other") %>% group_by(bpl, bpl2) %>% summarize(POP = sum(WEIGHT))
                                group_by(BPL) %>% summarize(POP = sum(WEIGHT))  %>% mutate(YEAR = 1901)
                              
bpl1911 <- raw1911 %>% mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                                                 str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                                                 str_starts(Dwelling_Unit_Type, "MU") ~ 4)) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian Poland", "Austrian-Galicia") | str_detect(bplstring, "Austria") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland") ~ "Russia",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Brittany","German Poland", "Bohemia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Normandy") ~ "France",
                         bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)", "East Indies") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick)") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", "Not Given", "At Sea") ~ "Unknown",
                         TRUE ~ bplstring)) %>% #filter(BPL == "Other") %>% group_by(INDIVIDUAL_BIRTH_COUNTRY) %>% summarize(POP = sum(WEIGHT))
  group_by(BPL) %>% summarize(POP = sum(WEIGHT)) %>% mutate(YEAR = 1911)

bpl1921 <- raw1921 %>% mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                                                 str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                                                 str_starts(Dwelling_Unit_Type, "MU") ~ 5)) %>%
  mutate(bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian Poland", "Austrian-Galicia") | str_detect(bplstring, "Austria") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland") ~ "Russia",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas", "Haiti", "Trinidad and Tobago") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Hesse", "Hamburg", 
                                          "Rhine Province", "Schleswig-Holstein", "Westphalia", "Wurtemberg", "Holstein", "Baden", 
                                          "Brandenburg", "Saxe-Weimar-Eisenach", "Thuringian State", "Pomerania", 'Posen', "West Prussia", "German Poland") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Alsace-Lorraine", "Brittany") ~ "France",
                         str_detect(bplstring, "East Indies") | bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick|Saskatchewan)") | 
                           bplstring %in% c("St. Pamphile de l'Islet") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", "Not Given", "At Sea") ~ "Unknown",
                         TRUE ~ bplstring)) %>% # filter(BPL == "Other") %>% group_by(INDIVIDUAL_BIRTH_COUNTRY) %>% summarize(POP = sum(WEIGHT))
  group_by(BPL) %>% summarize(POP = sum(WEIGHT))  %>% mutate(YEAR = 1921)


## combining all years
bplall <- list(bpl1871, bpl1881, bpl1891, bpl1901, bpl1911, bpl1921) %>% bind_rows()

########################
#### interpolation
#pivoting data wide (id col = year, diff val cols for each country)
bplall_wide <- bplall %>% pivot_wider(id_cols = YEAR, names_from = BPL, values_from = POP) 
#create new col with all foreignborn population
#discarding any country with fewer than 2 values
#interpolating all inter-decennial values using natural cubic spline (_spline) and linear (_linear)
#pivoting data to be long on year x country
bpl_interp <- data.frame(YEAR = 1871:1921) %>% left_join(bplall_wide %>% mutate(ForeignBorn = rowSums(bplall_wide %>% select(-c(YEAR, Canada)), na.rm=TRUE))) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-YEAR, function(.x) spline(YEAR, .x, method = "natural", xout = YEAR)$y, .names = "{.col}_spline"),
         across(-c(YEAR, ends_with("spline")), function(.x) approx(YEAR, .x, xout = YEAR)$y, .names = "{.col}_linear")) %>%
  select(c(ends_with("spline"), ends_with("linear"), "YEAR")) %>%
  pivot_longer(cols = -YEAR, names_to = c("BPL", "INTERP"), names_pattern = "(.*)_(.*)", values_to = "POP") %>%
  mutate(POP = ifelse(POP < 0, 0, POP)) #lower bound of zero

write_csv(bpl_interp, glue("{dbox}/cleaned/popstock_can.csv"))

#visual comparison of linear and natural spline interpolation for a few countries
bpllist_small = c("Belgium", "Denmark", "France", "Germany", "Greece", "India", "Mexico", "Brazil", "Chile",
                  "Japan", "Netherlands", "Norway","China", "Sweden", "Finland", "Australia")

ggplot(bpl_interp %>% filter(INTERP != "none" & BPL %in% bpllist_small), aes(x = YEAR, y = POP, color = INTERP)) + geom_line() +
  geom_point(data = bplall %>% filter(BPL %in% bpllist_small), aes(x = YEAR, y = POP), inherit.aes = FALSE) + facet_wrap(~BPL)
