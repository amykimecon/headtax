########################################################################
### FILE DESCRIPTION: Cleaning Raw Canadian Census Data
### PRIMARY OBJECTIVE: Standardizing Primary Variables, Creating Single Multi-Year Dataframe
### CREATED BY: Amy Kim
### CREATED ON: Sep 29 2022
### LAST MODIFIED: Oct 5 2022
########################################################################
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
raw1852 <- read.spss(glue("{dbox}/census1852.sav")) %>% as.data.frame() # 20% sample
raw1871 <- read.spss(glue("{dbox}/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/census1921.sav")) %>% as.data.frame() # 4% sample

########################################################################
### CLEANING DATA
########################################################################
## BASE VARIABLES: LASTNAME, FIRSTNAME, MALE = 1[Male], MAR = 1[Married], SIN = 1[Single (Never Married)], AGE, BPL, OCC
## ADDITIONAL VARIABLES: ETH, CANREAD, UNEMP, EARN, YRIMM, HOUSEOWN
## ADD: YEAR
## GROUP/CLEAN: OCCUPATION, BPLCHI, BPL[OTHER], ETHCHI, ETH[OTHER]

#### 1881 DATA ####
clean1881 <- raw1881 %>% rename(BPL = birthplace_ccri, ETH = ethnicity_ccri, OCC = DOCCUP, OCCGRP = occgrp2, 
                                CLASSGRP = classgrp, LASTNAME = NAMLAST, FIRSTNAME = NAMFRST) %>%
  mutate(MALE = case_when(SEX == "Female" ~ 0,
                          SEX == "Male" ~ 1,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Unknown" ~ NA_real_,
                         TRUE ~ 0),
         NAPHISCOSTR = as.character(NAPHISCO),
         IMM = ifelse(imm == "born in Canada", 0, 1),
         OCCGRP = case_when(OCCGRP == "White collar" | OCCGRP == "Profesl" | OCCGRP == "Merc/Agent/Manu" ~ "Skilled",
                            OCCGRP == "Artisan" | OCCGRP ==  "Semi & Unskilled" ~ "Skilled",
                            OCCGRP == "Labourer" | OCCGRP == "Servant" ~ "Unskilled",
                            OCCGRP == "Farmer" ~ "Unskilled",
                            TRUE ~ NA_character_)) %>% 
  select(c(MALE, AGE, MAR, ETH, OCC, NAPHISCOSTR, OCCGRP, BPL, IMM, LASTNAME, FIRSTNAME)) %>% 
  mutate(YEAR = 1881, BORNCHI = ifelse(str_detect(BPL, "China"),1,0), BORNRUS = ifelse(str_detect(BPL, "Russia"), 1, 0), 
         BORNFRA = ifelse(str_detect(BPL, "France"), 1, 0), 
         BORNGER = ifelse(str_detect(BPL, "Germany"), 1, 0),
         BORNJAP = ifelse(str_detect(BPL, "Japan"),1,0),
         BORNIND = ifelse(str_detect(BPL, "India"),1,0),
         BORNIRE = ifelse(str_detect(BPL, "Ireland"), 1, 0),
         BORNAUS = ifelse(str_detect(BPL, "Austria"), 1, 0),
         ETHCHI = ifelse(str_detect(ETH, "Chinese"), 1, 0), WEIGHT = 1)


clean1891 <- raw1891 %>% mutate(WEIGHT = case_when(samplesize == 5 ~ 20,
                                                   samplesize == 10 ~ 10,
                                                   samplesize == 100 ~ 1),
                                BORNCHI = ifelse(bplcode == 50000, 1, 0),
                                BORNRUS = ifelse(bplcode >= 46100 & bplcode < 49000, 1, 0),
                                BORNFRA = ifelse(bplcode == 42100, 1, 0),
                                BORNGER = ifelse(bplcode == 45300, 1, 0),
                                BORNJAP = ifelse(bplcode == 50100, 1, 0),
                                BORNIND = ifelse(bplcode == 52100, 1, 0),
                                BORNIRE = ifelse(bplcode == 41400, 1, 0),
                                BORNAUS = ifelse(bplcode == 45000, 1, 0),
                                MALE = case_when(SEX == "M  " ~ 1,
                                                 SEX == "F  " ~ 0,
                                                 TRUE ~ NA_real_),
                                MAR = case_when(MARST == "married" ~ 1,
                                                MARST == "single" | MARST == "Divorced" | MARST == "W" ~ 0,
                                                TRUE ~ NA_real_),
                                CANREAD = ifelse(CANREAD == "Yes, can read", 1, 0),
                                IMM = ifelse(bplcode < 16000 & bplcode >= 15000, 0, 1),
                                OCCGRP = case_when((occ50 < 975 & occ50 > 800) | occ50 == 720 ~ "Unskilled",
                                                   occ50 < 100 | (occ50 >= 200 & occ50 < 300) ~ "Skilled",
                                                   occ50 >= 100 & occ50 < 200 ~ "Unskilled",
                                                   occ50 >= 300 & occ50 < 700 ~ "Skilled",
                                                   TRUE ~ NA_character_)) %>%
  select(-c(AGE)) %>%
  rename(NAPHISCO = NappHisco, AGE = agecode, LASTNAME = NAMELAST, FIRSTNAME = NAMEFIRST) %>%
  select(c(MALE, AGE, MAR, CANREAD, starts_with("BORN"), UNEMP, OCCGRP, WEIGHT, IMM, LASTNAME, FIRSTNAME)) %>%
  mutate(YEAR = 1891)

clean1901 <- raw1901 %>% rename(BPL = bpl, SEX = sex, MARST = marst, AGE = ageyr, YRIMM = immyr, PROPOWNR = propownr, NATL = natl, OCC = occ, 
                                EARN = earnings, CANREAD = canread, LASTNAME = indlnm, FIRSTNAME = indfnm) %>%
  mutate(BORNCHI = ifelse(bpl2 == 50000, 1, 0),
         BORNRUS = ifelse(str_detect(BPL, "RUS"), 1, 0),
         BORNFRA = ifelse(str_detect(BPL, "FRA"), 1, 0),
         BORNGER = ifelse(str_detect(BPL, "GER"), 1, 0),
         BORNJAP = ifelse(str_detect(BPL, "JAP"), 1, 0),
         BORNIND = ifelse(str_detect(BPL, "IND"), 1, 0),
         BORNIRE = ifelse(str_detect(BPL, "IRE"), 1, 0),
         BORNAUS = ifelse(str_detect(BPL, "AUS") & !str_detect(BPL, "AUSTRALIA"), 1, 0),
         OCCSTR = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{2}")))), 
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         PROPOWNR = ifelse(PROPOWNR == "Yes", 1, 0),
         YRIMM = as.numeric(YRIMM),
         IMM = ifelse(bpl2 < 16000 & bpl2 >= 15000, 0, 1),
         LABOR = ifelse(str_detect(OCC, "LAB"), 1, 0),
         OCCGRP = case_when(OCCSTR >= 71 ~ "Unskilled",
                            OCCSTR >= 40 & OCCSTR <= 70 ~ "Skilled",
                            OCCSTR < 40 ~ "Skilled",
                            TRUE ~ NA_character_)) %>%
  select(c(MALE, AGE, starts_with("BORN"), MAR, YRIMM, PROPOWNR, NATL, OCCGRP, EARN, CANREAD, IMM, FIRSTNAME, LASTNAME, LABOR)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

bplcanadastrings <- "(Ontario)|(Quebec)|(Nova Scotia)|(New Brunswick)|(Manitoba)|(Saskatchewan)|(Prince Edward Island)|(British Columbia)|(Alberta)|(Canada)|(Newfoundland)|(Northwest Territories)|(Yukon)|(Cape Breton)|(Labrador)"

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(EARN = ifelse(grepl("[0-9]+", EARNINGS_AT_CHIEF_OCC), as.numeric(as.character(EARNINGS_AT_CHIEF_OCC)), 0) + 
           ifelse(grepl("[0-9]+", EARNINGS_AT_OTHER_OCC), as.numeric(as.character(EARNINGS_AT_OTHER_OCC)), 0),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), as.numeric(as.character(YRIMM)), NA),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 4),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCCIND = str_to_lower(OCCUPATION_CHIEF_OCC_IND_CL),
         LABOR = ifelse(str_detect(OCCIND, "Labour"), 1, 0),
         OCCGRP = case_when(OCC == "Laborers (n.e.c.)" ~ "Unskilled",
                            OCC == "Managers, officials, and proprietors (n.e.c.)" ~ "Skilled",
                            occ1 == "Agriculture" | occ1 == "Forestry and lumbering" | occ1 == "Mining" | occ1 == "Fisheries and hunting" | occ1 == "Building trades" | occ1 == "Transportation" | str_starts(occ1, "Domestic")~ "Unskilled",
                            str_starts(occ1, "Manufactures") | occ1 == "Trade and Merchandising" ~ "Skilled",
                            occ1 == "Civil and municipal service" | occ1 == "Professional pursuits" ~ "Skilled")) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, FIRSTNAME, LASTNAME, LABOR)) %>% 
  mutate(YEAR = 1911, BORNCHI = ifelse(BPL == "China", 1, 0), BORNRUS = ifelse(BPL == "Russia", 1, 0),
         BORNFRA = ifelse(BPL == "France", 1, 0), BORNGER = ifelse(BPL == "Germany", 1, 0),
         BORNJAP = ifelse(BPL == "Japan", 1, 0), BORNIND = ifelse(BPL == "India", 1, 0),
         BORNIRE = ifelse(BPL == "Ireland", 1, 0), BORNAUS = ifelse(BPL == "Austria", 1, 0))

clean1921 <- raw1921 %>% rename(AGE = Derived_Age_In_Years, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = CHIEFOCCUP, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(EARN = ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), as.numeric(as.character(ANNUAL_EARNING_AMOUNT)), 0),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), as.numeric(as.character(YRIMM)), NA),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 5),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCC = str_to_lower(OCC),
         LABOR = ifelse(str_detect(OCC, "labor") | str_detect(OCC, "labour"), 1, 0),
         OCCGRP = case_when(str_detect(CHIEF_OCCUPATION, "Farm") | str_detect(CHIEF_OCCUPATION, "Labor") | str_detect(CHIEF_OCCUPATION, "Mine") | CHIEF_OCCUPATION == "Private household workers (n.e.c.)" | str_detect(CHIEF_OCCUPATION, "Lumbermen") | CHIEF_OCCUPATION == "Carpenters" | CHIEF_OCCUPATION == "Machinists" | CHIEF_OCCUPATION == "Housekeepers, private household" | CHIEF_OCCUPATION == "Fishermen and oystermen" ~ "Unskilled",
                            CHIEF_OCCUPATION != "Blank" & CHIEF_OCCUPATION != "Unemployed/ without occupation" ~ "Skilled")) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, FIRSTNAME, LASTNAME, LABOR)) %>% 
  mutate(YEAR = 1921, BORNCHI = ifelse(BPL == "China", 1, 0), BORNRUS = ifelse(BPL == "Russia", 1, 0),
         BORNFRA = ifelse(BPL == "France", 1, 0), BORNGER = ifelse(BPL == "Germany", 1, 0),
         BORNJAP = ifelse(BPL == "Japan", 1, 0), BORNIND = ifelse(BPL == "India", 1, 0),
         BORNIRE = ifelse(BPL == "Ireland", 1, 0), BORNAUS = ifelse(BPL == "Austria", 1, 0))

# binding all years and cleaning together
clean_all <- bind_rows(clean1881, clean1891) %>% bind_rows(clean1901) %>%
  bind_rows(clean1911) %>% bind_rows(clean1921) %>%
  mutate(AGE = ifelse(AGE > 200, NA, AGE),
         YRIMM = ifelse(YRIMM < 1500 | YRIMM > YEAR, NA, YRIMM))
clean_imm <- clean_all %>% filter(IMM == 1)
clean_chi <- clean_all %>% filter(BORNCHI == 1)

# #### IMPUTING RACE/ETHNICITY FROM NAME FOR 1852 AND 1871 DATA ####
# ## using predictrace package
# library(predictrace)
# chineselastnames <- predict_race(unique(str_remove(str_extract(clean_chi$LASTNAME, "^[A-z]+ "), " $")))
# chineselastnames2 <- predict_ethnicity(lastnames = na.omit(unique(str_remove(str_extract(clean_chi$LASTNAME, "^[A-z]+ "), " $"))), method = "lastname")
# 
# chinesefirstnames <- predict_race(unique(str_remove(str_extract(clean_chi$FIRSTNAME, "^[A-z]+ "), " $")), surname = FALSE)
# 
# clean1852 <- raw1852 %>% mutate(firstname = str_remove(str_extract(namfrst, "^[A-z]+ "), " $"),
#                                 lastname = str_remove(namlast, "\\s+$"),
#                                 firstname_predrace = predict_race(firstname, surname = FALSE)$likely_race,
#                                 lastname_predrace = predict_race(lastname)$likely_race,
#                                 predasian = ifelse(firstname_predrace == "asian" & lastname_predrace == "asian", 1, 0),
#                                 predchi = ifelse((str_to_lower(lastname) %in% str_to_lower(chineselastnames)) & nchar(lastname) > 1 & lastname_predrace == "asian", 1, 0))

write_csv(clean_all, glue("{dbox}/cleaned/census_all.csv"))


