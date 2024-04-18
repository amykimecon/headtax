#_____________________________________________________________
# FILE DESCRIPTION: Cleaning Canadian Census Data, Consolidating Years
# CREATED BY: Amy Kim
# CREATED ON: Sep 2022
# LAST MODIFIED: April 2024
#_____________________________________________________________

#_____________________________________________________________
# IMPORTING RAW CENSUS DATA ----
#_____________________________________________________________
## canadian census data from 1871-1921
raw1871 <- read.spss(glue("{dbox}/raw/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/raw/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/raw/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 4% sample

#raw1911 <-read_csv(glue("{dbox}/raw/census1911.csv"), guess_max = 1000000)
#_____________________________________________________________
# DECLARING KEY VARIABLES AND LISTS ----
#_____________________________________________________________
# census years
years <- seq(1871, 1921, 10)

# us states, canadian provinces/places, UK countries (to define country of origin)
us_states <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[1:82]))
canadian <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[84:1130]))
uk <- c("England", "Ireland", "Scotland", "Wales", "Jersey", "Isle of Man", "Gibraltar", "Great Britain", "Channel Islands",
        "Guernsey", "United Kingdom, n.s", "Northern Ireland", "Orkney Islands", "Shetland Islands") 

#_____________________________________________________________
# CLEANING CENSUS DATA ----
#_____________________________________________________________
# variables to keep (for all censuses)
varnames <- c("WEIGHT", "YEAR", "AGE", "WHIPPLE", "MALE", "MAR", "BPL", "IMM")
varnames_extra <- c(varnames, "YRIMM", "CANREAD", "SPEAKENG", "SPEAKENGFR")
## 1871 Census ----
clean1871 <- raw1871 %>% 
  mutate(WEIGHT = popwgt,
         YEAR = 1871,
         AGE = ifelse(ageerr == "Error", NA, age),
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(sex == "Male" ~ 1, sex == "Female" ~ 0, TRUE ~ NA_integer_),
         MAR = case_when(AGE < 18 ~ NA_integer_,
                         marr == "M" ~ 1,
                         martyp %in% c("Child or servant", "Missing, so child") ~ NA_integer_,
                         TRUE ~ 0),
         BPL = case_when(birthp13 %in% c("Ontario", "Quebec", "Nova scotia", "New brunswick", "Other canada") ~ "Canada",
                         birthpl %in% c("ON", "QU", "NS", "NB", "CA", "PE", "NE") ~ "Canada",
                         birthp13 %in% c("England", "Ireland", "Scotland") ~ "UK",
                         birthp13 == "Us" ~ "US",
                         birthpl == "PR" | birthpl == "GE" | birthpl == "BA" ~ "Germany",
                         bpcod == "East india" | bpcod == "Ceylon" ~ "EIndies",
                         birthpl == "DU" ~ "Netherlands",
                         bpcod %in% c("West indies", "Trinidad", "Jamaica") ~ "WIndies",
                         birthpl == "FR" ~ "France",
                         bpcod %in% c("Austria", "Bohemia") ~ "AustriaHungary",
                         bpcod == "Cape of good hope" ~ "SAfrica",
                         bpcod == "Calcutta" ~ "India",
                         birthp13 == "Unknown" ~ "Unknown",
                         bpcod == "Africa" ~ "Other Foreign",
                         TRUE ~ as.character(bpcod)),
         IMM = case_when(BPL == "Canada" ~ 0,
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1)) %>%
  select(all_of(varnames)) 

## 1881 Census ----
clean1881 <- raw1881 %>% 
  mutate(WEIGHT = 1,
         YEAR = 1881,
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          SEX == "Unknown" ~ NA_integer_),
         MAR = case_when(AGE < 18 ~ NA_integer_,
                         MARST == "Married" ~ 1,
                         MARST %in% c("Divorced", "Widowed", "Single") ~ 0,
                         MARST == "Unknown" ~ NA_integer_),
         bplstring = trimws(as.character(birthplace_ccri)),
         BPL = case_when(bplstring %in% c("Prussia", "Bavaria", "Berlin", "Hamburg", "Wurtemberg", "Hesse", "Mecklenburg", "Hanover", "Saxony", "Holstein", "Brunswick") ~ "Germany",
                         str_detect(bplstring, "Poland") ~ "Poland",
                         bplstring %in% c("Hungary", "Austria-Vienna", "Austria", "Bohemia") ~ "AustriaHungary",
                         bplstring == "Australia and New Zealand" | bplstring == "Australia" ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Trinidad and Tobago", "Bahamas", 
                                          "British West Indies", "St. Vincent (West Indies)", "St. Kitts-Nevis", "Dominica", "Dominican Republic",
                                          "Grenada", "Haiti", "St. Lucia", "West Indies") ~ "WIndies",
                         bplstring == "Alsace" | bplstring == "Lorraine" ~ "France",
                         bplstring == "Southern Africa" | bplstring == "South Africa (Union of)" ~ "SAfrica",
                         bplstring %in% c("Malaysia", "Sri Lanka (Ceylon)", "Burma (Myanmar)", "Singapore", "East Indies") ~ "EIndies",
                         bplstring == "Hong Kong" ~ "China",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian ~ "Canada",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Illegible", "Not Mapped", "At Sea", "Invalid Value", "Blank", "Unknown", "NA") ~ "Unknown",
                         TRUE ~ bplstring),
         IMM = case_when(BPL == "Canada" ~ 0,
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1)) %>%
  select(all_of(varnames)) 

## 1891 Census ----
clean1891 <- raw1891 %>% 
  mutate(WEIGHT = case_when(samplesize == 5 ~ 20, samplesize == 10 ~ 10, samplesize == 100 ~ 1),
         YEAR = 1891,
         AGE = ifelse(agecode == 999, NA, as.numeric(agecode)),
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(sexcode == 0 ~ 0,
                          sexcode == 1 ~ 1,
                          TRUE ~ NA_integer_),
         MAR = case_when(AGE < 18 ~ NA_integer_,
                         mscode1 == "Married" ~ 1,
                         mscode1 %in% c(5,6) ~ 0,
                         TRUE ~ NA_integer_),
         BPL = case_when(bplcode < 10000 ~ "US",
                          str_detect(BPL, "ICELAND") ~ "Iceland",
                          bplcode == 40000 ~ "Denmark",
                          bplcode == 40100 ~ "Finland",
                          bplcode == 40400 ~ "Norway",
                          bplcode == 40500 ~ "Sweden",
                          bplcode >= 41000 & bplcode < 42000 | str_detect(BPL, "GIB") ~ "UK",
                          bplcode == 42000 ~ "Belgium",
                          bplcode == 42100 | bplcode == 42111 ~ "France",
                          bplcode == 42500 ~ "Netherlands",
                          bplcode == 42600 ~ "Switzerland",
                          bplcode == 43300 ~ "Greece",
                          bplcode == 43400 ~ "Italy",
                          bplcode == 43600 ~ "Portugal",
                          bplcode == 43800 ~ "Spain",
                          bplcode == 45000 | bplcode == 45400 | bplcode == 45010 | bplcode == 45510 | bplcode == 45511 | bplcode == 45210 ~ "AustriaHungary",
                          bplcode >= 45300 & bplcode <= 45360 | BPL == "MECKLENBURGH" | (bplcode >= 45520 & bplcode < 45530) ~ "Germany",
                          bplcode %in% c(45500, 45524, 45530) ~ "Poland",
                          bplcode == 45600 ~ "Romania",
                          (bplcode >= 46000 & bplcode < 49900) | bplcode == 45530 ~ "Russia",
                          bplcode == 50000 | BPL == "CANTON" | str_detect(BPL, "HONG KONG") ~ "China",
                          bplcode == 50100 ~ "Japan",
                          bplcode == 52100 ~ "India",
                          bplcode >= 70010 & bplcode < 70020 ~ "Australia",
                          bplcode == 70020 ~ "NZ",
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
                          BPL %in% c("C. OF GOOD HOPE", "CAPE OF GOOD HO", "CAPE TOWN AFRIC", "CC AFRICA") |
                            str_detect(BPL, "CC AFRICA") | str_detect(BPL, "S AFRICA") | str_detect(BPL, "SOUTH AFRICA") ~ "SAfrica",
                          str_detect(BPL, "SYR") | str_detect(BPL, "ASSYRIA") ~ "Syria",
                          bplcode >= 51200 & bplcode < 53000 ~ "EIndies",
                          str_detect(BPL, "MALTA") ~ "Malta",
                          bplcode >= 15000 & bplcode < 16000 ~ "Canada",
                          bplcode >= 90000 ~ "Unknown",
                          TRUE ~ "Other Foreign"),
         IMM = case_when(BPL == "Canada" ~ 0,
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1)) %>%
  select(all_of(varnames))

## 1901 Census ----
clean1901 <- raw1901 %>% 
  mutate(WEIGHT = 20,
         YEAR = 1901,
         AGE = ageyr,
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(sex == "Male" ~ 1,
                          sex == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         marst == "Married" ~ 1,
                         marst == "Single" | marst == "Widowed" | marst == "Divorced" | marst == "Separated" ~ 0,
                         TRUE ~ NA_real_),
         BPL = case_when(bpl2 < 10000 ~ "US",
                         bpl2 == 40000 ~ "Denmark",
                         bpl2 == 40100 ~ "Finland",
                         bpl2 == 40200 ~ "Iceland",
                         bpl2 == 40400 ~ "Norway",
                         bpl2 == 40500 ~ "Sweden",
                         bpl2 == 42000 | bpl2 == 45600 | bpl2 == 42010 | bpl2 == 45010 ~ "AustriaHungary",
                         bpl2 == 45711 | bpl2 == 45700  ~ "Poland",
                         bpl2 == 42100 ~ "Belgium",
                         bpl2 >= 42200 & bpl2 < 42600 ~ "France",
                         bpl2 == 42600 ~ "Netherlands",
                         bpl2 == 42700 ~ "Switzerland",
                         bpl2 == 43300 ~ "Greece",
                         bpl2 == 43500 ~ "Italy",
                         bpl2 == 43700 ~ "Portugal",
                         bpl2 == 44100 ~ "Spain",
                         bpl2 >= 45300 & bpl2 <= 45360 ~ "Germany",
                         bpl2 == 47100 ~ "Romania",
                         bpl2 >= 48900 & bpl2 < 50000 ~ "Russia",
                         bpl2 == 50000 | str_detect(bpl, "HONG KONG")~ "China",
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
                         bpl2 >= 41000 & bpl2 < 41500 | str_detect(bpl, "GIB") ~ "UK",
                         bpl2 >= 15000 & bpl2 < 16000 ~ "Canada",
                         bpl2 == 99999 | bpl2 == 99998 | str_detect(bpl, "AT SEA") | str_detect(bpl, "ATLANTIC OCEAN") |
                           str_detect(bpl, "NOT GIVEN") | str_detect(bpl, "UNKNOWN") ~ "Unknown",
                         TRUE ~ "Other Foreign"),
         YRIMM = ifelse(as.numeric(immyr) > YEAR, NA, as.numeric(immyr)),
         IMM = case_when(BPL == "Canada" ~ 0,
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             canread == "Yes" ~ 1,
                             canread == "No" ~ 0,
                             TRUE ~ NA_real_),
         SPEAKENG = case_when(AGE < 6 ~ NA_integer_,
                              english == "Yes" ~ 1,
                              english == "No" ~ 0,
                              TRUE ~ NA_integer_),
         SPEAKENGFR = case_when(AGE < 6 ~ NA_integer_,
                                SPEAKENG == 1 ~ 1,
                                french == "Yes" ~ 1,
                                is.na(SPEAKENG) ~ NA_integer_,
                                SPEAKENG == 0 & french == "No" ~ 0,
                                TRUE ~ NA_integer_),
         OCCSTR = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{3}")))), #extracts first three digits of occupation code (IPUMS)
         # HOUSEOWN = ifelse(AGE >= 18, ifelse(PROPOWNR == "Yes", 1, 0), NA),
         # LABOR = ifelse(AGE >= 18 & MALE == 1, ifelse(str_detect(OCC, "LAB"), 1, 0), NA),
         # OCCGRP = case_when(occ2 == "Labourer" | str_detect(OCC, "LAB") ~ "Laborer",
         #                    OCCSTR >= 711 & OCCSTR < 719 ~ "Farmer",
         #                    OCCSTR < 614 ~ "Skilled",
         #                    OCCSTR >= 812 & OCCSTR < 955 ~ "Skilled", #blacksmiths etc., with laborers already removed
         #                    !is.na(OCCSTR) ~ "Unskilled",
         #                    TRUE ~ NA_character_),
         # EARN = ifelse(AGE >= 18 & MALE == 1 & !is.na(earnings), earnings + ifelse(is.na(exearn), 0, exearn), NA)
  )%>%
  select(all_of(varnames_extra)) 

## 1911 Census ----
# getting crosswalk for csv from documentation

clean1911 <- raw1911 %>% 
  mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 4),
         YEAR = 1911,
         AGE = ifelse(AGE_AMOUNT > 120, NA, floor(AGE_AMOUNT)),
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         MARITAL_STATUS == "Married" ~ 1,
                         MARITAL_STATUS %in% c("Single", "Widowed", "Divorced", "Legally Separated", "Separated, n.s") ~ 0,
                         TRUE ~ NA_real_),
         bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian-Galicia") | str_detect(bplstring, "Austria") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland", "Austrian Poland","German Poland") ~ "Poland",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Brittany", "Bohemia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Normandy") ~ "France",
                         bplstring == "Hong Kong" ~ "China",
                         bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)", "East Indies") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick)") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", "Not Given", "At Sea", 
                                          "Damaged", "None", "Suspicious", "Unknown", "Unknown - Suggestion") ~ "Unknown",
                         TRUE ~ bplstring),
         YRIMM_raw = ifelse(grepl("[0-9]+", YEAR_OF_IMMIGRATION), suppressWarnings(as.numeric(as.character(YEAR_OF_IMMIGRATION))), NA_real_),
         YRIMM = ifelse(YRIMM_raw < 1500 | YRIMM_raw > YEAR, NA, YRIMM_raw),
         IMM = case_when(BPL == "Canada" ~ 0, 
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             CAN_READ_INDICATOR == "Yes" ~ 1,
                             CAN_READ_INDICATOR == "No" ~ 0,
                             TRUE ~ NA_real_),
         SPEAKENG = case_when(AGE < 6 ~ NA_integer_,
                              str_detect(LANGUAGE_SPOKEN_1, "English") | str_detect(LANGUAGE_SPOKEN_2, "English") | str_detect(LANGUAGE_SPOKEN_3, "English") ~ 1,
                              LANGUAGE_SPOKEN_1 %in% c("Uncodable", "Invalid Value", "Unknown", "Not Given", "None", "Unknown - Suggestion", "Missing -- Mandatory Field", 
                                                       "Suspicious", "Illegible", "Damaged", "Blank") ~ NA_integer_,
                              TRUE ~ 0),
         SPEAKENGFR = case_when(AGE < 6 ~ NA_integer_,
                              SPEAKENG == 1 ~ 1,
                              str_detect(LANGUAGE_SPOKEN_1, "French") | str_detect(LANGUAGE_SPOKEN_2, "French") | str_detect(LANGUAGE_SPOKEN_3, "French") ~ 1,
                              LANGUAGE_SPOKEN_1 %in% c("Uncodable", "Invalid Value", "Unknown", "Not Given", "None", "Unknown - Suggestion", "Missing -- Mandatory Field", 
                                                       "Suspicious", "Illegible", "Damaged", "Blank") ~ NA_integer_,
                              TRUE ~ 0),
         # OCCIND = str_to_lower(OCCUPATION_CHIEF_OCC_IND_CL),
         # LABOR = ifelse(AGE >= 18 & MALE == 1, ifelse(str_detect(OCCIND, "(L|l)abour") | str_detect(OCCIND, "(L|l)abor"), 1, 0), NA),
         # OCCGRP = case_when(occ1 == "Agriculture" ~ "Farmer",
         #                    OCC == "Laborers (n.e.c.)" |  str_detect(OCCIND, "Labour") ~ "Laborer",
         #                    OCC == "Managers, officials, and proprietors (n.e.c.)" ~ "Skilled",
         #                    occ1 == "Forestry and lumbering" | occ1 == "Mining" | occ1 == "Fisheries and hunting" | str_starts(occ1, "Domestic")~ "Unskilled",
         #                    str_starts(occ1, "Manufactures") | occ1 == "Trade and Merchandising" | occ1 == "Transportation" | occ1 == "Building trades" ~ "Skilled",
         #                    occ1 == "Civil and municipal service" | occ1 == "Professional pursuits" ~ "Skilled"),
         # RURAL = ifelse(str_detect(CCRI_URBAN_RURAL_1911,"Rural"), 1, 0),
         # PROVINCE = str_trim(PR_1911),
         # EARN = ifelse(AGE >= 18 & MALE == 1 & grepl("[0-9]+", EARNINGS_AT_CHIEF_OCC),
         #               suppressWarnings(as.numeric(as.character(EARNINGS_AT_CHIEF_OCC))) + 
         #                 ifelse(grepl("[0-9]+", EARNINGS_AT_OTHER_OCC), suppressWarnings(as.numeric(as.character(EARNINGS_AT_OTHER_OCC))), 0),
         #               NA),
         # HOUSEOWN = ifelse(AGE >= 18, ifelse(RELATIONSHIP == "Head", 1, 0), NA)) %>%
  )%>%
  select(all_of(varnames_extra))

## 1921 Census ----
clean1921 <- raw1921 %>% 
  mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 5),
         YEAR = 1921,
         AGE = ifelse(Derived_Age_In_Years < 120, floor(Derived_Age_In_Years), NA),
         WHIPPLE = ifelse(!is.na(AGE) & AGE >= 23 & AGE <= 62, ifelse(AGE %% 5 == 0, 1, 0), NA),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(AGE < 18 ~ NA_real_,
                         MARITAL_STATUS == "Married" ~ 1,
                         MARITAL_STATUS %in% c("Single", "Widowed", "Divorced", "Legally Separated", "Separated, n.s") ~ 0,
                         TRUE ~ NA_real_),
         bplstring = as.character(INDIVIDUAL_BIRTH_COUNTRY),
         BPL = case_when(bplstring %in% c("Hungary","Galicia", "Austrian-Galicia") | 
                           str_detect(bplstring, "Austria") | str_detect(bplstring, "Bohemia") ~ "AustriaHungary",
                         bplstring %in% uk ~ "UK",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Russian Poland", "Austrian Poland", "German Poland") ~ "Poland",
                         bplstring %in% c("Bukovina") ~ "Romania",
                         bplstring %in% c("Australia") ~ "Australia",
                         bplstring == "New Zealand" ~ "NZ",
                         bplstring %in% c("Jamaica", "Barbados", "Cuba", "Antigua-Barbuda", "Bahamas", "Haiti", "Trinidad and Tobago") | str_detect(bplstring, "West Indies") ~ "WIndies",
                         bplstring %in% c("Berlin", "Bavaria", "Prussia", "East Prussia", "Hanover", "Saxony", "Hesse", "Hamburg", 
                                          "Rhine Province", "Schleswig-Holstein", "Westphalia", "Wurtemberg", "Holstein", "Baden", 
                                          "Brandenburg", "Saxe-Weimar-Eisenach", "Thuringian State", "Pomerania", 'Posen', "West Prussia") ~ "Germany",
                         str_detect(bplstring, "Turkey") ~ "Turkey",
                         bplstring == "Hong Kong" ~ "China",
                         str_detect(bplstring, "South Africa") ~ "SAfrica",
                         bplstring %in% c("Alsace", "Lorraine", "Alsace-Lorraine", "Brittany") ~ "France",
                         str_detect(bplstring, "East Indies") | bplstring %in% c("Malaysia", "New Guinea", "Philippines", "Singapore", "Burma (Myanmar)", "Sri Lanka (Ceylon)") ~ "EIndies",
                         bplstring %in% us_states ~ "US",
                         bplstring %in% canadian | str_detect(bplstring, "(Quebec|Newfoundland|Nova Scotia|New Brunswick|Saskatchewan)") | 
                           bplstring %in% c("St. Pamphile de l'Islet") ~ "Canada",
                         bplstring %in% c("Missing -- Mandatory Field", "Illegible", "Uncodable", "Blank", 
                                          "Not Given", "At Sea", "Damaged", "In Error", "None", "Suspicious", "Unknown", "Unknown - Suggestion") ~ "Unknown",
                         TRUE ~ bplstring),
         YRIMM_raw = ifelse(grepl("[0-9]+", YEAR_OF_IMMIGRATION), 
                        suppressWarnings(as.numeric(as.character(YEAR_OF_IMMIGRATION))), NA_real_),
         YRIMM = ifelse(YRIMM_raw < 1500 | YRIMM_raw > YEAR, NA, YRIMM_raw),
         IMM = case_when(BPL == "Canada" ~ 0,
                         BPL == "Unknown" ~ NA_integer_,
                         TRUE ~ 1),
         CANREAD = case_when(AGE < 18 ~ NA_real_,
                             CAN_READ_INDICATOR == "Yes" ~ 1,
                             CAN_READ_INDICATOR == "No" ~ 0,
                             TRUE ~ NA_real_),
         SPEAKENG = case_when(AGE < 6 ~ NA_integer_,
                              SPEAK_ENGLISH_INDICATOR == "Yes" ~ 1,
                              SPEAK_ENGLISH_INDICATOR == "No" ~ 0,
                              TRUE ~ NA_integer_),
         SPEAKENGFR = case_when(AGE < 6 ~ NA_integer_,
                                SPEAKENG == 1 ~ 1,
                                SPEAK_FRENCH_INDICATOR == "Yes" ~ 1,
                                is.na(SPEAKENG) | SPEAK_FRENCH_INDICATOR != "No" ~ NA_integer_,
                                SPEAKENG == 0 & SPEAK_FRENCH_INDICATOR == "No" ~ 0,
                                TRUE ~ NA_integer_),
        #  OCC = str_to_lower(OCC),
        #  LABOR = ifelse(AGE >= 18 & MALE == 1, ifelse(str_detect(OCC, "labor") | str_detect(OCC, "labour"), 1, 0), NA),
        #  OCCGRP = case_when(str_detect(CHIEF_OCCUPATION, "Farm") ~ "Farmer",
        #                     str_detect(CHIEF_OCCUPATION, "Labor") | str_detect(CHIEF_OCCUPATION, "labor") ~ "Laborer",
        #                     str_detect(CHIEF_OCCUPATION, "Mine") | CHIEF_OCCUPATION == "Private household workers (n.e.c.)" | str_detect(CHIEF_OCCUPATION, "Lumbermen") | CHIEF_OCCUPATION == "Carpenters" | CHIEF_OCCUPATION == "Housekeepers, private household" | CHIEF_OCCUPATION == "Fishermen and oystermen" ~ "Unskilled",
        #                     str_detect(CHIEF_OCCUPATION, "home") | (str_detect(CHIEF_OCCUPATION, "house") & !str_detect(CHIEF_OCCUPATION, "except")) ~ "Unskilled",
        #                     CHIEF_OCCUPATION != "Blank" & CHIEF_OCCUPATION != "Unemployed/ without occupation" & CHIEF_OCCUPATION != "Illegible_duplicated_99999003" ~ "Skilled",
        #                     TRUE ~ NA_character_),
        #  HOUSEOWN = ifelse(AGE >= 18, ifelse(str_detect(HOME_OWNED_OR_RENTED, "Owned"), 1, 0), NA),
        #  HOUSEOWN2 = ifelse(AGE >= 18, ifelse(RELATIONSHIP == "Head", 1, 0), NA),
        # EARN = ifelse(AGE >= 18 & MALE == 1, ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), suppressWarnings(as.numeric(as.character(ANNUAL_EARNING_AMOUNT))), NA_real_), NA),
        # RURAL = ifelse(str_detect(CCRI_URBAN_RURAL_1921,"Rural"), 1, 0)
        ) %>%
  select(all_of(varnames_extra))

## Combining Censuses and Outputting ----
all_census_cleaned <- list(clean1871, clean1881, clean1891, clean1901, clean1911, clean1921)
clean_all <- bind_rows(all_census_cleaned)
write_csv(clean_all, glue("{dbox}/cleaned/can_clean.csv"))

#_____________________________________________________________
# POPULATION STOCK CALCULATIONS ----
#_____________________________________________________________
## Grouping by Year and BPL and interpolating ----
bplall <- clean_all %>% filter(IMM == 1) %>%
  group_by(YEAR, BPL) %>% summarize(POP = sum(WEIGHT)) 

# pivoting data wide (id col = year, diff val cols for each country)
bplall_wide <- bplall %>% pivot_wider(id_cols = YEAR, names_from = BPL, values_from = POP) %>%
  ungroup()

# interpolating intercensal values
bpl_interp <- data.frame(YEAR = 1871:1921) %>% 
  left_join(bplall_wide %>% 
              #create new col with all foreignborn population
              mutate(ForeignBorn = rowSums(bplall_wide %>% select(-c(YEAR)), na.rm=TRUE))) %>%
  #discarding any country with fewer than 2 nonmissing values
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  #interpolating all inter-decennial values using natural cubic spline (_spline) and linear (_linear)
  mutate(across(-YEAR, function(.x) spline(YEAR, .x, method = "natural", xout = YEAR)$y, .names = "{.col}_spline"),
         across(-c(YEAR, ends_with("spline")), function(.x) approx(YEAR, .x, xout = YEAR)$y, .names = "{.col}_linear")) %>%
  select(c(ends_with("spline"), ends_with("linear"), "YEAR")) %>%
  #pivoting data to be long on year x country
  pivot_longer(cols = -YEAR, names_to = c("BPL", "INTERP"), names_pattern = "(.*)_(.*)", values_to = "POP") %>%
  mutate(POP = ifelse(POP < 0, 0, POP)) #lower bound of zero

# outputting cleaned population stock data
write_csv(bpl_interp, glue("{dbox}/cleaned/popstock_can.csv"))

# visual comparison of linear and natural spline interpolation for a few countries
bpllist_small = c("Belgium", "Denmark", "France", "Germany", "Greece", "India", "Mexico", "Brazil", "Chile",
                  "Japan", "Netherlands", "Norway","China", "Sweden", "Finland", "Australia")

ggplot(bpl_interp %>% filter(INTERP != "none" & BPL %in% bpllist_small), aes(x = YEAR, y = POP, color = INTERP)) + geom_line() +
  geom_point(data = bplall %>% filter(BPL %in% bpllist_small), aes(x = YEAR, y = POP), inherit.aes = FALSE) + facet_wrap(~BPL)
