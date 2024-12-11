#_____________________________________________________________
# FILE DESCRIPTION: Cleaning Canadian Census Data, Consolidating Years
# CREATED BY: Amy Kim
# CREATED ON: Sep 2022
# LAST MODIFIED: April 2024
#_____________________________________________________________

#_____________________________________________________________
# IMPORTING RAW CENSUS DATA ----
#_____________________________________________________________
# census years
years <- seq(1871, 1921, 10)

raw1871 <- read.spss(glue("{dbox}/raw/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/raw/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/raw/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 4% sample

## crosswalk from 1901 occupation codes to occ1950
cfp_occ1950 <- read_xlsx(glue("{dbox}/crosswalks/cfp_to_occ1950_final.xlsx")) %>%
  select(c(OCC_CFP, OCC_STR, Occ1950_code, Manual_Code, FLAG_MANUAL_CORRECT)) %>%
  mutate(x = 1)

## csv versions (for cleaner occupational coding)
raw1911_csv <- read_csv(glue("{dbox}/raw/census1911.csv"), guess_max = 1000000)

# creating crosswalk from occupation string to occ1950 codes using cleaned 1911 census
occ1950_crosswalk <- inner_join(raw1911 %>% select(Derived_Household_Id, Derived_Person_Num_In_Household, OCCUPATION_CHIEF_OCC_IND), 
                                raw1911_csv %>% mutate(OCCCODE = OCCUPATION_CHIEF_OCC_IND, Derived_Household_Id = as.numeric(Derived_Household_Id)) %>% 
                                  select(Derived_Household_Id, Derived_Person_Num_In_Household, OCCCODE)) %>%
  group_by(OCCCODE) %>% summarize(n_1911=n(), OCCDESC = first(OCCUPATION_CHIEF_OCC_IND)) %>% filter(!is.na(OCCCODE))

# ## getting crosswalk from documentation
# raw_codes_1911 <- pdf_text(glue("{dbox}/docs/1911Codes-Combined.pdf"))
# bpl_raw_1911 <- raw_codes_1911[which(str_detect(raw_codes_1911, "Individual Birth Country"))] %>%
#   str_split("\n") %>% unlist()
# bpl_crosswalk_1911 <- data.frame(bplcode = str_trim(str_extract(bpl_raw_1911, "^\\s+[0-9]{4,}")),
#                                  bplname = str_trim(str_remove(bpl_raw_1911, "^\\s+[0-9]{4,}"))) %>%
#   filter(!is.na(bplcode))
# 
# ## linking spss with csv for occupation codes
# linked1911 <- full_join(raw1911, raw1911_csv, by = c("Individual_Id", "Household_Id"))
# 
# ## 1901 'laborer' occupations for comparison
# lab_occs_1901 <- filter(raw1901, occ2 == "Labourer") %>% group_by(occ, occ1) %>% summarize(n=n())
# 
# write_csv(raw1901 %>% group_by(occ, occ1) %>% summarize(n=n()), glue("{dbox}/raw/1901_occcodes.csv"))
#_____________________________________________________________
# DECLARING KEY VARIABLES, CROSSWALKS, LISTS ----
#_____________________________________________________________
# us states, canadian provinces/places, UK countries (to define country of origin)
us_states <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[1:82]))
canadian <- trimws(as.character(as.data.frame(table(raw1881$birthplace_ccri))$Var1[84:1130]))
uk <- c("England", "Ireland", "Scotland", "Wales", "Jersey", "Isle of Man", "Gibraltar", "Great Britain", "Channel Islands",
        "Guernsey", "United Kingdom, n.s", "Northern Ireland", "Orkney Islands", "Shetland Islands") 

# variables to keep (for all censuses)
varnames <- c("WEIGHT", "YEAR", "AGE", "WHIPPLE", "MALE", "MAR", "BPL", "IMM", "HHID", "PERNUM", "FIRSTNAME", "LASTNAME")

# additional variables to keep (for 1901-1921 censuses)
varnames_extra <- c(varnames, "YRIMM", "CANREAD", "SPEAKENG", "SPEAKENGFR", "OCC1950", "OCC_STR", "EARN", "EMPLOYEE", "EMPLOYED")

#_____________________________________________________________
# CLEANING CENSUS DATA ----
#_____________________________________________________________
## 1871 Census ----
clean1871 <- raw1871 %>% 
  mutate(WEIGHT = popwgt,
         HHID = as.character(serial),
         PERNUM = newpno,
         FIRSTNAME = fnam,
         LASTNAME = lnam,
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
         HHID = as.character(SERIAL),
         PERNUM = PERNUM,
         FIRSTNAME = NAMFRST,
         LASTNAME = NAMLAST,
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
  group_by(Reelnumber, DwellingPage, DwellingLine) %>%
  mutate(HHID = as.character(cur_group_id()),
         PERNUM = row_number()) %>% ungroup() %>%
  mutate(WEIGHT = case_when(samplesize == 5 ~ 20, samplesize == 10 ~ 10, samplesize == 100 ~ 1),
         FIRSTNAME = NAMEFIRST,
         LASTNAME = NAMELAST,
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
  distinct(hhdid, hhdpos, reel, pagenbr, linenbr, .keep_all = TRUE) %>% ## NEED TO DROP DUPLICATES FOR 1901 CENSUS!
  mutate(WEIGHT = 20,
         HHID = hhdid,
         PERNUM = hhdpos,
         LASTNAME = indlnm,
         FIRSTNAME = indfnm,
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
         OCC_CFP = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{5}")))),
         OCC_STR = trimws(occ),
         mos_emp = ifelse(is.na(moempfac) & is.na(moemphom) & is.na(moempoth), 12, rowSums(pick(moempfac:moempoth), na.rm=TRUE)),
         EARN = case_when(earnper == "Daily" & earnings < 100 ~ (earnings*300)*(mos_emp/12), #for daily earnings, multiply by 300 (exclude obvious outliers that are miscoded as daily)
                          earnper == "Weekly" ~ (earnings*50)*(mos_emp/12), #for weekly, multiply by 50 (scaled by months working)
                          earnper == "Monthly" ~ (earnings*mos_emp), #for monthly, scale by months working
                          !is.na(earnings) ~ earnings, #for everyone else, assume annual (just report total amount)
                          is.na(earnings) ~ NA_integer_ #unless missing (report NA)
                          ) + ifelse(!is.na(exearn), exearn, 0), #add extra earnings if nonmissing
         EMPLOYED = case_when(!is.na(employee) & employee == "Yes" ~ 1,
                              !is.na(employer) & employer == "Yes" ~ 1,
                              !is.na(ownacct) & ownacct == "Yes" ~ 1,
                              !is.na(EARN) ~ 1,
                              TRUE ~ 0),
         #ifelse(employee == "Yes" | employer == "Yes" | ownacct == "Yes" | ownmeans == "Yes" | !is.na(EARN), 1, 0),
         EMPLOYEE = ifelse(EMPLOYED == 1, ifelse(employee == "Yes", 1, 0), NA)
         ) %>% #numeric version of occcode
  left_join(cfp_occ1950, by = c("OCC_CFP" = "OCC_CFP", "OCC_STR" = "OCC_STR")) %>%
  mutate(OCC1950 = as.numeric(ifelse(str_detect(Occ1950_code, "NA"), Manual_Code, Occ1950_code)),
         OCC1950_ALT = as.numeric(ifelse(!is.na(FLAG_MANUAL_CORRECT), Manual_Code, OCC1950))
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
  select(all_of(c(varnames_extra, "OCC1950_ALT")))

## 1911 Census ----
clean1911 <- raw1911 %>% 
  left_join(raw1911_csv %>% mutate(OCCCODE = OCCUPATION_CHIEF_OCC_IND, Derived_Household_Id = suppressWarnings(as.numeric(Derived_Household_Id))) %>% 
              select(Derived_Household_Id, Derived_Person_Num_In_Household, OCCCODE)) %>%
  mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 4),
         HHID = as.character(Derived_Household_Id),
         PERNUM = Derived_Person_Num_In_Household,
         FIRSTNAME = str_to_upper(str_replace(FIRST_NAME, "([0-9]{7,}|\\*)", "")),
         LASTNAME = str_to_upper(str_replace(LAST_NAME, "([0-9]{7,}|\\*)", "")),
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
         OCC1950 = as.numeric(OCCCODE),
         OCC_STR = str_to_lower(OCCUPATION_CHIEF_OCC_IND_CL),
         EMPLOYED = ifelse(EMPLOYEE == "Yes" | EMPLOYER == "Yes" | WORKING_ON_OWN_ACCOUNT == "Yes", 1, 0),
         EMPLOYEE = ifelse(EMPLOYED == 1, ifelse(EMPLOYEE == "Yes", 1, 0), NA),
         EARNCHIEF = ifelse(grepl("[0-9]+", EARNINGS_AT_CHIEF_OCC), suppressWarnings(as.numeric(as.character(EARNINGS_AT_CHIEF_OCC))), NA),
         EARNOTH = ifelse(grepl("[0-9]+", EARNINGS_AT_OTHER_OCC), suppressWarnings(as.numeric(as.character(EARNINGS_AT_OTHER_OCC))), NA),
         EARN = case_when(is.na(EARNCHIEF) & is.na(EARNOTH) ~ NA_integer_,
                          is.na(EARNCHIEF) ~ EARNOTH,
                          is.na(EARNOTH) ~ EARNCHIEF,
                          !is.na(EARNCHIEF) & !is.na(EARNOTH) ~ EARNCHIEF + EARNOTH)
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
  ) %>%
  select(all_of(varnames_extra))

## 1921 Census ----
clean1921 <- raw1921 %>% 
  left_join(occ1950_crosswalk %>% select(-c(n_1911)), by = c("CHIEF_OCCUPATION" = "OCCDESC")) %>%
  mutate(WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 5),
         HHID = as.character(Derived_Household_Id),
         PERNUM = Derived_Person_Num_In_Household,
         FIRSTNAME = str_to_upper(str_replace(FIRST_NAME, "([0-9]{7,}|\\*)", "")),
         LASTNAME = str_to_upper(str_replace(LAST_NAME, "([0-9]{7,}|\\*)", "")),
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
         OCC1950 = OCCCODE,
         OCC_STR = CHIEFOCCUP,
         EARN = ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), suppressWarnings(as.numeric(as.character(ANNUAL_EARNING_AMOUNT))), NA),
         EMPLOYED = ifelse(EMPLOYMENT_STATUS_IND %in% c("Employer", "Worker", "Own account"), 1, 0),
         EMPLOYEE = ifelse(EMPLOYED == 0, NA, ifelse(EMPLOYMENT_STATUS_IND == "Worker", 1, 0))
        #  LABOR = ifelse(AGE >= 18 & MALE == 1, ifelse(str_detect(OCC, "labor") | str_detect(OCC, "labour"), 1, 0), NA),
        #  OCCGRP = case_when(str_detect(CHIEF_OCCUPATION, "Farm") ~ "Farmer",
        #                     str_detect(CHIEF_OCCUPATION, "Labor") | str_detect(CHIEF_OCCUPATION, "labor") ~ "Laborer",
        #                     str_detect(CHIEF_OCCUPATION, "Mine") | CHIEF_OCCUPATION == "Private household workers (n.e.c.)" | str_detect(CHIEF_OCCUPATION, "Lumbermen") | CHIEF_OCCUPATION == "Carpenters" | CHIEF_OCCUPATION == "Housekeepers, private household" | CHIEF_OCCUPATION == "Fishermen and oystermen" ~ "Unskilled",
        #                     str_detect(CHIEF_OCCUPATION, "home") | (str_detect(CHIEF_OCCUPATION, "house") & !str_detect(CHIEF_OCCUPATION, "except")) ~ "Unskilled",
        #                     CHIEF_OCCUPATION != "Blank" & CHIEF_OCCUPATION != "Unemployed/ without occupation" & CHIEF_OCCUPATION != "Illegible_duplicated_99999003" ~ "Skilled",
        #                     TRUE ~ NA_character_),
        #  HOUSEOWN = ifelse(AGE >= 18, ifelse(str_detect(HOME_OWNED_OR_RENTED, "Owned"), 1, 0), NA),
        #  HOUSEOWN2 = ifelse(AGE >= 18, ifelse(RELATIONSHIP == "Head", 1, 0), NA),
        # RURAL = ifelse(str_detect(CCRI_URBAN_RURAL_1921,"Rural"), 1, 0)
        ) %>%
  select(all_of(varnames_extra))

## Combining Censuses and Outputting ----
all_census_cleaned <- list(clean1871, clean1881, clean1891, clean1901, clean1911, clean1921)
clean_all <- bind_rows(all_census_cleaned)
write_csv(clean_all, glue("{dbox}/cleaned/can_clean.csv"))
write_csv(clean_all %>% filter(IMM == 1), glue("{dbox}/cleaned/can_clean_imm.csv"))
#_____________________________________________________________
# POPULATION STOCK CALCULATIONS ----
#_____________________________________________________________
## Grouping by Year and BPL and interpolating ----
bplall <- clean_all %>% filter(IMM == 1) %>%
  group_by(YEAR, BPL) %>% summarize(POP = sum(WEIGHT)) %>% mutate(ORIG = 1) #indicator for a non-interpolated data point

## Uninterpolated Totals
bpl_china <- bplall %>% filter(BPL == "China") %>% ungroup() %>%
  arrange(YEAR) %>% mutate(POPSTOCKCHANGE_China = POP - lag(POP))

write_csv(bpl_china, glue("{dbox}/cleaned/popstockchange_china.csv"))

# pivoting data wide (id col = year, diff val cols for each country)
bplall_wide <- bplall %>% pivot_wider(id_cols = YEAR, names_from = BPL, values_from = c(POP)) %>% 
  ungroup()

# interpolating intercensal values
bpl_interp <- data.frame(YEAR = 1871:1921) %>% 
  left_join(bplall_wide %>% 
              #create new col with all foreignborn population
              mutate(ForeignBorn = rowSums(bplall_wide %>% select(-c(YEAR)), na.rm=TRUE))) %>%
  #discarding any country with fewer than 2 nonmissing values
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  #interpolating all inter-decennial values using natural cubic spline (_spline) and linear (_linear)
  mutate(across(-c(YEAR), function(.x) spline(YEAR, .x, method = "natural", xout = YEAR)$y, .names = "{.col}_spline"),
         across(-c(YEAR, ends_with("spline")), function(.x) approx(YEAR, .x, xout = YEAR)$y, .names = "{.col}_linear")) %>%
  select(c(ends_with("spline"), ends_with("linear"), "YEAR")) %>% 
  #pivoting data to be long on year x country
  pivot_longer(cols = -YEAR, names_to = c("BPL", "INTERP"), names_pattern = "(.*)_(.*)", values_to = "POP") %>%
  mutate(POP = ifelse(POP < 0, 0, POP)) %>% #lower bound of zero
  left_join(bplall %>% select(-POP)) %>% 
  group_by(BPL, INTERP) %>% arrange(YEAR) %>% 
  fill(ORIG, .direction = "down") %>%
  mutate(POP = ifelse(is.na(ORIG), NA, POP)) #making sure no interpolation beyond endpoints

# outputting cleaned population stock data
write_csv(bpl_interp, glue("{dbox}/cleaned/popstock_can.csv"))

# visual comparison of linear and natural spline interpolation for a few countries
bpllist_small = c("Belgium", "Denmark", "France", "Germany", "Greece", "India", "Mexico", "Brazil", "Chile",
                  "Japan", "Netherlands", "Norway","China", "Sweden", "Finland", "Australia")

ggplot(bpl_interp %>% filter(INTERP != "none" & BPL %in% bpllist_small), aes(x = YEAR, y = POP, color = INTERP)) + geom_line() +
  geom_point(data = bplall %>% filter(BPL %in% bpllist_small), aes(x = YEAR, y = POP), inherit.aes = FALSE) + facet_wrap(~BPL)
