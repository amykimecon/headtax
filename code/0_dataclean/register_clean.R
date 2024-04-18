#_____________________________________________________________
# FILE DESCRIPTION: Cleaning Chinese Register Data
# CREATED BY: Amy Kim
# CREATED ON: Sep 2022 
# LAST MODIFIED: April 2024
#_____________________________________________________________
#_____________________________________________________________
# IMPORTING RAW DATA ----
#_____________________________________________________________
## chinese register -- immigration data through ports
# source: https://open.library.ubc.ca/cIRcle/collections/facultyresearchandpublications/52383/items/1.0075988
chiregraw <- read_excel(glue("{dbox}/raw/ChineseRegistry.xlsx"), guess_max = 1000000)

#_____________________________________________________________
# CLEANING REGISTER DATA ----
#_____________________________________________________________
chireg <- chiregraw %>% 
  mutate(COUNTYGRP = case_when(COUNTY_ID == 1 ~ "Taishan",
                               COUNTY_ID == 2 ~ "Xinhui",
                               COUNTY_ID == 3 ~ "Kaiping",
                               COUNTY_ID == 9 ~ "Panyu",
                               COUNTY_ID == 6 ~ "Zhongshan",
                               COUNTY_ID == 4 ~ "Enping",
                               COUNTY_ID == 5 ~ "Heshan",
                               TRUE ~ "Other"),
         YEAR = YEAR_ARRIV, #care about year of arrival, not year of registration
         MONTH = str_pad(MONTH_ARRI, 2, "left", pad = "0"),
         DAY = str_pad(DATE_ARRIV, 2, "left", pad = "0"),
         # date of arrival as date object (replacing missing month/day with middle of year/month)
         DATE = case_when(DAY == "00" & MONTH != "00" ~ as.Date(glue("{YEAR}-{MONTH}-15"), format = "%Y-%m-%d"),
                          MONTH == "00" & YEAR != 0 ~ as.Date(glue("{YEAR}-07-01"), format = "%Y-%m-%d"),
                          TRUE ~ as.Date(glue("{YEAR}-{MONTH}-{DAY}"), format = "%Y-%m-%d")),
         PORT = case_when(ARR_PORT == "Victoria" ~ "Victoria",
                          ARR_PORT == "Vancouver" ~ "Vancouver",
                          TRUE ~ "Other"),
         OCCGRP = case_when(PROFESSION == "Labourer" ~ "Labourer",
                            PROFESSION == "Student" | PROFESSION == "Merchant" ~ "Student or Merchant",
                            PROFESSION == "Farmer" ~ "Farmer",
                            PROFESSION == "Laundryman" | PROFESSION == "Cook" | PROFESSION == "Grocer" ~ "Service",
                            TRUE ~ "Other"),
         BIRTHYEAR = REG_Year - AGE,
         MALE = case_when(SEX == "Male" ~ 1, 
                          SEX == "Female" ~ 0,
                          TRUE ~ NA),
         LABOR = ifelse(MALE == 1 & AGE >= 18, ifelse(PROFESSION == "Labourer", 1, 0), NA),
         HEIGHT_IN_FRAC = str_extract(HEIGHT_IN, "[0-9]/[0-9]$"),
         HEIGHT_IN_WHOLE = as.numeric(str_extract(str_remove(HEIGHT_IN, "[0-9]/[0-9]$"), "^[0-9]{1,2}")),
         HEIGHT = 2.54*ifelse(is.na(HEIGHT_FT) | HEIGHT_FT == 0, NA, HEIGHT_FT*12 + ifelse(is.na(HEIGHT_IN_WHOLE), 0, HEIGHT_IN_WHOLE) + 
                                                 ifelse(is.na(HEIGHT_IN_FRAC), 0, as.numeric(str_extract(HEIGHT_IN_FRAC,"^[0-9]"))/as.numeric(str_extract(HEIGHT_IN_FRAC, "[0-9]$")))),
         HEIGHT_ADULTMEN = ifelse(MALE == 1 & AGE >= 18, HEIGHT, NA)) %>%
  filter(!is.na(DATE) & YEAR != 0) #excluding entries without valid arrival date (for now) -- only 522 such entries out of ~100k

#outputting to csv file
write_csv(chireg, glue("{dbox}/cleaned/chireg.csv"))



