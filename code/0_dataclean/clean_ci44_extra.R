# File Description: ONE-TIME CODE SNIPPETS FOR CLEANING CI44
# Author: Amy Kim
# Date Created: Wed Feb  5 13:02:10 2025

# IMPORTING PACKAGES ----
library(tidyverse)
library(glue)

# SETTING WORKING DIRECTORIES ----
dbox = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data"
git = "/Users/amykim/Documents/GitHub/headtax"

# READING IN DATA ----
matches <- read_csv(glue("{dbox}/cleaned/matchtest1_jan30.csv"))

## register data ----
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, 
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))

# GETTING SHIP LINE COMMON NAMES ----
shipcheck <- matches %>% 
  mutate(EntryShip_reg = sapply(EntryShip_reg,clean_entryship),
         EntryShip_ci44 = sapply(EntryShip_ci44,clean_entryship)) %>%
  filter(total_score >= 0.8) %>%
  group_by(EntryShip_reg, EntryShip_ci44) %>% summarize(n=n())

bluefunnel <- unique(filter(shipcheck,
                            adist("Blue Funnel", EntryShip_ci44, partial = TRUE)[1,] <= 2 &
                              n >= 5)$EntryShip_reg)

cpr <- unique(filter(shipcheck,
                     adist("C P R", EntryShip_ci44, partial = TRUE)[1,] <= 0 &
                       n >= 5)$EntryShip_reg)

japboat <- unique(filter(shipcheck,
                         adist("Jap Boat", EntryShip_ci44, partial = TRUE)[1,] <= 1 &
                           n >= 5)$EntryShip_reg)

via <- unique(filter(shipcheck,
                     adist("Via", EntryShip_ci44, partial = TRUE)[1,] <= 0 &
                       n >= 2)$EntryShip_reg)

npacific <- unique(filter(shipcheck,
                        adist("Northern Pacific", EntryShip_ci44, partial = TRUE)[1,] <= 3 &
                          n >= 5)$EntryShip_reg)

# GETTING COUNTY/DISTRICT CROSSWALK FROM REGISTER
county_cw <- reg_chi %>%
  mutate(COUNTY_CLEAN = ifelse(str_detect(COUNTY, "\\?")|nchar(COUNTY) <= 5, NA, str_to_lower(str_remove_all(COUNTY, "[^A-z]")))) %>%
  group_by(COUNTY_CLEAN, NEW_COUNTY) %>%
  summarize(n=n()) %>% filter(!is.na(COUNTY_CLEAN) & !is.na(NEW_COUNTY)) %>%
  mutate(NEW_COUNTY = str_replace(NEW_COUNTY, "\\?","")) %>%
  group_by(COUNTY_CLEAN) %>% mutate(matches = n()) %>%
  filter(n > 5 & matches <= 1) %>%
  arrange(desc(n))
write_csv(county_cw, glue("{dbox}/cleaned/reg_county_cw.csv"))
