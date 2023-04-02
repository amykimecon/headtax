########################################################################
### FILE DESCRIPTION: Cleaning Raw US Census Data
### PRIMARY OBJECTIVE: Standardizing Primary Variables, Creating Single Multi-Year Dataframe
### CREATED BY: Amy Kim
### CREATED ON: Oct 10 2022
### LAST MODIFIED: Oct 10 2022
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
us_raw <- read_csv(glue("{dbox}/usa_fullcount.csv"))
us_raw_1880 <- read_csv(glue("{dbox}/usa_fullcount_1880.csv"))

########################################################################
### CLEANING DATA FOR MERGE W CANADIAN CENSUS DATA
########################################################################
us_clean <- us_raw %>% 
  mutate(BORNCHI = ifelse(BPL == 500, 1, 0),
         BORNAUS = ifelse(BPL == 450, 1, 0),
         BORNIRE = ifelse(BPL == 414, 1, 0),
         BORNJAP = ifelse(BPL == 501, 1, 0),
         BORNIND = ifelse(BPL == 521, 1, 0),
         IMM = ifelse(NATIVITY == 5, 1, 0),
         YRIMM = YRIMMIG,
         MALE = ifelse(SEX == 1, 1, 0),
         CANREAD = ifelse(LIT == 3 | LIT == 4, 1, 0),
         MAR = ifelse(MARST == 1 | MARST == 2, 1, 0),
         LABOR = ifelse((OCC1950 <= 970 & OCC1950 >= 900), 1, 0),
         HOUSEOWN = ifelse(OWNERSHP == 1, 1, 0)) %>%
  select(c(YEAR,starts_with("BORN"),IMM, YRIMM, AGE, MALE, CANREAD, LABOR, MAR, HOUSEOWN))
write_csv(us_clean, glue("{dbox}/cleaned/us_clean.csv"))

us_clean_1880 <- us_raw_1880 %>% 
  mutate(BORNCHI = ifelse(BPL == 500, 1, 0),
         BORNAUS = ifelse(BPL == 450, 1, 0),
         BORNIRE = ifelse(BPL == 414, 1, 0),
         BORNJAP = ifelse(BPL == 501, 1, 0),
         BORNIND = ifelse(BPL == 521, 1, 0),
         IMM = ifelse(NATIVITY == 5, 1, 0),
         MALE = ifelse(SEX == 1, 1, 0),
         CANREAD = ifelse(LIT == 3 | LIT == 4, 1, 0),
         MAR = ifelse(MARST == 1 | MARST == 2, 1, 0),
         LABOR = ifelse((OCC1950 <= 970 & OCC1950 >= 900), 1, 0)) %>%
  select(c(YEAR,starts_with("BORN"),IMM, AGE, MALE, CANREAD, LABOR, MAR))
write_csv(us_clean_1880, glue("{dbox}/cleaned/us_clean_1880.csv"))

# us_clean_yrimm <- us_clean %>%
#   group_by(YEAR, YRIMM) %>%
#   summarize(across(starts_with("BORN"), sum, na.rm=TRUE), 
#             IMM = sum(IMM, na.rm=TRUE), IMMPCT = sum(IMM, na.rm=TRUE)/n()) %>%
#   ungroup() %>%
#   group_by(YRIMM) %>%
#   summarize(across(starts_with("BORN"), max, na.rm=TRUE), 
#             IMM = max(IMM, na.rm=TRUE), IMMPCT = max(IMMPCT, na.rm=TRUE))
# write_csv(us_clean_yrimm, glue("{dbox}/cleaned/us_clean_yrimm.csv"))
# 
# us_chi <- us_clean %>% filter(BORNCHI == 1) %>%
#   mutate(AGEATIMM = AGE - (YEAR - YRIMM),
#          LABOR = ifelse((OCC1950 <= 970 & OCC1950 > 800) | OCC1950 == 720 | OCC1950 == 690 | OCC1950 == 650, 1, 0))
# write_csv(us_chi, glue("{dbox}/cleaned/us_chi.csv"))






