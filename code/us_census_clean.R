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
us_raw <- read_csv(glue("{dbox}/usa_fullcount.csv"))
us_raw_1880 <- read_csv(glue("{dbox}/usa_fullcount_1880.csv"))

########################################################################
### CLEANING DATA FOR MERGE W CANADIAN CENSUS DATA
########################################################################
us_clean <- us_raw %>% 
  mutate(BORNCHI = ifelse(BPL == 500, 1, 0),
         IMM = ifelse(NATIVITY == 5, 1, 0),
         MALE = ifelse(SEX == 1, 1, 0),
         CANREAD = ifelse(LIT == 3 | LIT == 4, 1, 0),
         MAR = ifelse(MARST == 1 | MARST == 2, 1, 0),
         LABOR = ifelse((OCC1950 <= 970 & OCC1950 >= 900), 1, 0),
         HOUSEOWN = ifelse(OWNERSHP == 1, 1, 0),
         IDNUM = row_number()) %>%
  rename(YRIMM = YRIMMIG, EARN = ERSCOR50) %>%
  select(c(IDNUM,YEAR,BORNCHI,IMM, YRIMM, AGE, MALE, CANREAD, LABOR, MAR, HOUSEOWN, EARN))
write_csv(us_clean, glue("{dbox}/cleaned/us_clean.csv"))

us_clean_1880 <- us_raw_1880 %>% 
  mutate(BORNCHI = ifelse(BPL == 500, 1, 0),
         IMM = ifelse(NATIVITY == 5, 1, 0),
         MALE = ifelse(SEX == 1, 1, 0),
         CANREAD = ifelse(LIT == 3 | LIT == 4, 1, 0),
         MAR = ifelse(MARST == 1 | MARST == 2, 1, 0),
         LABOR = ifelse((OCC1950 <= 970 & OCC1950 >= 900), 1, 0),
         IDNUM1880 = row_number()) %>%
  select(c(IDNUM1880,YEAR,BORNCHI,IMM, AGE, MALE, CANREAD, LABOR, MAR))
write_csv(us_clean_1880, glue("{dbox}/cleaned/us_clean_1880.csv"))


## MATCHING NON-CHINESE IMMS WITH CHINESE IMMS IN 1880
match_data_us <- us_clean_1880 %>% filter(BORNCHI == 1)
match_data_us <- match_data_us %>% mutate(matched = ifelse(IDNUM1880 %in% sample(match_data_us$IDNUM1880, size = nrow(match_data_us)*0.1), 1, 0))
match_inds_us <- c()

# randomly sample 1% of immigrants to simplify computation
for (yr in seq(1900,1920,10)){
  yr_data_filt <- us_clean %>% filter(YEAR == yr & IMM == 1 & BORNCHI == 0)
  match_data_filt <- match_data_us %>% filter(matched == 1) %>% bind_rows(yr_data_filt[sample(nrow(yr_data_filt), size = round(nrow(yr_data_filt)*0.01)), ])
  matches <- matchit(BORNCHI ~ MALE + AGE + MAR, data = match_data_filt)
  match_inds_us <- c(match_inds_us, match_data_filt[matches$match.matrix,]$IDNUM)
  print(summary(matches))
}

us_clean_matched <- us_clean %>% mutate(matched = ifelse(IDNUM %in% match_inds_us, 1, 0))

write_csv(us_clean_matched, glue("{dbox}/cleaned/us_clean_matched.csv"))

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






