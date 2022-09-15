########################################################################
### FILE DESCRIPTION: Initial Look at Canadian and US Historical Data
### PRIMARY OBJECTIVE: Seeing what is available and reliable, initial patterns in data
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Sep 13 2022
########################################################################
library(tidyverse)
library(glue)
library(foreign)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/asianimmigration_canada"
}

########################################################################
### IMPORTING DATA
########################################################################
years <- c(1852, seq(1871, 1921, 10))
raw1852 <- read.spss(glue("{dbox}/census1852.sav")) %>% as.data.frame() # 20% sample
raw1871 <- read.spss(glue("{dbox}/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/census1921.sav")) %>% as.data.frame() # 4% sample

# copied directly from https://libguides.southernct.edu/c.php?g=15048&p=81577
us_chinese <- data.frame(YEAR = seq(1850,1920,10), 
                         CHIPOP = c(4018, 34933, 64199, 105465, 107488, 118746, 94414, 85202),
                         USPOP = c(23191876, 31443321, 38558371, 50189209, 62979766, 76212168, 92228496, 106021537))
########################################################################
### CLEANING DATA
########################################################################
# definitely want across all years: age, sex, marst, year, birthplace, occupation
# want if can get: ethnicity, canread, unemp, earnings, yrimm

# standardizing codes: MALE = 1[Male], MAR = 1[Married]
clean1881 <- raw1881 %>% rename(BPL = birthplace_ccri, ETH = ethnicity_ccri, OCC = DOCCUP, OCCGRP = occgrp2, CLASSGRP = classgrp) %>%
  mutate(MALE = case_when(SEX == "Female" ~ 0,
                          SEX == "Male" ~ 1,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Unknown" ~ NA_real_,
                         TRUE ~ 0),
         NAPHISCOSTR = as.character(NAPHISCO)) %>% 
  select(c(MALE, AGE, MAR, ETH, OCC, NAPHISCOSTR, OCCGRP, CLASSGRP, BPL)) %>% 
  mutate(YEAR = 1881, BPLCHI = ifelse(BPL == "China",1,0), ETHCHI = ifelse(ETH == "Chinese", 1, 0), WEIGHT = 1)
  
clean1891 <- raw1891 %>% mutate(WEIGHT = case_when(samplesize == 5 ~ 20,
                                                   samplesize == 10 ~ 10,
                                                   samplesize == 100 ~ 1),
                                BPLCHI = ifelse(bplcode == 50000, 1, 0),
                                MALE = case_when(SEX == "M  " ~ 1,
                                                 SEX == "F  " ~ 0,
                                                 TRUE ~ NA_real_),
                                MAR = case_when(MARST == "married" ~ 1,
                                                MARST == "single" | MARST == "Divorced" | MARST == "W" ~ 0,
                                                TRUE ~ NA_real_),
                                CANREAD = ifelse(CANREAD == "Yes, can read", 1, 0)) %>%
  select(-c(AGE)) %>%
  rename(NAPHISCO = NappHisco, AGE = agecode) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, BPLCHI, UNEMP, NAPHISCO)) %>%
  mutate(YEAR = 1891)

clean1901 <- raw1901 %>% rename(BPL = bpl, SEX = sex, MARST = marst, AGE = ageyr, YRIMM = immyr, PROPOWNR = propownr, NATL = natl, OCC = occ, 
                                OCCGRP = occ2, EARN = earnings, CANREAD = canread) %>%
  mutate(BPLCHI = ifelse(bpl2 == 50000, 1, 0),
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
         YRIMM = as.numeric(YRIMM)) %>%
  select(c(MALE, AGE, BPL, BPLCHI, MAR, YRIMM, PROPOWNR, NATL, OCC, OCCGRP, EARN, CANREAD)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND) %>%
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
                             TRUE ~ NA_real_)) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, EARN)) %>% 
  mutate(YEAR = 1911, WEIGHT = 20, BPLCHINA = ifelse(BPL == "China", 1, 0))

clean1921 <- raw1921 %>% rename(AGE = Derived_Age_In_Years, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                               CANREAD = CAN_READ_INDICATOR, OCC = CHIEFOCCUP) %>%
  mutate(EARN = ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), as.numeric(as.character(ANNUAL_EARNING_AMOUNT)), 0),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_)) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, EARN)) %>% 
  mutate(YEAR = 1921, WEIGHT = 25, BPLCHINA = ifelse(BPL == "China", 1, 0))

# binding all years and cleaning together
clean_all <- bind_rows(clean1881, clean1891) %>% bind_rows(clean1901) %>%
  bind_rows(clean1911) %>% bind_rows(clean1921) %>%
  mutate(AGE = ifelse(AGE > 200, NA, AGE))












# 
# histcolsnum <- str_count(readLines(glue("{dbox}/ipums_historical.csv"), n = 1), ",") + 1
# histraw <- read_csv(glue("{dbox}/ipums_historical.csv"),
#                     col_types = paste0(rep("d", histcolsnum), collapse=""))
# 
# contempcolsnum <- str_count(readLines(glue("{dbox}/ipums_contemp.csv"), n = 1), ",") + 1
# contempraw <- read_csv(glue("{dbox}/ipums_contemp.csv"),
#                     col_types = paste0(rep("d", contempcolsnum), collapse=""))
# 
# ########################################################################
# ### HISTORICAL DATA
# ########################################################################
# histclean <- histraw %>% 
#   mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
#                              COUNTRY == 124 ~ "Canada"),
#          BPLCAT = case_when(BPLCOUNTRY == 24020 ~ "Canada",
#                             BPLCOUNTRY == 24040 ~ "US",
#                             BPLCOUNTRY >= 10000 & BPLCOUNTRY < 20000 ~ "Africa",
#                             BPLCOUNTRY >= 20000 & BPLCOUNTRY < 30000 ~ "Americas",
#                             BPLCOUNTRY >= 30000 & BPLCOUNTRY < 40000 ~ "Asia",
#                             BPLCOUNTRY >= 40000 & BPLCOUNTRY < 50000 ~ "Europe",
#                             TRUE ~ "Other/Unknown"),
#          ASIAN = ifelse(((RACE >= 40 & RACE < 50) | (ORIGIN >= 8400 & ORIGIN < 8500)), 1, 0),
#          BPLCHINA = ifelse((BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020), 1, 0),
#          CHINESE = ifelse((RACE == 41 | ORIGIN == 8410), 1, 0))
# 
# histgroup <- histclean %>% group_by(YEAR, COUNTRY) %>% 
#   mutate(POP = sum(PERWT),
#          CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
#          BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/POP) %>%
#   summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean)) %>%
#   pivot_longer(-c(YEAR, COUNTRY), names_to = "STAT", values_to = "pct_of_pop")
# 
# histgroupocc <- histclean %>% group_by(YEAR, COUNTRY, OCCISCO) %>% 
#   mutate(POP = sum(PERWT),
#          CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
#          BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/POP) %>%
#   summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean))
# 
# #ggplot(histgroup, aes(x = YEAR, y = pct_of_pop, color = factor(COUNTRY)))+ geom_line() + facet_wrap(~STAT)
# #ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINAPCT, color = factor(COUNTRY)))+ geom_line() + facet_wrap(~OCCISCO)
# 
# ########################################################################
# ### CONTEMP. DATA
# ########################################################################
# contempclean <- contempraw %>% 
#   mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
#                              COUNTRY == 124 ~ "Canada"),
#          IMM = ifelse(is.na(YRIMM) | YRIMM == 0, 0, 1), #dummy for whether the person is an immigrant
#          BPLCHINA = case_when(BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020 ~ 1,
#                               TRUE ~ 0),
#          CHINESE = case_when(RACE == 41 | ETHNICCA == 17 ~ 1,
#                              TRUE ~ 0),
#          ASIAN = case_when((RACE >= 40 & RACE < 50) | (ETHNICCA == 16 | ETHNICCA <= 18) ~ 1, 
#                            TRUE ~ 0),
#          WHITE = case_when(RACE == 10 | ETHNICCA < 14 ~ 1, 
#                            TRUE ~ 0),
#          EDUC = case_when(EDATTAIN == 4 ~ "Univ +",
#                           EDATTAIN == 3 ~ "High School",
#                           EDATTAIN == 1 | EDATTAIN == 2 ~ "< High School",
#                           TRUE ~ "Unknown/NIU"),
#          INCWAGE = ifelse(INCWAGE < 9999998, INCWAGE, NA),
#          INCTOT = ifelse(INCTOT < 9999998, INCTOT, NA),
#          RACE = case_when(ASIAN == 1 ~ "Asian",
#                           WHITE == 1 ~ "White",
#                           TRUE ~ "Unknown"))
# 
# contempincwage <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
#   filter(EMPSTAT == 1) %>%
#   group_by(YEAR, COUNTRY, RACE) %>%
#   summarize(INCTOT = weighted.mean(INCTOT, PERWT, na.rm=TRUE))
# 
# contempincwageeduc <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
#   filter(EMPSTAT == 1) %>%
#   mutate(ASIAN = ifelse(is.na(ASIAN), 0, ASIAN),
#          WHITE = ifelse(is.na(WHITE), 0, WHITE),
#          RACE = case_when(ASIAN == 1 ~ "Asian",
#                           WHITE == 1 ~ "White",
#                           TRUE ~ "Unknown")) %>%
#   group_by(YEAR, COUNTRY, RACE) %>%
#   summarize(INCWAGE = weighted.mean(INCWAGE, PERWT, na.rm=TRUE))
# 
# # inc over time
# ggplot(contempincwage, aes(x = YEAR, y = INCTOT, color = RACE)) + geom_line() + facet_wrap(~COUNTRY)
# 
# # inc distribution in 2000/2001
# ggplot(contempincwage %>% filter(YEAR == 2000 | YEAR == 2001), 
#        aes(x = INCWAGE, fill = RACE)) + geom_density(weight = PERWT, alpha = 0.5) + facet_wrap(~COUNTRY)


