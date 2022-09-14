########################################################################
### FILE DESCRIPTION: Initial Look at Canadian and US Historical Data
### PRIMARY OBJECTIVE: Seeing what is available and reliable, initial patterns in data
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Aug 18 2022
########################################################################
library(tidyverse)
library(glue)
library(ggplot2)

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
histcolsnum <- str_count(readLines(glue("{dbox}/ipums_historical.csv"), n = 1), ",") + 1
histraw <- read_csv(glue("{dbox}/ipums_historical.csv"),
                    col_types = paste0(rep("d", histcolsnum), collapse=""))

contempcolsnum <- str_count(readLines(glue("{dbox}/ipums_contemp.csv"), n = 1), ",") + 1
contempraw <- read_csv(glue("{dbox}/ipums_contemp.csv"),
                    col_types = paste0(rep("d", contempcolsnum), collapse=""))

########################################################################
### HISTORICAL DATA
########################################################################
histclean <- histraw %>% 
  mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
                             COUNTRY == 124 ~ "Canada"),
         BPLCAT = case_when(BPLCOUNTRY == 24020 ~ "Canada",
                            BPLCOUNTRY == 24040 ~ "US",
                            BPLCOUNTRY >= 10000 & BPLCOUNTRY < 20000 ~ "Africa",
                            BPLCOUNTRY >= 20000 & BPLCOUNTRY < 30000 ~ "Americas",
                            BPLCOUNTRY >= 30000 & BPLCOUNTRY < 40000 ~ "Asia",
                            BPLCOUNTRY >= 40000 & BPLCOUNTRY < 50000 ~ "Europe",
                            TRUE ~ "Other/Unknown"),
         ASIAN = ifelse(((RACE >= 40 & RACE < 50) | (ORIGIN >= 8400 & ORIGIN < 8500)), 1, 0),
         BPLCHINA = ifelse((BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020), 1, 0),
         CHINESE = ifelse((RACE == 41 | ORIGIN == 8410), 1, 0))

histgroup <- histclean %>% group_by(YEAR, COUNTRY) %>% 
  mutate(POP = sum(PERWT),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/POP) %>%
  summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean)) %>%
  pivot_longer(-c(YEAR, COUNTRY), names_to = "STAT", values_to = "pct_of_pop")

histgroupocc <- histclean %>% group_by(YEAR, COUNTRY, OCCISCO) %>% 
  mutate(POP = sum(PERWT),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/POP) %>%
  summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean))

#ggplot(histgroup, aes(x = YEAR, y = pct_of_pop, color = factor(COUNTRY)))+ geom_line() + facet_wrap(~STAT)
#ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINAPCT, color = factor(COUNTRY)))+ geom_line() + facet_wrap(~OCCISCO)

########################################################################
### CONTEMP. DATA
########################################################################
contempclean <- contempraw %>% 
  mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
                             COUNTRY == 124 ~ "Canada"),
         IMM = ifelse(is.na(YRIMM) | YRIMM == 0, 0, 1), #dummy for whether the person is an immigrant
         BPLCHINA = case_when(BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020 ~ 1,
                              TRUE ~ 0),
         CHINESE = case_when(RACE == 41 | ETHNICCA == 17 ~ 1,
                             TRUE ~ 0),
         ASIAN = case_when((RACE >= 40 & RACE < 50) | (ETHNICCA == 16 | ETHNICCA <= 18) ~ 1, 
                           TRUE ~ 0),
         WHITE = case_when(RACE == 10 | ETHNICCA < 14 ~ 1, 
                           TRUE ~ 0),
         EDUC = case_when(EDATTAIN == 4 ~ "Univ +",
                          EDATTAIN == 3 ~ "High School",
                          EDATTAIN == 1 | EDATTAIN == 2 ~ "< High School",
                          TRUE ~ "Unknown/NIU"),
         INCWAGE = ifelse(INCWAGE < 9999998, INCWAGE, NA),
         INCTOT = ifelse(INCTOT < 9999998, INCTOT, NA),
         RACE = case_when(ASIAN == 1 ~ "Asian",
                          WHITE == 1 ~ "White",
                          TRUE ~ "Unknown"))

contempincwage <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
  filter(EMPSTAT == 1) %>%
  group_by(YEAR, COUNTRY, RACE) %>%
  summarize(INCTOT = weighted.mean(INCTOT, PERWT, na.rm=TRUE))

contempincwageeduc <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
  filter(EMPSTAT == 1) %>%
  mutate(ASIAN = ifelse(is.na(ASIAN), 0, ASIAN),
         WHITE = ifelse(is.na(WHITE), 0, WHITE),
         RACE = case_when(ASIAN == 1 ~ "Asian",
                          WHITE == 1 ~ "White",
                          TRUE ~ "Unknown")) %>%
  group_by(YEAR, COUNTRY, RACE) %>%
  summarize(INCWAGE = weighted.mean(INCWAGE, PERWT, na.rm=TRUE))

# inc over time
ggplot(contempincwage, aes(x = YEAR, y = INCTOT, color = RACE)) + geom_line() + facet_wrap(~COUNTRY)

# inc distribution in 2000/2001
ggplot(contempincwage %>% filter(YEAR == 2000 | YEAR == 2001), 
       aes(x = INCWAGE, fill = RACE)) + geom_density(weight = PERWT, alpha = 0.5) + facet_wrap(~COUNTRY)


