########################################################################
### FILE DESCRIPTION: Initial Look at Canadian and US Historical Data
### PRIMARY OBJECTIVE: Seeing what is available and reliable, initial patterns in data
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Aug 7 2022
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
         CHINESE = ifelse((RACE == 41 | ORIGIN == 8410), 1, 0)) %>%
  group_by(YEAR, COUNTRY) %>%
  mutate(POP = sum(PERWT),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/POP) %>% ungroup()

histgroup <- histclean %>% group_by(YEAR, COUNTRY) %>% 
  summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean)) %>%
  pivot_longer(-c(YEAR, COUNTRY), names_to = "STAT", values_to = "pct_of_pop")

#ggplot(histgroup, aes(x = YEAR, y = pct_of_pop, color = factor(COUNTRY)))+ geom_line() + facet_wrap(~STAT)


