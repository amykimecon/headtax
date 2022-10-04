########################################################################
### FILE DESCRIPTION: Looking at Chinese Registry Data
### PRIMARY OBJECTIVE: Graphing Trends, etc.
### CREATED BY: Amy Kim
### CREATED ON: Sep 29 2022
### LAST MODIFIED: Sep 29 2022
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)

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
## chinese registry -- immigration data through ports
# source: https://open.library.ubc.ca/cIRcle/collections/facultyresearchandpublications/52383/items/1.0075988
chiregraw <- read_excel(glue("{dbox}/ChineseRegistry.xlsx"), guess_max = 1000000)

chireg <- chiregraw %>% mutate(COUNTYGRP = case_when(COUNTY_ID == 1 ~ "Taishan",
                                                     COUNTY_ID == 2 ~ "Xinhui",
                                                     COUNTY_ID == 3 ~ "Kaiping",
                                                     COUNTY_ID == 9 ~ "Panyu",
                                                     COUNTY_ID == 6 ~ "Zhongshan",
                                                     COUNTY_ID == 4 ~ "Enping",
                                                     COUNTY_ID == 5 ~ "Heshan",
                                                     TRUE ~ "Other"),
                               YEAR = YEAR_ARRIV,
                               PORT = case_when(ARR_PORT == "Victoria" ~ "Victoria",
                                                ARR_PORT == "Vancouver" ~ "Vancouver",
                                                TRUE ~ "Other"),
                               OCCGRP = case_when(PROFESSION == "Labourer" ~ "Labourer",
                                                  PROFESSION == "Student | Merchant" ~ "Student or Merchant",
                                                  PROFESSION == "Farmer" ~ "Farmer",
                                                  PROFESSION == "Laundryman" | PROFESSION == "Cook" | PROFESSION == "Grocer" ~ "Service",
                                                  TRUE ~ "Other"),
                               WC = case_when(SEX == "Male" & AGE >= 18 ~ "Adult Male",
                                              SEX == "Female" & AGE >= 18 ~ "Adult Female",
                                              AGE < 18 ~ "Child",
                                              TRUE ~ "Unknown")) %>%
  filter(YEAR != 0) #excluding entries without any arrival year (for now) -- only 521 such entries out of ~100k

chinums <- chireg %>% filter(REG_Year != 0) %>% group_by(REG_Year) %>% 
  summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = REG_Year)
chinums_arrive <- chireg %>% group_by(YEAR_ARRIV) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = YEAR_ARRIV)

# entry by year by origin county (largest counties: Taishan = 1, Xinhui = 2, Kaiping = 3, Panyu = 9, Zhongshan = 6, Enping = 4, Heshan = 5)
chiorig <- chireg %>% group_by(COUNTYGRP, YEAR) %>% summarize(n=n()) %>%
  group_by(YEAR) %>% mutate(pct = n/sum(n))
ggplot(chiorig, aes(x = YEAR, y = pct, fill = COUNTYGRP)) + geom_area()+ 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry by year by arrival port (victoria, vancouver, other)
ggplot(chireg %>% group_by(PORT, YEAR) %>% summarize(n=n()) %>% group_by(YEAR) %>% mutate(pct = n/sum(n)), 
       aes(x = YEAR, y = pct, fill = PORT)) + geom_area() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry by year by occupation group (for adults)
ggplot(chireg %>% filter(AGE >= 18 & SEX == "Male") %>% group_by(OCCGRP, YEAR) %>% summarize(n=n()) %>% group_by(YEAR) %>% mutate(pct = n/sum(n)), 
       aes(x = YEAR, y = n, fill = OCCGRP)) + geom_area() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923) + geom_vline(xintercept = 1912)

# entry in levels -- comparing registration year to arrival year
ggplot(rbind(select(chinums, c(YEAR, n)) %>% mutate(cat = "Registration"), 
             select(chinums_arrive, c(YEAR, n)) %>% mutate(cat = "Arrival")) %>% filter(YEAR != 0), 
       aes(x = YEAR, y = n, color = factor(cat))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry in levels -- tax payers vs non tax payers
chibytax <- chireg %>% filter(AGE > 18) %>% mutate(taxpay = ifelse(FEES > 0, 1, 0)) %>% group_by(YEAR, taxpay) %>% summarize(n = n())
ggplot(chibytax, aes(x = YEAR, y = n, color = factor(taxpay))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry in levels by sex
ggplot(chireg %>% group_by(WC, YEAR) %>% summarize(n=n()), aes(x = YEAR, y = n, color = factor(WC))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# tax
ggplot(chinums_arrive %>% filter(YEAR != 0), aes(x = YEAR, y = tax)) + geom_line() +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)


