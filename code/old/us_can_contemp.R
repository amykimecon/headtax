############################################################################################################
### FILE DESCRIPTION: General comparisons of US and Canada immigration trends
### PRIMARY OBJECTIVE: Looking at summary statistics -- is trend mostly historical or contemporary?
### CREATED BY: Amy Kim
### CREATED ON: Nov 7 2022
### LAST MODIFIED: Nov 7 2022
############################################################################################################
library(tidyverse)
library(glue)

########################################################################
### DEFINING PATHS & IMPORTING RAW DATA
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

raw_ipums <- read_csv(glue("{dbox}/ipums_contemp.csv"))

clean_ipums <- raw_ipums %>% 
  mutate(COUNTRY = ifelse(COUNTRY == 124, "CAN", "USA"),
         IMM = ifelse(NATIVITY == 2, 1, 0),
         NONIMM = ifelse(NATIVITY == 1, 1, 0),
         IMMASIA = ifelse(BPLCOUNTRY >= 30000 & BPLCOUNTRY < 40000, 1, 0),
         IMMEA = ifelse(BPLCOUNTRY >= 31000 & BPLCOUNTRY < 32000, 1, 0),
         SPEAKENG = ifelse(SPEAKENG == 1, 1, 0),
         EDHS = ifelse(EDATTAIN == 3, 1, 0),
         EDCOL = case_when(EDATTAIN == 4 ~ 1,
                           AGE < 25 ~ NA_real_,
                           TRUE ~ 0),
         UNEMP = case_when(EMPSTAT == 2 ~ 1,
                           EMPSTAT == 1 ~ 0,
                           TRUE ~ NA_real_),
         URBAN = ifelse(URBAN == 2, 1, 0),
         HOMEOWN = case_when(OWNERSHIP == 1 ~ 1,
                             AGE < 18 ~ NA_real_,
                           TRUE ~ 0),
         EARACE = case_when(RACE == 41 | RACE == 42 | RACE == 43 ~ 1,
                            TRUE ~ 0),
         EA = case_when(RACEUS >= 400 & RACEUS <= 500 ~ 1,
                        RACEUS == 620 | RACEUS == 673 ~ 1,
                        ETHNICCA == 17 | ETHNICCA == 18 ~ 1,
                        TRUE ~ 0))

########################################################################
### FIRST PASS AT MAJOR SUMM STATS
########################################################################
ipums_agg <- clean_ipums %>%
  group_by(COUNTRY, YEAR) %>%
  summarize(POP = sum(PERWT),
            PCTIMM = sum(IMM * PERWT)/sum(PERWT),
            PCTIMMEA = sum(IMMEA * PERWT)/sum(PERWT),
            PCTNONIMMEA = sum(EA * NONIMM * PERWT)/sum(PERWT),
            across(c(SPEAKENG, EDHS, EDCOL, UNEMP, URBAN, HOMEOWN, AGE), 
                   .fns = list(~ weighted.mean(.x, PERWT, na.rm=TRUE), ~weighted.mean(.x, PERWT * IMMEA, na.rm=TRUE), ~weighted.mean(.x, PERWT * EA * NONIMM, na.rm=TRUE)), 
                   .names = "{.col}_{.fn}")) %>% 
  filter(YEAR >= 2000) %>%
  pivot_longer(-c(YEAR, COUNTRY, POP, PCTIMM, PCTIMMEA, PCTNONIMMEA), names_to = c(".value", "group"), names_pattern = "(.+)_(.+)")
  
ipums_recent <- clean_ipums %>% filter(YRIMM > 0 & YEAR >= 2010) %>% 
  group_by(YEAR, YRIMM, COUNTRY) %>%
  summarize(NUMIMM = sum(PERWT*IMM))


ggplot(data = ipums_agg, aes(x = YEAR, color = factor(COUNTRY))) + geom_line(aes(y = PCTIMM)) + geom_line(aes(y = PCTIMMEA), linetype = "dashed")

ggplot(data = ipums_agg, aes(x = YEAR, color = factor(COUNTRY))) + geom_line(aes(y = SPEAKENG_1)) + geom_line(aes(y = SPEAKENG_3), linetype = "dashed")


ggplot(data = ipums_recent, aes(x = YRIMM, y = NUMIMM, color = factor(COUNTRY))) + geom_line()


ipums_2011 <- clean_ipums %>% filter(YEAR >= 2010) %>%
  mutate(BPLCHI = )

