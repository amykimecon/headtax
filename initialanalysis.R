########################################################################
### FILE DESCRIPTION: Initial Look at Canadian and US Historical Data
### PRIMARY OBJECTIVE: Seeing what is available and reliable, initial patterns in data
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Sep 27 2022
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
# hannah's excel sheet
chireg <- read_excel(glue("{dbox}/ChineseRegistry.xlsx"), guess_max = 1000000)
chinums <- chireg %>% group_by(Registration_Year) %>% summarize(n = n())

# https://open.library.ubc.ca/cIRcle/collections/facultyresearchandpublications/52383/items/1.0075988
chiregfull <- read_excel(glue("{dbox}/ChineseRegistryFull.xlsx"), guess_max = 1000000)
chinumsfull <- chiregfull %>% group_by(REG_Year) %>% summarize(n=n())

clean_all <- read_csv(glue("{dbox}/cleaned/census_all.csv"), guess_max = 5715448)
clean_imm <- clean_all %>% filter(IMM == 1)
clean_chi <- clean_all %>% filter(BPLCHI == 1)

########################################################################
### AGGREGATION & OUTPUTS
########################################################################
summ_stats_all <- clean_all %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())

summ_stats_imm <- clean_imm %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())

summ_stats_chi <- clean_chi %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())


grouped_years <- clean_all %>% group_by(YEAR) %>% filter(IMM == 1) %>%
  dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIPCT = CHIPOP/IMMPOP,
            RUSPOP = sum(BPLRUS * WEIGHT, na.rm=TRUE), RUSPCT = RUSPOP/IMMPOP,
            FRAPOP = sum(BPLFRA * WEIGHT, na.rm=TRUE), FRAPCT = FRAPOP/IMMPOP,
            GERPOP = sum(BPLGER * WEIGHT, na.rm=TRUE), GERPCT = GERPOP/IMMPOP,
            JAPPOP = sum(BPLJAP * WEIGHT, na.rm=TRUE), JAPPCT = JAPPOP/IMMPOP,
            INDPOP = sum(BPLIND * WEIGHT, na.rm=TRUE), INDPCT = INDPOP/IMMPOP,
            CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
            EARNRATIO = CHIEARN/IMMEARN) %>%
  pivot_longer(ends_with("PCT"), names_to = "IMMGRP", names_pattern = "([A-Z]{3})PCT$", values_to = "PCT") %>% 
  filter(IMMGRP != "GER" & IMMGRP != "RUS")

immigpct <- ggplot(data = grouped_years, aes(x = YEAR, y = PCT * 100, color = IMMGRP)) + geom_line() +
  labs(x = "Year", y = "Immigrant Group as % of Total Immigrants", color = "Birthplace")
ggsave(glue("{git}/figs/immigpct.png"),immigpct, height = 4, width = 6)

occ_years <- clean_all %>% group_by(YEAR, OCCGRP) %>% filter(IMM == 1 & MALE == 1 & AGE >= 18) %>%
  dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE)) %>%
  filter(!is.na(OCCGRP)) %>%
  group_by(YEAR) %>% mutate(UNSKILLEDCHI = CHIPOP/sum(CHIPOP), UNSKILLEDIMM = IMMPOP/sum(IMMPOP)) %>% filter(OCCGRP != "Skilled") %>%
  ungroup() %>%
  pivot_longer(c(UNSKILLEDCHI, UNSKILLEDIMM), names_to = "IMMGRP", names_prefix = "UNSKILLED", values_to = "UNSKILLED") %>%
  mutate(IMMGRP = case_when(IMMGRP == "CHI" ~ "Chinese Immigrants",
                            IMMGRP == "IMM" ~ "All Immigrants"))

# %>% pivot_wider(names_from = YEAR, values_from = c(CHIPOP, IMMPOP))

immigoccs <- ggplot(occ_years, aes(x = YEAR, y = UNSKILLED*100, color = IMMGRP)) + geom_line() + labs(x = "Year", y = "% of Group Unskilled", color = "Group")
ggsave(glue("{git}/figs/immigocs.png"),immigoccs, height = 4, width = 6)

# immigrant inflows
grouped_years_imm <- clean_all %>% filter(YEAR > 1900 & IMM == 1) %>%
  group_by(YEAR, YRIMM) %>% dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIIMMPCT = CHIPOP/IMMPOP,
                                      CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
                                      EARNRATIO = CHIEARN/IMMEARN) %>% select(c(YEAR, YRIMM, CHIPOP)) %>% filter(YEAR > 1900) %>%
  mutate(YEAR = as.character(YEAR)) %>%
  rbind(chinums %>% filter(YEAR != 0) %>% 
          mutate(YRIMM = YEAR, CHIPOP = n, YEAR = "Chinese Registry FULL") %>%
          select(YRIMM, CHIPOP, YEAR))

yrimmpct <- ggplot(data = filter(grouped_years_imm, YRIMM >= 1880 & YRIMM <= 1930), aes(x = YRIMM, y = CHIPOP, color = factor(YEAR))) + geom_line() +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1924) +
  labs(x = "Year of Immigration", y = "Chinese Immigrants as % of Total Immigrants", color = "Census Year")
ggsave(glue("{git}/figs/yrimmpct.png"), yrimmpct, height = 4, width = 6)



grouped_years_earn <- clean_all %>% filter(YEAR > 1900 & IMM == 1, AGE >= 18, MALE == 1 & EARN > 0) %>%
  group_by(YEAR, YRIMM) %>% dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIIMMPCT = CHIPOP/IMMPOP,
                                             CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
                                             EARNRATIO = CHIEARN/IMMEARN)
ggplot(data = filter(grouped_years_imm, YRIMM > 1880), aes(x = YRIMM, y = CHIEARN, color = factor(YEAR))) + geom_line()


ggplot(data = grouped_years, aes(x = YEAR)) + geom_line(aes(y = CHIEARN))

ggplot(data = grouped_years, aes(x = YEAR)) + geom_line(aes(y = CHIPOP*50), color = "Red") + geom_line(aes(x = YEAR, y = IMMPOP), color = "Blue")


#### TODO:
# -benchmark BPLCHI totals with census tabs if available

# 
histcolsnum <- str_count(readLines(glue("{dbox}/ipums_historical.csv"), n = 1), ",") + 1
histraw <- read_csv(glue("{dbox}/ipums_historical.csv"),
                    col_types = paste0(rep("d", histcolsnum), collapse=""))
# 
# contempcolsnum <- str_count(readLines(glue("{dbox}/ipums_contemp.csv"), n = 1), ",") + 1
# contempraw <- read_csv(glue("{dbox}/ipums_contemp.csv"),
#                     col_types = paste0(rep("d", contempcolsnum), collapse=""))
# 
# ########################################################################
# ### HISTORICAL DATA
# ########################################################################
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
         CHINESE = ifelse((RACE == 41 | ORIGIN == 8410), 1, 0),
         IMM = ifelse((COUNTRY == "Canada" & BPLCOUNTRY != 24020) | (COUNTRY == "US" & BPLCOUNTRY != 24040), 1, 0),
         OCCGRP = case_when(OCCISCO >= 6 & OCCISCO <= 9 ~ "Unskilled/Farm",
                            OCCISCO <= 5 ~ "Skilled",
                            TRUE ~ NA_character_))

histgroup <- histclean %>% group_by(YEAR, COUNTRY) %>%
  mutate(POP = sum(PERWT),
         IMMPOP = sum(IMM * PERWT, na.rm=TRUE),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/IMMPOP) %>%
  dplyr::summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean))

histgroupocc <- histclean %>% filter(!is.na(OCCGRP)) %>%
  group_by(YEAR, COUNTRY, OCCGRP) %>%
  mutate(POP = sum(PERWT),
         IMMPOP = sum(IMM * PERWT, na.rm=TRUE),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINA = sum(BPLCHINA*PERWT, na.rm=TRUE),
         BPLCHINAPCT = BPLCHINA/IMMPOP) %>%
  dplyr::summarize(across(c(CHINESEPCT, BPLCHINAPCT, BPLCHINA, IMMPOP), mean))

ipums_chinapop <- ggplot(histgroup, aes(x = YEAR, y = BPLCHINAPCT, color = factor(COUNTRY)))+ geom_line()
ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINAPCT, color = factor(OCCGRP)))+ geom_line()
ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINA, color = factor(OCCGRP)))+ geom_line()

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
#   dplyr::summarize(INCTOT = weighted.mean(INCTOT, PERWT, na.rm=TRUE))
# 
# contempincwageeduc <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
#   filter(EMPSTAT == 1) %>%
#   mutate(ASIAN = ifelse(is.na(ASIAN), 0, ASIAN),
#          WHITE = ifelse(is.na(WHITE), 0, WHITE),
#          RACE = case_when(ASIAN == 1 ~ "Asian",
#                           WHITE == 1 ~ "White",
#                           TRUE ~ "Unknown")) %>%
#   group_by(YEAR, COUNTRY, RACE) %>%
#   dplyr::summarize(INCWAGE = weighted.mean(INCWAGE, PERWT, na.rm=TRUE))
# 
# # inc over time
# ggplot(contempincwage, aes(x = YEAR, y = INCTOT, color = RACE)) + geom_line() + facet_wrap(~COUNTRY)
# 
# # inc distribution in 2000/2001
# ggplot(contempincwage %>% filter(YEAR == 2000 | YEAR == 2001), 
#        aes(x = INCWAGE, fill = RACE)) + geom_density(weight = PERWT, alpha = 0.5) + facet_wrap(~COUNTRY)


