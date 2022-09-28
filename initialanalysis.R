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
years <- c(1852, seq(1871, 1921, 10))
raw1852 <- read.spss(glue("{dbox}/census1852.sav")) %>% as.data.frame() # 20% sample
raw1871 <- read.spss(glue("{dbox}/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/census1921.sav")) %>% as.data.frame() # 4% sample

########################################################################
### CLEANING DATA
########################################################################
## BASE VARIABLES: LASTNAME, FIRSTNAME, MALE = 1[Male], MAR = 1[Married], SIN = 1[Single (Never Married)], AGE, BPL, OCC
## ADDITIONAL VARIABLES: ETH, CANREAD, UNEMP, EARN, YRIMM, HOUSEOWN
## ADD: YEAR
## GROUP/CLEAN: OCCUPATION, BPLCHI, BPL[OTHER], ETHCHI, ETH[OTHER]


#### 1881 DATA ####
clean1881 <- raw1881 %>% rename(BPL = birthplace_ccri, ETH = ethnicity_ccri, OCC = DOCCUP, OCCGRP = occgrp2, 
                                CLASSGRP = classgrp, LASTNAME = NAMLAST, FIRSTNAME = NAMFRST) %>%
  mutate(MALE = case_when(SEX == "Female" ~ 0,
                          SEX == "Male" ~ 1,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Unknown" ~ NA_real_,
                         TRUE ~ 0),
         NAPHISCOSTR = as.character(NAPHISCO),
         IMM = ifelse(imm == "born in Canada", 0, 1),
         OCCGRP = case_when(OCCGRP == "White collar" | OCCGRP == "Profesl" | OCCGRP == "Merc/Agent/Manu" ~ "Skilled",
                            OCCGRP == "Artisan" | OCCGRP ==  "Semi & Unskilled" ~ "Skilled",
                            OCCGRP == "Labourer" | OCCGRP == "Servant" ~ "Unskilled",
                            OCCGRP == "Farmer" ~ "Unskilled",
                            TRUE ~ NA_character_)) %>% 
  select(c(MALE, AGE, MAR, ETH, OCC, NAPHISCOSTR, OCCGRP, BPL, IMM, LASTNAME, FIRSTNAME)) %>% 
  mutate(YEAR = 1881, BPLCHI = ifelse(str_detect(BPL, "China"),1,0), BPLRUS = ifelse(str_detect(BPL, "Russia"), 1, 0), 
         BPLFRA = ifelse(str_detect(BPL, "France"), 1, 0), 
         BPLGER = ifelse(str_detect(BPL, "Germany"), 1, 0),
         BPLJAP = ifelse(str_detect(BPL, "Japan"),1,0),
         BPLIND = ifelse(str_detect(BPL, "India"),1,0),
         ETHCHI = ifelse(str_detect(ETH, "Chinese"), 1, 0), WEIGHT = 1)


clean1891 <- raw1891 %>% mutate(WEIGHT = case_when(samplesize == 5 ~ 20,
                                                   samplesize == 10 ~ 10,
                                                   samplesize == 100 ~ 1),
                                BPLCHI = ifelse(bplcode == 50000, 1, 0),
                                BPLRUS = ifelse(bplcode >= 46100 & bplcode < 49000, 1, 0),
                                BPLFRA = ifelse(bplcode == 42100, 1, 0),
                                BPLGER = ifelse(bplcode == 45300, 1, 0),
                                BPLJAP = ifelse(bplcode == 50100, 1, 0),
                                BPLIND = ifelse(bplcode == 52100, 1, 0),
                                MALE = case_when(SEX == "M  " ~ 1,
                                                 SEX == "F  " ~ 0,
                                                 TRUE ~ NA_real_),
                                MAR = case_when(MARST == "married" ~ 1,
                                                MARST == "single" | MARST == "Divorced" | MARST == "W" ~ 0,
                                                TRUE ~ NA_real_),
                                CANREAD = ifelse(CANREAD == "Yes, can read", 1, 0),
                                IMM = ifelse(bplcode < 16000 & bplcode >= 15000, 0, 1),
                                OCCGRP = case_when((occ50 < 975 & occ50 > 800) | occ50 == 720 ~ "Unskilled",
                                                   occ50 < 100 | (occ50 >= 200 & occ50 < 300) ~ "Skilled",
                                                   occ50 >= 100 & occ50 < 200 ~ "Unskilled",
                                                   occ50 >= 300 & occ50 < 700 ~ "Skilled",
                                                   TRUE ~ NA_character_)) %>%
  select(-c(AGE)) %>%
  rename(NAPHISCO = NappHisco, AGE = agecode, LASTNAME = NAMELAST, FIRSTNAME = NAMEFIRST) %>%
  select(c(MALE, AGE, MAR, CANREAD, starts_with("BPL"), BPLRUS, UNEMP, OCCGRP, WEIGHT, IMM, LASTNAME, FIRSTNAME)) %>%
  mutate(YEAR = 1891)

clean1901 <- raw1901 %>% rename(BPL = bpl, SEX = sex, MARST = marst, AGE = ageyr, YRIMM = immyr, PROPOWNR = propownr, NATL = natl, OCC = occ, 
                                EARN = earnings, CANREAD = canread, LASTNAME = indlnm, FIRSTNAME = indfnm) %>%
  mutate(BPLCHI = ifelse(bpl2 == 50000, 1, 0),
         BPLRUS = ifelse(str_detect(BPL, "RUS"), 1, 0),
         BPLFRA = ifelse(str_detect(BPL, "FRA"), 1, 0),
         BPLGER = ifelse(str_detect(BPL, "GER"), 1, 0),
         BPLJAP = ifelse(str_detect(BPL, "JAP"), 1, 0),
         BPLIND = ifelse(str_detect(BPL, "IND"), 1, 0),
         OCCSTR = ifelse(occ1 == "99999", NA, as.numeric(as.character(str_extract(occ1,"^[0-9]{2}")))), 
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
         YRIMM = as.numeric(YRIMM),
         IMM = ifelse(bpl2 < 16000 & bpl2 >= 15000, 0, 1),
         OCCGRP = case_when(OCCSTR >= 71 ~ "Unskilled",
                            OCCSTR >= 40 & OCCSTR <= 70 ~ "Skilled",
                            OCCSTR < 40 ~ "Skilled",
                            TRUE ~ NA_character_)) %>%
  select(c(MALE, AGE, starts_with("BPL"), MAR, YRIMM, PROPOWNR, NATL, OCCGRP, EARN, CANREAD, IMM, FIRSTNAME, LASTNAME)) %>%
  mutate(YEAR = 1901, WEIGHT = 20)

bplcanadastrings <- "(Ontario)|(Quebec)|(Nova Scotia)|(New Brunswick)|(Manitoba)|(Saskatchewan)|(Prince Edward Island)|(British Columbia)|(Alberta)|(Canada)|(Newfoundland)|(Northwest Territories)|(Yukon)|(Cape Breton)|(Labrador)"

clean1911 <- raw1911 %>% rename(AGE = AGE_AMOUNT, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                                CANREAD = CAN_READ_INDICATOR, OCC = OCCUPATION_CHIEF_OCC_IND, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
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
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), as.numeric(as.character(YRIMM)), NA),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 20,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 4),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCCGRP = case_when(OCC == "Laborers (n.e.c.)" ~ "Unskilled",
                            OCC == "Managers, officials, and proprietors (n.e.c.)" ~ "Skilled",
                            occ1 == "Agriculture" | occ1 == "Forestry and lumbering" | occ1 == "Mining" | occ1 == "Fisheries and hunting" | occ1 == "Building trades" | occ1 == "Transportation" | str_starts(occ1, "Domestic")~ "Unskilled",
                            str_starts(occ1, "Manufactures") | occ1 == "Trade and Merchandising" ~ "Skilled",
                            occ1 == "Civil and municipal service" | occ1 == "Professional pursuits" ~ "Skilled")) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, FIRSTNAME, LASTNAME)) %>% 
  mutate(YEAR = 1911, BPLCHI = ifelse(BPL == "China", 1, 0), BPLRUS = ifelse(BPL == "Russia", 1, 0),
         BPLFRA = ifelse(BPL == "France", 1, 0), BPLGER = ifelse(BPL == "Germany", 1, 0),
         BPLJAP = ifelse(BPL == "Japan", 1, 0), BPLIND = ifelse(BPL == "India", 1, 0))

clean1921 <- raw1921 %>% rename(AGE = Derived_Age_In_Years, MARST = MARITAL_STATUS, YRIMM = YEAR_OF_IMMIGRATION, BPL = INDIVIDUAL_BIRTH_COUNTRY, NATL = NATIONALITY,
                               CANREAD = CAN_READ_INDICATOR, OCC = CHIEFOCCUP, FIRSTNAME = FIRST_NAME, LASTNAME = LAST_NAME) %>%
  mutate(EARN = ifelse(grepl("[0-9]+", ANNUAL_EARNING_AMOUNT), as.numeric(as.character(ANNUAL_EARNING_AMOUNT)), 0),
         MALE = case_when(SEX == "Male" ~ 1,
                          SEX == "Female" ~ 0,
                          TRUE ~ NA_real_),
         MAR = case_when(MARST == "Married" ~ 1,
                         MARST == "Single" | MARST == "Widowed" | MARST == "Divorced" ~ 0,
                         TRUE ~ NA_real_),
         CANREAD = case_when(CANREAD == "Yes" ~ 1,
                             CANREAD == "No" ~ 0,
                             TRUE ~ NA_real_),
         YRIMM = ifelse(grepl("[0-9]+", YRIMM), as.numeric(as.character(YRIMM)), NA),
         WEIGHT = case_when(str_starts(Dwelling_Unit_Type, "UU") ~ 25,
                            str_starts(Dwelling_Unit_Type, "SU") ~ 10,
                            str_starts(Dwelling_Unit_Type, "MU") ~ 5),
         IMM = ifelse(str_detect(BPL, bplcanadastrings), 0, 1),
         OCCGRP = case_when(str_detect(CHIEF_OCCUPATION, "Farm") | str_detect(CHIEF_OCCUPATION, "Labor") | str_detect(CHIEF_OCCUPATION, "Mine") | CHIEF_OCCUPATION == "Private household workers (n.e.c.)" | str_detect(CHIEF_OCCUPATION, "Lumbermen") | CHIEF_OCCUPATION == "Carpenters" | CHIEF_OCCUPATION == "Machinists" | CHIEF_OCCUPATION == "Housekeepers, private household" | CHIEF_OCCUPATION == "Fishermen and oystermen" ~ "Unskilled",
                            CHIEF_OCCUPATION != "Blank" & CHIEF_OCCUPATION != "Unemployed/ without occupation" ~ "Skilled")) %>%
  select(c(MALE, AGE, MAR, CANREAD, BPL, NATL, OCC, OCCGRP, EARN, YRIMM, WEIGHT, IMM, FIRSTNAME, LASTNAME)) %>% 
  mutate(YEAR = 1921, BPLCHI = ifelse(BPL == "China", 1, 0), BPLRUS = ifelse(BPL == "Russia", 1, 0),
         BPLFRA = ifelse(BPL == "France", 1, 0), BPLGER = ifelse(BPL == "Germany", 1, 0),
         BPLJAP = ifelse(BPL == "Japan", 1, 0), BPLIND = ifelse(BPL == "India", 1, 0))

# binding all years and cleaning together
clean_all <- bind_rows(clean1881, clean1891) %>% bind_rows(clean1901) %>%
  bind_rows(clean1911) %>% bind_rows(clean1921) %>%
  mutate(AGE = ifelse(AGE > 200, NA, AGE),
         YRIMM = ifelse(YRIMM < 1500 | YRIMM > YEAR, NA, YRIMM))
clean_imm <- clean_all %>% filter(IMM == 1)
clean_chi <- clean_all %>% filter(BPLCHI == 1)

#### IMPUTING RACE/ETHNICITY FROM NAME FOR 1852 AND 1871 DATA ####
## using predictrace package
library(predictrace)
chineselastnames <- predict_race(unique(str_remove(str_extract(clean_chi$LASTNAME, "^[A-z]+ "), " $")))
chineselastnames2 <- predict_ethnicity(lastnames = na.omit(unique(str_remove(str_extract(clean_chi$LASTNAME, "^[A-z]+ "), " $"))), method = "lastname")

chinesefirstnames <- predict_race(unique(str_remove(str_extract(clean_chi$FIRSTNAME, "^[A-z]+ "), " $")), surname = FALSE)

clean1852 <- raw1852 %>% mutate(firstname = str_remove(str_extract(namfrst, "^[A-z]+ "), " $"),
                                lastname = str_remove(namlast, "\\s+$"),
                                firstname_predrace = predict_race(firstname, surname = FALSE)$likely_race,
                                lastname_predrace = predict_race(lastname)$likely_race,
                                predasian = ifelse(firstname_predrace == "asian" & lastname_predrace == "asian", 1, 0),
                                predchi = ifelse((str_to_lower(lastname) %in% str_to_lower(chineselastnames)) & nchar(lastname) > 1 & lastname_predrace == "asian", 1, 0))

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

# immigrant males over 18
grouped_years_imm <- clean_all %>% filter(YEAR > 1900 & IMM == 1) %>%
  group_by(YEAR, YRIMM) %>% dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIIMMPCT = CHIPOP/IMMPOP,
                                      CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
                                      EARNRATIO = CHIEARN/IMMEARN)

yrimmpct <- ggplot(data = filter(grouped_years_imm, YRIMM > 1880), aes(x = YRIMM, y = CHIIMMPCT*100, color = factor(YEAR))) + geom_line() +
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


