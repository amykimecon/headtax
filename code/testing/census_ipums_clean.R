#_____________________________________________________________
# FILE DESCRIPTION: Testing quality of data cleaning for key variables from canadian census against ipums international
# CREATED BY: Amy Kim
# CREATED ON: Apr 16 2024
# LAST MODIFIED: Apr 2024
#_____________________________________________________________

# IMPORTING CLEANED AND RAW DATA FOR COMPARISON ----
## ipums
ipumsi <- read_csv(glue("{dbox}/raw/can_census_ipumsi.csv"))

## cleaned
clean_all <- read_csv(glue("{dbox}/cleaned/can_clean.csv")) %>%
  filter(YEAR %in% c(1901, 1911))

## raw
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() %>% 
  distinct(indlnm, indfnm, reel, linenbr, pagenbr, sex, ageyr, .keep_all = TRUE) # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw_all <- bind_rows(raw1901 %>% mutate(YEAR = 1901), raw1911 %>% mutate(YEAR = 1911)) %>%
  mutate(WEIGHT = 20)

# MERGING ----
merge_1901 <- inner_join(filter(ipumsi, YEAR == 1901) %>% select(SERIAL, PERNUM, AGE, SEX, CA1901A_OCC, CA1901A_OCC01CFP, NAMELAST, NAMEFRST, REEL, LINENUM, PAGENUM),
                        raw1901 %>% select(dwellid, hhdid, indlnm, indfnm, sex, ageyr, occ, occ1, occ2, reel, pagenbr, linenbr) %>%
                          mutate(indlnm = str_trim(indlnm), indfnm = str_trim(indfnm)),
                        by = c("REEL" = "reel", "PAGENUM" = "pagenbr","LINENUM" = "linenbr", 
                               "NAMELAST" = "indlnm", "NAMEFRST" = "indfnm", "AGE" = "ageyr"))

# COMPARING DATASETS ----
## Counts and Weights ----
ipumsi %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(PERWT))
clean_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))
raw_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))

## Birthplace ----



