#_____________________________________________________________
# FILE DESCRIPTION: Occupational Code Cleaning for Canadian Census
# CREATED BY: Amy Kim
# CREATED ON: Apr 16 2024
# LAST MODIFIED: Apr 2024
#_____________________________________________________________

# IMPORTING RAW DATA ----
## ipums
ipumsi <- read_csv(glue("{dbox}/raw/can_census_ipumsi.csv")) %>% filter(YEAR == 1901)

## raw census data
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() %>% 
  distinct(hhdid, hhdpos, reel, linenbr, pagenbr, .keep_all = TRUE) # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 5% sample

## csv versions (for cleaner occupational coding)
raw1911_csv <- read_csv(glue("{dbox}/raw/census1911.csv"), guess_max = 1000000)

## crosswalks
napphisco_hisco <- read_csv(glue("{dbox}/crosswalks/napphisco_to_hisco.csv"))
hisco_occ1950 <- read_csv(glue("{dbox}/crosswalks/hisco_to_occ1950.csv"))

# MERGING OCC CODES FOR 1901 CENSUS ----
raw_ipumsi_merge_1901 <- right_join(
  # first inner join: ipums with raw data to link CFP (raw) codes to NAPPHISCO
  ipumsi %>% select(AGE, SEX, CA1901A_OCC, CA1901A_OCC01CFP, NAMELAST, NAMEFRST, REEL, LINENUM, PAGENUM) %>% mutate(ipumsi=1),
                        raw1901 %>% select(indlnm, indfnm, sex, ageyr, occ, occ1, reel, pagenbr, linenbr) %>%
                          mutate(indlnm = str_trim(indlnm), indfnm = str_trim(indfnm)),
                        by = c("REEL" = "reel", "PAGENUM" = "pagenbr","LINENUM" = "linenbr", 
                               "NAMELAST" = "indlnm", "NAMEFRST" = "indfnm", "AGE" = "ageyr")) %>%
  rename(OCC_CFP = occ1, OCC_NAPPHISCO = CA1901A_OCC, OCC_STR = occ) %>%
  mutate(ipumsi = ifelse(is.na(ipumsi), 0, ipumsi))

## crosswalk for occ1 x occ (5-digit code x string)
merge_1901_codes <- raw_ipumsi_merge_1901 %>%
  group_by(OCC_CFP, OCC_NAPPHISCO, OCC_STR) %>%
  summarize(n = n(), ipumsi = max(ipumsi)) %>%
  ungroup() %>% 
  group_by(OCC_CFP, OCC_STR) %>% mutate(n_napphisco = sum(ifelse(!is.na(OCC_NAPPHISCO), 1, 0))) %>% 
  filter(!is.na(OCC_NAPPHISCO) | n_napphisco == 0) %>% #dropping NA cells that have non-missing OCC_NAPPHISCO
  mutate(dropped = ifelse(n_napphisco == 0, "Dropped in linking to IPUMSI", "Not Dropped")) %>% #otherwise flag as dropped during linkage
  # merge with napphisco to hisco crosswalk
  left_join(napphisco_hisco %>% select(napp.code.num, hisco.code.num), by = c("OCC_NAPPHISCO" = "napp.code.num")) %>%
  mutate(dropped = ifelse(dropped == "Not Dropped" & is.na(hisco.code.num), "Dropped in linking to HISCO", dropped)) %>%
  # merge with hisco to occ1950 crosswalk
  left_join(hisco_occ1950 %>% select(HISCO_code, Occ1950_code), by = c("hisco.code.num" = "HISCO_code")) %>%
  mutate(dropped = ifelse(dropped == "Not Dropped" & is.na(Occ1950_code), "Dropped in linking to OCC1950", dropped))

## CODES FOR OCC1950 FROM 1911
merge_1911_codes <- inner_join(raw1911 %>% select(Derived_Household_Id, Derived_Person_Num_In_Household, OCCUPATION_CHIEF_OCC_IND), 
                               raw1911_csv %>% mutate(OCCCODE = OCCUPATION_CHIEF_OCC_IND, Derived_Household_Id = as.numeric(Derived_Household_Id)) %>% 
                                 select(Derived_Household_Id, Derived_Person_Num_In_Household, OCCCODE)) %>%
  group_by(OCCCODE) %>% summarize(n_1911=n(), OCCDESC = first(OCCUPATION_CHIEF_OCC_IND)) %>% filter(!is.na(OCCCODE))

write_csv(merge_1911_codes, glue("{dbox}/crosswalks/occ1950codes.csv"))

## MERGING WITH 1901 AND SAVING
merge_1901_1911 <- merge_1901_codes %>% left_join(merge_1911_codes, by = c("Occ1950_code" = "OCCCODE"))
write_csv(merge_1901_1911, glue("{dbox}/crosswalks/cfp_to_occ1950.csv"))

## MERGING WITH 1921 
merge_1921_codes <- raw1921 %>% group_by(CHIEF_OCCUPATION) %>% summarize(n_1921 = n())
merge_1911_1921 <- merge_1911_codes %>% full_join(merge_1921_codes, by = c("OCCDESC" = "CHIEF_OCCUPATION"))
write_csv(merge_1911_1921, glue("{dbox}/crosswalks/merge_1911_1921.csv"))

## crosswalk for 1911 job descriptions
occ_1911_crosswalk <- raw1911_csv %>% 
  mutate(occ_string = str_remove(str_to_upper(OCCUPATION_CHIEF_OCC_IND_CL), "[[:punct:]]"),
         occ_code = OCCUPATION_CHIEF_OCC_IND) %>%
  group_by(occ_string, occ_code) %>%
  summarize(n=n())
write_csv(occ_1911_crosswalk, glue("{dbox}/crosswalks/occ_1911_crosswalk.csv"))

# COMPARING DATASETS ----
## Counts and Weights ----
ipumsi %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(PERWT))
clean_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))
raw_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))

## Birthplace ----


