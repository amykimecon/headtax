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
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw_all <- bind_rows(raw1901 %>% mutate(YEAR = 1901), raw1911 %>% mutate(YEAR = 1911)) %>%
  mutate(WEIGHT = 20)

# COMPARING DATASETS ----
## Counts and Weights ----
ipumsi %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(PERWT))
clean_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))
raw_all %>% group_by(YEAR) %>% summarize(n = n(), pop = sum(WEIGHT))

## Birthplace ----



