# File Description: Exploring CI44xreg matches!
# Author: Amy Kim
# Date Created: Tue Feb 11 17:58:28 2025

# IMPORTING PACKAGES ----
library(tidyverse)
library(glue)

# SETTING WORKING DIRECTORIES ----
root <- "/Users/amykim/Princeton Dropbox/Amy Kim/head_tax/head_tax_data"
code <- "/Users/amykim/Documents/GitHub/headtax/code"

# reading data
matches <- read_csv(glue("{dbox}/cleaned/matches_feb11.csv")) %>%
  filter(matchstat == "wellmatched") %>%
  select(c(ID_reg, total_score, Occupation_ci44)) %>%
  mutate(match_ci44 = 1)

reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, 
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70)) %>%
  left_join(matches, by = c("ID" = "ID_reg")) %>%
  mutate(match_ci44 = ifelse(!is.na(match_ci44), match_ci44, 0))

occstrings <- reg_chi %>% filter(!is.na(Occupation_ci44)) %>% group_by(Occupation_ci44) %>% summarize(n=n()) %>% filter(n >= 100)
reg_chi_post <- reg_chi %>%
  mutate(occ_match = sapply(Occupation_ci44, bestpartialmatch, patterns = occstrings$Occupation_ci44, returnval = TRUE),
         )occ_ci44_clean = case_when(is.na(Occupation_ci44) ~ NA_character_,
                                    str_detect(occ_match, "laundry|laundrey") ~ "Laundryman",
                                    str_detect(occ_match, "labor|labour") ~ "Labourer",
                                    str_detect(occ_match, "cook") ~ "Cook",
                                    str_detect(occ_match, "student|school") ~ "Student",
                                    str_detect(occ_match, "waiter") ~ "Waiter",
                                    str_detect(occ_match, "farmer") ~ "Farmer",
                                    str_detect(occ_match, "keeper|proprietor") | occ_match %in% c("restaurant", "restaurateur", "restauranteur") ~ "Proprietor",
                                    str_detect(occ_match, "housewife") ~ "Housewife",
                                    str_detect(occ_match, "gardener") ~ "Gardener",
                                    str_detect(occ_match, "clerk") ~ "Clerical",
                                    str_detect(occ_match, "")))
           
           case_when(is.na(Occupation_ci44) ~ NA_character_,
                                    str_detect(Occupation_ci44, "labour|labor")  ~ "Labourer",
                                    str_detect(Occupation_ci44, "cook") ~ "Cook",
                                    str_detect(Occupation_ci44, "laundry") ~ "Laundryman",
                                    str_detect(Occupation_ci44, "student|school") ~ "Student",
                                    str_detect(Occupation_ci44, "merchant") ~ "Merchant",
                                    str_detect(Occupation_ci44, "farmer") ~ "Farmer"))
# graphing match rates by...
## year
ggplot(reg_chi %>% filter(YRIMM >= 1880 & YRIMM < 1924) %>% group_by(YRIMM) %>% summarize(match_ci44 = mean(match_ci44)),
       aes(x = YRIMM, y = match_ci44)) + geom_point()

ggplot(reg_chi %>% group_by(AGE) %>% summarize(match_ci44 = mean(match_ci44)),
       aes(x = AGE, y = match_ci44)) + geom_point()

