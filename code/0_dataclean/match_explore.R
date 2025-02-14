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

occstrings <- reg_chi %>% filter(!is.na(Occupation_ci44)) %>% group_by(Occupation_ci44) %>% 
  summarise(n=n()) %>% filter(n >= 10)

reg_chi_post <- reg_chi %>%
  mutate(occ_match = sapply(Occupation_ci44, bestpartialmatch, patterns = occstrings$Occupation_ci44, returnval = TRUE),
         occ_match = ifelse(is.na(occ_match) & !is.na(Occupation_ci44), 
                            sapply(str_remove_all(Occupation_ci44, " "), bestpartialmatch, patterns = occstrings$Occupation_ci44, returnval = TRUE, max.distance = 0.4),
                            occ_match),
         occ_nonmatch = ifelse(is.na(occ_match) & is.na(occ_match2) & !is.na(Occupation_ci44), Occupation_ci44, NA),
         occ_ci44_clean = case_when(is.na(Occupation_ci44) ~ NA_character_,
                                    str_detect(occ_match, "laun|jaundry|landry") ~ "Laundryman",
                                    str_detect(occ_match, "erchant|marchant") ~ "Merchant",
                                    str_detect(occ_match, "labor|labour|miner|gardener|gardoner|bour|hand|lahorer|borer|logger|labonrer|minor|woodsman|section man|shingle mill worker") ~ "Labourer",
                                    str_detect(occ_match, "book keeper|bookkeeper|teacher|druggist") ~ "Professional",
                                    str_detect(occ_match, "ook|chef|cock") ~ "Cook",
                                    str_detect(occ_match, "student|school|scholar") ~ "Student",
                                    str_detect(occ_match, "waiter|barber|dishwasher|employee|washer|dish|weiter|janitor|kitchen|porter|driver|chauffeur|housekeeper|house|domestic") ~ "Service",
                                    str_detect(occ_match, "farmer|farner|former") ~ "Farmer",
                                    str_detect(occ_match, "clerk|peddler|salesman|dealer|olerk|vegetable") ~ "Salesman",
                                    str_detect(occ_match, "tailor|baker|butcher|maker|actor|printer|photographer|carpenter|confectioner|smith|jeweller|cobbler") ~ "Craftsman/Artist",
                                    str_detect(occ_match, "keeper|proprietor|grocer|owner|kpr") | sapply(occ_match, adist, y = "restaurant") <= 2 |
                                      sapply(occ_match, adist, y = "restauranteur") <= 2 |
                                      occ_match %in% c("cafe", "restaurant prop") ~ "Proprietor",
                                    sapply(occ_match, adist, y = "laundryman") <= 2 ~ "Laundryman",
                                    sapply(occ_match, adist, y = "merchant") <= 2 ~ "Merchant",
                                    sapply(occ_match, adist, y = "laborer") <= 2 | sapply(occ_match, adist, y = "labourer") <= 2 |
                                      sapply(occ_match, adist, y = "gardener") <= 2 | sapply(occ_match, adist, y = "miner") <= 2 ~ "Labourer",
                                    sapply(occ_match, adist, y = "farmer") <= 2 | sapply(occ_match, adist, y = "farming") ~ "Farmer"
                                    str_detect(occ_match, "restaurant") ~ "Service",
                                    str_detect(occ_match, "housewife|none|not working|wife|married woman") ~ "Non-occupational",
                                    TRUE ~ "Unmatched"))



         str_detect(occ_match, "laun") ~ "Laundryman",
         str_detect(occ_match, "erchant") ~ "Merchant",
         str_detect(occ_match, "labor|labour|miner|gardener|gardoner") | 
           str_detect(occ_nonmatch, "hand|labor|labour|teamster|woodsman") ~ "Labourer",
         str_detect(occ_match, "cook") | 
           str_detect(occ_nonmatch, "chef") ~ "Cook",
         str_detect(occ_match, "student|school") |
           str_detect(occ_nonmatch, "scholar|school") ~ "Student",
         str_detect(occ_match, "waiter") | str_detect(Occupation_ci44, "waiter") |
           str_detect(occ_nonmatch, "barber|berber|washer|dish|janitor|kitchen|porter|driver|chauffeur|housekeeper|house|domestic") ~ "Service",
         str_detect(occ_match, "farmer") |
           str_detect(occ_nonmatch, "farm") ~ "Farmer",
         str_detect(occ_match, "housewife") ~ "Housewife",
         str_detect(occ_match, "clerk|peddler|dealer") | 
           str_detect(occ_nonmatch, "salesman|peddler|clerk|dealer|peddling|monger") ~ "Salesman",
         str_detect(occ_match, "tailor") |
           str_detect(occ_nonmatch, "baker|butcher|maker|actor|printer|carpenter|confectioner|smith|jeweller|cobbler") ~ "Craftsman/Artist",
         str_detect(occ_nonmatch, "teacher|photographer|druggist|editor|secretary|interpreter") ~ "Professional/Clerical",
         str_detect(occ_nonmatch, "logg|cannery|bourer|borer|minor|lehourer|labo|lebeurer|inhourer|worker|labonrar|labou|lahore|mill|le beurer|lehorer") ~ "Labourer",
         str_detect(occ_match, "keeper|proprietor") | occ_match %in% c("restaurant", "restaurateur", "restauranteur") |
           str_detect(occ_nonmatch, "kpr|keeper|proprietor|owner|grocer|rest|manager|prop") ~ "Proprietor/Manager",
         TRUE ~ "Not Matched")

#checking occ clean
View(sample_n(reg_chi_post, 100) %>% select(c(Occupation_ci44, occ_match, occ_match2)))

# graphing match rates by...
## year
ggplot(reg_chi %>% filter(YRIMM >= 1880 & YRIMM < 1924) %>% group_by(YRIMM) %>% summarize(match_ci44 = mean(match_ci44)),
       aes(x = YRIMM, y = match_ci44)) + geom_point()

ggplot(reg_chi %>% group_by(AGE) %>% summarize(match_ci44 = mean(match_ci44)),
       aes(x = AGE, y = match_ci44)) + geom_point()

