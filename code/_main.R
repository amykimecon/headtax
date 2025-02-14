#_____________________________________________________________
# FILE DESCRIPTION: Main code file -- initial data imports + calls to other files
# CREATED BY: Amy Kim
# CREATED ON: Aug 7 2022 
# LAST MODIFIED: Apr 2024
#_____________________________________________________________

# Importing Packages ----------------------------------
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)
library(stargazer)
library(fastDummies)
library(duckdb)
library(tictoc)
library(sandwich)
library(lmtest)
library(lubridate)
library(strucchange)
summarize <- dplyr::summarize

#_____________________________________________________________
# Defining Paths and Colors ----------------------------------
#_____________________________________________________________
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}
figs = glue("{git}/output/paper/figures")
tabs = glue("{git}/output/paper/tables")
slides_out = glue("{git}/output/slides")

us <- "#00BFC4"
ht <- "#808080"

c1 <- "#0E2954"
c2 <- "#1F6E8C"
c3 <- "#2E8A99"
c4 <- "#59C1BD"
c5 <- "#94DBD2"
c6 <- "#CFF5E7"

hk1 <- "#3C1518"
hk2 <- "#69140E"
hk3 <- "#A84007"
hk4 <- "#E66B00"
hk5 <- "#FFB370"

## key head tax years for graphing & labeling vertical lines
headtaxcuts <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("Initial $50 Head Tax", "Increase to $100", "Increase to $500", "Total Imm. Ban"))
headtaxcuts_slides <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("$50 Head Tax", "Incr. to $100", "Incr. to $500", "Total Ban"))
headtaxcuts_month <- data.frame(dates = c(as.Date("1885-07-20"), as.Date("1900-07-18"), as.Date("1901-01-01"),
                                          as.Date("1903-07-10"), as.Date("1904-01-01"), as.Date("1923-07-01")),
                                labs = c("$50 HT", "Incr to $100 Announced", "Incr to $100 Eff",
                                         "Incr to $500 Announced", "Incr to $500 Eff", "Ban"))
## running helper function 
source(glue("{git}/code/helper.R"))

# #_____________________________________________________________
# # CLEANING DATA - ONLY NEED TO RUN ONCE -----
# #_____________________________________________________________
# ## Chinese Register
# source(glue("{git}/code/0_dataclean/register_clean.R"))
# 
# ## Canadian Census
# source(glue("{git}/code/0_dataclean/can_census_clean.R"))

#_____________________________________________________________
# IMPORTING DATA -----
#_____________________________________________________________
# historical cpi ---
cpi <- read_xlsx(glue("{dbox}/crosswalks/cpi_historical.xlsx")) %>%
  filter(!is.na(index_fed)) %>%
  mutate(Year = as.numeric(str_remove_all(Year, "[^0-9]")),
         curr_cpi = ifelse(Year == 2024, index_fed, 0),
         mult1913today = 31.8) %>%
  group_by(1) %>%
  mutate(curr_cpi = max(curr_cpi)) %>%
  ungroup() %>%
  mutate(pricemult = curr_cpi/index_fed,
         pricemult_bls = 100*mult1913today/index_bls)

# CI44 Matches ----
matches <- read_csv(glue("{dbox}/cleaned/matches_feb11.csv")) %>%
  filter(matchstat == "wellmatched") %>%
  select(c(ID_reg, total_score, Occupation_ci44)) %>%
  mutate(match_ci44 = 1)

## Canadian Data ----
# chinese register, note that sample pre-1885 is biased (non mandatory registration, so only some selected people registered)
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  left_join(cpi %>% select(Year, pricemult), by = c("YEAR" = "Year")) %>%
  mutate(source = "Register", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, 
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 50,
                         YRIMM <= 1903 ~ 100,
                         YRIMM < 1924 ~ 500),
         cost = (tax + 50)*pricemult,
         cost2 = (tax + 25)*pricemult,
         cost3 = (tax + 75)*pricemult) %>%
  left_join(matches, by = c("ID" = "ID_reg")) %>%
  mutate(match_ci44 = ifelse(!is.na(match_ci44), match_ci44, 0)) %>%
  mutate(occ_match = sapply(Occupation_ci44, bestpartialmatch, patterns = occstrings$Occupation_ci44, returnval = TRUE),
         occ_match = ifelse(is.na(occ_match) & !is.na(Occupation_ci44), 
                            sapply(str_remove_all(Occupation_ci44, " "), bestpartialmatch, patterns = occstrings$Occupation_ci44, returnval = TRUE, max.distance = 0.4),
                            occ_match),
         occ_nonmatch = ifelse(is.na(occ_match) & !is.na(Occupation_ci44), Occupation_ci44, NA),
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
                                    sapply(occ_match, adist, y = "farmer") <= 2 | sapply(occ_match, adist, y = "farming") ~ "Farmer",
                                    str_detect(occ_match, "restaurant") ~ "Service",
                                    str_detect(occ_match, "housewife|none|not working|wife|married woman") ~ "Non-occupational",
                                    TRUE ~ "Unmatched"))

# CA census
can_imm <- read_csv(glue("{dbox}/cleaned/can_clean_imm.csv")) %>% 
  left_join(cpi %>% select(Year, pricemult), by = c("YRIMM" = "Year")) %>%
  mutate(source = "CA Census", group = "All Immigrants",
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 50,
                         YRIMM <= 1903 ~ 100,
                         YRIMM < 1924 ~ 500),
         cost = (tax + 50)*pricemult,
         cost2 = (tax + 25)*pricemult,
         cost3 = (tax + 100)*pricemult)

# # US census
# us_imm <- read_csv(glue("{dbox}/cleaned/us_clean_imm.csv")) %>%
#   mutate(source = "US Census", group = "All Immigrants",
#          WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
#          tax = case_when(YRIMM <= 1885 ~ 0,
#                          YRIMM <= 1900 ~ 1496.19,
#                          YRIMM <= 1903 ~ 2992.61,
#                          YRIMM < 1924 ~ 14115.70))

# historical macro data
canhistmacro <- read_xls(glue("{dbox}/raw/CANMACRO_data.xls")) %>% rename(RGNP = RGDP)

# official immigration inflow counts 
immflow <- read_csv(glue("{dbox}/raw/imm_nums.csv"))

# interpolated population stock
popstock <- read_csv(glue("{dbox}/cleaned/popstock_can.csv")) %>% 
  filter(INTERP == "spline") %>%
  rename(CANPOP = POP, CANPOP_INTERP = INTERP)

# population stock change for china
popstockchange <- read_csv(glue("{dbox}/cleaned/popstockchange_china.csv"))

## Chinese Data ----
# chinese emigration from HK (canada and total) 
hk_departure <- read_csv(glue("{dbox}/raw/hk_harbor_departures.csv"))

# heights from other sources (as read off fig 2 of baten et al. 2010)
china_height <- read_csv(glue("{dbox}/cleaned/china_height_plotdata.csv")) %>%
  mutate(source = case_when(file_name == "mig_au_melbvic_plot.csv" ~ "AUS Melb/Vic Migrants",
                            file_name == "mig_au_nthterr_plot.csv" ~ "AUS NT Migrants",
                            file_name == "mig_au_qld_plot.csv" ~ "AUS QLD Migrants",
                            file_name == "mig_to_id_plot.csv" ~ "Indonesia Migrants",
                            file_name == "mig_us_plot.csv" ~ "US Migrants",
                            file_name == "pris_au_plot.csv" ~ "AUS Prisoners",
                            file_name == "pris_us_plot.csv" ~ "US Prisoners",
                            file_name == "sth_unskilled_rlwy_plot.csv" ~ "Chinese Railway Workers"))

# age heaping from other sources (as read off fig 5 of baten et al. 2010)
china_age <- read_csv(glue("{dbox}/cleaned/china_age_plotdata.csv")) %>%
  mutate(source = case_when(file_name == "beij_milit_plot.csv" ~ "Beijing Military",
                            file_name == "mig_au_melb_b_d_plot.csv" ~ "AUS Melb/Bris/Darw Migrants",
                            file_name == "mig_au_syd_plot.csv" ~ "AUS Syd Migrants",
                            file_name == "mig_ind_plot.csv" ~ "Indonesia Migrants",
                            file_name == "mig_us_plot.csv" ~ "US Migrants",
                            file_name == "natl_census_plot.csv" ~ "Chinese 1953 Census",
                            file_name == "pris_au_plot.csv" ~ "AUS Prisoners"))

#_____________________________________________________________
# CALLING OTHER SCRIPTS ----------------------------------
#_____________________________________________________________
# ## Descriptive Analysis -- summary stats table, raw immigration trends, misc facts
# source(glue("{git}/code/1_descriptive_analysis.R"))
# 
# ## First Stage -- Effects of Head Tax on immigration inflows
# source(glue("{git}/code/2_firststage.R"))
# 
# ## Selection -- Effects of Head Tax on selection
# source(glue("{git}/code/3_selection.R"))

#_____________________________________________________________
# DEPRECATED DATA ----
#_____________________________________________________________
# chinese population (+ guangdong interp)
chinapop <- read_csv(glue("{dbox}/raw/CHINAPOP.csv")) %>%
  mutate(Guangdong_POP_interp = spline(Year, Guangdong_POP, method = "natural", xout = Year)$y) %>%
  filter(Year < 1930 & Year > 1870)

# official immigration inflow counts to canada by country (forensczi and wilcox)
immflow_nber <- read_csv(glue("{dbox}/raw/immflow_nber.csv")) %>%
  mutate(YRIMM = ifelse(YEAR <= 1907, YEAR - 0.5, YEAR - 0.75)) # fiscal year

# maddison population and gdp per capita data
maddison_pop <- read_csv(glue("{dbox}/raw/maddison_population.csv")) %>%
  pivot_longer(-Country, names_to = "Year", values_to = "POP") %>%
  filter(!str_detect(Year, "^\\.")) %>%
  mutate(POP = ifelse(POP == 0, NA, POP*1000)) %>%
  pivot_wider(id_cols = Year, names_from = "Country", names_glue = "POP_{Country}", values_from = c(POP)) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-Year, function(.x) spline(Year, .x, method = "natural", xout = Year)$y, .names = "INTERP_{.col}")) %>%
  mutate(INTERP_POP_EIndies = `INTERP_POP_Indonesia (including Timor until 1999)` + INTERP_POP_Philippines + INTERP_POP_Malaysia + INTERP_POP_Burma + INTERP_POP_Singapore + `INTERP_POP_Sri Lanka`,
         INTERP_POP_UK = `INTERP_POP_United Kingdom` + INTERP_POP_Ireland,
         INTERP_POP_AustriaHungary = INTERP_POP_Austria + INTERP_POP_Hungary,
         INTERP_POP_WIndies = INTERP_POP_Haiti + INTERP_POP_Jamaica + `INTERP_POP_Dominican Republic` + INTERP_POP_Cuba + `INTERP_POP_Trinidad and Tobago`,
         .keep = "unused") %>%
  rename(INTERP_POP_NZ = `INTERP_POP_New Zealand`,
         INTERP_POP_SAfrica = `INTERP_POP_South Africa`,
         INTERP_POP_Russia = INTERP_POP_USSR,
         INTERP_POP_US = `INTERP_POP_United States`) %>%
  select(c(Year, starts_with("INTERP_POP")))

maddison_gdp <- read_csv(glue("{dbox}/raw/maddison_gdp.csv")) %>%
  pivot_longer(-Country, names_to = "Year", values_to = "GDP") %>%
  filter(!str_detect(Year, "^\\.")) %>%
  mutate(GDP = ifelse(GDP == 0, NA, GDP*1000000)) %>%
  pivot_wider(id_cols = Year, names_from = "Country", names_glue = "GDP_{Country}", values_from = c(GDP)) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-Year, function(.x) spline(Year, .x, method = "natural", xout = Year)$y, .names = "INTERP_{.col}")) %>%
  mutate(INTERP_GDP_EIndies = `INTERP_GDP_Indonesia (including Timor until 1999)` + INTERP_GDP_Philippines + INTERP_GDP_Malaysia + INTERP_GDP_Burma + INTERP_GDP_Singapore + `INTERP_GDP_Sri Lanka`,
         INTERP_GDP_UK = `INTERP_GDP_United Kingdom` + INTERP_GDP_Ireland,
         INTERP_GDP_AustriaHungary = INTERP_GDP_Austria + INTERP_GDP_Hungary,
         INTERP_GDP_WIndies = INTERP_GDP_Haiti + INTERP_GDP_Jamaica + INTERP_GDP_Cuba,
         .keep = "unused") %>%
  rename(INTERP_GDP_NZ = `INTERP_GDP_New Zealand`,
         INTERP_GDP_SAfrica = `INTERP_GDP_South Africa`,
         INTERP_GDP_Russia = INTERP_GDP_USSR,
         INTERP_GDP_US = `INTERP_GDP_United States`) %>%
  select(c(Year, starts_with("INTERP_GDP")))

maddison_data <- maddison_pop %>% full_join(maddison_gdp) %>%
  left_join(chinapop %>% select(c(Year, Guangdong_POP_interp)) %>% mutate(Year = as.character(Year))) %>%
  mutate(INTERP_POP_Guangdong = Guangdong_POP_interp*1000000, .keep = "unused")

# wid.world inequality data
wid_data_raw <- read_delim(glue("{dbox}/raw/WID/wid_inequality_raw.csv"), delim = ";", skip = 1)
newnames <- str_extract(names(wid_data_raw), "^.+[A-Z]{2}")[3:length(names(wid_data_raw))] %>%
  str_replace("CN","China") %>% str_replace("BE", "Belgium") %>% str_replace("DK", "Denmark") %>%
  str_replace("FR", "France") %>% str_replace("DE", "Germany") %>% str_replace("GR", "Greece") %>%
  str_replace("IN", "India") %>% str_replace("JP", "Japan") %>% str_replace("NL", "Netherlands") %>%
  str_replace("NO", "Norway") %>% str_replace("SE", "Sweden") %>% str_replace('FI', "Finland") %>%
  str_replace("IT", "Italy") %>% str_replace("CL", "Chile") %>% str_replace("BR", "Brazil") %>%
  str_replace("MX", "Mexico") %>% str_replace("TR", "Turkey") %>% str_replace("AU", "Australia") %>%
  str_replace("AT", "Austria") %>% str_replace("CA", "Canada") %>% str_replace("GB", "UK") %>%
  str_replace("ID", "EIndies") %>% str_replace("ES", "Spain") %>% str_replace("ZA", "SAfrica") %>%
  str_replace("RU", "Russia") %>% str_replace("HU", "Hungary") %>%
  str_replace("CO", "Colombia") %>% str_replace("AR", "Argentina") %>% str_replace("DZ", "Algeria") %>%
  str_replace("EG", "Egypt") %>%
  str_replace("sptinc_z","INCSHARE50PCT")
names(wid_data_raw) <- c("Percentile", "Year", newnames)
wid_data <- wid_data_raw %>% filter(Percentile == "p0p50") %>% #taking bottom 50 percent only
  select(c("Year", starts_with("INCSHARE50PCT"))) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-Year, function(.x) spline(Year, .x, method = "natural", xout = Year)$y, .names = "INTERP_{.col}"))

# # # US Census
# # us_imm <- read_csv(glue("{dbox}/cleaned/us_clean_imm.csv")) %>% 
# #   mutate(source = "US Census", group = "All Immigrants", WEIGHT = 1,
# #          tax = case_when(YRIMM <= 1885 ~ 0,
# #                          YRIMM <= 1900 ~ 1496.19,
# #                          YRIMM <= 1903 ~ 2992.61,
# #                          YRIMM < 1924 ~ 14115.70))
# # us_chi <- us_imm %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
# # us_jap <- us_imm %>% filter(BORNJAP == 1) %>% mutate(group = "Japanese Immigrants")
# 
