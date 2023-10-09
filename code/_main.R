########################################################################
### FILE DESCRIPTION: Main code file -- initial data imports + calls to other files
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Oct 2023
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)
library(stargazer)
library(fastDummies)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

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

########################################################################
### IMPORTING DATA
########################################################################
## chinese registry -- immigration data through ports
# sample pre-1885 is biased (non mandatory registration, so only some selected people registered)
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, YRIMM_FISCAL = FISCALYEAR,
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))

## census data
can_imm <- read_csv(glue("{dbox}/cleaned/can_clean_imm.csv")) %>% 
  mutate(source = "CA Census", group = "All Immigrants",
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))
#can_chi <- can_imm %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
#can_jap <- can_imm %>% filter(BORNJAP == 1) %>% mutate(group = "Japanese Immigrants")

## us census data
us_imm <- read_csv(glue("{dbox}/cleaned/us_clean_imm.csv")) %>% 
  mutate(source = "US Census", group = "All Immigrants", WEIGHT = 1,
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))
us_chi <- us_imm %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
us_jap <- us_imm %>% filter(BORNJAP == 1) %>% mutate(group = "Japanese Immigrants")

## key head tax years for graphing & labeling vertical lines
headtaxcuts <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("Initial $50 Head Tax", "Increase to $100", "Increase to $500", "Total Imm. Ban"))

## historical macro data (canada)
canhistmacro <- read_xls(glue("{dbox}/raw/CANMACRO_data.xls")) %>% rename(RGNP = RGDP)

## official immigration inflow counts (canada)
immflow <- read_csv(glue("{dbox}/raw/imm_nums.csv"))

## official immigration inflow counts by country (forensczi and wilcox)
immflow_nber <- read_csv(glue("{dbox}/raw/immflow_nber.csv")) %>% 
  mutate(YRIMM = ifelse(YEAR <= 1907, YEAR - 0.5, YEAR - 0.75)) # fiscal year

## interpolated population stock (canada)
popstock <- read_csv(glue("{dbox}/cleaned/popstock_can.csv")) %>% 
  filter(INTERP == "spline") %>%
  rename(CANPOP = POP, CANPOP_INTERP = INTERP)

## chinese emigration from HK (canada and total)
hk_departure <- read_csv(glue("{dbox}/raw/hk_harbor_departures.csv"))
ggplot(hk_departure %>% filter(YEAR > 1880) %>%
         #mutate(EMIG_TOT = EMIG_TOT/10) %>%
         #select(-EMIG_US) %>%
         pivot_longer(cols = starts_with("EMIG"), names_to = "Group",
                      names_prefix = "EMIG_", values_to = "EMIG") %>%
         filter(YEAR < 1914),
       aes(x = YEAR, y = EMIG, color = Group)) + geom_line()

## chinese population (+ guangdong interp)
chinapop <- read_csv(glue("{dbox}/raw/CHINAPOP.csv")) %>%
  mutate(Guangdong_POP_interp = spline(Year, Guangdong_POP, method = "natural", xout = Year)$y) %>%
  filter(Year < 1930 & Year > 1870)

## maddison population and gdp per capita data
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

## wid.world inequality data
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


########################################################################
### CALLING OTHER SCRIPTS
########################################################################
source(glue("{git}/code/1_descriptive_analysis.R"))
source(glue("{git}/code/2_firststage.R"))
source(glue("{git}/code/3_selection.R"))


