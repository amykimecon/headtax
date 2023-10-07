########################################################################
### FILE DESCRIPTION: Descriptive graphs & tables 
### PRIMARY OBJECTIVE: Initial look at trends & patterns in data, comparing various data sources
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

# country_list <- c("Australia", "China", "France","Germany","India","Italy",
#                   "Japan","Sweden","Turkey")
# country_list_old <- c("Australia","Brazil","Chile","China", 
#                         "France","Germany","India","Italy",
#                         "Japan","Mexico","Sweden","Turkey")
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
### TABLE 1: SUMMARY STATS BY DATA SOURCE & GROUP (1900-1920)
########################################################################
# # us -- entire population
# us_summ <- read_csv(glue("{dbox}/cleaned/us_all_summ.csv"))
# 
# # canada -- entire population
# can_summ <- read_csv(glue("{dbox}/cleaned/can_all_summ.csv"))

# computing and binding all summary stats for other groups
summ_stats_df <- bind_rows(#us_summ, #can_summ, 
                           summstats(us_imm),
                           summstats(us_chi),
                           summstats(us_jap),
                           summstats(can_imm),
                           summstats(can_chi),
                           summstats(can_jap),
                           summstats(reg_chi, vars = c("MALE", "AGE", "LABOR", "YRIMM"))) %>%
  select(c(-OBS, OBS)) %>%
  arrange(across(c(group, source)))

summ_stats_out <- t(summ_stats_df) %>% as.data.frame()

# writing
summ_cats <- c("\\% Male", "\\% Married*", "Age", "\\% Literate*", "\\% Laborers*", "Earnings","Year of Imm.")
summtex <- file(glue("{git}/figs/summstats.tex"), open = "w")
writeLines(c("\\begin{tabular}{lccccccccc}", 
             "\\hhline{==========}", 
             "& \\multicolumn{2}{c}{All Foreign-Born} & & \\multicolumn{3}{c}{Chinese Immigrants} & & \\multicolumn{2}{c}{Japanese Immigrants} \\\\ ", 
             "\\hhline{~--~---~--}", "& (1) & (2) & & (3) & (4) & (5) & & (6) & (7) \\\\ ",
              "& CA Census & US Census & & CA Census & US Census & Chinese Reg. & & CA Census & US Census \\\\ ", " \\hhline{----------}"), summtex)
for (i in 1:length(summ_cats)){
  means <- ifelse(is.na(summ_stats_out[2*i+1,]), "-", summ_stats_out[2*i+1,])
  sds <- ifelse(is.na(summ_stats_out[2*i+2,]), "", paste0("(",round(as.numeric(summ_stats_out[2*i+2,]),4),")"))
  writeLines(c(paste(summ_cats[i], "&", glue_collapse(c(means[1:2],"",means[3:5],"",means[6:7]), sep = "&", last = ""), "\\\\ "), 
               paste("&", glue_collapse(c(sds[1:2],"",sds[3:5],"",sds[6:7]), sep = "&", last = ""), "\\\\ ")), summtex)
}
obs <- ifelse(is.na(summ_stats_out[2*(length(summ_cats)+1)+1,]), "-", round(as.numeric(summ_stats_out[2*(length(summ_cats)+1)+1,]), 0))
writeLines(c("Obs. (Thousands)", "&", glue_collapse(c(obs[1:2],"",obs[3:5],"",obs[6:7]), sep = "&", last = ""), "\\\\ "), summtex)
writeLines(c("\\hhline{----------}","\\end{tabular}"), summtex)
close(summtex)


########################################################################
### FIGURE 1: AVG TAX PAID BY CHINESE IMM BY YEAR
########################################################################
tax_by_year_reg <- reg_chi %>% filter(REG_Year != 0) %>% group_by(REG_Year) %>% 
  summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE), 
            taxpayers = sum(ifelse(FEES > 0, 1, 0), na.rm=TRUE)/n) %>% rename(YEAR = REG_Year) %>%
  mutate(CAT = "Year Registered")

tax_by_year_arriv <- reg_chi %>% group_by(YEAR_ARRIV) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = YEAR_ARRIV) %>%
  mutate(CAT = "Year Arrived")

fig1_taxespaid <- ggplot(tax_by_year_arriv %>% filter(YEAR <= 1923), aes(x = YEAR, y = tax)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = ht) +
  geom_text(aes(x = yrs, y = 400, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = -1.2, size = 4, color = ht) + expand_limits(y = 0) +
  xlab("Year of Arrival") + ylab("Average Tax Paid Among Tax Payers") + theme_minimal()

ggsave(glue("{git}/figs/fig1_taxespaid.png"), fig1_taxespaid, height = 5, width = 9)


########################################################################
### FIGURE 2: NUMBER OF CHINESE IMMIGRANTS: INFLOW BY YEAR AND DATA SOURCE
########################################################################
# number of chinese immigrants by year from chinese register data
yrimm_reg <- reg_chi %>%
  group_by(YRIMM, tax) %>% 
  summarize(CHIFLOW_REGISTER = n()) %>%
  arrange(YRIMM)

# number of immigrants by year and by birthplace from ca census data
yrimm_census <- can_imm %>% group_by(YEAR, YRIMM, BPL, tax) %>% 
  summarize(FLOW = sum(WEIGHT)) %>%
  filter((YEAR == 1901 & YRIMM < 1901) | 
           (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | 
           (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) # only taking YRIMM from most recent census (lowest rate of loss to outmigration)

# register data for chinese immigrants; canadian govt stats for all
yrimm_flow_graph <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigrants") %>%
                            select(c(YRIMM, FLOW, variable)),
                          immflow %>% mutate(YRIMM = Year, FLOW = IMMFLOW_NATL/50000, variable = "All Immigrants") %>%
                            select(c(YRIMM, FLOW, variable))) %>%
  filter(YRIMM >= 1886 & YRIMM <= 1930)

ggplot(data = yrimm_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')
  # annotate("rect", xmin = 1893, xmax = 1897, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3) +
  # annotate("rect", xmin = 1899.5, xmax = 1901, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  # annotate("rect", xmin = 1902.75, xmax = 1904.75, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  # annotate("rect", xmin = 1907.5, xmax = 1908.5, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  # annotate("rect", xmin = 1910, xmax = 1912, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  # annotate("rect", xmin = 1913, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)


ggsave(glue("{git}/figs/fig2_flow.png"), height = 4, width = 7)

# census data
yrimm_flow_census_graph <- rbind(yrimm_census %>% filter(BPL == "China") %>% 
                                   mutate(FLOW = FLOW/1000, variable = "Chinese Immigrants") %>%
                                   select(c(YRIMM, FLOW, variable)),
                                 yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(FLOW = sum(FLOW)) %>% 
                                   mutate(FLOW = FLOW/50000, variable = "All Immigrants") %>%
                                   select(c(YRIMM, FLOW, variable))) %>%
  filter(YRIMM >= 1880 & YRIMM <= 1920)

ggplot(data = yrimm_flow_census_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 3, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_census_flow.png"), height = 4, width = 7)

## chinese emigration to canada and total
yrem_flow_graph <- hk_departure %>% filter(YEAR > 1880) %>% mutate(EMIG_CA = EMIG_CA/1000, EMIG_TOT = EMIG_TOT/10000) %>%
  select(-EMIG_US) %>% pivot_longer(cols = starts_with("EMIG"), names_to = "variable",
               names_prefix = "EMIG_", values_to = "EMIG") %>%
  mutate(YRIMM = YEAR, FLOW = EMIG, variable = ifelse(variable == "CA", "Emigration from Hong Kong to Canada", "Total Emigration from Hong Kong"))

ggplot(data = yrem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(hk2,hk4)) +
  scale_linetype_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 13, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("HK Emigration Outflow to Canada (Thous.)", sec.axis = sec_axis(~ . *10, name = "Total HK Emigration Outflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow_hkemig.png"), height = 4, width = 7)

yrimmandem_flow_graph <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigration to Canada") %>%
                                 select(c(YRIMM, FLOW, variable)),
                               hk_departure %>% mutate(variable = "Canadian Emigration from Hong Kong", FLOW = EMIG_CA/1000, YRIMM = YEAR) %>%
                                 select(c(FLOW, YRIMM, variable))) %>%
  filter(YRIMM >= 1880 & YRIMM <= 1930)

ggplot(data = yrimmandem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(hk4,c4)) +
  scale_linetype_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(4, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Number of Migrants", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow_immandem.png"), height = 4, width = 7)

########################################################################
### TABLE 2: INFLOW REGRESSION 1
########################################################################
#first combining all long datasets, then normalizing flow and popstock by source country pop,
# then pivoting wide and adding single country datasets
flow_regress <- yrimm_census %>% group_by(BPL) %>% mutate(n = n()) %>% filter(n > 5) %>% # at least five years of nonzero imm
  ungroup() %>% select(-c(YEAR,tax,n)) %>% #census data (flows)
  full_join(popstock, by = c("YRIMM"="YEAR", "BPL")) %>% #population stocks
  inner_join(maddison_data %>% mutate(Year = as.double(Year)) %>% #source country population and gdp per capita data
              inner_join(wid_data) %>% #source country inequality data
            pivot_longer(-Year, names_to = c(".value", "BPL"), names_pattern = "(.*)_([A-z]+)$"),
            by = c("YRIMM" = "Year", "BPL")) %>%
  group_by(BPL) %>% arrange(YRIMM) %>%
  mutate(#FLOW = ifelse(is.na(FLOW) & cumprod(is.na(FLOW)) != 1, min(FLOW, na.rm=TRUE), FLOW),
         logFLOWOVERPOP = log(FLOW/INTERP_POP), #dividing flows and stocks by source country population
         POPSTOCK = lag(CANPOP),
         POPSTOCKOVERPOP = POPSTOCK/INTERP_POP,
         INTERP_GDPPERCAP = INTERP_GDP/INTERP_POP) %>%
  pivot_wider(id_cols = c(YRIMM), names_from = BPL,
              values_from = c(FLOW, CANPOP, INTERP_POP, INTERP_GDPPERCAP, INTERP_INCSHARE50PCT, logFLOWOVERPOP, POPSTOCK)) %>%
  left_join(yrimm_reg) %>% #register data
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(IMMFLOW_CENSUS = sum(FLOW))) %>%
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "ForeignBorn") %>% rename(IMMPOP = CANPOP) %>% select(c(YEAR, IMMPOP)), by = c("YRIMM" = "YEAR")) %>%
  mutate(across(starts_with("INTERP_GDPPERCAP"), ~ .x/INTERP_GDPPERCAP_Canada, .names = "REL_{.col}"),
         across(starts_with("INTERP_INCSHARE50PCT"), ~ .x/INTERP_INCSHARE50PCT_Canada, .names = "REL_{.col}"),
         EMIGOVERPOP = EMIG_TOT/INTERP_POP_China,
         IMMOVERPOP = IMMFLOW_NATL/IMMPOP,
         REGFLOWOVERPOP_China = CHIFLOW_REGISTER/INTERP_POP_China,
         logREGFLOWOVERPOP_China = log(REGFLOWOVERPOP_China),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
                                 YRIMM %in% c(1901,1902) ~ "post100",
                                 YRIMM %in% c(1904,1905) ~ "post500",
                                 TRUE ~ "_notpost"),
         oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
                                 YRIMM == 1901 ~ "post100",
                                 YRIMM == 1904 ~ "post500",
                                 TRUE ~ "_notpost"))

ggplot(flow_regress, aes(x = YRIMM, y = FLOW_China)) + geom_line()

## REGRESSIONS V1: CHINA ONLY
toggle = "display"
#toggle = "output"

# using register flows
# equation 1: controls for imm, emm, popstock, and tax
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM > 1885 & YRIMM < 1924), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) + POPSTOCK_China + I(POPSTOCK_China^2))

# using controls for 2yr drops only
flowreg2 <- lm(data = flow_regress %>% filter(YRIMM > 1885 & YRIMM < 1924) %>% mutate(twoyearpost = ifelse(twoyearpost == "_post50", "_notpost", twoyearpost)), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# controls for both
flowreg2b <- lm(data = flow_regress %>% filter(YRIMM > 1885 & YRIMM < 1924) %>% mutate(twoyearpost = ifelse(twoyearpost == "_post50", "_notpost", twoyearpost)), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) +  factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# using census flows
# equation 1: controls for imm, emm, popstock, and tax
flowcensus1 <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
               FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(tax) + POPSTOCK_China + I(POPSTOCK_China^2))

# using controls for 2yr drops only
flowcensus2 <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
               FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# controls for both
flowcensus2b <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
                FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(tax) +  factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# sub log flow over pop
flowcensus3 <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
               logFLOWOVERPOP_China ~ IMMFLOW_CENSUS + factor(tax) +  POPSTOCK_China + I(POPSTOCK_China^2))

if (toggle == "display"){
  print(summary(flowreg1))
  print(summary(flowreg2))
  print(summary(flowreg2b))
  print(summary(flowcensus1))
  print(summary(flowcensus2))
  print(summary(flowcensus2b))
  print(summary(flowcensus3))
}

reg_mean = round(mean(filter(flow_regress, YRIMM > 1885 & YRIMM < 1924)$CHIFLOW_REGISTER), 1)
reg_se = round(sd(filter(flow_regress, YRIMM > 1885 & YRIMM < 1924)$CHIFLOW_REGISTER)/sqrt(1923-1886),1)
census_mean = round(mean(filter(flow_regress, YRIMM >= 1880 & YRIMM < 1921)$FLOW_China),1)
census_se = round(sd(filter(flow_regress, YRIMM >= 1880 & YRIMM < 1921)$FLOW_China)/sqrt(1920-1880),1)

flowreg_stats = c(rep(glue("{reg_mean} ({reg_se})"),3),rep(glue("{census_mean} ({census_se})"),3))
flowreg_means = c(rep(glue("{reg_mean}"),3),rep(glue("{census_mean}"),3))
flowreg_ses = c(rep(glue("({reg_se})"),3),rep(glue("({census_se})"),3))

## output
stargazer(flowreg1, flowreg2, flowreg2b, flowcensus1, flowcensus2, flowcensus2b,
          out = glue("{git}/figs/immflow_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.caption = "Dependent Variable: $FLOW_{China, t}$",
          dep.var.labels.include = FALSE,
          column.labels = c("Chinese Register (1886-1923)", "Canadian Census (1880-1920)"),
          column.separate = c(3,3),
          keep = "^factor",
          covariate.labels = c("$\\gamma_{50}$ (\\$50 Tax)", "$\\gamma_{100}$ (\\$100 Tax)", "$\\gamma_{500}$ (\\$500 Tax)",
                               "$\\gamma_{50}^{2YR}$ (2 Yr $\\times$ \\$50 Tax)", "$\\gamma_{100}^{2YR}$ (2 Yr $\\times$ \\$100 Tax)", 
                               "$\\gamma_{500}^{2YR}$ (2 Yr $\\times$ \\$500 Tax)"),
          add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
          table.layout = "=lc#-t-as=")

## REGRESSIONS V2: All countries
reg_countries <- filter(yrimm_census %>% filter(YRIMM >= 1880 & YRIMM < 1921 & BPL != "Other" & BPL != "Unknown" & BPL != "Iceland") %>% group_by(BPL) %>% summarize(n=n()), n > 20)$BPL
graph_country_regs <- list()
i = 1
for (country in reg_countries){
  shiftcoef = -0.4 + (i-1)*(0.8/length(reg_countries))
  flow_regress_filt <-  filter(flow_regress, YRIMM >= 1880 & YRIMM < 1921)
  flow_regress_filt <- flow_regress_filt[which(!is.na(flow_regress_filt[[glue("logFLOWOVERPOP_{country}")]])),]
  reg <- lm(data = flow_regress_filt, glue("logFLOWOVERPOP_{country} ~ IMMFLOW_CENSUS + factor(tax) +  POPSTOCK_{country} + I(POPSTOCK_{country}^2)"))
  regcoef <- summary(reg)$coefficients
  taxrows <- which(str_detect(rownames(regcoef),"factor\\(tax\\)"))
  out <- as.data.frame(summary(reg)$coefficients[taxrows,1:2]) 
  graph_country_regs[[i]] <- out %>% 
    mutate(tax = rownames(regcoef)[taxrows], meanimm = mean(flow_regress_filt[[glue("FLOW_{country}")]], na.rm=TRUE),
           country = country, shift = shiftcoef)
  i = i + 1
}
graph_countries <- bind_rows(graph_country_regs) %>%
  mutate(est_lb = Estimate - 1.96*`Std. Error`,
         est_ub = Estimate + 1.96*`Std. Error`) %>%
  group_by(tax) %>% 
  arrange(desc(Estimate), .by_group = TRUE) %>%
  mutate(row = row_number(),
         shift = -0.4 + (row-1)*(0.8/max(row)),
         taxyear = case_when(tax == "factor(tax)1496.19" ~ 1 + shift,
                             tax == "factor(tax)2992.61" ~ 2 + shift,
                             tax == "factor(tax)14115.7" ~ 3 + shift))

ggplot(graph_countries, aes(x = taxyear, y = Estimate, color = "Other")) + 
  geom_errorbar(aes(min = est_lb, max = est_ub, color = 'Other'), width = 0.05) + geom_point() +
  geom_errorbar(data = graph_countries %>% filter(country == "China"), aes(min = est_lb, max = est_ub, color = 'China'), width = 0.05) + 
  geom_point(data = graph_countries %>% filter(country == "China"), aes(x = taxyear, y = Estimate, color = 'China')) +
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"= expression(paste("$50 Head Tax (",gamma[50] ^c, ")")),
                                                       "2"= expression(paste("$100 Head Tax (",gamma[100] ^c, ")")),
                                                       "3"= expression(paste("$500 Head Tax (",gamma[500] ^c, ")")))) +
  labs(y = expression(paste("Estimated Effect of Head Tax on Immigration"))) + 
  scale_color_manual(name = "Country", breaks = c('Other', 'China'),
                     values = c('Other' = ht, 'China'='red')) + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/immflow_countries.png"), height = 4, width = 7)


########################################################################
### TABLE 3: DIFF IN DIFF REGRESSION OF CHI IMM VS OTHER IMM AT EACH 'EVENT'
########################################################################
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM >= 1880 & YRIMM <= 1910) %>% #only keeping years with earnings/yrimm data
  mutate(YEARSAFTER1890 = YRIMM - 1890,
         BORNCHI = ifelse(BPL == "China", 1, 0),
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         YEARSSINCEIMM = YEAR - YRIMM) %>%
  filter(AGE >= 18 & MALE == 1)

did_reg_labor <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)


stargazer(did_reg_labor, did_reg_canread, did_reg_earn, did_reg_houseown,
          out = glue("{git}/figs/8aug23/outcome_regs_can.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          keep = c("BORNCHI*"),
          single.row = TRUE,
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$50 Tax", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "=cd#-ta-s-",
          add.lines = list(c("Dep. Var. Mean (SE)", "0.207 (0.002)", "0.923 (0.001)", "800.5 (6.515)", "0.473 (0.002)")))



## register data
did_data_reg <- reg_chi %>% filter(YRIMM >= 1886 & YRIMM <= 1910) %>%
  mutate(LABOR = ifelse(PROFESSION == "Labourer", 1, 0),
         t = YRIMM - 1880) %>%
           filter(AGE >= 18 & MALE == 1 & FEES > 0)


ggplot(did_data_reg %>% group_by(YRIMM) %>% summarize(LABORPCT = mean(LABOR, na.rm=TRUE)), 
       aes(x = YRIMM, y = LABORPCT)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 0.8, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Arrival", y = "Fraction Laborers [Register]")
  
ggsave(glue("{git}/figs/8aug23/register_labor.png"), height = 4, width = 7)

ggplot(did_data_reg %>% filter(HEIGHT > 48) %>% group_by(YRIMM) %>% summarize(HEIGHT = mean(HEIGHT, na.rm=TRUE)), 
       aes(x = YRIMM, y = HEIGHT)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 63.8, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Arrival", y = "Average Height (in.) [Register]")
  
ggsave(glue("{git}/figs/8aug23/register_height.png"), height = 4, width = 7)

# ggplot(did_data %>% filter(BPL == "China")  %>% filter((YEAR == 1901 & YRIMM < 1901) | 
#                                (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | 
#                                (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
#        group_by(YRIMM) %>% summarize(LABORPCT = mean(LABOR, na.rm=TRUE)), 
#        aes(x = YRIMM, y = LABORPCT)) + geom_line() + geom_vline(aes(xintercept = 1903))

did_reg_labor_reg <- lm(LABOR ~ AGE + factor(tax) + t , data = did_data_reg)
did_reg_height_reg <- lm(HEIGHT ~ AGE + factor(tax) + t , data = did_data_reg)


stargazer(did_reg_labor_reg, did_reg_height_reg,
          out = glue("{git}/figs/8aug23/register_regs.tex"), float = FALSE, 
          intercept.bottom =FALSE, keep = c("factor(tax)*"),
          covariate.labels = c("\\$100 Tax", "\\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          single.row = TRUE,
          table.layout = "=cd#-ta-s-",
          add.lines = list(c("Dep. Var. Mean","0.798 (0.002)","64.22 (0.010)")))


########################################################################
### FIGURE 3: US VS CANADA
########################################################################
yrimm_us <- us_imm %>% 
  filter((YEAR == 1900 & YRIMM < 1900) | (YEAR == 1910 & YRIMM < 1910 & YRIMM >= 1900) | (YEAR == 1920 & YRIMM < 1920 & YRIMM >= 1910)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  group_by(YEAR, YRIMM) %>% 
  summarize(IMMFLOW = n(), CHIFLOW = sum(BORNCHI)) 

yrimm_flow_graph_all <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigrants") %>%
                                               select(c(YRIMM, FLOW, variable)),
                                             immflow %>% mutate(YRIMM = Year, FLOW = IMMFLOW_NATL/50000, variable = "All Immigrants") %>%
                                               select(c(YRIMM, FLOW, variable))) %>%
  mutate(source = "Canada") %>%
  rbind(yrimm_us %>% mutate(FLOW = IMMFLOW/50000, variable = "All Immigrants", source = "US") %>% select(c(YRIMM, FLOW, variable, source)),
        yrimm_us %>% mutate(FLOW = CHIFLOW/1000, variable = "Chinese Immigrants", source = "US") %>% select(c(YRIMM, FLOW, variable, source))) %>%
  filter(YRIMM >= 1870 & YRIMM <= 1930)

ggplot(data = yrimm_flow_graph_all, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c1,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = rbind(headtaxcuts,data.frame(yrs = 1882,labs="US Chinese Excl. Act")), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 11, label = labs), data = rbind(headtaxcuts,data.frame(yrs = 1882,labs="US Chinese Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') +
  facet_wrap(~source, ncol = 1)

ggsave(glue("{git}/figs/fig3_us_can.png"), height = 5, width = 7)

# sep 6
ggplot(data = yrimm_flow_graph_all %>% filter(variable == "All Immigrants"), aes(x = YRIMM, y = FLOW, color = source)) + geom_line() +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') +
  annotate("rect", xmin = 1893, xmax = 1897, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 1899.5, xmax = 1901, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1902.75, xmax = 1904.75, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  annotate("rect", xmin = 1907.5, xmax = 1908.5, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  annotate("rect", xmin = 1910, xmax = 1912, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1913, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)

ggsave(glue("{git}/figs/6sep23/immflow.png"), height = 4, width = 7)


# 
# fig3_us_can <- ggplot(data = yrimm_us_can %>% filter(YRIMM >= 1870), aes(x = YRIMM, y = POP, color = source, linetype = IMM)) + geom_line() +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE) +
#   geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
#   geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
#   geom_text(aes(x = x, y = 7000, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
#   #scale_color_manual(breaks=c("CA Census", "US Census"), values=c("black","blue")) +
#   scale_linetype_manual(breaks=c("Chinese", "Japanese"), values=c(1,3)) +
#   labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", linetype = "Data Source", color = "Immigrant Group") + theme_minimal() + theme(legend.position='bottom')
# 
# 
# ggsave(glue("{git}/figs/fig3_us_can.png"), fig3_us_can, height = 5, width = 9)
# 

########################################################################
### TABLE 4: US VS CAN REGRESSIONS
########################################################################
did_data_us <- us_imm %>% filter(YEAR >= 1900 & YRIMM >= 1880 & YRIMM <= 1910) %>% #only keeping years with earnings/yrimm data
  mutate(BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         ERSCORE = ifelse(EARN > 100, NA, EARN)) %>%
  filter(AGE >= 18 & MALE == 1)

did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
did_reg_canread_us <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
did_reg_earn_us <- lm(ERSCORE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
did_reg_houseown_us <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)

stargazer(did_reg_labor_us, did_reg_canread_us, did_reg_earn_us, did_reg_houseown_us, 
          out = glue("{git}/figs/8aug23/outcome_regs_us.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          keep = c("BORNCHI*"),
          single.row = TRUE,
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$50 Tax", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "=cd#-ta-s-",
          add.lines = list(c("Dep. Var. Mean (SE)", "0.219 (0.000)", "0.884 (0.000)", "48.3 (0.007)", "0.332 (0.000)")))

mean(filter(did_data_us, !is.na(LABOR))$LABOR)
sd(filter(did_data_us, !is.na(LABOR))$LABOR)/sqrt(nrow(filter(did_data_us, !is.na(LABOR))))

mean(filter(did_data_us, !is.na(CANREAD))$CANREAD)
sd(filter(did_data_us, !is.na(CANREAD))$CANREAD)/sqrt(nrow(filter(did_data_us, !is.na(CANREAD))))

mean(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)
sd(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)/sqrt(nrow(filter(did_data_us, !is.na(ERSCORE))))

mean(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)
sd(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)/sqrt(nrow(filter(did_data_us, !is.na(HOUSEOWN))))

# run regressions -- triple diff with us and canada
did_reg_uscan_labor <- lm(LABOR ~ factor(YRIMM)*CAN + BORNCHI*CAN*factor(tax), data = did_data_us_can, weights = WEIGHT)
did_reg_uscan_canread <- lm(CANREAD ~ factor(YRIMM)*CAN + BORNCHI*CAN*factor(tax), data = did_data_us_can, weights = WEIGHT)
did_reg_uscan_houseown <- lm(HOUSEOWN  ~ factor(YRIMM)*CAN + BORNCHI*CAN*factor(tax), data = did_data_us_can, weights = WEIGHT)

stargazer(did_reg_uscan_labor, did_reg_uscan_canread, did_reg_uscan_houseown,
          out = glue("{git}/figs/uscan_regs.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          dep.var.labels = c("Laborer", "Literate", "Owns Home"),
          keep = c(31, 62, 63, 64, 67, 68),
          covariate.labels = c("$BORNCHI \\times US$", 
                               "$BORNCHI \\times CANADA $",
                               "$BORNCHI \\times US \\times $ \\$100 Tax",
                               "$BORNCHI \\times US \\times $ \\$500 Tax",
                               "$BORNCHI \\times CANADA \\times $ \\$100 Tax",
                               "$BORNCHI \\times CANADA \\times $ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Includes Year $\\times$ Country FE", rep("Yes", 3))),
          table.layout = "=cld#-ta-s-")



