########################################################################
### FILE DESCRIPTION: Descriptive graphs & tables 
### PRIMARY OBJECTIVE: Initial look at trends & patterns in data, comparing various data sources
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Mar 24 2023
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)
library(stargazer)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

ca_chi_census <- "#F8766D"
ca_jap_census <- 
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
                                YRIMM <= 1900 ~ 50,
                                YRIMM <= 1903 ~ 100,
                                YRIMM < 1924 ~ 500))

## census data
can_imm <- read_csv(glue("{dbox}/cleaned/can_clean_imm.csv")) %>% 
  mutate(source = "CA Census", group = "All Immigrants",
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 50,
                         YRIMM <= 1903 ~ 100,
                         YRIMM < 1924 ~ 500))
#can_chi <- can_imm %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
#can_jap <- can_imm %>% filter(BORNJAP == 1) %>% mutate(group = "Japanese Immigrants")

## us census data
us_imm <- read_csv(glue("{dbox}/cleaned/us_clean_imm.csv")) %>% 
  mutate(source = "US Census", group = "All Immigrants", WEIGHT = 1,
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 50,
                         YRIMM <= 1903 ~ 100,
                         YRIMM < 1924 ~ 500))
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
popstock <- read_csv(glue("{dbox}/cleaned/popstock_can.csv"))

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
maddison_data <- read_csv(glue("{dbox}/raw/maddison_population.csv")) %>%
  pivot_longer(-Country, names_to = "Year", values_to = "POP") %>%
  left_join(read_csv(glue("{dbox}/raw/maddison_gdppercapita.csv")) %>%
              pivot_longer(-Country, names_to = "Year", values_to = "GDPPERCAP")) %>%
  pivot_wider(id_cols = Year, names_from = "Country", names_sep = "_", values_from = c(POP, GDPPERCAP)) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-Year, function(.x) spline(Year, .x, method = "natural", xout = Year)$y, .names = "{.col}_INTERP"))

## wid.world inequality data
wid_data_raw <- read_delim(glue("{dbox}/raw/WID/wid_inequality_raw.csv"), delim = ";", skip = 1)
newnames <- str_extract(names(wid_data_raw), "^.+[A-Z]{2}")[3:length(names(wid_data_raw))] %>%
  str_replace("CN","China") %>% str_replace("BE", "Belgium") %>% str_replace("DK", "Denmark") %>%
  str_replace("FR", "France") %>% str_replace("DE", "Germany") %>% str_replace("GR", "Greece") %>%
  str_replace("IN", "India") %>% str_replace("JP", "Japan") %>% str_replace("NL", "Netherlands") %>%
  str_replace("NO", "Norway") %>% str_replace("SE", "Sweden") %>% str_replace('FI', "Finland") %>%
  str_replace("IT", "Italy") %>% str_replace("CL", "Chile") %>% str_replace("BR", "Brazil") %>% 
  str_replace("MX", "Mexico") %>% str_replace("TR", "Turkey") %>% str_replace("AU", "Australia") %>% 
  str_replace("AT", "Austria") %>% str_replace("sptinc_z","INCSHARE50PCT")
names(wid_data_raw) <- c("Percentile", "Year", newnames)
wid_data <- wid_data_raw %>% filter(Percentile == "p0p50") %>% #taking bottom 50 percent only 
  select(c("Year", starts_with("INCSHARE50PCT"))) %>%
  purrr::discard(~length(.x)-sum(is.na(.x)) < 2) %>%
  mutate(across(-Year, function(.x) spline(Year, .x, method = "natural", xout = Year)$y, .names = "{.col}_INTERP"))
  
########################################################################
### QUICK ACCOUNTING EXERICSE: OUTMIGRATION IN CANADA
########################################################################
chipop_census <- can_chi %>% group_by(YEAR) %>% summarize(pop = sum(WEIGHT))
chipop_reg <- reg_chi %>% filter(YEAR_ARRIV < 1921 & YEAR_ARRIV >= 1901) %>%
  mutate(YEAR_GRP = case_when(YEAR_ARRIV < 1911 ~ "1901-1911",
                                                      YEAR_ARRIV < 1921 ~ "1911-1921")) %>%
  group_by(YEAR_GRP) %>% summarize(inflow = n())

chipop_grp <- chipop_reg %>% mutate(CENSUSDIFF = diff(chipop_census$pop),
                                    outflow = inflow - CENSUSDIFF)
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
  filter(YRIMM >= 1880 & YRIMM <= 1930)

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
### TABLE 2: REGRESSION OF IMM INFLOWS ON TAX RAISES
########################################################################
# combining register & census (wide), adding historical GNP and official total immigration numbers
yrimm_flow_regress <- yrimm_reg %>%
  left_join(canhistmacro %>% select(c(Year, RGNP)), by = c("YRIMM" = "Year")) %>%
  left_join(immflow, by = c("YRIMM" = "Year")) %>% 
  left_join(yrimm_census %>% left_join(popstock %>% filter(INTERP == "spline"), by = c("YRIMM"="YEAR", "BPL")) %>%
              pivot_wider(id_cols = c(YRIMM, tax), names_from = BPL,
                          values_from = c(FLOW, POP)), by = c("YRIMM", "tax")) %>%
  left_join(can_imm %>% group_by(YRIMM) %>% summarize(FLOW_All = sum(WEIGHT))) %>% ungroup() %>%
  arrange(YRIMM) %>% mutate(lagRGNP = dplyr::lag(RGNP),
                            t = YRIMM - 1885,
                            GNP_GROWTH = (RGNP - lagRGNP)/lagRGNP) %>%
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>%
  left_join(chinapop, by = c("YRIMM" = "Year")) %>%
  filter(YRIMM <= 1923 & YRIMM >= 1880) %>%
  mutate(taxchange = (tax - lag(tax))/lag(tax))

names(yrimm_flow_regress) <- make.names(names(yrimm_flow_regress))

## base model: includes quadratic time trend, gnp growth, census imm reg uses total census imm

## checking if flows change with year -- register
yrimm_flow1 <- lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + t + I(t^2) + GNP_GROWTH + IMMFLOW_NATL)

# original
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + t + I(t^2) + GNP_GROWTH + IMMFLOW_NATL))

# without tax controls
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ POP_China + GNP_GROWTH + IMMFLOW_NATL))
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax)))
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ POP_China + I(POP_China^2) + EMIG_TOT + IMMFLOW_NATL + factor(tax)))

# with population stock instead of time and time squared
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + POP_China + GNP_GROWTH + IMMFLOW_NATL))

# with control for total HK emigration
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + POP_China + IMMFLOW_NATL+ EMIG_TOT))

# with linear term on tax instead of tax indicators
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ tax + POP_China + GNP_GROWTH + IMMFLOW_NATL+ EMIG_TOT))

# with tax and tax change indicators
summary(lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + factor(taxchange) + POP_China + GNP_GROWTH + IMMFLOW_NATL+ EMIG_TOT))


## checking if flows change with year -- census
yrimm_flow2 <- lm(data = yrimm_flow_regress, FLOW_China ~ factor(tax) + t + I(t^2) + GNP_GROWTH + FLOW_All)

## checking if flows change with year -- japanese (census)
yrimm_flow3 <- lm(data = yrimm_flow_regress, FLOW_Japan ~ factor(tax) + t + I(t^2) +  GNP_GROWTH + FLOW_All)

## output
stargazer(yrimm_flow1, yrimm_flow2, yrimm_flow3,
          out = glue("{git}/figs/immflow_regs.tex"), 
          float = FALSE, 
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          column.labels = c("Chinese Register", "Canadian Census", "Canadian Census"),
          column.separate = c(1,1,1),
          dep.var.labels = c("Chinese Imm. Inflow", "Chinese Imm. Inflow", "Japanese Imm. Inflow"),
          keep = c(2:4),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax"),
          add.lines = list(c("Time Trends", "Yes", "Yes", "Yes"), c("Ctrl. for Tot. Imm.", "Yes", "Yes", "Yes"), c("Ctrl. for Lagged Real GNP", "Yes", "Yes", "Yes")))

## checking if flows change with year -- census [pre 1908]
yrimm_flow4 <- lm(data = yrimm_flow_regress %>% filter(YRIMM < 1908), FLOW_China ~ factor(tax) + t + I(t^2) + GNP_GROWTH + FLOW_All)

## checking if flows change with year -- japanese (census)
yrimm_flow5 <- lm(data = yrimm_flow_regress %>% filter(YRIMM < 1908), JAPFLOW_CENSUS ~ factor(tax) + t + I(t^2) +  GNP_GROWTH + IMMFLOW_CENSUS)

stargazer(yrimm_flow1, yrimm_flow2, yrimm_flow3, yrimm_flow4, yrimm_flow5,
          out = glue("{git}/figs/21jul23/immflow_regs.tex"), 
          float = FALSE, 
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.labels = c("$CHIFLOW^R$ (Register)", "$CHIFLOW^C$ (Census)", "$JAPANFLOW^C$ (Census)","$CHIFLOW^C$ (Pre-1908)", "$JAPANFLOW^C$ (Pre-1908)"),
          keep = c(2:4),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax"))

## aug 8 regressions
# reg_countries <- c("Germany", "France", "Italy", "Netherlands", "Norway", "Denmark", "Australia and NZ", "Switzerland", "Spain", "Poland", "Sweden",
#                "Austria and Hungary", "Greece", "Russia", "Belgium", "China", "Japan", "India", "Iceland", "Chile", "Romania",
#                "West Indies", "Bermuda", "Mexico", "Finland", "Portugal", "East Indies", "Turkey", "Brazil") %>%
#   make.names()

reg_countries <- c("India", "Belgium", "Australia.and.NZ", "France", "Poland", "Russia", "Italy", "Denmark", "Norway", "Germany",
                   "Switzerland", "Sweden", "West.Indies", "Austria.and.Hungary", "Finland" ,"China",  "Japan")%>%
  make.names()
graph_country_regs <- list()
i = 1
for (country in reg_countries){
  shiftcoef = -0.3 + (i-1)*(0.6/length(reg_countries))
  #reg <- lm(data = yrimm_flow_regress, glue("FLOW_{country} ~ factor(tax) + t + I(t^2) + GNP_GROWTH + FLOW_All"))
  reg <- lm(data = yrimm_flow_regress, glue("FLOW_{country} ~ factor(tax) + POP_{country} + GNP_GROWTH + FLOW_All"))
  print(country)
  print(summary(reg))
  regcoef <- summary(reg)$coefficients
  taxrows <- which(str_detect(rownames(regcoef),"factor\\(tax\\)"))
  out <- as.data.frame(summary(reg)$coefficients[taxrows,1:2]) 
  graph_country_regs[[i]] <- out %>% 
    mutate(tax = rownames(regcoef)[taxrows], meanimm = mean(yrimm_flow_regress[[glue("FLOW_{country}")]], na.rm=TRUE),
           country = country, shift = shiftcoef)
  i = i + 1
}
minyear = min(filter(yrimm_flow_regress, !is.na(FLOW_China))$YRIMM)
maxyear = max(filter(yrimm_flow_regress, !is.na(FLOW_China))$YRIMM)
graph_countries <- bind_rows(graph_country_regs) %>%
  mutate(est_scale = asinh(Estimate/meanimm),
         stderr_scale = asinh(`Std. Error`/meanimm),
         est_lb = est_scale - 1.96*stderr_scale,
         est_ub = est_scale + 1.96*stderr_scale) %>%
  group_by(tax) %>% 
  arrange(desc(est_scale), .by_group = TRUE) %>%
  mutate(row = row_number(),
         shift = -0.3 + (row-1)*(0.6/max(row)),
         taxyear = case_when(tax == "factor(tax)50" ~ 1 + shift,
                             tax == "factor(tax)100" ~ 2 + shift,
                             tax == "factor(tax)500" ~ 3 + shift))
         # taxyear = case_when(tax == "factor(tax)0" ~ minyear + (1885-minyear)/2,
         #                     tax == "factor(tax)50" ~ 1885 + (1900-1885)/2,
         #                     tax == "factor(tax)100" ~ 1900 + (1903-1900)/2,
         #                     tax == "factor(tax)500" ~ 1903 + (maxyear-1903)/2))

ggplot(graph_countries %>% filter(country != "China" & country != "Japan" & country != "India"), aes(x = taxyear, y = est_scale, group = country, color = 'other')) + geom_point() +
  geom_errorbar(aes(min = est_lb, max = est_ub), width = 0.1, color = "grey") +
  geom_point(data = graph_countries %>% filter(country == "Japan"), aes(x = taxyear, y = est_scale, color = "japan")) +
  geom_errorbar(data = graph_countries %>% filter(country == "Japan"), aes(min = est_lb, max = est_ub), width = 0.1, color = "orange") +
  geom_point(data = graph_countries %>% filter(country == "India"), aes(x = taxyear, y = est_scale, color = "india")) +
  geom_errorbar(data = graph_countries %>% filter(country == "India"), aes(min = est_lb, max = est_ub), width = 0.1, color = "green") +
  geom_point(data = graph_countries %>% filter(country == "China"), aes(x = taxyear, y = est_scale, color = 'china')) +
  geom_errorbar(data = graph_countries %>% filter(country == "China"), aes(min = est_lb, max = est_ub), width = 0.1, color = "red") + 
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"="$50 Head Tax","2"="$100 Head Tax","3"="$500 Head Tax")) +
  labs(y = "Coefficient (Scaled by Mean Imm. Inflow)") + 
  scale_color_manual(name = "Country", breaks = c('other', 'china','japan','india'),
                     values = c('other' = 'grey', 'china'='red','japan'='orange','india'='green')) +
  ggtitle("Inflow Regression Coefficients by Country (1880-1910)")

#ggsave(glue("{git}/figs/8aug23/reg_coefs.png"), height = 4, width = 7)
ggsave(glue("{git}/figs/6sep23/reg_coefs.png"), height = 4, width = 7)

########################################################################
### TABLE 3: DIFF IN DIFF REGRESSION OF CHI IMM VS OTHER IMM AT EACH 'EVENT'
########################################################################
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM >= 1880 & YRIMM <= 1910) %>% #only keeping years with earnings/yrimm data
  mutate(YEARSAFTER1890 = YRIMM - 1890,
         BORNCHI = ifelse(BPL == "China", 1, 0),
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 50, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 100, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 500, 1, 0),
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
  mutate(BORNCHI_tax50 = BORNCHI*ifelse(tax == 50, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 100, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 500, 1, 0),
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



