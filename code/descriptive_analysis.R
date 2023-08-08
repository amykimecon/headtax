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
us_imm <- read_csv(glue("{dbox}/cleaned/us_clean_imm.csv")) %>% mutate(source = "US Census", group = "All Immigrants", WEIGHT = 1)
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
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c1,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow.png"), height = 4, width = 7)

## aug 8 slides
yrimm_compare <- rbind(yrimm_census %>% ungroup() %>% select(-c(YEAR, tax)) %>% mutate(source = "Census"),
                       immflow_nber %>% select(-YEAR) %>% rename(`Austria and Hungary` = Austria) %>%
                         pivot_longer(-YRIMM, names_to = "BPL", values_to = "FLOW") %>% 
                         mutate(source = "NBER"))

bpllist = c("Austria and Hungary", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Iceland", "India",
            "Italy", "Japan", "Netherlands", "Norway", "Poland", "Romania", "Russia", "Sweden", "Switzerland", "China")
bpllist_small = c("Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "India",
            "Japan", "Netherlands", "Norway", "Romania", "Sweden")

ggplot(data = yrimm_compare %>% filter(YRIMM > 1890) %>% filter(BPL %in% bpllist_small),
       aes(x = YRIMM, y = FLOW, color = source)) + geom_line() + facet_wrap(~BPL)

ggsave(glue("{git}/figs/8aug23/nber_census_compare_all.png"), height = 4, width = 7)

ggplot(data = yrimm_compare %>% filter(YRIMM > 1890) %>% filter(BPL == "Belgium"),
       aes(x = YRIMM, y = FLOW, color = source)) + geom_line() + facet_wrap(~BPL)

ggsave(glue("{git}/figs/8aug23/nber_census_compare_belgium.png"), height = 4, width = 7)

ggplot(data = yrimm_compare %>% filter(YRIMM > 1890) %>% filter(BPL == "Japan"),
       aes(x = YRIMM, y = FLOW, color = source)) + geom_line() + facet_wrap(~BPL)

ggsave(glue("{git}/figs/8aug23/nber_census_compare_japan.png"), height = 4, width = 7)

ggplot(data = yrimm_compare %>% filter(YRIMM > 1890) %>% filter(BPL == "Germany"),
       aes(x = YRIMM, y = FLOW, color = source)) + geom_line() + facet_wrap(~BPL)

ggsave(glue("{git}/figs/8aug23/nber_census_compare_germany.png"), height = 4, width = 7)

########################################################################
### TABLE 2: REGRESSION OF IMM INFLOWS ON TAX RAISES
########################################################################
# combining register & census (wide), adding historical GNP and official total immigration numbers
yrimm_flow_regress <- yrimm_reg %>%
  left_join(canhistmacro %>% select(c(Year, RGNP)), by = c("YRIMM" = "Year")) %>%
  left_join(immflow, by = c("YRIMM" = "Year")) %>% 
  left_join(yrimm_census %>% 
              pivot_wider(id_cols = c(YRIMM, tax), names_from = BPL, names_prefix = "FLOW_",
                          values_from = FLOW), by = c("YRIMM", "tax")) %>%
  left_join(can_imm %>% group_by(YRIMM) %>% summarize(FLOW_All = sum(WEIGHT))) %>% ungroup() %>%
  arrange(YRIMM) %>% mutate(lagRGNP = dplyr::lag(RGNP),
                            t = YRIMM - 1885,
                            GNP_GROWTH = (RGNP - lagRGNP)/lagRGNP) %>%
  filter(YRIMM <= 1910 & YRIMM >= 1880)

names(yrimm_flow_regress) <- make.names(names(yrimm_flow_regress))

## base model: includes quadratic time trend, gnp growth, census imm reg uses total census imm

## checking if flows change with year -- register
yrimm_flow1 <- lm(data = yrimm_flow_regress %>% filter(YRIMM > 1885), CHIFLOW_REGISTER ~ factor(tax) + t + I(t^2) + GNP_GROWTH + IMMFLOW_NATL)

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
  reg <- lm(data = yrimm_flow_regress, glue("FLOW_{country} ~ factor(tax) + t + I(t^2) + GNP_GROWTH + FLOW_All"))
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
  mutate(est_scale = Estimate/meanimm,
         stderr_scale = `Std. Error`/meanimm,
         est_lb = est_scale - 1.96*stderr_scale,
         est_ub = est_scale + 1.96*stderr_scale,
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

ggsave(glue("{git}/figs/8aug23/reg_coefs.png"), height = 4, width = 7)

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


stargazer(did_reg_labor, did_reg_labor1, did_reg_labor2, did_reg_labor_jap, did_reg_labor_jap1, did_reg_labor_jap2,
          out = glue("{git}/figs/21jul23/outcome_regs_labor.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          column.labels = c("All (1890-1920)", "All (1870-1920)", "All (All Census Yrs)", "Japan. (1890-1908)", "Japan. (1870-1908)", "Japan. (All Census Yrs)"),
          column.separate = c(1,1,1,1,1,1), keep = c("BORNCHI*"),
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$50 Tax", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "=c#-ta-s-")



## register data
did_data_reg <- reg_chi %>% filter(YRIMM >= 1880 & YRIMM <= 1910) %>%
  mutate(LABOR = ifelse(PROFESSION == "Labourer", 1, 0)) %>%
           filter(AGE >= 18 & MALE == 1)

ggplot(did_data_reg %>% group_by(YRIMM) %>% summarize(LABORPCT = mean(LABOR, na.rm=TRUE)), 
       aes(x = YRIMM, y = LABORPCT)) + geom_line()

ggplot(did_data %>%   filter((YEAR == 1901 & YRIMM < 1901) | 
                               (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | 
                               (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
       group_by(YRIMM) %>% summarize(LABORPCT = mean(LABOR, na.rm=TRUE)), 
       aes(x = YRIMM, y = LABORPCT)) + geom_line()

did_reg_labor <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)








### CHECKING FOR LONG-TERM TRENDS B/W CENSUS YEARS
compare_census <- did_data %>% group_by(YRIMM,YEAR) %>%
  summarize(across(c(LABOR, CANREAD, EARN, HOUSEOWN), 
                   list(function(.x) weighted.mean(.x, WEIGHT, na.rm=TRUE), 
                        function(.x) weighted.mean(ifelse(BORNCHI == 1, .x, NA), WEIGHT, na.rm=TRUE))))

ggplot(compare_census %>% filter(YRIMM > 1880), aes(x = YRIMM, y = LABOR_2, color = factor(YEAR))) + geom_smooth()
ggplot(compare_census %>% filter(YRIMM > 1880), aes(x = YRIMM, y = CANREAD_2, color = factor(YEAR))) + geom_smooth()
ggplot(compare_census %>% filter(YRIMM > 1880), aes(x = YRIMM, y = EARN_2, color = factor(YEAR))) + geom_smooth()
ggplot(compare_census %>% filter(YRIMM > 1880), aes(x = YRIMM, y = HOUSEOWN_2, color = factor(YEAR))) + geom_smooth()


# run regressions
did_reg_labor <- lm(LABOR ~ AGE + factor(YEAR) + factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500 - 1, data = did_data, weights = WEIGHT)
did_reg_labor_jap <- lm(LABOR ~ AGE + factor(YEAR) + factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)

did_reg_canread <- lm(CANREAD ~ AGE + factor(YEAR) + factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_canread_jap <- lm(CANREAD ~ AGE + factor(YEAR) + factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)

summary(lm(CANREAD ~ AGE + factor(YEAR) + factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT))







did_reg_earn <- lm(EARN ~ AGE + factor(YEAR) + factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_earn_jap <- lm(EARN ~ AGE + factor(YEAR) + factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)

did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_houseown_jap <- lm(HOUSEOWN ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)

stargazer(did_reg_labor, did_reg_canread, did_reg_earn, did_reg_houseown, did_reg_labor_jap, did_reg_canread_jap, did_reg_earn_jap, did_reg_houseown_jap,
          out = glue("{git}/figs/outcome_regs.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          column.labels = c("Sample: All Immigrants", "Sample: Chinese and Japanese Immigrants"),
          column.separate = c(4,4),
          dep.var.labels = c("$LABORER$", "$LITERATE$", "$EARNINGS$", "$HOMEOWN$","$LABORER$","$LITERATE$", "$EARNINGS$", "$HOMEOWN$"),
          keep = c(31,32,33),
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Includes Year FE", rep("Yes", 8))),
          table.layout = "=cld#-ta-s-")

stargazer(did_reg_labor, did_reg_canread, did_reg_earn, did_reg_houseown, did_reg_labor_match, did_reg_canread_match, did_reg_earn_match, did_reg_houseown_match,
          out = glue("{git}/figs/outcome_regs_match.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          column.labels = c("Sample: All Immigrants", "Sample: Chinese and Matched Immigrants"),
          column.separate = c(4,4),
          dep.var.labels = c("$LABORER$", "$LITERATE$", "$EARNINGS$", "$HOMEOWN$","$LABORER$","$LITERATE$", "$EARNINGS$", "$HOMEOWN$"),
          keep = c(31,32,33),
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Includes Year FE", rep("Yes", 8))),
          table.layout = "=cld#-ta-s-")


########################################################################
### FIGURE 3: US VS CANADA
########################################################################
yrimm_us <- us_imm %>% filter(BORNJAP == 1 | BORNCHI == 1) %>% group_by(YEAR, YRIMM) %>% summarize(ChinesePOP = sum(BORNCHI), JapanesePOP = sum(BORNJAP)) %>% mutate(source = "US Census", tax = case_when(YRIMM <= 1885 ~ 0,
                                                                                                                                  YRIMM <= 1900 ~ 50,
                                                                                                                                  YRIMM <= 1903 ~ 100,
                                                                                                                                  YRIMM < 1924 ~ 500)) %>% 
  arrange(YRIMM) %>%
  filter((YEAR == 1900 & YRIMM < 1900) | (YEAR == 1910 & YRIMM < 1910 & YRIMM >= 1900) | (YEAR == 1920 & YRIMM < 1920 & YRIMM >= 1910)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  pivot_longer(c(ChinesePOP, JapanesePOP), names_to = "IMM", names_pattern = "(.*)POP", values_to = "POP")

yrimm_census_jap <- can_imm %>% filter(BORNJAP == 1) %>% group_by(YEAR, YRIMM) %>% summarize(POP = sum(WEIGHT)) %>% mutate(source = "CA Census", tax = case_when(YRIMM <= 1885 ~ 0,
                                                                                                                                                           YRIMM <= 1900 ~ 50,
                                                                                                                                                           YRIMM <= 1903 ~ 100,
                                                                                                                                                           YRIMM < 1924 ~ 500),
                                                                                                                              IMM = "Japanese") %>% 
  arrange(YRIMM) %>%
  filter((YEAR == 1901 & YRIMM < 1901) | (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) # only taking YRIMM from most recent census (lowest rate of loss to outmigration)

yrimm_us_can <- bind_rows(yrimm_us, yrimm_census %>% mutate(IMM = "Chinese", POP = CHIPOP)) %>% bind_rows(yrimm_census_jap)

fig3_us_can <- ggplot(data = yrimm_us_can %>% filter(YRIMM >= 1870), aes(x = YRIMM, y = POP, color = source, linetype = IMM)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE) +
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
  geom_text(aes(x = x, y = 7000, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
  #scale_color_manual(breaks=c("CA Census", "US Census"), values=c("black","blue")) +
  scale_linetype_manual(breaks=c("Chinese", "Japanese"), values=c(1,3)) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", linetype = "Data Source", color = "Immigrant Group") + theme_minimal() + theme(legend.position='bottom')


ggsave(glue("{git}/figs/fig3_us_can.png"), fig3_us_can, height = 5, width = 9)


########################################################################
### TABLE 4: US VS CAN REGRESSIONS
########################################################################
did_data_us <- us_imm %>% filter(BORNCHI == 1 | BORNJAP == 1) %>% filter(YEAR >= 1900 & YRIMM > 1890) %>% #only keeping years with earnings/yrimm data
  mutate(tax = case_when(YRIMM < 1885 ~ 0,
                         YRIMM < 1900 ~ 50,
                         YRIMM < 1903 ~ 100,
                         YRIMM <= 1924 ~ 500),
         YEARSAFTER1890 = YRIMM - 1890,
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 50, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 100, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 500, 1, 0),
         YEARSSINCEIMM = YEAR - YRIMM) %>%
  filter(AGE >= 18 & MALE == 1) %>% #only looking at men over 18
  filter((YEAR == 1900 & YRIMM < 1900) | (YEAR == 1910 & YRIMM < 1910 & YRIMM >= 1900) | (YEAR == 1920 & YRIMM < 1920 & YRIMM >= 1910)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  mutate(CAN = 0, WEIGHT = 1) 

did_data_us_jap <- did_data_us %>% filter(BORNCHI == 1 | BORNJAP == 1) 

did_data_us_can <- did_data_us %>% bind_rows(did_data %>% filter(YRIMM < 1920 & (BORNCHI == 1 | BORNJAP == 1)) %>% mutate(CAN = 1))

# run regressions -- did with just us data
did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us, weights = WEIGHT)
did_reg_labor_us_jap <- lm(LABOR ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us_jap, weights = WEIGHT)
did_reg_canread_us <- lm(CANREAD ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us, weights = WEIGHT)
did_reg_canread_us_jap <- lm(CANREAD ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us_jap, weights = WEIGHT)
did_reg_earn_us <- lm(EARN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us, weights = WEIGHT)
did_reg_earn_us_jap <- lm(EARN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us_jap, weights = WEIGHT)
did_reg_houseown_us <- lm(HOUSEOWN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us, weights = WEIGHT)
did_reg_houseown_us_jap <- lm(HOUSEOWN ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us_jap, weights = WEIGHT)

stargazer(did_reg_labor_us, did_reg_canread_us, did_reg_earn_us, did_reg_houseown_us, did_reg_labor_us_jap, did_reg_canread_us_jap, did_reg_earn_us_jap, did_reg_houseown_us_jap,
          out = glue("{git}/figs/outcome_regs_us.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          column.labels = c("Sample: All Immigrants", "Sample: Chinese and Japanese Immigrants"),
          column.separate = c(4,4),
          dep.var.labels = c("$LABORER$", "$LITERATE$", "$EARNINGS$", "$HOMEOWN$","$LABORER$","$LITERATE$", "$EARNINGS$", "$HOMEOWN$"),
          keep = c(31,32,33),
          covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Includes Year FE", rep("Yes", 8))),
          table.layout = "=cld#-ta-s-")


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



