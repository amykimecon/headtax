########################################################################
### FILE DESCRIPTION: Initial look at data
### PRIMARY OBJECTIVE: 
### CREATED BY: Amy Kim
### CREATED ON: Aug 7 2022 
### LAST MODIFIED: Oct 10 2022
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)
library(xtable)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

########################################################################
### IMPORTING DATA
########################################################################
## chinese registry -- immigration data through ports
# hannah's excel sheet
chireg <- read_csv(glue("{dbox}/cleaned/chireg.csv"))
                   
## census data
clean_all <- read_csv(glue("{dbox}/cleaned/census_all.csv"), guess_max = 5715448)
clean_imm <- clean_all %>% filter(IMM == 1)
clean_chi <- clean_all %>% filter(BORNCHI == 1)

########################################################################
### SUMMARY STATISTICS
########################################################################
census_summ <- clean_all %>% filter(YEAR > 1900) %>% mutate(AGEATIMM = ifelse(IMM == 1, AGE - (YEAR - YRIMM), NA),
                                                            EARN = ifelse(EARN == 0, NA, EARN)) %>%
  select(c(MALE, AGEATIMM, AGE, MAR, CANREAD, EARN, WEIGHT, IMM, BORNCHI))
summ_stats <- bind_rows(census_summ %>% mutate(group = "All", AGEATIMM = NA), census_summ %>% mutate(group = "Immigrants") %>% filter(IMM == 1)) %>%
  bind_rows(census_summ %>% mutate(group = "Chinese Imm") %>% filter(BORNCHI == 1)) %>%
  bind_rows(chireg %>% mutate(group = "Chinese Registry", AGEATIMM = AGE - (REG_Year - YEAR), WEIGHT = 1) %>% select(c(group, MALE, AGE, AGEATIMM, WEIGHT))) %>% 
  group_by(group) %>% summarize(across(-c(WEIGHT, IMM, BORNCHI), .fns = c(~round(weighted.mean(.x, WEIGHT, na.rm=TRUE), 2), 
                                                                          ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                                             NA,
                                                                                             paste0("(", round(sqrt(wtd.var(.x, WEIGHT, na.rm=TRUE)), 2), ")")))), 
                                OBS = n()) 

summ_stats_out <- t(summ_stats)[,c(1,4,2,3)] %>% as.data.frame()
colnames(summ_stats_out) <- summ_stats_out[1,]
rownames(summ_stats_out) <- c("group", "% Male", "", "Avg. Imm Age", " ", "Avg. Age", "  ", "% Married","   ",  "% Can Read", "     ", "Avg. Earnings", "        ", "Obs")

print(xtable(summ_stats_out[-1,], align = "lcccc", type = "latex"), 
      file = glue("{git}/figs/summstats.tex"), 
      floating = FALSE, NA.string = "-",
      hline.after = c(-1,0,nrow(summ_stats_out)-2,nrow(summ_stats_out)-1))


########################################################################
### IMMIGRATION FLOWS BY YEAR 
########################################################################
chinums <- chireg %>% group_by(YEAR) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE),
                                                   numtaxpayers = sum(ifelse(FEES > 0, 1, 0))) %>%
  mutate(pcttaxpayers = 100*numtaxpayers/n)

headtaxcuts <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("Initial Head Tax", "Incr. to $100", "Incr. to $500", "Total Imm. Ban"))

### FLOW OF IMMIGRANTS BY YEAR OF IMMIGRATION AND DATA SOURCE
yrimm_censusdata <- clean_imm %>% filter(YEAR > 1900) %>% 
  group_by(YRIMM, YEAR) %>% 
  summarize(across(starts_with("BORN"), ~ sum(.x*WEIGHT, na.rm=TRUE))) %>% ungroup() %>%
  filter(YRIMM < YEAR - 1) %>% # drop imm numbers from year & year before that census was taken
  group_by(YRIMM) %>% dplyr::summarize(across(starts_with("BORN"), mean)) %>% ungroup() %>%
  mutate(datasource = "Census (1901-1921 Average)")

# chinese immigrants -- by date
dateimmchi <- ggplot(data = chireg %>% filter(YEAR >= 1880 & YEAR <= 1930), 
                   aes(x = DATE)) + geom_density() +
  geom_vline(xintercept = as.Date("1885-01-01")) + geom_vline(xintercept = as.Date("1900-01-01")) + geom_vline(xintercept = as.Date("1903-01-01")) + geom_vline(xintercept = as.Date("1923-01-01")) +
  labs(x = "Date of Immigration", y = "Density of Chinese Immigrant Inflow")
ggsave(glue("{git}/figs/dateimmchi.png"), dateimmchi, height = 4, width = 6)

geom_vline(mapping = aes(xintercept = vals,
                         colour = Ref),
           data = cuts,
           show.legend = FALSE) +
  geom_text(mapping = aes(x = vals,
                          y = 0,
                          label = Ref,
                          hjust = -1,
                          vjust = -1),
            data = cuts)
# chinese immigrants -- by year (census + registry)
yrimmchi <- ggplot(data = yrimm_censusdata %>% select(c(YRIMM, BORNCHI, datasource)) %>%
                  rbind(chinums %>% mutate(YRIMM = YEAR, BORNCHI = n, datasource = "Chinese Registry") %>% select(c(YRIMM, BORNCHI, datasource))) %>%
                  filter(YRIMM >= 1880 & YRIMM <= 1930), 
                aes(x = YRIMM, y = BORNCHI, color = datasource)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", color = "Data Source") + theme(legend.position='bottom')

ggsave(glue("{git}/figs/yrimmchi.png"), yrimmchi, height = 4, width = 6)

# immigrants of various countries
yrimmall <- ggplot(data = yrimm_censusdata %>% filter(YRIMM >= 1880 & YRIMM <= 1930) %>%
                     pivot_longer(starts_with("BORN"), names_to = "BPL", names_prefix = "BORN", values_to = "NUM") %>%
                     filter(BPL != "RUS", BPL != "IND", BPL != "GER"), 
                   aes(x = YRIMM, y = NUM, color = BPL)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 5000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.5) +
  labs(x = "Year of Immigration", y = "Inflow of Immigrants (Census Avg)", color = "Birth Country") + theme(legend.position='bottom') + guides(color = guide_legend(nrow = 1))

ggsave(glue("{git}/figs/yrimmall.png"), yrimmall, height = 4, width = 6)

### TAXPAYERS AND TAXES PAID BY YEAR OF IMMIGRATION
taxbyyear <- ggplot(chinums %>% filter(YEAR <= 1930), aes(x=YEAR)) +
  geom_line(aes(y=tax), color="dark grey", size = 2) +
  geom_line(aes(y=pcttaxpayers*5), color="blue") + 
  scale_y_continuous(
    name = "Average Tax Paid Among Tax Payers",
    sec.axis = sec_axis(~./5, name="Tax Payers as % of Arrivals")
  ) + xlab("Year of Immigration") +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923) +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue")
  )
ggsave(glue("{git}/figs/taxbyyear.png"), taxbyyear, height = 4, width = 6)

########################################################################
### DECOMPOSITION OF CHINESE IMMIGRANT INFLOW
########################################################################
chiocc <- ggplot(chireg %>% filter(AGE >= 18 & SEX == "Male") %>% group_by(OCCGRP, YEAR) %>% summarize(n=n()) %>% 
                   group_by(YEAR) %>% mutate(pct = n/sum(n)) %>% filter(YEAR >= 1880 & YEAR <= 1930), 
       aes(x = YEAR, y = n, fill = OCCGRP)) + geom_area() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 6000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.5) +
  labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow", fill = "Occupation Group") + theme(legend.position='bottom') 
ggsave(glue("{git}/figs/chiocc.png"), chiocc, height = 4, width = 6)

chiorig <- ggplot(chireg %>% group_by(COUNTYGRP, YEAR) %>% summarize(n=n()) %>%
         group_by(YEAR) %>% mutate(pct = n/sum(n)) %>% filter(YEAR >= 1880 & YEAR <= 1930), aes(x = YEAR, y = pct, fill = COUNTYGRP)) + geom_area()+ 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  labs(x = "Year of Immigration", y = "Fraction of Chinese Immigrants", fill = "County of Origin") + theme(legend.position='bottom') + guides(color = guide_legend(nrow = 1))
ggsave(glue("{git}/figs/chiorig.png"), chiorig, height = 4, width = 6)


########################################################################
### AGGREGATION & OUTPUTS -- OLD
########################################################################
summ_stats_all <- clean_all %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())

summ_stats_imm <- clean_imm %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())

summ_stats_chi <- clean_chi %>% group_by(1) %>%
  dplyr::summarize(across(c(MALE, MAR, CANREAD, AGE, EARN, YRIMM), .fns = c(~ weighted.mean(x = .x, w = WEIGHT, na.rm=TRUE), ~ sqrt(wtd.var(x = .x, weights = WEIGHT, na.rm=TRUE)))),
                   n = n())


grouped_years <- clean_all %>% group_by(YEAR) %>% filter(IMM == 1) %>%
  dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIPCT = CHIPOP/IMMPOP,
            RUSPOP = sum(BPLRUS * WEIGHT, na.rm=TRUE), RUSPCT = RUSPOP/IMMPOP,
            FRAPOP = sum(BPLFRA * WEIGHT, na.rm=TRUE), FRAPCT = FRAPOP/IMMPOP,
            GERPOP = sum(BPLGER * WEIGHT, na.rm=TRUE), GERPCT = GERPOP/IMMPOP,
            JAPPOP = sum(BPLJAP * WEIGHT, na.rm=TRUE), JAPPCT = JAPPOP/IMMPOP,
            INDPOP = sum(BPLIND * WEIGHT, na.rm=TRUE), INDPCT = INDPOP/IMMPOP,
            CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
            EARNRATIO = CHIEARN/IMMEARN) %>%
  pivot_longer(ends_with("PCT"), names_to = "IMMGRP", names_pattern = "([A-Z]{3})PCT$", values_to = "PCT") %>% 
  filter(IMMGRP != "GER" & IMMGRP != "RUS")

immigpct <- ggplot(data = grouped_years, aes(x = YEAR, y = PCT * 100, color = IMMGRP)) + geom_line() +
  labs(x = "Year", y = "Immigrant Group as % of Total Immigrants", color = "Birthplace")
ggsave(glue("{git}/figs/immigpct.png"),immigpct, height = 4, width = 6)

occ_years <- clean_all %>% group_by(YEAR, OCCGRP) %>% filter(IMM == 1 & MALE == 1 & AGE >= 18) %>%
  dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE)) %>%
  filter(!is.na(OCCGRP)) %>%
  group_by(YEAR) %>% mutate(UNSKILLEDCHI = CHIPOP/sum(CHIPOP), UNSKILLEDIMM = IMMPOP/sum(IMMPOP)) %>% filter(OCCGRP != "Skilled") %>%
  ungroup() %>%
  pivot_longer(c(UNSKILLEDCHI, UNSKILLEDIMM), names_to = "IMMGRP", names_prefix = "UNSKILLED", values_to = "UNSKILLED") %>%
  mutate(IMMGRP = case_when(IMMGRP == "CHI" ~ "Chinese Immigrants",
                            IMMGRP == "IMM" ~ "All Immigrants"))

# %>% pivot_wider(names_from = YEAR, values_from = c(CHIPOP, IMMPOP))

immigoccs <- ggplot(occ_years, aes(x = YEAR, y = UNSKILLED*100, color = IMMGRP)) + geom_line() + labs(x = "Year", y = "% of Group Unskilled", color = "Group")
ggsave(glue("{git}/figs/immigocs.png"),immigoccs, height = 4, width = 6)

# immigrant inflows
grouped_years_imm <- clean_all %>% filter(YEAR > 1900 & IMM == 1) %>%
  group_by(YEAR, YRIMM) %>% dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIIMMPCT = CHIPOP/IMMPOP,
                                      CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
                                      EARNRATIO = CHIEARN/IMMEARN) %>% select(c(YEAR, YRIMM, CHIPOP)) %>% filter(YEAR > 1900) %>%
  mutate(YEAR = as.character(YEAR)) %>%
  rbind(chinums %>% filter(YEAR != 0) %>% 
          mutate(YRIMM = YEAR, CHIPOP = n, YEAR = "Chinese Registry FULL") %>%
          select(YRIMM, CHIPOP, YEAR))

yrimmpct <- ggplot(data = filter(grouped_years_imm, YRIMM >= 1880 & YRIMM <= 1930), aes(x = YRIMM, y = CHIPOP, color = factor(YEAR))) + geom_line() +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1924) +
  labs(x = "Year of Immigration", y = "Chinese Immigrants as % of Total Immigrants", color = "Census Year")
ggsave(glue("{git}/figs/yrimmpct.png"), yrimmpct, height = 4, width = 6)



grouped_years_earn <- clean_all %>% filter(YEAR > 1900 & IMM == 1, AGE >= 18, MALE == 1 & EARN > 0) %>%
  group_by(YEAR, YRIMM) %>% dplyr::summarize(CHIPOP = sum(BPLCHI * WEIGHT, na.rm=TRUE), IMMPOP = sum(WEIGHT, na.rm=TRUE), CHIIMMPCT = CHIPOP/IMMPOP,
                                             CHIEARN = weighted.mean(EARN, WEIGHT*BPLCHI, na.rm=TRUE), IMMEARN = weighted.mean(EARN, WEIGHT, na.rm=TRUE),
                                             EARNRATIO = CHIEARN/IMMEARN)
ggplot(data = filter(grouped_years_imm, YRIMM > 1880), aes(x = YRIMM, y = CHIEARN, color = factor(YEAR))) + geom_line()


ggplot(data = grouped_years, aes(x = YEAR)) + geom_line(aes(y = CHIEARN))

ggplot(data = grouped_years, aes(x = YEAR)) + geom_line(aes(y = CHIPOP*50), color = "Red") + geom_line(aes(x = YEAR, y = IMMPOP), color = "Blue")


#### TODO:
# -benchmark BPLCHI totals with census tabs if available

# 
histcolsnum <- str_count(readLines(glue("{dbox}/ipums_historical.csv"), n = 1), ",") + 1
histraw <- read_csv(glue("{dbox}/ipums_historical.csv"),
                    col_types = paste0(rep("d", histcolsnum), collapse=""))
# 
# contempcolsnum <- str_count(readLines(glue("{dbox}/ipums_contemp.csv"), n = 1), ",") + 1
# contempraw <- read_csv(glue("{dbox}/ipums_contemp.csv"),
#                     col_types = paste0(rep("d", contempcolsnum), collapse=""))
# 
# ########################################################################
# ### HISTORICAL DATA
# ########################################################################
histclean <- histraw %>% 
  mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
                             COUNTRY == 124 ~ "Canada"),
         BPLCAT = case_when(BPLCOUNTRY == 24020 ~ "Canada",
                            BPLCOUNTRY == 24040 ~ "US",
                            BPLCOUNTRY >= 10000 & BPLCOUNTRY < 20000 ~ "Africa",
                            BPLCOUNTRY >= 20000 & BPLCOUNTRY < 30000 ~ "Americas",
                            BPLCOUNTRY >= 30000 & BPLCOUNTRY < 40000 ~ "Asia",
                            BPLCOUNTRY >= 40000 & BPLCOUNTRY < 50000 ~ "Europe",
                            TRUE ~ "Other/Unknown"),
         ASIAN = ifelse(((RACE >= 40 & RACE < 50) | (ORIGIN >= 8400 & ORIGIN < 8500)), 1, 0),
         BPLCHINA = ifelse((BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020), 1, 0),
         CHINESE = ifelse((RACE == 41 | ORIGIN == 8410), 1, 0),
         IMM = ifelse((COUNTRY == "Canada" & BPLCOUNTRY != 24020) | (COUNTRY == "US" & BPLCOUNTRY != 24040), 1, 0),
         OCCGRP = case_when(OCCISCO >= 6 & OCCISCO <= 9 ~ "Unskilled/Farm",
                            OCCISCO <= 5 ~ "Skilled",
                            TRUE ~ NA_character_))

histgroup <- histclean %>% group_by(YEAR, COUNTRY) %>%
  mutate(POP = sum(PERWT),
         IMMPOP = sum(IMM * PERWT, na.rm=TRUE),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINAPCT = sum(BPLCHINA*PERWT, na.rm=TRUE)/IMMPOP) %>%
  dplyr::summarize(across(c(CHINESEPCT, BPLCHINAPCT), mean))

histgroupocc <- histclean %>% filter(!is.na(OCCGRP)) %>%
  group_by(YEAR, COUNTRY, OCCGRP) %>%
  mutate(POP = sum(PERWT),
         IMMPOP = sum(IMM * PERWT, na.rm=TRUE),
         CHINESEPCT = sum(CHINESE*PERWT, na.rm=TRUE)/POP,
         BPLCHINA = sum(BPLCHINA*PERWT, na.rm=TRUE),
         BPLCHINAPCT = BPLCHINA/IMMPOP) %>%
  dplyr::summarize(across(c(CHINESEPCT, BPLCHINAPCT, BPLCHINA, IMMPOP), mean))

ipums_chinapop <- ggplot(histgroup, aes(x = YEAR, y = BPLCHINAPCT, color = factor(COUNTRY)))+ geom_line()
ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINAPCT, color = factor(OCCGRP)))+ geom_line()
ggplot(histgroupocc, aes(x = YEAR, y = BPLCHINA, color = factor(OCCGRP)))+ geom_line()

# ########################################################################
# ### CONTEMP. DATA
# ########################################################################
# contempclean <- contempraw %>% 
#   mutate(COUNTRY = case_when(COUNTRY == 840 ~ "US",
#                              COUNTRY == 124 ~ "Canada"),
#          IMM = ifelse(is.na(YRIMM) | YRIMM == 0, 0, 1), #dummy for whether the person is an immigrant
#          BPLCHINA = case_when(BPLCOUNTRY >= 31010 & BPLCOUNTRY < 31020 ~ 1,
#                               TRUE ~ 0),
#          CHINESE = case_when(RACE == 41 | ETHNICCA == 17 ~ 1,
#                              TRUE ~ 0),
#          ASIAN = case_when((RACE >= 40 & RACE < 50) | (ETHNICCA == 16 | ETHNICCA <= 18) ~ 1, 
#                            TRUE ~ 0),
#          WHITE = case_when(RACE == 10 | ETHNICCA < 14 ~ 1, 
#                            TRUE ~ 0),
#          EDUC = case_when(EDATTAIN == 4 ~ "Univ +",
#                           EDATTAIN == 3 ~ "High School",
#                           EDATTAIN == 1 | EDATTAIN == 2 ~ "< High School",
#                           TRUE ~ "Unknown/NIU"),
#          INCWAGE = ifelse(INCWAGE < 9999998, INCWAGE, NA),
#          INCTOT = ifelse(INCTOT < 9999998, INCTOT, NA),
#          RACE = case_when(ASIAN == 1 ~ "Asian",
#                           WHITE == 1 ~ "White",
#                           TRUE ~ "Unknown"))
# 
# contempincwage <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
#   filter(EMPSTAT == 1) %>%
#   group_by(YEAR, COUNTRY, RACE) %>%
#   dplyr::summarize(INCTOT = weighted.mean(INCTOT, PERWT, na.rm=TRUE))
# 
# contempincwageeduc <- contempclean %>% filter(ASIAN == 1|WHITE == 1) %>% 
#   filter(EMPSTAT == 1) %>%
#   mutate(ASIAN = ifelse(is.na(ASIAN), 0, ASIAN),
#          WHITE = ifelse(is.na(WHITE), 0, WHITE),
#          RACE = case_when(ASIAN == 1 ~ "Asian",
#                           WHITE == 1 ~ "White",
#                           TRUE ~ "Unknown")) %>%
#   group_by(YEAR, COUNTRY, RACE) %>%
#   dplyr::summarize(INCWAGE = weighted.mean(INCWAGE, PERWT, na.rm=TRUE))
# 
# # inc over time
# ggplot(contempincwage, aes(x = YEAR, y = INCTOT, color = RACE)) + geom_line() + facet_wrap(~COUNTRY)
# 
# # inc distribution in 2000/2001
# ggplot(contempincwage %>% filter(YEAR == 2000 | YEAR == 2001), 
#        aes(x = INCWAGE, fill = RACE)) + geom_density(weight = PERWT, alpha = 0.5) + facet_wrap(~COUNTRY)


