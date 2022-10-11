########################################################################
### FILE DESCRIPTION: Descriptive graphs & tables 
### PRIMARY OBJECTIVE: Initial look at trends & patterns in data, comparing various data sources
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

ca <- "#F8766D"
us <- "#00BFC4"

########################################################################
### IMPORTING DATA
########################################################################
## chinese registry -- immigration data through ports
# hannah's excel sheet
chireg <- read_csv(glue("{dbox}/cleaned/chireg.csv"))

## census data
clean_all <- read_csv(glue("{dbox}/cleaned/census_all.csv"), guess_max = 5715448)
clean_chi <- clean_all %>% filter(BORNCHI == 1)

## us census data
us_clean_yrimm <- read_csv(glue("{dbox}/cleaned/us_clean_yrimm.csv"))
us_clean_chi <- read_csv(glue("{dbox}/cleaned/us_clean_chi.csv"))

########################################################################
### CLEANING DATA FOR ANALYSIS
########################################################################
# grouping chinese immigrants by year of immigration -- FROM REGISTRY DATA
chinums <- chireg %>% group_by(YEAR) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE),
                                                   numtaxpayers = sum(ifelse(FEES > 0, 1, 0))) %>%
  mutate(pcttaxpayers = 100*numtaxpayers/n)

# key head tax years for graphing & labeling vertical lines
headtaxcuts <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("Initial Head Tax", "Incr. to $100", "Incr. to $500", "Total Imm. Ban"))

# grouping immigrants from various birth countries by year of immigration -- FROM CA CENSUS DATA
yrimm_censusdata_means <- clean_imm %>% filter(YEAR > 1900) %>% 
  group_by(YRIMM, YEAR) %>% 
  summarize(across(starts_with("BORN"), ~ sum(.x*WEIGHT, na.rm=TRUE))) %>% ungroup() %>%
  filter(YRIMM < YEAR) %>% # drop imm numbers from year that census was taken
  group_by(YRIMM) %>% dplyr::summarize(across(starts_with("BORN"), mean, na.rm=TRUE)) %>% ungroup() %>%
  mutate(datasource = "Census (1901-1921 Average)")

yrimm_censusdata_maxes <- clean_imm %>% filter(YEAR > 1900) %>% 
  group_by(YRIMM, YEAR) %>% 
  summarize(across(starts_with("BORN"), ~ sum(.x*WEIGHT, na.rm=TRUE)), 
            IMM = sum(IMM*WEIGHT, na.rm=TRUE)) %>% ungroup() %>%
  filter(YRIMM < YEAR) %>% # drop imm numbers from year that census was taken
  group_by(YRIMM) %>% dplyr::summarize(across(starts_with("BORN"), max, na.rm=TRUE), IMM = max(IMM, na.rm=TRUE)) %>% ungroup() %>%
  mutate(datasource = "Census (1901-1921 Max)")
########################################################################
### SUMMARY STATISTICS
########################################################################
census_summ <- clean_all %>% filter(YEAR > 1900) %>% mutate(AGEATIMM = ifelse(IMM == 1, AGE - (YEAR - YRIMM), NA),
                                                            EARN = ifelse(EARN == 0, NA, EARN)) %>%
  select(c(MALE, AGEATIMM, AGE, MAR, CANREAD, EARN, WEIGHT, IMM, BORNCHI))

summ_stats_census <- bind_rows(census_summ %>% mutate(group = "All", AGEATIMM = NA), census_summ %>% mutate(group = "Immigrants") %>% filter(IMM == 1)) %>%
  bind_rows(census_summ %>% mutate(group = "Chinese Imm") %>% filter(BORNCHI == 1)) %>%
  group_by(group) %>% summarize(across(-c(WEIGHT, IMM, BORNCHI), .fns = c(~round(weighted.mean(.x, WEIGHT, na.rm=TRUE), 2), 
                                                                          ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                                  NA,
                                                                                  paste0("(", round(sqrt(wtd.var(.x, WEIGHT, na.rm=TRUE)), 2), ")")))), 
                                OBS = n()) 

summ_stats <- bind_rows(census_summ %>% mutate(group = "CA Census", NUM = 1) %>% filter(BORNCHI == 1), 
                        chireg %>% mutate(group = "Chinese Registry", AGEATIMM = AGE - (REG_Year - YEAR), WEIGHT = 1, NUM = 1) %>% select(c(group, MALE, AGE, AGEATIMM, WEIGHT))) %>%
  bind_rows(us_clean_chi %>% mutate(group = "US Census", WEIGHT = 1)) %>%
  group_by(group) %>% summarize(across(-c(WEIGHT, IMM, BORNCHI), .fns = c(~round(weighted.mean(.x, WEIGHT, na.rm=TRUE), 2), 
                                                                          ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                                  NA,
                                                                                  paste0("(", round(sqrt(wtd.var(.x, WEIGHT, na.rm=TRUE)), 2), ")")))), 
                                OBS = sum(NUM)) 

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
### FLOW OF IMMIGRANTS BY YEAR OF IMMIGRATION AND DATA SOURCE
# chinese immigrants -- by date
dateimmchi <- ggplot(data = chireg %>% filter(YEAR >= 1880 & YEAR <= 1930), 
                     aes(x = DATE)) + geom_density() +
  geom_vline(xintercept = as.Date("1885-01-01")) + geom_vline(xintercept = as.Date("1900-01-01")) + geom_vline(xintercept = as.Date("1903-01-01")) + geom_vline(xintercept = as.Date("1923-01-01")) +
  labs(x = "Date of Immigration", y = "Density of Chinese Immigrant Inflow")
ggsave(glue("{git}/figs/dateimmchi.png"), dateimmchi, height = 4, width = 6)

# chinese immigrants -- by year (census + registry)
yrimmchi_means <- ggplot(data = yrimm_censusdata_means %>% select(c(YRIMM, BORNCHI, datasource)) %>%
                     rbind(chinums %>% mutate(YRIMM = YEAR, BORNCHI = n, datasource = "Chinese Registry") %>% select(c(YRIMM, BORNCHI, datasource))) %>%
                     filter(YRIMM >= 1880 & YRIMM <= 1930), 
                   aes(x = YRIMM, y = BORNCHI, color = datasource)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", color = "Data Source") + theme(legend.position='bottom')

yrimmchi_maxes <- ggplot(data = yrimm_censusdata_maxes %>% select(c(YRIMM, BORNCHI, datasource)) %>%
                          rbind(chinums %>% mutate(YRIMM = YEAR, BORNCHI = n, datasource = "Chinese Registry") %>% select(c(YRIMM, BORNCHI, datasource))) %>%
                          filter(YRIMM >= 1880 & YRIMM <= 1930), 
                        aes(x = YRIMM, y = BORNCHI, color = datasource)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", color = "Data Source") + theme(legend.position='bottom')

ggsave(glue("{git}/figs/yrimmchi_means.png"), yrimmchi_means, height = 4, width = 6)
ggsave(glue("{git}/figs/yrimmchi_maxes.png"), yrimmchi_maxes, height = 4, width = 6)

# immigrants of various countries
yrimmall <- ggplot(data = yrimm_censusdata_maxes %>% filter(YRIMM >= 1880 & YRIMM <= 1930) %>%
                     pivot_longer(starts_with("BORN"), names_to = "BPL", names_prefix = "BORN", values_to = "NUM") %>%
                     filter(BPL != "RUS", BPL != "IND", BPL != "GER"), 
                   aes(x = YRIMM, y = NUM, color = BPL)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 5000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.5, size = 3) +
  labs(x = "Year of Immigration", y = "Inflow of Immigrants (Census Max)", color = "Birth Country") + theme(legend.position='bottom') + guides(color = guide_legend(nrow = 1))

ggsave(glue("{git}/figs/yrimmall.png"), yrimmall, height = 4, width = 6)

### TAXPAYERS AND TAXES PAID BY YEAR OF IMMIGRATION
taxespaid <- ggplot(chinums %>% filter(YEAR <= 1930), aes(x=YEAR, y = tax)) + 
  geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) + 
  geom_text(aes(x = yrs, y = 400, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 2, size = 3) + expand_limits(y = 0) +
  labs(x = "Year of Immigration", y = "Average Tax Paid Among Tax Payers")
ggsave(glue("{git}/figs/taxespaid.png"), taxespaid, height = 4, width = 6)

taxbyyear <- ggplot(chinums %>% filter(YEAR <= 1930), aes(x=YEAR)) +
  geom_line(aes(y=pcttaxpayers), color="#00BA38") +
  geom_line(aes(y=n/90), color=ca) + 
  scale_y_continuous(
    name = "Tax Payers as % of Arrivals",
    sec.axis = sec_axis(~.*90, name="Inflow of Chinese Immigrants")
  ) + xlab("Year of Immigration") +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923) +
  theme(
    axis.title.y = element_text(color = "#00BA38"),
    axis.title.y.right = element_text(color = ca)
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
### COMPARING WITH US DATA
########################################################################
## inflows -- chinese immigrants
yrimmchi_us <- ggplot(data = chinums %>% mutate(YRIMM = YEAR, BORNCHI = n, datasource = "CA Chinese Registry") %>% select(c(YRIMM, BORNCHI, datasource)) %>%
                        rbind(us_clean_yrimm %>% mutate(datasource = "US Census") %>% select(c(YRIMM, BORNCHI, datasource))) %>%
                           filter(YRIMM >= 1870 & YRIMM <= 1925), 
                         aes(x = YRIMM, y = BORNCHI, color = datasource)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "dark red") + 
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
  geom_text(aes(x = x, y = 7000, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", color = "Data Source") + theme(legend.position='bottom')

ggsave(glue("{git}/figs/yrimmchi_us.png"), yrimmchi_us, height = 4, width = 6)

# all immigrants
yrimm_us <- ggplot(data = yrimm_censusdata_maxes %>% mutate(CANIMM = IMM) %>% select(c(CANIMM, YRIMM))%>%
                     full_join(us_clean_yrimm %>% mutate(USIMM = IMM) %>% select(c(USIMM, YRIMM))) %>%
                     filter(YRIMM >= 1880 & YRIMM <= 1930), aes(x = YRIMM)) +
  geom_line(aes(y = CANIMM/1000), color = ca) + 
  geom_line(aes(y = USIMM/10000), color = us) +
  scale_y_continuous(name = "Canadian Immigrant Inflow (Thousands)",
                     sec.axis = sec_axis(~.*10, name = "US Immigrant Inflow (Thousands)")) +
  xlab("Year of Immigration") +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "dark red") + 
  geom_text(aes(x = yrs, y = 150, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
  geom_text(aes(x = x, y = 150, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3) +
  theme(
    axis.title.y = element_text(color = ca),
    axis.title.y.right = element_text(color = us)
  )
ggsave(glue("{git}/figs/yrimm_us.png"), yrimm_us, height = 4, width = 6)
