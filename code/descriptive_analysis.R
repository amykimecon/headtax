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

ca <- "#F8766D"
us <- "#00BFC4"
ht <- "#808080"

########################################################################
### IMPORTING DATA
########################################################################
## chinese registry -- immigration data through ports
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% mutate(source = "xRegister", group = "Chinese Immigrants")

## census data
can_all <- read_csv(glue("{dbox}/cleaned/census_all.csv"), guess_max = 5715448) %>% mutate(group = "All", source = "CA Census")
can_imm <- can_all %>% filter(IMM == 1) %>% mutate(group = "All Immigrants")
can_chi <- can_all %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
can_match <- can_imm %>% filter(matched == 1) %>% mutate(group = "Matched Immigrants")

## us census data
us_all <- read_csv(glue("{dbox}/cleaned/us_clean_matched.csv")) %>% mutate(group = "All", source = "US Census")
us_imm <- us_all %>% filter(IMM == 1) %>% mutate(group = "All Immigrants")
us_chi <- us_all %>% filter(BORNCHI == 1) %>% mutate(group = "Chinese Immigrants")
us_match <- us_imm %>% filter(matched == 1) %>% mutate(group = "Matched Immigrants")

## key head tax years for graphing & labeling vertical lines
headtaxcuts <- data.frame(yrs = c(1885, 1900, 1903, 1923), labs = c("Initial Head Tax", "Incr. to $100", "Incr. to $500", "Total Imm. Ban"))

########################################################################
### QUICK ACCOUNTING EXERICSE: OUTMIGRATION IN CANADA
########################################################################
chipop_census <- can_chi %>% group_by(YEAR) %>% summarize(pop = sum(WEIGHT))
chipop_reg <- reg_chi %>% filter(YEAR_ARRIV < 1921 & YEAR_ARRIV >= 1881) %>%
  mutate(YEAR_GRP = case_when(YEAR_ARRIV < 1891 ~ "1881-1891",
                                                      YEAR_ARRIV < 1901 ~ "1891-1901",
                                                      YEAR_ARRIV < 1911 ~ "1901-1911",
                                                      YEAR_ARRIV < 1921 ~ "1911-1921")) %>%
  group_by(YEAR_GRP) %>% summarize(inflow = n())

chipop_grp <- chipop_reg %>% mutate(CENSUSDIFF = diff(chipop_census$pop),
                                    outflow = inflow - CENSUSDIFF)
########################################################################
### TABLE 1: SUMMARY STATS BY DATA SOURCE & GROUP (1900-1920)
########################################################################
summ_vars <- c("MALE", "MAR", "AGE", "CANREAD", "LABOR", "YRIMM")

# helper function to calculate weighted standard errors 
wtd_se <- function(x, w){
  i <- !is.na(x)
  w <- w[i]
  x <- x[i]
  n_eff = (sum(w)**2)/sum(w**2)
  return(sqrt(wtd.var(x, w)/(n_eff)))
}

# us -- entire population
us_summ <- us_all %>% mutate(YRIMM = NA, across(c(LABOR, MAR, CANREAD), ~ ifelse(AGE >= 18, .x, NA))) %>% #restricting to only count those over 18 for these variables
  filter(YEAR >= 1900) %>% select(c(source, group, all_of(summ_vars))) %>% #only keeping census years 1900-1920
  group_by(source, group) %>% summarize(across(everything(), .fns = c(~round(mean(.x, na.rm=TRUE), 2), #rounded mean
                                                                    ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                            NA,
                                                                            sd(.x, na.rm=TRUE)/sqrt(length(!is.na(.x)))))), #rounded sd
                                        OBS = n()) #n obs

# canada -- entire population
can_summ <- can_all %>% mutate(YRIMM = NA, across(c(LABOR, MAR, CANREAD), ~ ifelse(AGE >= 18, .x, NA))) %>% #restricting to only count those over 18 for these variables
  filter(YEAR >= 1900) %>% select(c(source, group, all_of(summ_vars), WEIGHT)) %>% #only keeping census years 1901-1921
  group_by(source, group) %>% summarize(across(-c(WEIGHT), .fns = c(~round(weighted.mean(.x, WEIGHT, na.rm=TRUE), 2), #weighted mean
                                                                    ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                            NA,
                                                                            wtd_se(.x, WEIGHT)))), #weighted sd
                                        OBS = n()) #n obs

# us -- all immigrants
us_imm_summ <- us_imm %>% mutate(across(c(LABOR, MAR, CANREAD), ~ ifelse(AGE >= 18, .x, NA))) %>% filter(YEAR >= 1900) %>% select(c(source, group, all_of(summ_vars))) %>%
  group_by(source, group) %>% summarize(across(everything(), .fns = c(~round(mean(.x, na.rm=TRUE), 2), # mean
                                                                      ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                              NA,
                                                                              sd(.x, na.rm=TRUE)/sqrt(length(!is.na(.x)))))), # sd
                                        OBS = n())

# us -- matched immigrants
us_match_summ <- us_match %>% filter(YRIMM >= 1885 & YRIMM <= 1920) %>% mutate(across(c(LABOR, MAR, CANREAD), ~ ifelse(AGE >= 18, .x, NA))) %>% select(c(source, group, all_of(summ_vars))) %>%
  group_by(source, group) %>% summarize(across(everything(), .fns = c(~round(mean(.x, na.rm=TRUE), 2), # mean
                                                                      ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                              NA,
                                                                              sd(.x, na.rm=TRUE)/sqrt(length(!is.na(.x)))))), # sd
                                        OBS = n())

summ_stats <- bind_rows(can_imm %>% filter(YEAR >= 1900), 
                        can_chi %>% filter(YRIMM >= 1885 & YRIMM <= 1920), 
                        us_chi %>% mutate(WEIGHT = 1) %>% filter(YRIMM >= 1885 & YRIMM <= 1920), 
                        reg_chi %>% mutate(WEIGHT = 1, YRIMM = YEAR_ARRIV) %>% filter(YRIMM >= 1885 & YRIMM <= 1920),
                        can_match %>% filter(YEAR >= 1900)) %>% 
  mutate(across(c(LABOR, MAR, CANREAD), ~ ifelse(AGE >= 18, .x, NA))) %>%
  select(c(source, group, all_of(summ_vars), WEIGHT)) %>%
  group_by(source, group) %>% summarize(across(-c(WEIGHT), .fns = c(~round(weighted.mean(.x, WEIGHT, na.rm=TRUE), 2), #weighted mean
                                                            ~ifelse(is.na(mean(.x,na.rm=TRUE)),
                                                                    NA,
                                                                    wtd_se(.x, WEIGHT)))), #weighted sd
                                OBS = n()) %>%
  bind_rows(us_summ, can_summ, us_imm_summ, us_match_summ) %>%
  select(c(group, source, starts_with("MALE"), starts_with("MAR"), starts_with("AGE"), starts_with("CANREAD"), starts_with("LABOR"), starts_with("YRIMM"), OBS)) %>%
  arrange(across(c(group, source)))

summ_stats_out <- t(summ_stats) %>% as.data.frame()

# writing
summ_cats <- c("\\% Male", "\\% Married*", "Age", "\\% Literate*", "\\% Laborers*","Year of Imm.")
summtex <- file(glue("{git}/figs/summstats.tex"), open = "w")
writeLines(c("\\begin{tabular}{lcccccccccccc}", "\\hhline{=============}", "& \\multicolumn{2}{c}{Entire Population} & & \\multicolumn{2}{c}{All Immigrants} & & \\multicolumn{3}{c}{Chinese Immigrants (1885-1920)} & & \\multicolumn{2}{c}{Matched Imm. (1885-1920)} \\\\ ", 
             "\\hhline{~--~--~---~--}", "& (1) & (2) & & (3) & (4) & & (5) & (6) & (7) & & (8) & (9) \\\\ ",
              "& CA Census & US Census & & CA Census & US Census & & CA Census & US Census & Chinese Reg. & & CA Census & US Census \\\\ ", " \\hhline{-------------}"), summtex)
for (i in 1:length(summ_cats)){
  means <- ifelse(is.na(summ_stats_out[2*i+1,]), "-", summ_stats_out[2*i+1,])
  sds <- ifelse(is.na(summ_stats_out[2*i+2,]), "", paste0("(",round(as.numeric(summ_stats_out[2*i+2,]),4),")"))
  writeLines(c(paste(summ_cats[i], "&", glue_collapse(c(means[1:2],"",means[3:4],"",means[5:7], "", means[8:9]), sep = "&", last = ""), "\\\\ "), 
               paste("&", glue_collapse(c(sds[1:2],"",sds[3:4],"",sds[5:7], "", sds[8:9]), sep = "&", last = ""), "\\\\ ")), summtex)
}
obs <- ifelse(is.na(summ_stats_out[2*(length(summ_cats)+1)+1,]), "-", round(as.numeric(summ_stats_out[2*(length(summ_cats)+1)+1,])/1000, 0))
writeLines(c("Obs. (Thousands)", "&", glue_collapse(c(obs[1:2],"",obs[3:4],"",obs[5:7],"", obs[8:9]), sep = "&", last = ""), "\\\\ "), summtex)
writeLines(c("\\hhline{-------------}","\\end{tabular}"), summtex)
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
yrimm_census <- can_chi %>% group_by(YEAR, YRIMM) %>% summarize(CHIPOP = sum(WEIGHT)) %>% mutate(source = "CA Census", tax = case_when(YRIMM <= 1885 ~ 0,
                                                                                                                                       YRIMM <= 1900 ~ 50,
                                                                                                                                       YRIMM <= 1903 ~ 100,
                                                                                                                                        YRIMM < 1924 ~ 500)) %>% 
  arrange(YRIMM) %>%
  filter((YEAR == 1901 & YRIMM < 1901) | (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  
yrimm_reg <- reg_chi %>% mutate(YRIMM = YEAR_ARRIV) %>% group_by(YRIMM) %>% summarize(CHIPOP = n()) %>% mutate(source = "Chinese Register",
                                                                                                               tax = case_when(YRIMM <= 1885 ~ 0,
                                                                                                                               YRIMM <= 1900 ~ 50,
                                                                                                                               YRIMM <= 1903 ~ 100,
                                                                                                                               YRIMM < 1924 ~ 500)) %>%
  arrange(YRIMM) %>% filter(YRIMM >= 1880 & YRIMM <= 1930)

yrimm_census_allim <- can_imm %>% group_by(YEAR, YRIMM) %>% summarize(IMMPOP = sum(WEIGHT), 
                                                                      JAPPOP = sum(ifelse(BORNJAP == 1, WEIGHT, 0)), 
                                                                      CHIPOP = sum(ifelse(BORNCHI == 1, WEIGHT, 0)),
                                                                      AUSPOP = sum(ifelse(BORNAUS == 1, WEIGHT, 0))) %>% 
  mutate(source = "CA Census", tax = case_when(YRIMM <= 1885 ~ 0,
                                                                                                                                             YRIMM <= 1900 ~ 50,
                                                                                                                                             YRIMM <= 1903 ~ 100,
                                                                                                                                             YRIMM < 1924 ~ 500)) %>% filter(YRIMM >= 1880) %>%
  arrange(YRIMM) %>%
  filter((YEAR == 1901 & YRIMM < 1901) | (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  mutate(CHIFRAC = CHIPOP/IMMPOP, JAPFRAC = JAPPOP/IMMPOP, AUSFRAC = AUSPOP/IMMPOP, CHIJAP = CHIPOP/JAPPOP) 

yrimm_all <- rbind(filter(yrimm_census, YRIMM >= 1880), yrimm_reg)

fig2_flow <- ggplot(data = yrimm_all, aes(x = YRIMM, y = CHIPOP, linetype = source)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE) +
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
  scale_linetype_manual(breaks=c("Chinese Register","CA Census"), values=c(1,2)) +
  labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", linetype = "Data Source") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow.png"), fig2_flow, height = 5, width = 9)

########################################################################
### TABLE 2: REGRESSION OF IMM INFLOWS ON TAX RAISES
########################################################################
## checking if flows change with year -- register
yrimm_flow1 <- lm(data = yrimm_reg, CHIPOP ~ tax)
yrimm_flow2 <- lm(data = yrimm_reg %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ tax + startyear_minus1880 + I(startyear_minus1880^2))
yrimm_flow3 <- lm(data = yrimm_reg %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2))

## checking if flows change with year -- census
yrimm_flow4 <- lm(data = yrimm_census, CHIPOP ~ tax)
yrimm_flow5 <- lm(data = yrimm_census %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ tax + startyear_minus1880 + I(startyear_minus1880^2))
yrimm_flow6 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ tax + startyear_minus1880 + I(startyear_minus1880^2) + IMMPOP)
yrimm_flow7 <- lm(data = yrimm_census %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2))
yrimm_flow8 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880), CHIPOP ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2) + IMMPOP)

yrimm_flowextra <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880), JAPPOP ~ tax + startyear_minus1880 + I(startyear_minus1880^2 + IMMPOP))
yrimm_flowextra2 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880), JAPPOP ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2 + IMMPOP))

#yrimm_flowextra2 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880), JAPPOP ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2 + IMMPOP))

# ## checking if chi pct of flows change with year -- census (chi)
# yrimm_flow7 <- lm(data = yrimm_census_allim %>% mutate(CHIFRAC = CHIFRAC*10000), CHIFRAC ~ tax)
# yrimm_flow8 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880, CHIFRAC = CHIFRAC*10000), CHIFRAC ~ tax + startyear_minus1880 + I(startyear_minus1880^2))
# yrimm_flow9 <- lm(data = yrimm_census_allim %>% mutate(startyear_minus1880 = YRIMM - 1880, CHIFRAC = CHIFRAC*10000), CHIFRAC ~ factor(tax) + startyear_minus1880 + I(startyear_minus1880^2))

stargazer(yrimm_flow1, yrimm_flow2, yrimm_flow3, yrimm_flow4, yrimm_flow5, yrimm_flow7, yrimm_flow6, yrimm_flow8, yrimm_flowextra, yrimm_flowextra2,
          out = glue("{git}/figs/immflow_regs.tex"), 
          float = FALSE, 
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          column.labels = c("Chinese Register", "Canadian Census"),
          column.separate = c(3,7),
          dep.var.labels = c("Chinese Immigrant Inflow (Thousands)"),
          keep = c(2:5),
          covariate.labels = c("$TAX$", "\\$50 Tax", "\\$100 Tax", "\\$500 Tax"),
          add.lines = list(c("Time Trends", c(rep(c("No", "Yes", "Yes"), 2)), "Yes", "Yes",  "Yes", "Yes"), c("Ctrl. for Total Immigration", c(rep("No", 6), rep("Yes", 4)))),
          table.layout = "=c#-ta-s-")


########################################################################
### TABLE 3: DIFF IN DIFF REGRESSION OF CHI IMM VS OTHER IMM AT EACH 'EVENT'
########################################################################
# test which countries immigrants have similar characteristics to chinese immigrants
compare_imms <- can_imm %>% 
 mutate(nationality = case_when(BORNCHI == 1 ~ "China",
                                 BORNRUS == 1 ~ "Russia",
                                 BORNFRA == 1 ~ "France",
                                 BORNGER == 1 ~ "Germany",
                                 BORNJAP == 1 ~ "Japan",
                                 BORNIND == 1 ~ "India",
                                 BORNIRE == 1 ~ "Ireland",
                                 BORNAUS == 1 ~ "Austria",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(nationality)) %>%
  group_by(nationality) %>% summarize(pop = sum(WEIGHT), across(summ_vars, ~weighted.mean(.x, WEIGHT, na.rm=TRUE)))

# subset/clean data for regressions
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM > 1890) %>% #only keeping years with earnings/yrimm data
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
  filter((YEAR == 1901 & YRIMM < 1901) | (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) # only taking YRIMM from most recent census (lowest rate of loss to outmigration)

did_data_jap <- did_data %>% filter(BORNCHI == 1 | BORNJAP == 1) 


# run regressions
did_reg_labor <- lm(LABOR ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_labor_jap <- lm(LABOR ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)
did_reg_canread <- lm(CANREAD ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_canread_jap <- lm(CANREAD ~ factor(YRIMM) + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)
did_reg_earn <- lm(EARN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data, weights = WEIGHT)
did_reg_earn_jap <- lm(EARN ~ factor(YRIMM) +  BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_jap, weights = WEIGHT)
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

fig3_us_can <- ggplot(data = yrimm_us_can %>% filter(YRIMM >= 1870), aes(x = YRIMM, y = POP, color = IMM, linetype = source)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE) +
  geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
  geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
  geom_text(aes(x = x, y = 7000, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
  scale_linetype_manual(breaks=c("CA Census", "US Census"), values=c(2,3)) +
  scale_color_manual(breaks=c("Chinese", "Japanese"), values=c("black", "red")) +
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



