## ANALYSING TRENDS IN REGISTER DATA 
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, YRIMM_FISCAL = FISCALYEAR,
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         arr_cohort = case_when(YRIMM >= 1886 & YRIMM < 1890 ~ "1886-1889", 
                                YRIMM >= 1890 & YRIMM < 1895 ~ "1890-1894", 
                                YRIMM >= 1895 & YRIMM < 1900 ~ "1895-1899", 
                                YRIMM >= 1900 & YRIMM < 1904 ~ "1900-1903",
                                YRIMM >= 1904 & YRIMM < 1910 ~ "1904-1909", 
                                YRIMM >= 1910 & YRIMM < 1915 ~ "1910-1914", 
                                TRUE ~ NA_character_),
         BIRTHYR = ifelse(REG_Year != 0, REG_Year - AGE, NA),
         birthcohort = cut(BIRTHYR, breaks = c(1850, 1860, 1870, 1880, 1890)),
         HEIGHT = HEIGHT * 2.54,
         pred_height_AUQLD = case_when(BIRTHYR < 1850 ~ 164.2,
                                 BIRTHYR < 1855 ~ 164,
                                 BIRTHYR < 1860 ~ 163.8,
                                 BIRTHYR < 1865 ~ 163.9,
                                 BIRTHYR < 1870 ~ 163.9,
                                 BIRTHYR < 1875 ~ 163.6,
                                 BIRTHYR < 1880 ~ 163.3,
                                 BIRTHYR < 1885 ~ 162.8,
                                 BIRTHYR < 1890 ~ 162,
                                 BIRTHYR < 1895 ~ 162.1,
                                 BIRTHYR < 1900 ~ 162.1,
                                 BIRTHYR < 1905 ~ 162.4,
                                 BIRTHYR < 1910 ~ 162.9,
                                 TRUE ~ NA),
         pred_whipple_AUSYD = case_when(BIRTHYR < 1850 ~ 197,
                                        BIRTHYR < 1860 ~ 139,
                                        BIRTHYR < 1870 ~ 134,
                                        BIRTHYR < 1880 ~ 122,
                                        BIRTHYR < 1890 ~ 100,
                                        BIRTHYR < 1910 ~ 100,
                                        TRUE ~ NA),)

# height sample: males age 24-50 with height at least 100 cm and age arriving between 1886-1923
height_sample <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1885 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1)

# age of sample over time
ggplot(height_sample %>% group_by(YRIMM) %>% summarize(meanage = mean(AGE, na.rm=TRUE), 
                                                 medianage = median(AGE, na.rm=TRUE),
                                                 n= n()),
       aes(x = YRIMM, y = meanage)) + geom_smooth(method = "lm", aes(weight = n)) +
       geom_point(aes(size = n)) 

ggsave(glue("{git}/figs/5oct23/ageplot1.png"), width = 8, height = 4)

# #height distrib by birth cohort
# ggplot(reg_chi %>% filter(AGE >= 24 & AGE <= 50) %>% group_by(BIRTHYR, MALE) %>%
#          summarize(birthyr = mean(BIRTHYR, na.rm=TRUE), height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE)),
#        aes(x = birthyr, y = height, color = factor(MALE))) +
#   geom_point()

#height distrib by year of arrival *****
height_plot <- height_sample %>% group_by(YRIMM) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE), 
            pred_height = mean(pred_height_AUQLD, na.rm=TRUE),
            n= n())

ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  geom_smooth(method = "lm", mapping = aes(weight = n)) +
  geom_point(aes(size = n))

ggsave(glue("{git}/figs/5oct23/heightplot1.png"), width = 8, height = 4)

ggplot(height_plot %>% pivot_longer(cols = c(height, pred_height), names_to = "cat", values_to = "ht"),
       aes(x = YRIMM, y = ht, color = cat)) + geom_point(aes(size = n)) + geom_smooth(method = "lm", mapping = aes(weight = n))
ggsave(glue("{git}/figs/5oct23/heightplot2.png"), width = 8, height = 4)


## REPLICATING W WOMEN
height_sample_women <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1885 & YRIMM < 1924 & HEIGHT > 100 & MALE == 0)

# age of sample over time
ggplot(height_sample_women %>% group_by(YRIMM) %>% summarize(meanage = mean(AGE, na.rm=TRUE), 
                                                       medianage = median(AGE, na.rm=TRUE),
                                                       n= n()),
       aes(x = YRIMM, y = meanage)) + geom_smooth(method = "lm", aes(weight = n)) +
  geom_point(aes(size = n)) 

ggsave(glue("{git}/figs/5oct23/ageplot1_women.png"), width = 8, height = 4)

height_plot_women <- height_sample_women %>% group_by(YRIMM) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE), 
            pred_height = mean(pred_height_AUQLD, na.rm=TRUE),
            n= n())

ggplot(height_plot_women, aes(x = YRIMM, y = height)) + 
  geom_smooth(method = "lm", mapping = aes(weight = n)) +
  geom_point(aes(size = n))

ggsave(glue("{git}/figs/5oct23/heightplot1_women.png"), width = 8, height = 4)

## REGRESSIONS
# age
taxed_age <- filter(reg_chi, YRIMM > 1885 & YRIMM < 1924 & FEES > 0 & MALE == 1) %>% mutate(t = YRIMM - 1885)
summary(lm(data = taxed_age, AGE ~ t))
summary(lm(data = taxed_age, AGE ~ factor(tax)))
summary(lm(data = taxed_age, AGE ~ factor(tax) + t))

# height
taxed_height <-filter(reg_chi, AGE>= 24 & AGE <=50 & YRIMM > 1885 & YRIMM < 1924 & FEES > 0 & MALE == 1 & !is.na(HEIGHT)) %>% mutate(t = YRIMM - 1885)
summary(lm(data = taxed_height, HEIGHT ~ t))
summary(lm(data = taxed_height, HEIGHT ~ factor(tax)))
summary(lm(data = taxed_height, HEIGHT ~ factor(tax) + t))
summary(lm(data = taxed_height, HEIGHT ~ pred_height_AUQLD + factor(tax) + t))

################################
## AGE HEAPING
# histogram of ages in register
ggplot(data =reg_chi %>% filter(YRIMM >= 1886 & YRIMM <= 1923), aes(x = AGE)) + 
  geom_vline(xintercept = seq(5,60,5), linetype = 2, color = "grey")  + geom_histogram(binwidth = 1)
ggsave(glue("{git}/figs/5oct23/agehist.png"), width = 8, height = 4)

# histogram of ages in census
ggplot(data = can_imm, aes(x = AGE, weight = WEIGHT)) + 
  geom_vline(xintercept = seq(5,90,5), linetype = 2, color = "grey")  + geom_histogram(binwidth = 1)
ggsave(glue("{git}/figs/5oct23/agehist_censusall.png"), width = 8, height = 4)

# histogram of ages in census
ggplot(data = can_imm %>% filter(BPL == "China"), aes(x = AGE, weight = WEIGHT)) + 
  geom_vline(xintercept = seq(5,90,5), linetype = 2, color = "grey") + geom_histogram(binwidth = 1)
ggsave(glue("{git}/figs/5oct23/agehist_census.png"), width = 8, height = 4)

#reg_chi_whipple = 5*sum(ifelse(filter(reg_chi, AGE >= 23 & AGE <= 62)$AGE %% 5 == 0, 1, 0))/nrow(filter(reg_chi, AGE >= 23 & AGE <= 62))

# over time (register)
reg_chi_whipple <- reg_chi %>% filter(YRIMM >= 1886 & YRIMM <= 1923 & AGE >= 23 & AGE <= 62) %>%
  group_by(YRIMM) %>%
  summarize(whipple = 500*sum(ifelse(AGE %% 5 == 0, 1, 0))/n(), n = n(),
            pred_whipple = mean(pred_whipple_AUSYD, na.rm=TRUE))

ggplot(reg_chi_whipple, aes(x = YRIMM, y = whipple)) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
ggsave(glue("{git}/figs/5oct23/whippleplot1.png"), width = 8, height = 4)

ggplot(reg_chi_whipple %>% pivot_longer(cols = c(whipple, pred_whipple), values_to = "whp", names_to = "cat"),
       aes(x = YRIMM, y = whp, color = cat)) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
ggsave(glue("{git}/figs/5oct23/whippleplot2.png"), width = 8, height = 4)

# over time (census)
census_whipple <- can_imm %>% filter(YRIMM >= 1880 & YRIMM <= 1923 & !is.na(AGE) & AGE >= 23 & AGE <= 62 & 
                                       BPL %in% c("China","Japan")) %>% 
  group_by(BPL, YRIMM) %>% summarize(whipple = 500*sum(ifelse(AGE %% 5 == 0, WEIGHT, 0))/sum(WEIGHT), n = sum(WEIGHT))

ggplot(census_whipple %>% filter(BPL == "China"), aes(x = YRIMM, y = whipple)) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
ggsave(glue("{git}/figs/5oct23/whippleplot1_census.png"), width = 8, height = 4)

ggplot(census_whipple, aes(x = YRIMM, y = whipple, color = factor(BPL))) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
ggsave(glue("{git}/figs/5oct23/whippleplot3_census.png"), width = 8, height = 4)

## literacy (census)
literacy_census <- can_imm %>% filter(YRIMM >= 1880 & YRIMM <= 1923 & !is.na(CANREAD) & AGE >= 23 & AGE <= 62 &
                                        BPL %in% c("China", "Japan") & MALE == 1) %>% 
  group_by(BPL, YRIMM) %>% summarize(literacy = 100*sum(ifelse(CANREAD == 1, WEIGHT, 0))/sum(WEIGHT), n = sum(WEIGHT))

ggplot(literacy_census %>% filter(BPL == "China"), aes(x = YRIMM, y = literacy)) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
#ggsave(glue("{git}/figs/5oct23/whippleplot1_census.png"), width = 8, height = 4)


ggplot(literacy_census, aes(x = YRIMM, y = literacy, color = factor(BPL))) + geom_smooth(method = "lm", aes(weight = n)) + geom_point(aes(size = n))
#ggsave(glue("{git}/figs/5oct23/whippleplot3_census.png"), width = 8, height = 4)

## REDOING CENSUS SELECTION STUFF
census_regs <- can_imm %>% filter(BPL %in% c("China", "Japan") & YRIMM >= 1880 & YRIMM <= 1923 & !is.na(CANREAD) & AGE >= 23 & MALE == 1) %>%
  mutate(CHI50 = ifelse(BPL == "China" & tax == 1496.19, 1, 0),
         CHI100 = ifelse(BPL == "China" & tax == 2992.61, 1, 0),
         CHI500 = ifelse(BPL == "China" & tax == 14115.7, 1, 0))
summary(lm(data = census_regs, CANREAD ~ factor(YRIMM) + factor(BPL) + CHI50 + CHI100 + CHI500))


# 
# 
# # regression
# summary(lm(data = reg_chi %>% mutate(whipple_prob = ifelse(AGE %% 5 == 0, 1, 0)) %>% filter(YRIMM <= 1923 & YRIMM > 1885),
#            whipple_prob ~ factor(YRIMM)))
# 
# imm_whipple = 5*sum(ifelse(filter(can_imm, !is.na(AGE))$AGE %% 5 == 0, 1, 0))/nrow(filter(can_imm, !is.na(AGE)))
# 
# # for china only
# ggplot(data = can_imm %>% filter(BPL == "China"),
#        aes(x = AGE)) + geom_histogram(binwidth = 1) + 
#   geom_vline(xintercept = seq(5,70,5), linetype = 2, color = "grey") + facet_wrap(~BPL)
# 
# imm_chi_whipple = 5*sum(ifelse(filter(can_imm, !is.na(AGE) & BPL == "China")$AGE %% 5 == 0, 1, 0))/nrow(filter(can_imm, !is.na(AGE) & BPL == "China"))
# 






