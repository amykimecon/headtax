# outcome/selection analysis 

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
