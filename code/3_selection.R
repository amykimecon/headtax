# outcome/selection analysis 

#### HEIGHT PLOT ####
height_plot <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE), 
            height25 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.25,na.rm=TRUE),
            height75 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.75,na.rm=TRUE),
            n= n())

height_plot2 <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(taxgrp = ifelse(FEES == 0, "nofee", "fee")) %>%
  group_by(YRIMM, taxgrp) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE),
            n= n()) %>% filter(n > 1)
# 
# ggplot(height_plot, aes(x = YRIMM, y = height)) + 
#   geom_smooth(method = "lm", mapping = aes(weight = n), color = c3) +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
#   geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
#   geom_point(aes(size = n), color = c1, alpha = 0.9) + 
#   #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
#   theme_minimal() + theme(legend.position='bottom') + 
#   labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

taxmeanheights = summarize(group_by(height_plot, tax), meanheight = weighted.mean(height, n))$meanheight
ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  geom_segment(aes(y = taxmeanheights[1], yend = taxmeanheights[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[2], yend = taxmeanheights[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[3], yend = taxmeanheights[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[4], yend = taxmeanheights[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanheights[4] - 0.2, label = "Mean Height Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

ggsave(glue("{git}/figs/height_selection.png"), height = 4, width = 7)

# summary(lm(data = reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1885 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1),
#            ))
# 

# 
# ggplot(height_plot2, aes(x = YRIMM, y = height, color = taxgrp)) +
#   geom_smooth(method = "lm", mapping = aes(weight = n)) +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
#   geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
#   geom_point(aes(size = n), alpha = 0.9) + theme_minimal() + theme(legend.position='bottom') +
#   labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# Chinese Immigrants")


########################################################################
### TABLE 3: DIFF IN DIFF REGRESSION OF CHI IMM VS OTHER IMM AT EACH 'EVENT'
########################################################################
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM >= 1880 & YRIMM <= 1913) %>% #only keeping years with earnings/yrimm data
  mutate(BORNCHI = ifelse(BPL == "China", 1, 0),
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         BORNCHI_notax = BORNCHI * ifelse(tax == 0, 1, 0),
         AGEATIMM = AGE - (YEAR - YRIMM),
         ageheap = ifelse(AGE %% 5 == 0, 1, 0),
         twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
                                 YRIMM %in% c(1901,1902) ~ "post100",
                                 YRIMM %in% c(1904,1905) ~ "post500",
                                 TRUE ~ "_notpost"),
         oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
                                 YRIMM == 1901 ~ "post100",
                                 YRIMM == 1904 ~ "post500",
                                 TRUE ~ "_notpost"),
         BORNCHI_twoyearpost50 = BORNCHI*ifelse(twoyearpost == "_post50", 1, 0),
         BORNCHI_twoyearpost100 = BORNCHI*ifelse(twoyearpost == "post100", 1, 0),
         BORNCHI_twoyearpost500 = BORNCHI*ifelse(twoyearpost == "post500", 1, 0)) %>%
  filter(MALE == 1 & AGEATIMM >= 18) 

did_data_japan <- did_data %>% filter(BORNCHI == 1 | BPL == "Japan") %>% filter(YRIMM >= 1890 & YRIMM <= 1907)# & YEAR != 1921)

# normal regs
did_reg_labor <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                    data = did_data %>% filter(YRIMM != 1900), weights = WEIGHT)
did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                      data = did_data, weights = WEIGHT)
did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                   data = did_data, weights = WEIGHT)
did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                       data = did_data, weights = WEIGHT)
# 
# # regs w/ two year post
# did_reg_labor <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500 +
#                       BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                     data = did_data, weights = WEIGHT)
# did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                         BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                       data = did_data, weights = WEIGHT)
# did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                      BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                    data = did_data, weights = WEIGHT)
# did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                          BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                        data = did_data, weights = WEIGHT)


# normal japan regs
did_reg_labor_japan <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
                          data = did_data_japan, weights = WEIGHT)
did_reg_canread_japan <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
                            data = did_data_japan, weights = WEIGHT)
did_reg_earn_japan <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
                         data = did_data_japan, weights = WEIGHT)
did_reg_houseown_japan <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
                             data = did_data_japan, weights = WEIGHT)

# # japan regs w/ two year post
# did_reg_labor_japan <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500 +
#                             BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                           data = did_data_japan, weights = WEIGHT)
# did_reg_canread_japan <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                               BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                             data = did_data_japan, weights = WEIGHT)
# did_reg_earn_japan <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                            BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                          data = did_data_japan, weights = WEIGHT)
# did_reg_houseown_japan <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                                BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                              data = did_data_japan, weights = WEIGHT)

# dep var means & ses
did_data_means <- c(weighted.mean(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
did_data_japan_means <- c(weighted.mean(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
                          weighted.mean(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
                          weighted.mean(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))
did_data_ses <- c(wtd_se(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
                    wtd_se(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
                    wtd_se(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
did_data_japan_ses <- c(wtd_se(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
                          wtd_se(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
                          wtd_se(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))


stargazer(did_reg_labor, did_reg_canread, did_reg_houseown,did_reg_labor_japan, did_reg_canread_japan, did_reg_houseown_japan,
          out = glue("{git}/figs/selection.tex"), float = FALSE, 
          intercept.bottom =FALSE,
          column.labels = c("All Immigrants (1880-1913)","Chinese \\& Japanese Imm. (1890-1907)"),
          column.separate = c(3,3),
          dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$",
                             "$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$"),
          keep = c("BORNCHI*"),
          covariate.labels = c("$\\hat{\\beta}_{1}$ ($BORNCHI$)", 
                               "$\\hat{\\gamma}_{50}^{DD}$ ($BORNCHI \\times$ \\$50 Tax)", 
                               "$\\hat{\\gamma}_{100}^{DD}$ ($BORNCHI \\times$ \\$100 Tax)", 
                               "$\\hat{\\gamma}_{500}^{DD}$ ($BORNCHI \\times$ \\$500 Tax)"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "=cld#-ta-s-",
          add.lines = list(c("Dep. Var. Mean (SE)", formatC(did_data_means, format = "f"), formatC(did_data_japan_means, format = "f"), "\\\\"),
                           c("", paste0("(",c(formatC(did_data_ses, format = "f"), formatC(did_data_japan_ses, format = "f")), ")"), "\\\\")))

                             
                             
                            
# ########################################################################
# ### TABLE 4: US VS CAN REGRESSIONS
# ########################################################################
# did_data_us <- us_imm %>% filter(YEAR >= 1900 & YRIMM >= 1890 & YRIMM <= 1906 & YEAR != 1920) %>% #only keeping years with earnings/yrimm data
#   mutate(BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
#          BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
#          BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
#          AGEATIMM = AGE - (YEAR - YRIMM),
#          ERSCORE = ifelse(EARN > 100, NA, EARN)) %>%
#   filter(AGEATIMM >= 18 & MALE == 1) %>% filter(BORNCHI == 1 | BORNJAP == 1)
# 
# did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_canread_us <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_earn_us <- lm(ERSCORE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_houseown_us <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# 
# stargazer(did_reg_labor_us, did_reg_canread_us, did_reg_earn_us, did_reg_houseown_us, 
#           out = glue("{git}/figs/8aug23/outcome_regs_us.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           keep = c("BORNCHI*"),
#           single.row = TRUE,
#           covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$50 Tax", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
#           keep.stat=c("n","adj.rsq"),
#           table.layout = "=cd#-ta-s-",
#           add.lines = list(c("Dep. Var. Mean (SE)", "0.219 (0.000)", "0.884 (0.000)", "48.3 (0.007)", "0.332 (0.000)")))
# 
# mean(filter(did_data_us, !is.na(LABOR))$LABOR)
# sd(filter(did_data_us, !is.na(LABOR))$LABOR)/sqrt(nrow(filter(did_data_us, !is.na(LABOR))))
# 
# mean(filter(did_data_us, !is.na(CANREAD))$CANREAD)
# sd(filter(did_data_us, !is.na(CANREAD))$CANREAD)/sqrt(nrow(filter(did_data_us, !is.na(CANREAD))))
# 
# mean(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)
# sd(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)/sqrt(nrow(filter(did_data_us, !is.na(ERSCORE))))
# 
# mean(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)
# sd(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)/sqrt(nrow(filter(did_data_us, !is.na(HOUSEOWN))))
# 
# did_data_us_can <- bind_rows(did_data_japan %>% mutate(CAN = 1), did_data_us %>% mutate(CAN = 0))
# # run regressions -- triple diff with us and canada
# did_reg_uscan_labor <- lm(LABOR ~ factor(YRIMM)*CAN + factor(YEAR) +  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# did_reg_uscan_canread <- lm(CANREAD ~ factor(YRIMM)*CAN + factor(YEAR)+  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# did_reg_uscan_houseown <- lm(HOUSEOWN  ~ factor(YRIMM)*CAN + factor(YEAR)+  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# 
# stargazer(did_reg_uscan_labor, did_reg_uscan_canread, did_reg_uscan_houseown,
#           out = glue("{git}/figs/uscan_regs.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           dep.var.labels = c("Laborer", "Literate", "Owns Home"),
#           keep = c(31, 62, 63, 64, 67, 68),
#           covariate.labels = c("$BORNCHI \\times US$", 
#                                "$BORNCHI \\times CANADA $",
#                                "$BORNCHI \\times US \\times $ \\$100 Tax",
#                                "$BORNCHI \\times US \\times $ \\$500 Tax",
#                                "$BORNCHI \\times CANADA \\times $ \\$100 Tax",
#                                "$BORNCHI \\times CANADA \\times $ \\$500 Tax"),
#           keep.stat=c("n","adj.rsq"),
#           add.lines = list(c("Includes Year $\\times$ Country FE", rep("Yes", 3))),
#           table.layout = "=cld#-ta-s-")
