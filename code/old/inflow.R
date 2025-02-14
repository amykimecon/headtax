## FIRST STAGE ANALYSIS: EFFECT OF HEAD TAX ON CHI IMM INFLOWS

#_____________________________________________________________
# TABLE 2: INFLOW REGRESSION --------------------------
#_____________________________________________________________

## Creating Dataset ----
# first combining all long datasets, then normalizing flow and popstock by source country pop,
# then pivoting wide and adding single country datasets
flow_regress <- yrimm_census %>% group_by(BPL) %>% mutate(n = n()) %>% filter(n > 5) %>% # at least five years of nonzero imm
  ungroup() %>% select(-c(YEAR,tax,n)) %>% #census data (flows)
  full_join(popstock, by = c("YRIMM"="YEAR", "BPL")) %>% #population stocks
  inner_join(maddison_data %>% mutate(Year = as.double(Year)) %>% #source country population and gdp per capita data
               inner_join(wid_data) %>% #source country inequality data
               pivot_longer(-Year, names_to = c(".value", "BPL"), names_pattern = "(.*)_([A-z]+)$"),
             by = c("YRIMM" = "Year", "BPL")) %>%
  group_by(BPL) %>% arrange(YRIMM) %>% 
  mutate(logFLOWOVERPOP = log(FLOW/INTERP_POP), #dividing flows and stocks by source country population
         POPSTOCKLAG = lag(CANPOP),
         POPSTOCKLAGOVERPOP = POPSTOCKLAG/INTERP_POP,
         INTERP_GDPPERCAP = INTERP_GDP/INTERP_POP) %>%
  pivot_wider(id_cols = c(YRIMM), names_from = BPL,
              values_from = c(FLOW, CANPOP, INTERP_POP, INTERP_GDPPERCAP, INTERP_INCSHARE50PCT, logFLOWOVERPOP, 
                              POPSTOCKLAG, POPSTOCKLAGOVERPOP)) %>%
  left_join(yrimm_reg) %>% #register data
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(IMMFLOW_CENSUS = sum(FLOW))) %>%
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "ForeignBorn") %>% rename(IMMPOP = CANPOP) %>% select(c(YEAR, IMMPOP)), by = c("YRIMM" = "YEAR")) %>%
  mutate(tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         cost = tax + 1496.19,
         DECADE = case_when(YRIMM <= 1871 ~ 1871,
                            YRIMM <= 1881 ~ 1881,
                            YRIMM <= 1891 ~ 1891,
                            YRIMM <= 1901 ~ 1901,
                            YRIMM <= 1911 ~ 1911,
                            YRIMM <= 1921 ~ 1921),
         twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
                                 YRIMM %in% c(1901,1902) ~ "post100",
                                 YRIMM %in% c(1904,1905) ~ "post500",
                                 TRUE ~ "_notpost"),
         oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
                                 YRIMM == 1901 ~ "post100",
                                 YRIMM == 1904 ~ "post500",
                                 TRUE ~ "_notpost")) %>%
  left_join(popstockchange %>% select(c(YEAR, POPSTOCKCHANGE_China)), by = c("DECADE" = "YEAR"))

## V1: China Only Regressions ----
toggle = "display"
#toggle = "output"

## using register flows
# equation 1: controls for imm, emm, popstock, and tax
regstart = 1886
regend = 1923
flowreg0 <- lm(data = flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1923), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + cost + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               log(CHIFLOW_REGISTER) ~ EMIG_TOT + IMMFLOW_NATL + log(cost) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend) %>% 
                 mutate(flowratio_REG = CHIFLOW_REGISTER/INTERP_POP_China), 
               log(flowratio_REG) ~ EMIG_TOT + IMMFLOW_NATL + log(cost) + POPSTOCKLAGOVERPOP_China + I(POPSTOCKLAGOVERPOP_China^2))
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend) %>% 
                 mutate(flowratio_REG = CHIFLOW_REGISTER/INTERP_POP_China), 
               log(flowratio_REG) ~ EMIG_TOT + IMMFLOW_NATL + log(cost) + log(POPSTOCKLAGOVERPOP_China))

coeftest(flowreg1, vcov. = vcovHC(flowreg1, vcov = "HC1"))

plot(data$YRIMM, resid(flowreg0)) + abline(h = 0, col = "red")

flowreg1_tax <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + tax + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))



## using census flows
# equation 1: controls for imm, emm, popstock, and tax
censtart = 1882
cenend = 1920
flowcensus1 <- lm(data = flow_regress %>% filter(YRIMM >= censtart & YRIMM <= cenend), 
                  FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

flowcensus1_tax <- lm(data = flow_regress %>% filter(YRIMM >= censtart & YRIMM <= cenend), 
                  FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + tax + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

if (toggle == "display"){
  print(summary(flowreg1))
  print(summary(flowcensus1))
}

reg_mean = round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER), 1)
reg_se = round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/sqrt(regend-regstart),1)
census_mean = round(mean(filter(flow_regress, YRIMM >= censtart & YRIMM <= cenend)$FLOW_China),1)
census_se = round(sd(filter(flow_regress, YRIMM >= censtart & YRIMM <= cenend)$FLOW_China)/sqrt(regend-regstart),1)

n = 2
flowreg_stats = c(rep(glue("{reg_mean} ({reg_se})"),n),rep(glue("{census_mean} ({census_se})"),n))
flowreg_means = c(rep(glue("{reg_mean}"),n),rep(glue("{census_mean}"),n))
flowreg_ses = c(rep(glue("({reg_se})"),n),rep(glue("({census_se})"),n))

## output
stargazer(flowreg1, flowreg1_tax, flowcensus1, flowcensus1_tax,
          out = glue("{git}/output/paper/tables/immflow_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.caption = "Dependent Variable: Chinese Immigrant Inflow ($FLOW_{China, t}$)",
          dep.var.labels.include = FALSE,
          column.labels = c("Chinese Register (1882-1920)", "Canadian Census (1882-1920)"),
          column.separate = c(n,n),
          keep = c("^factor","tax"),
          covariate.labels = c("$\\gamma_{50}$ (\\$50 Tax)", "$\\gamma_{100}$ (\\$100 Tax)", "$\\gamma_{500}$ (\\$500 Tax)",
                               "$\\gamma^C$ (Linear Effect of Tax)"),
          add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
          table.layout = "=lc#-t-as=")

## Counterfactual: Register ----
flowregcf <- cf_flow_calc(flow_regress, flowreg1)


ggplot(flowregcf %>% 
                          pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`, `Predicted Inflow`), names_to = "cat", values_to = "flow") %>%
                          mutate(flow = flow/1000),
                        aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs <= endyr), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = max(df$`Counterfactual Inflow`)/1000, label = labs), data = headtaxcuts %>% filter(yrs <= endyr), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

fig_flowregcf <- cf_flow_out(flowregcf)
ggsave(glue("{git}/output/paper/immflow_cf_reg.png"), fig_flowregcf, height = 4, width = 7)

## Counterfactual: Census
flowcencf <- cf_flow_calc(flow_regress, flowcensus1, cen = TRUE)


ggplot(flowcencf %>% 
         pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`, `Predicted Inflow`), names_to = "cat", values_to = "flow") %>%
         mutate(flow = flow/1000),
       aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs <= endyr), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = max(df$`Counterfactual Inflow`)/1000, label = labs), data = headtaxcuts %>% filter(yrs <= endyr), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

fig_flowcencf <- cf_flow_out(flowcencf)
ggsave(glue("{git}/output/paper/immflow_cf_cen.png"), fig_flowcencf, height = 4, width = 7)


flowregcf_cen <- flow_regress %>% select(c(CHIFLOW_REGISTER, FLOW_China, POPSTOCK_China, EMIG_TOT, IMMFLOW_CENSUS, YRIMM, POPSTOCKCHANGE_China, tax, DECADE)) %>%
  filter(!is.na(POPSTOCK_China) & YRIMM > 1879 & YRIMM < 1921) %>% arrange(YRIMM)

intercept_cen <- flowcensus1$coefficients[["(Intercept)"]]
coef_emigtot_cen <- flowcensus1$coefficients[["EMIG_TOT"]]
coef_immflownatl_cen <- flowcensus1$coefficients[["IMMFLOW_CENSUS"]]
coef_popstock_cen <- flowcensus1$coefficients[["POPSTOCK_China"]]
coef_popstock2_cen <- flowcensus1$coefficients[["I(POPSTOCK_China^2)"]]

popstock_cf_cen = flowregcf_cen$POPSTOCK_China[1]
popstock_cf_all_cen <- numeric(length = nrow(flowregcf_cen))
flow_cf_all_cen <- numeric(length= nrow(flowregcf_cen))
for (i in 1:nrow(flowregcf_cen)){
  popstock_cf_all_cen[i] <- popstock_cf_cen
  outflow_share_cen = (flowregcf_cen$FLOW_China[i] - flowregcf_cen$POPSTOCKCHANGE_China[i])/flowregcf_cen$FLOW_China[i]
  flow_cf_cen = intercept_cen + coef_emigtot_cen*flowregcf_cen$EMIG_TOT[i] + coef_immflownatl_cen*flowregcf_cen$IMMFLOW_CENSUS[i] + coef_popstock_cen*popstock_cf_cen + coef_popstock2_cen*(popstock_cf_cen^2)
  flow_cf_all_cen[i] <- flow_cf_cen 
  popstock_cf_cen = popstock_cf_cen + flow_cf_cen*(1-outflow_share_cen) # - outflow_cen
}

flowregcf_cen <- flowregcf_cen %>% mutate(`Actual Inflow` = FLOW_China, `Counterfactual Inflow` = flow_cf_all_cen, diff = `Counterfactual Inflow` - `Actual Inflow`) %>%
  filter(!is.na(`Counterfactual Inflow`))
flowregcf_cen %>% group_by(tax) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`, na.rm=TRUE), tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), yrs= n()) %>% mutate(diff= (tot_flow_cf-tot_flow_act)/yrs)
flowregcf_cen %>% filter(YRIMM > 1885) %>% group_by(1) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`, na.rm=TRUE), tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), yrs= n()) %>% mutate(diff= (tot_flow_cf-tot_flow_act)/yrs)
#32.8% decrease overall

fig_flowregcf_cen <- ggplot(flowregcf_cen %>% 
                              pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`), names_to = "cat", values_to = "flow") %>%
                              mutate(flow = flow/1000),
                            aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
  scale_color_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(c4,c1)) +
  scale_linetype_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(1, 2)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs < 1922), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 3.5, label = labs), data = headtaxcuts %>% filter(yrs < 1922), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Chinese Imm. Census Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/immflow_counterfactual_cen.png"), fig_flowregcf_cen, height = 4, width = 7)

## REGRESSIONS V2: All countries
reg_countries <- filter(yrimm_census %>% filter(YRIMM >= 1880 & YRIMM < 1921 & BPL != "Other" & BPL != "Unknown" & BPL != "Iceland") %>% group_by(BPL) %>% summarize(n=n()), n > 20)$BPL
graph_country_regs <- list()
i = 1
for (country in reg_countries){
  shiftcoef = -0.4 + (i-1)*(0.8/length(reg_countries))
  flow_regress_filt <-  filter(flow_regress, YRIMM >= 1880 & YRIMM < 1921)
  flow_regress_filt <- flow_regress_filt[which(!is.na(flow_regress_filt[[glue("logFLOWOVERPOP_{country}")]])),]
  reg <- lm(data = flow_regress_filt, glue("logFLOWOVERPOP_{country} ~ IMMFLOW_CENSUS + factor(tax) +  POPSTOCKLAG_{country} + I(POPSTOCKLAG_{country}^2)"))
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

# graph of gamma coefs
immflow_countries <- ggplot(graph_countries, aes(x = taxyear, y = Estimate, color = "Other")) + 
  geom_errorbar(aes(min = est_lb, max = est_ub, color = 'Other'), width = 0.05) + geom_point() +
  geom_errorbar(data = graph_countries %>% filter(country == "China"), aes(min = est_lb, max = est_ub, color = 'China'), width = 0.05) + 
  geom_point(data = graph_countries %>% filter(country == "China"), aes(x = taxyear, y = Estimate, color = 'China')) +
  geom_errorbar(data = graph_countries %>% filter(country == "Germany"), aes(min = est_lb, max = est_ub, color = 'Germany'), width = 0.05) + 
  geom_point(data = graph_countries %>% filter(country == "Germany"), aes(x = taxyear, y = Estimate, color = 'Germany')) +
  geom_errorbar(data = graph_countries %>% filter(country == "India"), aes(min = est_lb, max = est_ub, color = 'India'), width = 0.05) + 
  geom_point(data = graph_countries %>% filter(country == "India"), aes(x = taxyear, y = Estimate, color = 'India')) +
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"= expression(paste("$50 Head Tax (",gamma[50] ^c, ")")),
                                                       "2"= expression(paste("$100 Head Tax (",gamma[100] ^c, ")")),
                                                       "3"= expression(paste("$500 Head Tax (",gamma[500] ^c, ")")))) +
  labs(y = expression(paste("Estimated Effect of Head Tax on Immigration"))) + 
  scale_color_manual(name = "Country", breaks = c('Other', 'China', 'Germany', "India"),
                     values = c('Other' = ht, 'China'='red', "Germany" = c2, "India" = c4)) + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/immflow_countries.png"), immflow_countries, height = 4, width = 7)

immflow_countries_slides <- ggplot(graph_countries %>% filter(!(country %in% c("China", "Germany", "India"))), aes(x = taxyear, y = Estimate, color = "Other")) + 
  geom_errorbar(aes(min = est_lb, max = est_ub, color = 'Other'), width = 0, linewidth = 2.8, alpha = 0.2) + geom_point(size=2, shape = 1) +
  geom_errorbar(data = graph_countries %>% filter(country == "China"), aes(min = est_lb, max = est_ub, color = 'China'), width = 0, linewidth = 2.8, alpha = 0.4) + 
  geom_point(data = graph_countries %>% filter(country == "China"), aes(x = taxyear, y = Estimate, color = 'China'), size = 3, shape = 17) +
  geom_errorbar(data = graph_countries %>% filter(country == "Germany"), aes(min = est_lb, max = est_ub, color = 'Germany'), width = 0, linewidth = 2.8, alpha = 0.4) + 
  geom_point(data = graph_countries %>% filter(country == "Germany"), aes(x = taxyear, y = Estimate, color = 'Germany'), size = 3, shape = 15) +
  geom_errorbar(data = graph_countries %>% filter(country == "India"), aes(min = est_lb, max = est_ub, color = 'India'), width = 0, linewidth = 2.8, alpha = 0.4) + 
  geom_point(data = graph_countries %>% filter(country == "India"), aes(x = taxyear, y = Estimate, color = 'India'), size = 3, shape = 18) +
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"= expression(paste("$50 Head Tax (",gamma[50] ^c, ")")),
                                                       "2"= expression(paste("$100 Head Tax (",gamma[100] ^c, ")")),
                                                       "3"= expression(paste("$500 Head Tax (",gamma[500] ^c, ")")))) +
  labs(y = expression(paste("Est. Effect of HT on Imm."))) + 
  scale_color_manual(name = "Country", breaks = c('Other', 'China', 'Germany', "India"),
                     values = c('Other' = ht, 'China'='red', "Germany" = c2, "India" = c4)) + theme_minimal() + theme(legend.position='bottom') +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/figs/slides/immflow_countries.png"), immflow_countries_slides, height = 5, width = 8)

#_____________________________________________________________
# ROBUSTNESS CHECKS --------------------------
#_____________________________________________________________
## REGISTER ----
# using controls for 2yr drops only
flowreg2 <- lm(data = flow_regress %>% filter(YRIMM > 1880 & YRIMM < 1924) %>% mutate(twoyearpost = ifelse(twoyearpost == "_post50", "_notpost", twoyearpost)),
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# controls for both
flowreg2b <- lm(data = flow_regress %>% filter(YRIMM > 1885 & YRIMM < 1924) %>% mutate(twoyearpost = ifelse(twoyearpost == "_post50", "_notpost", twoyearpost)),
                CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) +  factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

## CENSUS ----
# using controls for 2yr drops only
flowcensus2 <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
                  FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# controls for both
flowcensus2b <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
                   FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(tax) +  factor(twoyearpost) + POPSTOCK_China + I(POPSTOCK_China^2))

# sub log flow over pop
flowcensus3 <- lm(data = flow_regress %>% filter(YRIMM > 1879 & YRIMM < 1921), 
                  logFLOWOVERPOP_China ~ IMMFLOW_CENSUS + factor(tax) +  POPSTOCK_China + I(POPSTOCK_China^2))

# flow_regress_old <- yrimm_census %>% group_by(BPL) %>% mutate(n = n()) %>% filter(n > 5) %>% # at least five years of nonzero imm
#   ungroup() %>% select(-c(YEAR,tax,n)) %>% #census data (flows)
#   full_join(popstock, by = c("YRIMM"="YEAR", "BPL")) %>% #population stocks
#   inner_join(maddison_data %>% mutate(Year = as.double(Year)) %>% #source country population and gdp per capita data
#                inner_join(wid_data) %>% #source country inequality data
#                pivot_longer(-Year, names_to = c(".value", "BPL"), names_pattern = "(.*)_([A-z]+)$"),
#              by = c("YRIMM" = "Year", "BPL")) %>%
#   group_by(BPL) %>% arrange(YRIMM) %>%
#   mutate(#FLOW = ifelse(is.na(FLOW) & cumprod(is.na(FLOW)) != 1, min(FLOW, na.rm=TRUE), FLOW),
#     logFLOWOVERPOP = log(FLOW/INTERP_POP), #dividing flows and stocks by source country population
#     POPSTOCK = lag(CANPOP),
#     POPSTOCKOVERPOP = POPSTOCK/INTERP_POP,
#     INTERP_GDPPERCAP = INTERP_GDP/INTERP_POP) %>%
#   pivot_wider(id_cols = c(YRIMM), names_from = BPL,
#               values_from = c(FLOW, CANPOP, INTERP_POP, INTERP_GDPPERCAP, INTERP_INCSHARE50PCT, logFLOWOVERPOP, POPSTOCK)) %>%
#   left_join(yrimm_reg) %>% #register data
#   left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
#   left_join(yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(IMMFLOW_CENSUS = sum(FLOW))) %>%
#   left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
#   left_join(popstock %>% filter(BPL == "ForeignBorn") %>% rename(IMMPOP = CANPOP) %>% select(c(YEAR, IMMPOP)), by = c("YRIMM" = "YEAR")) %>%
#   mutate(across(starts_with("INTERP_GDPPERCAP"), ~ .x/INTERP_GDPPERCAP_Canada, .names = "REL_{.col}"),
#          across(starts_with("INTERP_INCSHARE50PCT"), ~ .x/INTERP_INCSHARE50PCT_Canada, .names = "REL_{.col}"),
#          EMIGOVERPOP = EMIG_TOT/INTERP_POP_China,
#          IMMOVERPOP = IMMFLOW_NATL/IMMPOP,
#          REGFLOWOVERPOP_China = CHIFLOW_REGISTER/INTERP_POP_China,
#          logREGFLOWOVERPOP_China = log(REGFLOWOVERPOP_China),
#          tax = case_when(YRIMM <= 1885 ~ 0,
#                          YRIMM <= 1900 ~ 1496.19,
#                          YRIMM <= 1903 ~ 2992.61,
#                          YRIMM < 1924 ~ 14115.70),
#          twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
#                                  YRIMM %in% c(1901,1902) ~ "post100",
#                                  YRIMM %in% c(1904,1905) ~ "post500",
#                                  TRUE ~ "_notpost"),
#          oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
#                                  YRIMM == 1901 ~ "post100",
#                                  YRIMM == 1904 ~ "post500",
#                                  TRUE ~ "_notpost"))

#_____________________________________________________________
# 9/9/24 RESULTS --------------------------
#_____________________________________________________________

## Register Results ----
# result 1: 1882-1922
reg1 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1922), 
           CHIFLOW_REGISTER ~ factor(tax) + EMIG_TOT + IMMFLOW_NATL +  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

# result 2: 1886-1922
reg2 <- lm(data = flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1922), 
           CHIFLOW_REGISTER ~ factor(tax) + EMIG_TOT + IMMFLOW_NATL +  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

# result 3: log popstock
reg3 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1922), 
           CHIFLOW_REGISTER ~ factor(tax) + EMIG_TOT + IMMFLOW_NATL + log(POPSTOCKLAG_China))

# result 4: log popstock (1886-1922)
reg4 <- lm(data = flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1922), 
           CHIFLOW_REGISTER ~ factor(tax) + EMIG_TOT + IMMFLOW_NATL + log(POPSTOCKLAG_China))

# result 5: linear in tax
reg5 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1922), 
           CHIFLOW_REGISTER ~  tax + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

## Output to Stargazer ----
stargazer(reg1, reg2, reg3, reg4, reg5,
          out = glue("{git}/output/slides/immflow_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.caption = "Dependent Variable: Chinese Immigrant Inflow ($FLOW_{China, t}$)",
          dep.var.labels.include = FALSE,
          single.row = TRUE,
          covariate.labels = c("$\\alpha_0$ (Constant)",
                               "$\\gamma_{50}$ (\\$50 Tax)", 
                               "$\\gamma_{100}$ (\\$100 Tax)", 
                               "$\\gamma_{500}$ (\\$500 Tax)",
                               "$\\gamma^C$ ($TAX_t$ in 2023 USD)",
                               "$\\alpha_{1}$ ($HKEMIG_{t}$)",
                               "$\\alpha_{2}$ ($CANIMMIG_{t}$)",
                               "$\\alpha_{3}$ ($POPSTOCK_{t-1}$)",
                               "$\\alpha_{4}$ ($POPSTOCK_{t-1}^{2})$",
                               "$\\log(POPSTOCK_{t-1})$"),
          table.layout = "=lc#-t-as=")

## Counterfactual graphs and calculations ----
cfreg1 <- cf_flow_calc(flow_regress, reg1)
cfout1 <- cf_flow_predout(cfreg1)
ggsave(glue("{git}/output/slides/cfpred1.png"), cfout1, width = 7, height = 4)

cfreg2 <- cf_flow_calc(flow_regress, reg2)
cfout2 <- cf_flow_predout(cfreg2)
ggsave(glue("{git}/output/slides/cfpred2.png"), cfout2, width = 7, height = 4)

cfreg3 <- cf_flow_calc(flow_regress, reg3, logpopstock = TRUE)
cfout3 <- cf_flow_predout(cfreg3)
ggsave(glue("{git}/output/slides/cfpred3.png"), cfout3, width = 7, height = 4)

cfreg4 <- cf_flow_calc(flow_regress, reg4, startyr = 1886, logpopstock = TRUE)
cfout4 <- cf_flow_predout(cfreg4)
ggsave(glue("{git}/output/slides/cfpred4.png"), cfout4, width = 7, height = 4)

cfreg5 <- cf_flow_calc(flow_regress, reg5, lintax = TRUE)
cfout5 <- cf_flow_predout(cfreg5)
ggsave(glue("{git}/output/slides/cfpred5.png"), cfout5, width = 7, height = 4)


## CENSUS Results ----
# result 1: 1882-1920
cen1 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1920), 
           FLOW_China ~ factor(tax) + EMIG_TOT + IMMFLOW_CENSUS +  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

# result 2: 1886-1922
cen2 <- lm(data = flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1920), 
           FLOW_China ~ factor(tax) + EMIG_TOT + IMMFLOW_CENSUS +  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

# result 3: log popstock
cen3 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1920), 
           FLOW_China ~ factor(tax) + EMIG_TOT + IMMFLOW_CENSUS + log(POPSTOCKLAG_China))

# result 4: log popstock (1886-1922)
cen4 <- lm(data = flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1920), 
           FLOW_China ~ factor(tax) + EMIG_TOT + IMMFLOW_CENSUS + log(POPSTOCKLAG_China))

# result 5: linear in tax
cen5 <- lm(data = flow_regress %>% filter(YRIMM >= 1882 & YRIMM <= 1920), 
           FLOW_China ~  tax + EMIG_TOT + IMMFLOW_CENSUS + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

## Output to Stargazer ----
stargazer(cen1, cen2, cen3, cen4, cen5,
          out = glue("{git}/output/slides/immflow_cen_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.caption = "Dependent Variable: Chinese Immigrant Inflow ($FLOW_{China, t}$)",
          dep.var.labels.include = FALSE,
          single.row = TRUE,
          covariate.labels = c("$\\alpha_0$ (Constant)",
                               "$\\gamma_{50}$ (\\$50 Tax)", 
                               "$\\gamma_{100}$ (\\$100 Tax)", 
                               "$\\gamma_{500}$ (\\$500 Tax)",
                               "$\\gamma^C$ ($TAX_t$ in 2023 USD)",
                               "$\\alpha_{1}$ ($HKEMIG_{t}$)",
                               "$\\alpha_{2}$ ($CANIMMIG_{t}$)",
                               "$\\alpha_{3}$ ($POPSTOCK_{t-1}$)",
                               "$\\alpha_{4}$ ($POPSTOCK_{t-1}^{2})$",
                               "$\\log(POPSTOCK_{t-1})$"),
          table.layout = "=lc#-t-as=")

## Counterfactual graphs and calculations ----
cfcen1 <- cf_flow_calc(flow_regress, cen1, endyr = 1920, cen = TRUE)
cfcenout1 <- cf_flow_predout(cfcen1)
ggsave(glue("{git}/output/slides/cfcenpred1.png"), cfcenout1, width = 7, height = 4)

cfcen2 <- cf_flow_calc(flow_regress, cen2, endyr = 1920, cen = TRUE)
cfcenout2 <- cf_flow_predout(cfcen2)
ggsave(glue("{git}/output/slides/cfcenpred2.png"), cfcenout2, width = 7, height = 4)

cfcen3 <- cf_flow_calc(flow_regress, cen3, endyr = 1920, cen = TRUE, logpopstock = TRUE)
cfcenout3 <- cf_flow_predout(cfcen3)
ggsave(glue("{git}/output/slides/cfcenpred3.png"), cfcenout3, width = 7, height = 4)

cfcen4 <- cf_flow_calc(flow_regress, cen4, endyr = 1920, startyr = 1886, cen = TRUE, logpopstock = TRUE)
cfcenout4 <- cf_flow_predout(cfcen4)
ggsave(glue("{git}/output/slides/cfcenpred4.png"), cfcenout4, width = 7, height = 4)

cfcen5 <- cf_flow_calc(flow_regress, cen5, endyr = 1920, cen = TRUE, lintax = TRUE)
cfcenout5 <- cf_flow_predout(cfcen5)
ggsave(glue("{git}/output/slides/cfcenpred5.png"), cfcenout5, width = 7, height = 4)


## LOG-LOG REGRESSIONS ----
# register
log1 <- lm(data = flow_regress %>% filter(YRIMM >= 1882, YRIMM <= 1922) %>% mutate(tax = tax + 1500),
           log(CHIFLOW_REGISTER) ~ log(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

log2 <- lm(data = flow_regress %>% filter(YRIMM >= 1882, YRIMM <= 1922) %>% mutate(tax = tax + 3000),
           log(CHIFLOW_REGISTER) ~ log(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

log3 <- lm(data = flow_regress %>% filter(YRIMM >= 1886, YRIMM <= 1922) %>% mutate(tax = tax + 1500),
           log(CHIFLOW_REGISTER) ~ log(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

log4 <- lm(data = flow_regress %>% filter(YRIMM >= 1886, YRIMM <= 1922) %>% mutate(tax = tax + 3000),
           log(CHIFLOW_REGISTER) ~ log(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

# census
log5 <- lm(data = flow_regress %>% filter(YRIMM >= 1882, YRIMM <= 1920) %>% mutate(tax = tax + 1500),
           log(FLOW_China) ~ log(tax) + EMIG_TOT + IMMFLOW_CENSUS + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

log6 <- lm(data = flow_regress %>% filter(YRIMM >= 1882, YRIMM <= 1920) %>% mutate(tax = tax + 3000),
           log(FLOW_China) ~ log(tax) + EMIG_TOT + IMMFLOW_CENSUS + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

#stargazer output
stargazer(log1, log2, log3, log4, log5, log6,
          out = glue("{git}/output/slides/immflow_log_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          dep.var.caption = "Dependent Variable: Log Chinese Immigrant Inflow ($\\log(FLOW_{China, t})$)",
          dep.var.labels.include = FALSE,
          single.row = TRUE,
          covariate.labels = c("$\\alpha_0$ (Constant)",
                               "$\\gamma^e$ ($\\log(TAX_t)$ in 2023 USD)",
                               "$\\alpha_{1}$ ($HKEMIG_{t}$)",
                               "$\\alpha_{2}$ ($CANIMMIG_{t}$)",
                               "$\\alpha_{2}$ ($CANIMMIG_{t}$ census)",
                               "$\\alpha_{3}$ ($POPSTOCK_{t-1}$)",
                               "$\\alpha_{4}$ ($POPSTOCK_{t-1}^{2})$"),
          table.layout = "=lc#-t-as=")

## V1: China Only Regressions ----
toggle = "display"
#toggle = "output"

## using register flows
# equation 1: controls for imm, emm, popstock, and tax
regstart = 1882
regend = 1920
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

flowreg1_tax <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
                   CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + tax + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

## using census flows
# equation 1: controls for imm, emm, popstock, and tax
censtart = 1882
cenend = 1920
flowcensus1 <- lm(data = flow_regress %>% filter(YRIMM >= censtart & YRIMM <= cenend), 
                  FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

flowcensus1_tax <- lm(data = flow_regress %>% filter(YRIMM >= censtart & YRIMM <= cenend), 
                      FLOW_China ~ EMIG_TOT + IMMFLOW_CENSUS + tax + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

if (toggle == "display"){
  print(summary(flowreg1))
  print(summary(flowcensus1))
}

reg_mean = round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER), 1)
reg_se = round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/sqrt(regend-regstart),1)
census_mean = round(mean(filter(flow_regress, YRIMM >= censtart & YRIMM <= cenend)$FLOW_China),1)
census_se = round(sd(filter(flow_regress, YRIMM >= censtart & YRIMM <= cenend)$FLOW_China)/sqrt(regend-regstart),1)

n = 2
flowreg_stats = c(rep(glue("{reg_mean} ({reg_se})"),n),rep(glue("{census_mean} ({census_se})"),n))
flowreg_means = c(rep(glue("{reg_mean}"),n),rep(glue("{census_mean}"),n))
flowreg_ses = c(rep(glue("({reg_se})"),n),rep(glue("({census_se})"),n))



