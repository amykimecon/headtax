### FIRST STAGE ANALYSIS: EFFECT OF HEAD TAX ON CHI IMM INFLOWS ####


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

## GRAPHING COUNTERFACTUAL
flowregcf <- flow_regress %>% mutate(`Actual Inflow` = CHIFLOW_REGISTER,
                                     `Counterfactual Inflow` = -5038.87798039 -0.00413598*EMIG_TOT +0.01910090*IMMFLOW_NATL+0.79610697*POPSTOCK_China-0.00001107*POPSTOCK_China^2)
ggplot(flowregcf %>% filter(YRIMM > 1885 & YRIMM < 1922) %>% 
         pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`), names_to = "cat", values_to = "flow") %>%
         mutate(flow = flow/1000),
       aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
  scale_color_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(c4,c1)) +
  scale_linetype_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(1, 2)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs < 1922), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 13.5, label = labs), data = headtaxcuts %>% filter(yrs < 1922), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/immflow_counterfactual.png"), height = 4, width = 7)

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
          dep.var.caption = "Dependent Variable: Chinese Immigrant Inflow ($FLOW_{China, t}$)",
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

# graph of gamma coefs
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
