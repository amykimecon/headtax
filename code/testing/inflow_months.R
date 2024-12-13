## FIRST STAGE ANALYSIS: EFFECT OF HEAD TAX ON CHI IMM INFLOWS

#_____________________________________________________________
# TABLE 2: INFLOW REGRESSION --------------------------
#_____________________________________________________________

## Creating Dataset ----
# first combining all long datasets, then normalizing flow and popstock by source country pop,
# then pivoting wide and adding single country datasets
moimm_reg <- reg_chi %>%
  filter(!is.na(DATE)) %>%
  mutate(MOIMM = lubridate::floor_date(DATE, "month")) %>%
  group_by(YRIMM, MOIMM, tax) %>%
  summarize(CHIFLOW_REGISTER = n())

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
  left_join(moimm_reg) %>% #register data
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(IMMFLOW_CENSUS = sum(FLOW))) %>%
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "ForeignBorn") %>% rename(IMMPOP = CANPOP) %>% select(c(YEAR, IMMPOP)), by = c("YRIMM" = "YEAR")) %>%
  mutate(tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         cost = tax + 1496.19,
         month = as.character(month(MOIMM)),
         logFLOW = log(CHIFLOW_REGISTER))

## V1: China Only Regressions ----
regstart = 1882
regend = 1923

# levels + factor tax
flowreg0 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
summary(flowreg0)

# elasticity
flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
                 POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
summary(flowreg1)

regmean1 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW),1)
regse1 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW)/
                  sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
regmean0 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER),1)
regse0 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/
                  sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)

# ht 50
flowreg50 <- lm(data = flow_regress %>% filter(YRIMM >= 1883 & YRIMM <= 1888), 
               logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
                 POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
summary(flowreg50)
regmean50 <- round(mean(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW),1)
regse50 <- round(sd(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW)/
                   sqrt(nrow(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888))-1),1)

flowreg50_v2 <- lm(data = flow_regress %>% filter(YRIMM >= 1883 & YRIMM <= 1888), 
                CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
                  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
regmean50_v2 <- round(mean(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER),1)
regse50_v2 <- round(sd(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER)/
                   sqrt(nrow(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888))-1),1)


# ht 100
flowreg100 <- lm(data = flow_regress %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")), 
                logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
                  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
summary(flowreg100)
regmean100 <- round(mean(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW),1)
regse100 <- round(sd(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW)/
                   sqrt(nrow(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)

flowreg100_v2 <- lm(data = flow_regress %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")), 
                    CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
regmean100_v2 <- round(mean(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER),1)
regse100_v2 <- round(sd(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER)/
                    sqrt(nrow(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)

# ht 500
flowreg500 <- lm(data = flow_regress %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906), 
                 logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
summary(flowreg500)
regmean500 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW),1)
regse500 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW)/
                    sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)

flowreg500_v2 <- lm(data = flow_regress %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906), 
                    CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
regmean500_v2 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER),1)
regse500_v2 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER)/
                    sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)

flowreg_means <- c(regmean1, regmean50, regmean100, regmean500, regmean0)
flowreg_ses <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),glue("({regse0})"))

flowreg_means_v2 <- c(regmean1, regmean50, regmean100, regmean500, regmean0, regmean50_v2, regmean100_v2, regmean500_v2)
flowreg_ses_v2 <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),
                    glue("({regse0})"), glue("({regse50_v2})"),glue("({regse100_v2})"),glue("({regse500_v2})"))

# stargazer
stargazer(list(flowreg1, flowreg50, flowreg100, flowreg500, flowreg0),
          se = list(robustse(flowreg1), robustse(flowreg50), robustse(flowreg100), 
                     robustse(flowreg500), robustse(flowreg0)),
          keep = c("^factor\\(tax\\)","log\\(cost\\)"),
          out = glue("{git}/output/paper/tables/immflow_regs.tex"), 
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)", 
                            "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)", "All Years (1882-1921)"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
                               "$\\gamma_{50}$ (\\$50 Tax)", 
                               "$\\gamma_{100}$ (\\$100 Tax)", 
                               "$\\gamma_{500}$ (\\$500 Tax)"
                               ),
          add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
          dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)", "Monthly Inflow ($F_{t}$)"),
          table.layout = "=cld#-t-as=")
