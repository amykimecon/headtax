## FIRST STAGE ANALYSIS: EFFECT OF HEAD TAX ON CHI IMM INFLOWS

#_____________________________________________________________
# TABLE 2: INFLOW REGRESSION --------------------------
#_____________________________________________________________
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}
## Creating Dataset ----
# monthly imm inflow, merged with total canadian immigration, total HK emigration, and chinese popstock in canada
moimm_reg <- reg_chi %>%
  filter(!is.na(DATE)) %>%
  mutate(MOIMM = lubridate::floor_date(DATE, "month")) %>%
  group_by(YRIMM, MOIMM, tax, cost, cost2, cost3) %>%
  summarize(CHIFLOW_REGISTER = n(),
            CHIFLOW_REGISTER_TAX = sum(ifelse(FEES > 0 | YRIMM < 1886, 1, 0)), #think more about how to define this
            CHIFLOW_REGISTER_NOTAX = sum(ifelse(FEES == 0 | YRIMM < 1886, 1, 0))) %>%
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "China") %>% mutate(POPSTOCKLAG_China = lag(CANPOP)) %>%
              select(c(YEAR, POPSTOCKLAG_China)), by = c("YRIMM" = "YEAR")) %>%
  ungroup() %>%
  arrange(MOIMM) %>%
  mutate(month = as.character(month(MOIMM)),
         logFLOW = log(CHIFLOW_REGISTER),
         logFLOW_TAX = log(CHIFLOW_REGISTER_TAX + 1), # FIX LATER
         logFLOW_NOTAX = log(CHIFLOW_REGISTER_NOTAX + 1), # FIX LATER
         lagFLOW = lag(logFLOW),
         t = interval(as.Date("1880-01-01"), MOIMM)%/% months(1)) 

#_____________________________________________________________
# FIGURE 2: DETRENDED MONTHLY INFLOW AROUND INCREASES, STACKED ------
#_____________________________________________________________
# takes window of 3 years around each HT increase and gets detrended logFlow (on month FE + const)
#     also gets Fstat for Chow Test (on month FE + const + linear time trend) at each point in window
moimm_stack <- all_disconts(moimm_reg, yvar = "logFLOW")

# graphing raw trends
flow_raw <- graph_disconts(moimm_stack, "logFLOW_RAW", "Raw monthly log Inflow")
ggsave(glue("{git}/output/paper/figures/figa2_flowraw.png"), flow_raw, height = 4, width = 7)

# detrended 6-month MA
flow_detr_ma6 <- graph_disconts(moimm_stack, "logFLOW_DETR", "6-month MA of Detrended monthly log Inflow",
                                rollmean = TRUE, rollk = 6, sampmean = 0)
ggsave(glue("{git}/output/paper/figures/fig2_flowdetr_ma6.png"), flow_detr_ma6, height = 4, width = 7)

#_____________________________________________________________
# TABLE 2: ELASTICITY REGRESSIONS ----
#_____________________________________________________________
## MAIN ----
regstart = 1882
regend = 1908

moimm_reg_filt <- filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)

datasets <- list(moimm_reg_filt,
                 moimm_reg_filt %>% filter(YRIMM >= 1882) %>% 
                   mutate(logFLOW = logFLOW_TAX, CHIFLOW_REGISTER = CHIFLOW_REGISTER_TAX),
                 moimm_reg_filt %>% filter(YRIMM >= 1886) %>% 
                   mutate(logFLOW = logFLOW_NOTAX, CHIFLOW_REGISTER = CHIFLOW_REGISTER_NOTAX),
                 moimm_stack %>% filter(group == "$50 Tax"),
                 moimm_stack %>% filter(group == "$100 Tax"),
                 moimm_stack %>% filter(group == "$500 Tax"))

models = list()
ses = list()
means = list()
i = 1
for (i in 1:length(datasets)){
  reg <- lm(data = datasets[[i]], logFLOW ~ factor(month) + log(cost) + t)
  models[[i]] <- reg
  ses[[i]] <- sqrt(diag(NeweyWest(reg)))
  means[[i]] <- mean(datasets[[i]]$CHIFLOW_REGISTER)
  i = i + 1
}

col_labels = c("All Chi. Imm. (1882-1908)", "Taxpayers (1882-1908)", "Non-Taxpayers (1886-1908)", "\\$50 Tax: All (1883-1888)",
               "\\$100 Tax: All (1898-1903)", "\\$500 Tax: All (1901-1906)")
stargazer(models, se = ses,
          keep = c("log\\(cost\\)"),
          out = glue("{git}/output/paper/tables/immflow_regs_new.tex"),
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          column.labels = c("All Chi. Imm.", "Taxpayers", "Non-Taxpayers", "All ($\\leq$ 3yr from $\\uparrow$\\$50)",
                            "All ($\\leq$ 3yr from $\\uparrow$\\$100)", "All ($\\leq$ 3yr from $\\uparrow$\\$500)"),
          column.separate = c(1,1,1,1,1,1),
          covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)"
          ),
          add.lines = list(c("Time Period", "1882-1908", "1882-1908", "1886-1908", "1883-1888", "1898-1903", "1901-1906"),
                           c("Mean Monthly Inflow ($\\overline{F_t}$)", round(unlist(means), 1))),
          dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)"),
          table.layout = "=cld#-t-as=")


## APPENDIX ----
moimm_reg_filt_app <- moimm_reg_filt %>% mutate(EMIG_TOT = EMIG_TOT/1000,
                                            IMMFLOW_NATL = IMMFLOW_NATL/1000,
                                            POPSTOCKLAG_China = POPSTOCKLAG_China/1000)
moimm_reg_app <- moimm_reg %>% mutate(EMIG_TOT = EMIG_TOT/1000,
                                       IMMFLOW_NATL = IMMFLOW_NATL/1000,
                                       POPSTOCKLAG_China = POPSTOCKLAG_China/1000)

appdatasets <- list(moimm_reg_filt_app,
                 moimm_reg_filt_app,
                 moimm_reg_app %>% filter(YRIMM >= regstart & YRIMM <= 1921),
                 moimm_reg_app %>% filter(YRIMM >= regstart & YRIMM <= 1921),
                 moimm_reg_app %>% filter(YRIMM >= 1886 & YRIMM <= regend),
                 moimm_reg_app %>% filter(YRIMM >= 1886 & YRIMM <= regend),
                 moimm_reg_filt_app,
                 moimm_reg_filt_app)

appctrls <- list("factor(month) + log(cost) + t",
              "factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
                 POPSTOCKLAG_China + I(POPSTOCKLAG_China^2)",
              "factor(month) + log(cost) + t",
              "factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
                 POPSTOCKLAG_China + I(POPSTOCKLAG_China^2)",
              "factor(month) + log(cost) + t",
              "factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
                 POPSTOCKLAG_China + I(POPSTOCKLAG_China^2)",
              "factor(month) + log(cost2) + t",
              "factor(month) + log(cost3) + t")

appmodels = list()
appses = list()
appmeans = list()
i = 1
for (i in 1:length(appdatasets)){
  reg <- lm(data = appdatasets[[i]], glue("logFLOW ~ {appctrls[[i]]}"))
  appmodels[[i]] <- reg
  appses[[i]] <- sqrt(diag(NeweyWest(reg)))
  appmeans[[i]] <- mean(appdatasets[[i]]$CHIFLOW_REGISTER)
  i = i + 1
}

stargazer(appmodels, se = appses,
          keep = c("log\\(cost\\)", "log\\(cost2\\)", "log\\(cost3\\)"),
          out = glue("{git}/output/paper/tables/app_immflow_regs.tex"),
          float = FALSE,
          digits = 2,
          intercept.bottom = FALSE,
          keep.stat=c("n","adj.rsq"),
          column.labels = c("All Chi. Imm.", "All Chi. Imm.", "All (to 1921)", "All (to 1921)",
                            "All (from 1886)", "All (from 1886)", "All", "All"),
          column.separate = c(1,1,1,1,1,1),
          covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
                               "$\\gamma^{\\varepsilon}_{lowcost}$ (log Cost)",
                               "$\\gamma^{\\varepsilon}_{highcost}$ (log Cost)"
          ),
          add.lines = list(c("Linear Time Trend", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "Yes"),
                           c("Addtl. Push/Pull Ctrls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "No"),
                           c("Time Period", "1882-1908", "1882-1908", "1882-1921", "1882-1921", 
                             "1886-1908", "1886-1908", "1882-1908", "1882-1908"),
                           c("Mean Monthly Inflow ($\\overline{F_t}$)", round(unlist(appmeans), 1))),
          dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)"),
          table.layout = "=cld#-t-as=")

# # OLD STUFF
# 
# # col a1: same as col 1
# 
# # col a2: full model (imm/emm, popstock)
# 
# # col a3: full sample (-1923) time trend only
# 
# # col a4: full sample (-1923) full model
# 
# # col a5: late sample (1886-1908) time trend only
# 
# # col a6: late sample (1886-1908) full model
# 
# # col a7: alt cost $25
# 
# # col a8: alt cost $75
# 
# # col a9: nonparam?? (factor tax)
# 
# 
# ## MAIN TABLE REGS ----
# # helper: for each outcome variable, runs inflow regression for main spec, $50 HT, $100 HT, $500 HT
# flowregs_out <- function(df_full, df_stack, yvar, incl50 = TRUE){
#   dfs <- list(df_full, 
#               df_stack %>% filter(group == "$50 Tax"),
#               df_stack %>% filter(group == "$100 Tax"),
#               df_stack %>% filter(group == "$500 Tax"))
#   
#   models = list()
#   ses = list()
#   i = 1
#   for (i in 1:length(dfs)){
#     if (i != 2 | incl50){
#       reg <- lm(data = dfs[[i]], glue("{yvar} ~ factor(month) + log(cost) + t"))
#       models[[i]] <- reg
#       ses[[i]] <- sqrt(diag(NeweyWest(reg)))
#       i = i + 1
#     }
#   }
#   
#   return(list(models,ses))
# }
# 
# 
# for (yvar in c("logFLOW", "logFLOW_TAX", "logFLOW_NOTAX")){
#   regout <- flowregs_out(moimm_reg_filt, moimm_stack, yvar, ifelse(yvar == "logFLOW", TRUE, FALSE))
#   stargazer(regout[[1]], se = regout[[2]], keep = c("log\\(cost\\)"), digits = 2, intercept.bottom = FALSE,
#             keep.stat = c("n", "adj.rsq"),
#             column.labels = c("All Years ", "\\$50 Tax (1883-1888)",
#                               "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)"),
#             column.separate = c(1,1,1,1),
#             covariate.labels=c("$\\gamma^{\\varepsilon}$ (log Cost)"),
#             dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)"),
#             table.layout = "=cld#-t-as=")
# }
# 
# stargazer(list(flowreg1, flowreg50, flowreg100, flowreg500, flowreg0),
#           se = list(robustse(flowreg1), robustse(flowreg50), robustse(flowreg100),
#                     robustse(flowreg500), robustse(flowreg0)),
#           keep = c("log\\(cost\\)"),
#           #out = glue("{git}/output/paper/tables/immflow_regs_new.tex"),
#           float = FALSE,
#           digits = 2,
#           intercept.bottom = FALSE,
#           keep.stat=c("n","adj.rsq"),
#           column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)",
#                             "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)", "All Years (1882-1921)"),
#           column.separate = c(1,1,1,1,1),
#           covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
#                                "$\\gamma_{50}$ (\\$50 Tax)",
#                                "$\\gamma_{100}$ (\\$100 Tax)",
#                                "$\\gamma_{500}$ (\\$500 Tax)"
#           ),
#           add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
#           dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)", "Monthly Inflow ($F_{t}$)"),
#           table.layout = "=cld#-t-as=")
# 
# 
# 
# 
# flowregtex <- file(glue("{tabs}/immflow_regs_new.tex"), open = "w")
# 
# # writing table header
# writeLines(c("\\begin{tabular}{lcccc}", 
#              "\\hhline{======}", 
#              "& \\multicolumn{5}{c}{\\textit{Dependent variable:} log Monthly Chinese Immigrant Inflow ($\\log F_t$)} \\\\ ", 
#              "\\hhline{~-~---}", "& (1) & & (2) & (3) & (4)\\\\ ",
#              glue("& All Years & & \\$50 Tax (1883-1888) & \\$100 Tax (1898-1903) & \\$500 Tax (1901-1906) \\\\ "), 
#              " \\hhline{------}"), flowregtex)
# 
# # iterating through each yvar
# for (yvar in c("logFLOW", "logFLOW_TAX", "logFLOW_NOTAX")){
#   regout <- flowregs_out(moimm_reg_filt, moimm_stack, yvar, ifelse(yvar == "logFLOW", TRUE, FALSE))
#   writeLines(c("\\multicolumn{6}{l}{\\textbf{Sample A: All Immigrants (1882-1908)}} \\\\",
#                paste0(glue_collapse(c("hi",round(unlist(regout[[1]]), 2)), sep = " & "), "\\\\")
#   )
#   )  
# }
# 
# # stargazer
# stargazer(list(flowreg1, flowreg50, flowreg100, flowreg500, flowreg0),
#           se = list(robustse(flowreg1), robustse(flowreg50), robustse(flowreg100),
#                     robustse(flowreg500), robustse(flowreg0)),
#           keep = c("^factor\\(tax\\)","log\\(cost\\)"),
#           out = glue("{git}/output/paper/tables/immflow_regs.tex"),
#           float = FALSE,
#           digits = 2,
#           intercept.bottom = FALSE,
#           keep.stat=c("n","adj.rsq"),
#           column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)",
#                             "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)", "All Years (1882-1921)"),
#           column.separate = c(1,1,1,1,1),
#           covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
#                                "$\\gamma_{50}$ (\\$50 Tax)",
#                                "$\\gamma_{100}$ (\\$100 Tax)",
#                                "$\\gamma_{500}$ (\\$500 Tax)"
#           ),
#           add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
#           dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)", "Monthly Inflow ($F_{t}$)"),
#           table.layout = "=cld#-t-as=")
# # iterating through lines
# ind = 1
# for (i in 1:length(summ_cats)){
#   # skip category rownames
#   if(summ_cats[i] == "Age at Immigration (\\%)" | summ_cats[i] == "Year of Immigration (\\%)"){
#     writeLines(glue_collapse(c(summ_cats[i],rep("&",5), "\\\\")), summtex)
#   }else{
#     means <- ifelse(is.na(summ_stats_out[2*ind+1,]) | as.numeric(summ_stats_out[2*ind+1,])==0, "-", formatC(as.numeric(summ_stats_out[2*ind+1,]), digits = 4))
#     writeLines(c(paste(summ_cats[i], "&", glue_collapse(c(means[1:2],"",means[3:4]), sep = "&", last = ""), "\\\\ ")), summtex) 
#     ind = ind + 1
#   }
# }
# 
# # writing number of observations for each sample
# obs <- ifelse(is.na(summ_stats_out[2*ind+1,]), "-", formatC(as.numeric(summ_stats_out[2*ind+1,]), format = "d", big.mark = ","))
# writeLines(c("Obs.", "&", glue_collapse(c(obs[1:2],"",obs[3:4]), sep = "&", last = ""), "\\\\ "), summtex)
# writeLines(c("\\hhline{------}","\\end{tabular}"), summtex)
# close(summtex)
# 
# x <- flowregs_out(moimm_reg_filt, moimm_stack, "logFLOW")
# 
# 
# #HELPER TO OUTPUT RESULTS
# report_flowregs <- function(df, taxpayerincl = TRUE){
#   #all
#   r1 <- lm(data = df, logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
#   r2 <- lm(data = df, logFLOW ~ factor(month) + log(cost) + t)
#   
#   print("Full Sample, $50 Baseline Cost")
#   print(glue("Full Model: {round(r1$coefficients['log(cost)'],2)} ({round(summary(r1)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r1)$r.squared,2)}]"))
#   print(glue("t Only: {round(r2$coefficients['log(cost)'],2)} ({round(summary(r2)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r2)$r.squared,2)}]"))
#   
#   #taxpayers
#   if (taxpayerincl){
#     r3 <- lm(data = df %>% filter(YRIMM >= 1886), ihs(CHIFLOW_REGISTER_TAX) ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
#     r4 <- lm(data = df%>% filter(YRIMM >= 1886), ihs(CHIFLOW_REGISTER_TAX) ~ factor(month) + log(cost) + t)
#     
#     print("Taxpayers Only, $50 Baseline Cost")
#     print(glue("Full Model: {round(r3$coefficients['log(cost)'],2)} ({round(summary(r3)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r3)$r.squared,2)}]"))
#     print(glue("t Only: {round(r4$coefficients['log(cost)'],2)} ({round(summary(r4)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r4)$r.squared,2)}]"))
#     
#     #non taxpayers
#     r5 <- lm(data = df%>% filter(YRIMM >= 1886), ihs(CHIFLOW_REGISTER_NOTAX) ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
#     r6 <- lm(data = df%>% filter(YRIMM >= 1886), ihs(CHIFLOW_REGISTER_NOTAX) ~ factor(month) + log(cost) + t)
#     
#     print("Non-Taxpayers Only, $50 Baseline Cost")
#     print(glue("Full Model: {round(r5$coefficients['log(cost)'],2)} ({round(summary(r5)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r5)$r.squared,2)}]"))
#     print(glue("t Only: {round(r6$coefficients['log(cost)'],2)} ({round(summary(r6)$coefficients['log(cost)','Std. Error'], 2)}) [R Squared: {round(summary(r6)$r.squared,2)}]"))
#     
#   }
#   
#   #alt costs: 25
#   r7 <- lm(data = df, logFLOW ~ factor(month) + log(cost2) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
#   r8 <- lm(data = df, logFLOW ~ factor(month) + log(cost2) + t)
#   
#   print("Full Sample, $25 Baseline Cost")
#   print(glue("Full Model: {round(r7$coefficients['log(cost2)'],2)} ({round(summary(r7)$coefficients['log(cost2)','Std. Error'], 2)}) [R Squared: {round(summary(r7)$r.squared,2)}]"))
#   print(glue("t Only: {round(r8$coefficients['log(cost2)'],2)} ({round(summary(r8)$coefficients['log(cost2)','Std. Error'], 2)}) [R Squared: {round(summary(r8)$r.squared,2)}]"))
#   
#   #alt costs: 75
#   r9 <- lm(data = df, logFLOW ~ factor(month) + log(cost3) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
#   r10 <- lm(data = df, logFLOW ~ factor(month) + log(cost3) + t)
#   
#   print("Full Sample, $75 Baseline Cost")
#   print(glue("Full Model: {round(r9$coefficients['log(cost3)'],2)} ({round(summary(r9)$coefficients['log(cost3)','Std. Error'], 2)}) [R Squared: {round(summary(r9)$r.squared,2)}]"))
#   print(glue("t Only: {round(r10$coefficients['log(cost3)'],2)} ({round(summary(r10)$coefficients['log(cost3)','Std. Error'], 2)}) [R Squared: {round(summary(r10)$r.squared,2)}]"))
#   
# }
# 
# regstart = 1882
# regend = 1908
# 
# moimm_reg_filt <- filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)
# print("MAIN SAMPLE: 1882-1908")
# report_flowregs(moimm_reg_filt)
# 
# moimm_reg_full <- filter(moimm_reg, YRIMM >= 1882 & YRIMM <= 1923)
# print("FULL SAMPLE: 1882-1923")
# report_flowregs(moimm_reg_full)
# 
# moimm_reg_50 <- moimm_stack %>% filter(group == "$50 Tax")
# print("WINDOW AROUND $50 ONLY")
# report_flowregs(moimm_reg_50, taxpayerincl = FALSE)
# 
# moimm_reg_100 <- moimm_stack %>% filter(group == "$100 Tax")
# print("WINDOW AROUND $100 ONLY")
# report_flowregs(moimm_reg_100)
# 
# moimm_reg_500 <- moimm_stack %>% filter(group == "$500 Tax")
# print("WINDOW AROUND $500 ONLY")
# report_flowregs(moimm_reg_500)
# 
# 
# 
# # levels + factor tax
# flowreg0 <- lm(data = moimm_reg_filt,
#                CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg0)
# 
# # elasticity
# flowreg1 <- lm(data = moimm_reg_filt,
#                logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg1)
# 
# ## ALTERNATE VERSIONS
# # moimm ctrl only
# flowregm1 <- lm(data = moimm_reg_filt,
#                 logFLOW ~ factor(month) + log(cost) + t)
# summary(flowregm1)
# 
# # all years of imm until 1923
# flowrega1 <- lm(data = moimm_reg %>% filter(YRIMM >= 1882 & YRIMM <= 1923),
#                 logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowrega1)
# 
# # taxpayers only
# flowrega2 <- lm(data = moimm_reg_filt %>% filter(YRIMM >= 1886),
#                 ihs(CHIFLOW_REGISTER_TAX) ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowrega2)
# 
# flowrega2alt <- lm(data = moimm_reg_filt %>% filter(YRIMM >= 1886),
#                    ihs(CHIFLOW_REGISTER_TAX) ~ factor(month) + log(cost) + t)
# summary(flowrega2alt)
# 
# # non-taxpayers only
# flowrega3 <- lm(data = moimm_reg_filt %>% filter(YRIMM >= 1886),
#                 ihs(CHIFLOW_REGISTER_NOTAX) ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowrega3)
# 
# flowrega3alt <- lm(data = moimm_reg_filt %>% filter(YRIMM >= 1886),
#                    ihs(CHIFLOW_REGISTER_NOTAX) ~ factor(month) + log(cost) + t)
# summary(flowrega3alt)
# 
# # cost = 25
# flowrega4 <- lm(data = moimm_reg_filt,
#                 logFLOW ~ factor(month) + log(cost2) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowrega4)
# 
# flowrega4alt <- lm(data = moimm_reg_filt,
#                    logFLOW ~ factor(month) + log(cost2) + t)
# summary(flowrega4alt)
# 
# # cost = 75
# flowrega5 <- lm(data = moimm_reg_filt,
#                 logFLOW ~ factor(month) + log(cost3) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowrega5)
# 
# flowrega5alt <- lm(data = moimm_reg_filt,
#                    logFLOW ~ factor(month) + log(cost3) + t)
# summary(flowrega5alt)
# 
# ## BY INDIV HEAD TAX
# # $50 HEAD TAX
# flowreg50 <- lm(data = moimm_stack %>% filter(group == "$50 Tax"),
#                 logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg50)
# 
# flowreg50alt <- lm(data = moimm_stack %>% filter(group == "$50 Tax"),
#                    logFLOW ~ factor(month) + log(cost) + t)
# summary(flowreg50alt)
# 
# # $100 HEAD TAX
# flowreg100 <- lm(data = moimm_stack %>% filter(group == "$100 Tax"),
#                  logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg100)
# 
# flowreg100alt <- lm(data = moimm_stack %>% filter(group == "$100 Tax"),
#                     logFLOW ~ factor(month) + log(cost) + t)
# summary(flowreg100alt)
# 
# # $500 HEAD TAX
# flowreg500 <- lm(data = moimm_stack %>% filter(group == "$500 Tax"),
#                  logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg500)
# 
# flowreg500alt <- lm(data = moimm_stack %>% filter(group == "$500 Tax"),
#                     logFLOW ~ factor(month) + log(cost) + t)
# summary(flowreg500alt)
# 
# flowreg1_raw <- lm(data = moimm_reg %>% filter(YRIMM >= regstart & YRIMM <= regend),
#                    logFLOW ~ factor(month) + log(cost) + MOIMM)
# summary(flowreg1_raw)
# 
# regmean1 <- round(mean(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)$logFLOW),1)
# regse1 <- round(sd(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)$logFLOW)/
#                   sqrt(nrow(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
# regmean0 <- round(mean(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER),1)
# regse0 <- round(sd(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/
#                   sqrt(nrow(filter(moimm_reg, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
# 
# # ht 50
# flowreg50 <- lm(data = moimm_reg %>% filter(YRIMM >= 1883 & YRIMM <= 1888),
#                 logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg50)
# regmean50 <- round(mean(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW),1)
# regse50 <- round(sd(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW)/
#                    sqrt(nrow(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888))-1),1)
# 
# flowreg50 <- lm(data = moimm_reg %>% filter(YRIMM >= 1883 & YRIMM <= 1888),
#                 logFLOW ~ factor(month) + log(cost) + MOIMM + I(MOIMM^2))
# summary(flowreg50)
# 
# regmean50_v2 <- round(mean(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER),1)
# regse50_v2 <- round(sd(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER)/
#                       sqrt(nrow(filter(moimm_reg, YRIMM >= 1883 & YRIMM <= 1888))-1),1)
# 
# 
# # ht 100
# flowreg100 <- lm(data = moimm_reg %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")),
#                  logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg100)
# regmean100 <- round(mean(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW),1)
# regse100 <- round(sd(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW)/
#                     sqrt(nrow(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)
# 
# flowreg100 <- lm(data = moimm_reg %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")),
#                  logFLOW ~ factor(month) + log(cost) + MOIMM + MOIMM^2)
# summary(flowreg100)
# 
# regmean100_v2 <- round(mean(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER),1)
# regse100_v2 <- round(sd(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER)/
#                        sqrt(nrow(filter(moimm_reg, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)
# 
# # ht 500
# flowreg500 <- lm(data = moimm_reg %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906),
#                  logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL +
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg500)
# regmean500 <- round(mean(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW),1)
# regse500 <- round(sd(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW)/
#                     sqrt(nrow(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)
# 
# flowreg500_v2 <- lm(data = moimm_reg %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906),
#                     CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL +
#                       POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# regmean500_v2 <- round(mean(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER),1)
# regse500_v2 <- round(sd(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER)/
#                        sqrt(nrow(filter(moimm_reg, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)
# 
# flowreg_means <- c(regmean1, regmean50, regmean100, regmean500, regmean0)
# flowreg_ses <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),glue("({regse0})"))
# 
# flowreg_means_v2 <- c(regmean1, regmean50, regmean100, regmean500, regmean0, regmean50_v2, regmean100_v2, regmean500_v2)
# flowreg_ses_v2 <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),
#                     glue("({regse0})"), glue("({regse50_v2})"),glue("({regse100_v2})"),glue("({regse500_v2})"))
# 
# # stargazer
# stargazer(list(flowreg1, flowreg50, flowreg100, flowreg500, flowreg0),
#           se = list(robustse(flowreg1), robustse(flowreg50), robustse(flowreg100),
#                     robustse(flowreg500), robustse(flowreg0)),
#           keep = c("^factor\\(tax\\)","log\\(cost\\)"),
#           out = glue("{git}/output/paper/tables/immflow_regs.tex"),
#           float = FALSE,
#           digits = 2,
#           intercept.bottom = FALSE,
#           keep.stat=c("n","adj.rsq"),
#           column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)",
#                             "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)", "All Years (1882-1921)"),
#           column.separate = c(1,1,1,1,1),
#           covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
#                                "$\\gamma_{50}$ (\\$50 Tax)",
#                                "$\\gamma_{100}$ (\\$100 Tax)",
#                                "$\\gamma_{500}$ (\\$500 Tax)"
#           ),
#           add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
#           dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)", "Monthly Inflow ($F_{t}$)"),
#           table.layout = "=cld#-t-as=")
# 
# 

# 
# startyr = 1880
# endyr = 1924
# moimm1 <- moimm_reg %>% mutate(MONTH = month(MOIMM)) %>% filter(YRIMM >= startyr & YRIMM <= endyr)
# moimm_mo <- lm(CHIFLOW_REGISTER ~ factor(MONTH), data = moimm1)
# moimm1$FLOW_DETR <- resid(moimm_mo)
# ggplot(data = moimm1,
#        aes(x = MOIMM, y = FLOW_DETR)) + geom_line() +
#   geom_vline(aes(xintercept = dates), data = headtaxcuts_month %>% filter(year(dates) < endyr & year(dates) > startyr), 
#              show.legend = FALSE, color = "#808080", linetype = 3) +
#   geom_text(aes(x = dates, y = 1000, label = labs), data = headtaxcuts_month %>% filter(year(dates) < endyr & year(dates) > startyr), 
#             inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
#   labs(x = "Year of Immigration") + theme_minimal() + theme(legend.position='bottom')
# 
# 

# flow_regress <- yrimm_census %>% group_by(BPL) %>% mutate(n = n()) %>% filter(n > 5) %>% # at least five years of nonzero imm
#   ungroup() %>% select(-c(YEAR,tax,n)) %>% #census data (flows)
#   full_join(popstock, by = c("YRIMM"="YEAR", "BPL")) %>% #population stocks
#   inner_join(maddison_data %>% mutate(Year = as.double(Year)) %>% #source country population and gdp per capita data
#                inner_join(wid_data) %>% #source country inequality data
#                pivot_longer(-Year, names_to = c(".value", "BPL"), names_pattern = "(.*)_([A-z]+)$"),
#              by = c("YRIMM" = "Year", "BPL")) %>%
#   group_by(BPL) %>% arrange(YRIMM) %>% 
#   mutate(logFLOWOVERPOP = log(FLOW/INTERP_POP), #dividing flows and stocks by source country population
#          POPSTOCKLAG = lag(CANPOP),
#          POPSTOCKLAGOVERPOP = POPSTOCKLAG/INTERP_POP,
#          INTERP_GDPPERCAP = INTERP_GDP/INTERP_POP) %>%
#   pivot_wider(id_cols = c(YRIMM), names_from = BPL,
#               values_from = c(FLOW, CANPOP, INTERP_POP, INTERP_GDPPERCAP, INTERP_INCSHARE50PCT, logFLOWOVERPOP, 
#                               POPSTOCKLAG, POPSTOCKLAGOVERPOP)) %>%
#   left_join(moimm_reg) %>% #register data
#   left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
#   left_join(yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(IMMFLOW_CENSUS = sum(FLOW))) %>%
#   left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
#   left_join(popstock %>% filter(BPL == "ForeignBorn") %>% rename(IMMPOP = CANPOP) %>% select(c(YEAR, IMMPOP)), by = c("YRIMM" = "YEAR")) %>%
#   mutate(month = as.character(month(MOIMM)),
#          logFLOW = log(CHIFLOW_REGISTER))
# 
# ## V1: China Only Regressions ----
# regstart = 1882
# regend = 1923
# 
# # levels + factor tax
# flowreg0 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
#                CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg0)
# 
# # elasticity
# flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
#                logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
#                  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg1)
# 
# regmean1 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW),1)
# regse1 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW)/
#                   sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
# regmean0 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER),1)
# regse0 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/
#                   sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
# 
# # ht 50
# flowreg50 <- lm(data = flow_regress %>% filter(YRIMM >= 1883 & YRIMM <= 1888), 
#                logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
#                  POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg50)
# regmean50 <- round(mean(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW),1)
# regse50 <- round(sd(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW)/
#                    sqrt(nrow(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888))-1),1)
# 
# flowreg50_v2 <- lm(data = flow_regress %>% filter(YRIMM >= 1883 & YRIMM <= 1888), 
#                 CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# regmean50_v2 <- round(mean(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER),1)
# regse50_v2 <- round(sd(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$CHIFLOW_REGISTER)/
#                    sqrt(nrow(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888))-1),1)
# 
# 
# # ht 100
# flowreg100 <- lm(data = flow_regress %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")), 
#                 logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
#                   POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg100)
# regmean100 <- round(mean(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW),1)
# regse100 <- round(sd(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW)/
#                    sqrt(nrow(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)
# 
# flowreg100_v2 <- lm(data = flow_regress %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")), 
#                     CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# regmean100_v2 <- round(mean(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER),1)
# regse100_v2 <- round(sd(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$CHIFLOW_REGISTER)/
#                     sqrt(nrow(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)
# 
# # ht 500
# flowreg500 <- lm(data = flow_regress %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906), 
#                  logFLOW ~ factor(month) + log(cost) + EMIG_TOT + IMMFLOW_NATL + 
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# summary(flowreg500)
# regmean500 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW),1)
# regse500 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW)/
#                     sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)
# 
# flowreg500_v2 <- lm(data = flow_regress %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906), 
#                     CHIFLOW_REGISTER ~ factor(month) + factor(tax) + EMIG_TOT + IMMFLOW_NATL + 
#                    POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))
# regmean500_v2 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER),1)
# regse500_v2 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER)/
#                     sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)
# 
# flowreg_means <- c(regmean1, regmean50, regmean100, regmean500, regmean0)
# flowreg_ses <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),glue("({regse0})"))
# 
# flowreg_means_v2 <- c(regmean1, regmean50, regmean100, regmean500, regmean0, regmean50_v2, regmean100_v2, regmean500_v2)
# flowreg_ses_v2 <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),
#                     glue("({regse0})"), glue("({regse50_v2})"),glue("({regse100_v2})"),glue("({regse500_v2})"))
# 
# # stargazer
# stargazer(list(flowreg1, flowreg50, flowreg100, flowreg500, flowreg0),
#           se = list(robustse(flowreg1), robustse(flowreg50), robustse(flowreg100), 
#                      robustse(flowreg500), robustse(flowreg0)),
#           keep = c("^factor\\(tax\\)","log\\(cost\\)"),
#           out = glue("{git}/output/paper/tables/immflow_regs.tex"), 
#           float = FALSE,
#           digits = 2,
#           intercept.bottom = FALSE,
#           keep.stat=c("n","adj.rsq"),
#           column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)", 
#                             "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)", "All Years (1882-1921)"),
#           column.separate = c(1,1,1,1,1),
#           covariate.labels = c("$\\gamma^{\\varepsilon}$ (log Cost)",
#                                "$\\gamma_{50}$ (\\$50 Tax)", 
#                                "$\\gamma_{100}$ (\\$100 Tax)", 
#                                "$\\gamma_{500}$ (\\$500 Tax)"
#                                ),
#           add.lines = list(c("Dep. Var. Mean (SE)", flowreg_means),c("", flowreg_ses)),
#           dep.var.labels = c("log Monthly Chinese Immigrant Inflow ($\\log(F_{t})$)", "Monthly Inflow ($F_{t}$)"),
#           table.layout = "=cld#-t-as=")
