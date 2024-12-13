# BREAKPOINT DETECTION

# number of chinese immigrants by month 
moimm_reg <- reg_chi %>%
  filter(!is.na(DATE)) %>%
  mutate(MOIMM = lubridate::floor_date(DATE, "month")) %>%
  group_by(YRIMM, MOIMM, tax) %>%
  summarize(CHIFLOW_REGISTER = n()) %>%
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "China") %>% mutate(POPSTOCKLAG_China = lag(CANPOP)) %>%
              select(c(YEAR, POPSTOCKLAG_China)), by = c("YRIMM" = "YEAR")) %>%
  ungroup() %>%
  arrange(MOIMM) %>%
  mutate(tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         cost = tax + 1496.19,
         month = as.character(month(MOIMM)),
         logFLOW = log(CHIFLOW_REGISTER),
         lagFLOW = lag(logFLOW)) 

# number of chinese immigrants by n months
nmoimm_reg <- reg_chi %>%
  mutate(MOIMM = lubridate::floor_date(DATE, "3 months")) %>%
  group_by(YRIMM, MOIMM, tax) %>%
  summarize(CHIFLOW_REGISTER = n()) %>%
  left_join(immflow, by = c("YRIMM" = "Year")) %>% #total immigration inflow into canada
  left_join(hk_departure, by = c("YRIMM" = "YEAR")) %>% #total emigration outflow from hong kong
  left_join(popstock %>% filter(BPL == "China") %>% mutate(POPSTOCKLAG_China = lag(CANPOP)) %>%
              select(c(YEAR, POPSTOCKLAG_China)), by = c("YRIMM" = "YEAR")) %>%
  mutate(tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70),
         cost = tax + 1496.19,
         month = as.character(month(MOIMM))) %>%
  arrange(MOIMM)


chowgraphdata_month <- function(df, k=2, controls = ""){
  n = nrow(df)
  fstat <- numeric(n)
  pval <- numeric(n)
  critval <- numeric(n)
  mos <- numeric(n)
  for (i in (k+1):(n-k-1)){
    #print(df$MOIMM[i])
    chowtest <- sctest(as.formula(paste0("log(CHIFLOW_REGISTER) ~ factor(MONTH) + YRIMM",controls)), 
                       data = df, type = "Chow", point = i)
    mos[i] <- df$MOIMM[i]
    fstat[i] <- chowtest$statistic
    pval[i] <- chowtest$p.value
    critval[i] <- qf(0.95, k, n-(2*k))
    #print(chowtest$statistic)
    #print(chowtest$p.value)
  }
  #print(mos)
  chow_out <- data.frame(fstat = fstat, pval = pval, critval = critval, MOIMM = as.Date(mos)) %>% 
    filter(MOIMM != 0)
  return(chow_out)
}

qlrtest_month <- function(df, k=2, controls = ""){
  qlr <- Fstats(logFLOW ~ factor(month) + MOIMM, data = df, from = k)
  print(df$MOIMM[qlr$breakpoint])
  return(qlr)
}

bp_detect <- function(bp, bw, startyr = year(bp - 366*bw), endyr = year(bp + 366*bw),
                      df = moimm_reg, breaks = 1){
  #print(startyr)
  #print(endyr)
  df_filt <- df %>% mutate(MONTH = month(MOIMM)) %>% filter(YRIMM >= startyr & YRIMM <= endyr)
  bp_out <- breakpoints(log(CHIFLOW_REGISTER) ~ 1, data = df_filt, h = 13, breaks = breaks) 
  print(df_filt$MOIMM[bp_out$breakpoints])
  # sctest(CHIFLOW_REGISTER ~ factor(MONTH), point = which(df_filt$MOIMM == bp), type = "Chow", 
  #        data = df_filt)
  return(chowgraphdata_month(df_filt, k = 14))
}

x <- bp_detect(as.Date("1895-01-01"), 13, breaks = 3)
x <- bp_detect(as.Date("1895-01-01"), 13, breaks = 3, df = nmoimm_reg)

chowdata_out <- bind_rows(list(bp_detect(as.Date("1886-01-01"), 2) %>% mutate(type = "50 tax"),
                               bp_detect(as.Date("1901-01-01"), 2) %>% mutate(type = "100 tax"),
                               bp_detect(as.Date("1904-01-01"), 2) %>% mutate(type = "500 tax")))
           
ggplot(chowdata_out, aes(x = MOIMM, y = fstat, color = type)) + 
  geom_point() + geom_line(aes(y = critval)) 


startyr = 1880
endyr = 1924
moimm1 <- moimm_reg %>% mutate(MONTH = month(MOIMM)) %>% filter(YRIMM >= startyr & YRIMM <= endyr)
moimm_mo <- lm(CHIFLOW_REGISTER ~ factor(MONTH), data = moimm1)
moimm1$FLOW_DETR <- resid(moimm_mo)
ggplot(data = moimm1,
       aes(x = MOIMM, y = FLOW_DETR)) + geom_line() +
  geom_vline(aes(xintercept = dates), data = headtaxcuts_month %>% filter(year(dates) < endyr & year(dates) > startyr), 
             show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = dates, y = 1000, label = labs), data = headtaxcuts_month %>% filter(year(dates) < endyr & year(dates) > startyr), 
            inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration") + theme_minimal() + theme(legend.position='bottom')

# MONTH REGRESSIONS
regstart = 1880
regend = 1923
flowreg0 <- lm(data = moimm_reg %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ month + EMIG_TOT + IMMFLOW_NATL + log(POPSTOCKLAG_China))

summary(lm(data = moimm_reg %>% filter(YRIMM>= regstart & YRIMM <= regend),
           logFLOW ~ factor(month) + lagFLOW))

flowreg1 <- lm(data = moimm_reg %>%filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ month + EMIG_TOT + IMMFLOW_NATL + factor(tax) + log(POPSTOCKLAG_China))

summary(lm(data = moimm_reg %>%filter(YRIMM >= regstart & YRIMM <= regend), 
           CHIFLOW_REGISTER ~ month + lag(CHIFLOW_REGISTER)*factor(tax))
)

flowreg1 <- lm(data = moimm_reg %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               log(CHIFLOW_REGISTER) ~ month + EMIG_TOT + IMMFLOW_NATL + log(cost) + log(POPSTOCKLAG_China))

summary(lm(data = moimm_reg %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               log(CHIFLOW_REGISTER) ~ month + log(cost) + lag(logFLOW, 12)))

coeftest(flowreg1, vcov. = vcovHC(flowreg1, vcov = "HC1"))

## STACKED DESIGN
moimm_stack <- all_disconts(moimm_reg, yvar = "logFLOW")
flow_detr_final <- graph_disconts(moimm_stack, "logFLOW_DETR", "Detrended monthly log Inflow")
flow_detr_ma3 <- graph_disconts(moimm_stack, "logFLOW_DETR", "3-month MA of Detrended monthly log Inflow", 
                                rollmean = TRUE, rollk = 3)
flow_detr_ma6 <- graph_disconts(moimm_stack, "logFLOW_DETR", "6-month MA of Detrended monthly log Inflow", 
                                rollmean = TRUE, rollk = 6)
ggsave(glue("{git}/output/paper/figures/fig2_flowdetr.png"), flow_detr_final, height = 4, width = 7)
ggsave(glue("{git}/output/paper/figures/fig2_flowdetr_ma3.png"), flow_detr_ma3, height = 4, width = 7)
ggsave(glue("{git}/output/paper/figures/fig2_flowdetr_ma6.png"), flow_detr_ma6, height = 4, width = 7)

qlrfstats <- graph_fstats(moimm_stack)
ggsave(glue("{git}/output/paper/figures/fig2b_fstats.png"), qlrfstats, height = 4, width = 7)

## HI THIS IS FIGURE 2 FROM PAPER!
flow_detr_final <- ggplot(data = moimm_stack, aes(x = t, y = logFLOW_DETR, color = group, linetype = group)) + 
  geom_vline(xintercept = 0, color = "#808080", linetype = 1, alpha = 0.5, linewidth = 1) +
  annotate("text", x = 0, y = max(moimm_stack$logFLOW_DETR, na.rm=TRUE)*0.95, label = "Head Tax Effective", 
           color = "#808080", hjust = -0.05, size = 3) +
  geom_vline(xintercept = -6, color = "#808080", linetype = 3, alpha = 0.5, linewidth = 1) +
  annotate("text", x = -6, y = min(moimm_stack$logFLOW_DETR, na.rm=TRUE)*0.95, label = "Head Tax Announced", 
           color = "#808080", hjust = 1.05, size = 3) +
  geom_hline(yintercept = 0, color = "#808080", linetype = 1, alpha = 0.2) +
  #geom_rect(aes(xmin = -5, xmax = 0, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.1, inherit.aes = FALSE) +
  geom_line() +
  scale_color_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(c5, c3, c1)) +
  scale_linetype_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(1, 2, 6)) +
  labs(x = "Month of Immigration Relative to Head Tax Increase", y = "Detrended log(Chinese Immigrant Inflow)",
       linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/output/paper/figures/fig2_flowdetr.png"), flow_detr_final, height = 4, width = 7)


# plotting f stats 
graph_fstats(moimm_stack)
qlrfstats <- ggplot(data = moimm_stack %>% filter(!is.na(FStat)), aes(x = t, y = FStat, color = group, linetype = group)) + 
  geom_vline(xintercept = 0, color = "#808080", linetype = 1, alpha = 0.5, linewidth = 1) +
  annotate("text", x = 0, y = max(moimm_stack$FStat, na.rm=TRUE)*0.95, label = "Head Tax Effective",
           color = "#808080", hjust = -0.05, size = 3) +
  geom_vline(xintercept = -6, color = "#808080", linetype = 3, alpha = 0.5, linewidth = 1) +
  annotate("text", x = -6, y = max(moimm_stack$FStat, na.rm=TRUE)*0.95, label = "Head Tax Announced",
           color = "#808080", hjust = 1.05, size = 3) +
  geom_hline(aes(yintercept = bound), alpha = 0.5, color = "black") +
  annotate("text", x = -20, y = max(moimm_stack$bound, na.rm=TRUE), label = "95% Critical Value",
           color = "black", vjust = -1, size = 3) +
  #geom_rect(aes(xmin = -5, xmax = 0, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.1, inherit.aes = FALSE) +
  geom_line() +
  scale_color_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(c5, c3, c1)) +
  scale_linetype_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(1, 2, 6)) +
  labs(x = "Month of Immigration Relative to Head Tax Increase",
       y = "Chow F Statistic",
       linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')
ggsave(glue("{git}/output/paper/figures/fig2b_fstats.png"), qlrfstats, height = 4, width = 7)



## OLD STUFF

window = 3
# 1885
date50 = as.Date("1886-01-01")
moimm_50 <- moimm_reg %>% filter(YRIMM >= year(date50) - window & YRIMM <= year(date50) + window - 1) %>% 
  mutate(group = "$50 Tax", t = interval(date50, MOIMM) %/% months(1))
moimm_50_reg <- lm(logFLOW ~ factor(month), data = moimm_50)
moimm_50$FLOW_DETR = resid(moimm_50_reg)
qlr50 <- qlrtest_month(moimm_50, k = 15)
moimm_50$FStat = NA
moimm_50$FStat[15:(nrow(moimm_50)-15)] = qlr50$Fstats
moimm_50$bound = NA
moimm_50$bound[15:(nrow(moimm_50)-15)] = boundary(qlr50)

# 1900
date100 = as.Date("1901-01-01")
moimm_100 <- moimm_reg %>% filter(YRIMM >= year(date100) - window & MOIMM < as.Date("1903-07-01")) %>% 
  mutate(group = "$100 Tax", t = interval(date100, MOIMM) %/% months(1))
moimm_100_reg <- lm(logFLOW ~ factor(month), data = moimm_100)
moimm_100$FLOW_DETR = resid(moimm_100_reg)
qlr100 <- qlrtest_month(moimm_100, k = 15)
moimm_100$FStat = NA
moimm_100$FStat[15:(nrow(moimm_100)-15)] = qlr100$Fstats
moimm_100$bound = NA
moimm_100$bound[15:(nrow(moimm_100)-15)] = boundary(qlr100)

# 1903
date500 = as.Date("1904-01-01")
moimm_500 <- moimm_reg %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= year(date500) + window - 1) %>% 
  mutate(group = "$500 Tax", t = interval(date500, MOIMM) %/% months(1))
moimm_500_reg <- lm(logFLOW ~ factor(month), data = moimm_500)
moimm_500$FLOW_DETR = resid(moimm_500_reg)
qlr500 <- qlrtest_month(moimm_500, k = 15)
moimm_500$FStat = NA
moimm_500$FStat[15:(nrow(moimm_500)-15)] = qlr500$Fstats
moimm_500$bound = NA
moimm_500$bound[15:(nrow(moimm_500)-15)] = boundary(qlr500)

# stacking
moimm_stack <- bind_rows(list(moimm_50, moimm_100, moimm_500)) %>% 
  mutate(group = factor(group, levels = c("$50 Tax", "$100 Tax", "$500 Tax")))


ggplot(data = moimm_stack, aes(x = t, y = FLOW_DETR)) + geom_line() + geom_hline(yintercept = 0) +
  facet_wrap(~group, ncol = 1, scales = "free_y")


rdplot(moimm_stack$logFLOW, moimm_stack$t, p = 1)
rdplot(filter(moimm_stack, group == "$50")$logFLOW, filter(moimm_stack, group == "$50")$t, p = 1)
rdplot(filter(moimm_stack, group == "$100")$logFLOW, filter(moimm_stack, group == "$100")$t, p = 1)
rdplot(filter(moimm_stack, group == "$500")$logFLOW, filter(moimm_stack, group == "$500")$t, p = 1)

rdplot(moimm_stack$FLOW_DETR, moimm_stack$t, p = 1, nbins = c(20,20)) 
# binscatter
binsreg(moimm_stack$FLOW_DETR, as.numeric(moimm_stack$t), by = moimm_stack$group)

binsreg(moimm_stack$logFLOW, as.numeric(moimm_stack$t), by = moimm_stack$group)

