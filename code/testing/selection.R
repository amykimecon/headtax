# outcome/selection analysis 

## SETTING PARAMETERS
regstart = 1882
regend = 1923
minage = 18
maxage = 65

## CLEANING INDIV DATA
reg_clean <- reg_chi %>% 
  filter(!is.na(DATE)) %>%
  mutate(MOIMM = lubridate::floor_date(DATE, "month"),
         month = as.character(month(MOIMM)),
         height_samp = ifelse(AGE >= 23 & AGE <= 50 & 
                                !is.na(HEIGHT) & HEIGHT > 100 & 
                                !is.na(MALE) & MALE == 1, 1, 0),
         whipple_samp = ifelse(AGE >= minage & AGE <= (minage + 39) & !is.na(MALE) & MALE == 1, 1, 0),
         occ_samp = ifelse(AGE >= minage & AGE <= maxage & !is.na(MALE) & MALE == 1, 1, 0),
         AGE_ARR = AGE - (ifelse(REG_Year == 0, NA, REG_Year) - YRIMM),
         AGE_ARR = ifelse(AGE_ARR < 0 | AGE == 0, NA, AGE_ARR),
         age_samp = ifelse(!is.na(AGE_ARR), 1, 0),
         cost = tax + 1496.19)

## CREATING MONTHLY GROUPED DATAFRAME
monthly <- reg_clean %>% group_by(YRIMM, MOIMM, month, tax) %>% 
  summarise(height = mean(ifelse(height_samp == 1, HEIGHT, NA), na.rm=TRUE),
            logheight = mean(ifelse(height_samp == 1, log(HEIGHT), NA), na.rm=TRUE),
            n_height = sum(height_samp),
            whipple = mean(ifelse(whipple_samp == 1, WHIPPLE, NA), na.rm=TRUE),
            n_whipple = sum(whipple_samp),
            labor = mean(ifelse(occ_samp, ifelse(OCCGRP == "Labourer", 1, 0), NA), na.rm=TRUE),
            merchant = mean(ifelse(occ_samp, ifelse(OCCGRP == "Merchant", 1, 0), NA), na.rm=TRUE),
            student = mean(ifelse(occ_samp, ifelse(OCCGRP == "Student", 1, 0), NA), na.rm=TRUE),
            merge = mean(match_ci44),
            labor_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean == "Labourer", 1, 0), NA), na.rm=TRUE),
            merch_owner_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0), NA), na.rm=TRUE),
            service_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0), NA), na.rm=TRUE),
            student_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean == "Student", 1, 0), NA), na.rm=TRUE),
            n_occ = sum(occ_samp),
            age = mean(ifelse(age_samp == 1, AGE_ARR, NA), na.rm=TRUE),
            n_age = sum(age_samp),
            n = n()) 

# same thing by year
yearly <- reg_chi %>% 
  mutate(height_samp = ifelse(AGE >= minage & AGE <= maxage & HEIGHT > 100 & MALE == 1, 1, 0),
         whipple_samp = ifelse(AGE >= minage & AGE <= maxage & MALE == 1, 1, 0),
         occ_samp = ifelse(AGE >= 18 & AGE <= maxage & MALE == 1, 1, 0)) %>%
  group_by(YRIMM, tax) %>% 
  summarise(height = mean(ifelse(height_samp == 1, HEIGHT, NA), na.rm=TRUE),
            n_height = sum(height_samp),
            whipple = mean(ifelse(whipple_samp == 1, WHIPPLE, NA), na.rm=TRUE),
            n_whipple = sum(whipple_samp),
            labor = mean(ifelse(occ_samp, ifelse(OCCGRP == "Labourer", 1, 0), NA), na.rm=TRUE),
            merchant = mean(ifelse(occ_samp, ifelse(OCCGRP == "Merchant", 1, 0), NA), na.rm=TRUE),
            student = mean(ifelse(occ_samp, ifelse(OCCGRP == "Student", 1, 0), NA), na.rm=TRUE),
            merge = mean(match_ci44),
            labor_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean == "Labourer", 1, 0), NA), na.rm=TRUE),
            merch_owner_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0), NA), na.rm=TRUE),
            service_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0), NA), na.rm=TRUE),
            student_post = mean(ifelse(occ_samp, ifelse(occ_ci44_clean == "Student", 1, 0), NA), na.rm=TRUE),
            n_occ = sum(occ_samp),
            age = mean(AGE),
            n = n())

#_____________________________________________________________
# Main Paper Graphs  ----------------------------------
#_____________________________________________________________
saveDefault = FALSE
#helper: 
out_ma6 <- function(df, outcomevar, ylab = str_to_title(outcomevar), save = saveDefault){
  monthly_stack <- all_disconts(df, yvar = outcomevar, indiv = TRUE)
  plot_out <- graph_disconts(monthly_stack[which(!is.na(monthly_stack[[glue("{outcomevar}_RAW_MA6")]])),],
                             glue("{outcomevar}_RAW_MA6"), glue("6-month MA {ylab}"),
                             sampmean = mean(df[[outcomevar]])) + 
    theme(text = element_text(size=16), axis.text = element_text(size = 14),
          legend.position='right')
  print(plot_out)
  if(save){
    ggsave(glue("{git}/output/paper/figures/{str_to_lower(outcomevar)}_raw_ma6.png"), 
           plot_out, height = 4, width = 8)
  }
}

out_ma6_detr <- function(df, outcomevar, ylab = str_to_title(outcomevar), save = saveDefault){
  monthly_stack <- all_disconts(df, yvar = outcomevar, indiv = TRUE)
  plot_out <- graph_disconts(monthly_stack[which(!is.na(monthly_stack[[glue("{outcomevar}_DETR_MA6")]])),],
                             glue("{outcomevar}_DETR_MA6"), glue("6-month Detrended MA {ylab}"),
                             sampmean = 0) + 
    theme(text = element_text(size=16), axis.text = element_text(size = 14),
          legend.position='right')
  print(plot_out)
  if(save){
    ggsave(glue("{git}/output/paper/figures/{str_to_lower(outcomevar)}_detr_ma6.png"), 
           plot_out, height = 4, width = 8)
  }
}

out_raw <- function(df, outcomevar, ylab = str_to_title(outcomevar), save = saveDefault){
  monthly_stack <- all_disconts(df, yvar = outcomevar, indiv = TRUE)
  plot_out <- graph_disconts(monthly_stack[which(!is.na(monthly_stack[[glue("{outcomevar}_RAW")]])),],
                             glue("{outcomevar}_RAW"), glue("Monthly mean {ylab}"),
                             sampmean = mean(df[[outcomevar]]))
  print(plot_out)
  if(save){
    ggsave(glue("{git}/output/paper/figures/{str_to_lower(outcomevar)}_raw.png"), plot_out, height = 4, width = 7)
  }
}

# HEIGHT: INDIV RAW VALS -> 6MO MA
out_ma6(reg_clean %>% filter(height_samp == 1), "HEIGHT", ylab = "Height (cm)")

# AGE
out_ma6(reg_clean %>% filter(age_samp == 1), "AGE_ARR", ylab = "Age at Arrival")

# WHIPPLE/AGE HEAP
out_ma6(reg_clean %>% filter(whipple_samp == 1), "WHIPPLE", ylab = "Whipple Index")

# OCCGRP
out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(LABOR = ifelse(OCCGRP == "Labourer", 1, 0)), 
        "LABOR", ylab = "Share Laborer")

out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(STUMERCH = ifelse(OCCGRP == "Student" | OCCGRP == "Merchant", 1, 0)), 
        "STUMERCH", ylab = "Indicator for Student/Merchant")

# MERGE
out_ma6(reg_clean, 
        "match_ci44", ylab = "Indicator for Matching to 1924")
out_ma6_detr(reg_clean, 
        "match_ci44", ylab = "Indicator for Matching to 1924")
out_raw(reg_clean, 
        "match_ci44", ylab = "Indicator for Matching to 1924")

# POST OCCS
out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(LABOR_POST = ifelse(match_ci44 == 1 & occ_ci44_clean == "Labourer", 1, 0)), 
        "LABOR_POST", ylab = "Working as Laborer in 1924 (Unconditional)")
out_ma6(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(LABOR_POST = ifelse(occ_ci44_clean == "Labourer", 1, 0)), 
        "LABOR_POST", ylab = "Working as Laborer in 1924 (Conditional on matching)")

out_ma6_detr(reg_clean %>% filter(occ_samp == 1) %>% mutate(LABOR_POST = ifelse(match_ci44 == 1 & occ_ci44_clean == "Labourer", 1, 0)), 
        "LABOR_POST", ylab = "Working as Laborer in 1924 (Unconditional)")
out_ma6_detr(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(LABOR_POST = ifelse(occ_ci44_clean == "Labourer", 1, 0)), 
        "LABOR_POST", ylab = "Working as Laborer in 1924 (Conditional on matching)")

# merchant/owner
out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(MERCH_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0)), 
        "MERCH_POST", ylab = "Working as Merchant/Proprietor in 1924 (Unconditional)")
out_ma6(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(MERCH_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0)), 
        "MERCH_POST", ylab = "Working as Merchant/Proprietor in 1924 (Conditional on matching)")

out_ma6_detr(reg_clean %>% filter(occ_samp == 1) %>% mutate(MERCH_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0)), 
        "MERCH_POST", ylab = "Working as Merchant/Proprietor in 1924 (Unconditional)")
out_ma6_detr(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(MERCH_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Merchant", "Proprietor", "Professional"), 1, 0)), 
        "MERCH_POST", ylab = "Working as Merchant/Proprietor in 1924 (Conditional on matching)")

# service worker
out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(SERVICE_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0)), 
             "SERVICE_POST", ylab = "Working in Service in 1924 (Unconditional)")
out_ma6(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(SERVICE_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0)), 
             "SERVICE_POST", ylab = "Working in Service in 1924 (Conditional on matching)")

out_ma6_detr(reg_clean %>% filter(occ_samp == 1) %>% mutate(SERVICE_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0)), 
             "SERVICE_POST", ylab = "Working in Service in 1924 (Unconditional)")
out_ma6_detr(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(SERVICE_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Laundryman", "Cook", "Service"), 1, 0)), 
             "SERVICE_POST", ylab = "Working in Service in 1924 (Conditional on matching)")

# student worker
out_ma6(reg_clean %>% filter(occ_samp == 1) %>% mutate(STUDENT_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Student"), 1, 0)), 
        "STUDENT_POST", ylab = "Student in 1924 (Unconditional)")
out_ma6(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(STUDENT_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Student"), 1, 0)), 
        "STUDENT_POST", ylab = "Student in 1924 (Conditional on matching)")

out_ma6_detr(reg_clean %>% filter(occ_samp == 1) %>% mutate(STUDENT_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Student"), 1, 0)), 
        "STUDENT_POST", ylab = "Student in 1924 (Unconditional)")
out_ma6_detr(reg_clean %>% filter(occ_samp == 1 & match_ci44 == 1) %>% mutate(STUDENT_POST = ifelse(match_ci44 == 1 & occ_ci44_clean %in% c("Student"), 1, 0)), 
        "STUDENT_POST", ylab = "Student in 1924 (Conditional on matching)")


### REGRESSIONS ####

outreg <- function(df, yvar, ylab){
  # reg0: no ctrls for birthyear ----
  reg0 <- lm(data = df, as.formula(glue("{yvar} ~ factor(month) + factor(tax)")))

  # reg1: quadratic birthyear ----
  reg1 <- lm(data = df, as.formula(glue("{yvar} ~ factor(month) + factor(tax) + BIRTHYEAR + I(BIRTHYEAR^2)")))

  # reg2: birthyear fes ----
  reg2 <- lm(data = df, as.formula(glue("{yvar} ~ factor(month) + factor(tax) + factor(BIRTHYEAR)")))
  print(summary(reg2))
  regmean <- round(mean(df[[yvar]]),1)
  regse <- round(sd(df[[yvar]])/sqrt(nrow(df) - 1),1)
  
  # reg3: log-log ----
  # reg3 <- lm(data = df, as.formula(glue("log({yvar}) ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
  # regmean3 <- round(mean(log(df[[yvar]])),1)
  # regse3 <- round(sd(log(df[[yvar]]))/sqrt(nrow(df) - 1),1)
  # 
  # reg50: ht 50 ----
  reg50 <- lm(data = df %>% filter(YRIMM >= 1883 & YRIMM <= 1888),  
              as.formula(glue("log({yvar}) ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
  print(summary(reg50))
  regmean50 <- round(mean(filter(df, YRIMM >= 1883 & YRIMM <= 1888)[[yvar]]),1)
  regse50 <- round(sd(filter(df, YRIMM >= 1883 & YRIMM <= 1888)[[yvar]])/
                     sqrt(nrow(filter(df, YRIMM >= 1883 & YRIMM <= 1888))-1),1)
  
  # ht 100 ----
  reg100 <- lm(data = df %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")),  
               as.formula(glue("log({yvar}) ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
  regmean100 <- round(mean(filter(df, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))[[yvar]]),1)
  print(summary(reg100))
  regse100 <- round(sd(filter(df, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))[[yvar]])/
                     sqrt(nrow(filter(df, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)

  # ht 500 ----
  reg500 <- lm(data = df %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906),  
               as.formula(glue("log({yvar}) ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
  print(summary(reg500))
  regmean500 <- round(mean(filter(df, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)[[yvar]]),1)
  regse500 <- round(sd(filter(df, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)[[yvar]])/
                     sqrt(nrow(filter(df, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)
  
  # compiling lists ----
  reg_models <- list(reg0, reg2, reg50, reg100, reg500)
  reg_means <- c(regmean, regmean, regmean50, regmean100, regmean500)
  reg_ses <- c(glue("({regse})"), glue("({regse})"), 
                   glue("({regse50})"),glue("({regse100})"),glue("({regse500})"))

  # stargazer ----
  stargazer(reg_models,
            se = lapply(reg_models, robustse),
            keep = c("^factor\\(tax\\)"),
            #out = glue("{git}/output/paper/tables/{str_to_lower(yvar)}_regs.tex"),
            float = FALSE,
            digits = 2,
            intercept.bottom = FALSE,
            keep.stat=c("n","adj.rsq"),
            column.labels = c("All Years (1882-1921)", "\\$50 Tax (1883-1888)",
                              "\\$100 Tax (1898-1903)", "\\$500 Tax (1901-1906)"),
            column.separate = c(2,1,1,1),
            covariate.labels = c("$\\gamma_{50}$ (\\$50 Tax)",
                                 "$\\gamma_{100}$ (\\$100 Tax)",
                                 "$\\gamma_{500}$ (\\$500 Tax)"
            ),
            add.lines = list(c("Dep. Var. Mean (SE)", reg_means),c("", reg_ses),
                             c('Birth Year FE', "No", rep("Yes",4))),
            dep.var.caption = paste0("\\textit{Dependent variable:} ", ylab),
            table.layout = "=cl#-t-as=")
}

ht_samp <- function(df, ht){
  if(ht == 50){
    df_filt <- df %>% filter(YRIMM >= 1883 & YRIMM <= 1888)
  }
  else if (ht == 100){
    df_filt <- df %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))
  }
  else if (ht == 500){
    df_filt <- df %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)
  }
  return(df_filt)
}

outreg_vars <- function(df, yvars, ylabs = yvars, loglog = FALSE, outfile_root = "selection"){
  reg_models = list()
  reg_means = numeric(length(yvars))
  reg_ses = character(length(yvars))
  i = 1
  for (yvar in yvars){
    df_filt <- df[which(!is.na(df[[yvar]])),] 
    if (loglog){
      if (yvar == "AGE"){
        reg2 <- lm(data = df_filt, as.formula(glue("{yvar} ~ factor(month) + log(cost)")))
      }
      else{
        reg2 <- lm(data = df_filt, as.formula(glue("{yvar} ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
      }
      #reg2 <- lm(data = df_filt, as.formula(glue("{yvar} ~ factor(month) + log(cost) + factor(BIRTHYEAR)")))
      #regmean = round(mean(log(df_filt[[yvar]])),1)
      regmean = round(mean(df_filt[[yvar]]),1)
      #regse = glue("({round(sd(log(df_filt[[yvar]]))/sqrt(nrow(df_filt) - 1),1)})")
      regse = glue("({round(sd(df_filt[[yvar]])/sqrt(nrow(df_filt) - 1),1)})")
    }
    else{
      reg2 <- lm(data = df_filt, as.formula(glue("{yvar} ~ factor(month) + factor(tax) + factor(BIRTHYEAR)")))
      regmean = round(mean(df_filt[[yvar]]),1)
      regse = glue("({round(sd(df_filt[[yvar]])/sqrt(nrow(df_filt) - 1),1)})")
    }
    reg_models[[i]] <- reg2
    reg_means[i] <- regmean
    reg_ses[i] <- regse
    i = i + 1
  }
    # stargazer ----
  if (loglog){
    keepvars = c("log\\(cost\\)")
    covlabs = c("$\\gamma^{\\varepsilon}$ (log Cost)")
    extralines = list(c("Dep. Var. Mean", reg_means))
  }
  else{
    keepvars = c("^factor\\(tax\\)")
    covlabs = c("$\\gamma_{50}$ (\\$50 Tax)",
                "$\\gamma_{100}$ (\\$100 Tax)",
                "$\\gamma_{500}$ (\\$500 Tax)")
    extralines = list(c("Dep. Var. Mean", reg_means))
  }
    stargazer(reg_models,
              se = lapply(reg_models, robustse),
              keep = keepvars,
              out = glue("{git}/output/paper/tables/{outfile_root}_regs.tex"),
              float = FALSE,
              digits = 2,
              intercept.bottom = FALSE,
              keep.stat=c("n","adj.rsq"),
              column.labels = ylabs,
              column.separate = rep(1,length(ylabs)),
              covariate.labels = covlabs,
              add.lines = extralines,
              table.layout = "=lc#-t-as=")
}

# REG DATA
reg_clean_sample <- reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & BIRTHYEAR > 0) %>%
  mutate(HEIGHT = ifelse(height_samp == 1, HEIGHT, NA),
         logHEIGHT = log(HEIGHT),
         AGE = ifelse(age_samp == 1, AGE_ARR, NA),
         WHIPPLE = ifelse(whipple_samp == 1, WHIPPLE, NA),
         LABOR = ifelse(occ_samp == 1, ifelse(OCCGRP == "Labourer", 1, 0), NA),
         MERCHANT = ifelse(occ_samp == 1, ifelse(OCCGRP == "Merchant", 1, 0), NA),
         STUDENT = ifelse(occ_samp == 1, ifelse(OCCGRP == "Student", 1, 0), NA),
         MERCHSTUD = ifelse(occ_samp == 1, ifelse(OCCGRP == "Merchant" | OCCGRP == "Student", 1, 0), NA)
  )

outreg_vars(reg_clean_sample %>% ht_samp(50), 
            c("HEIGHT", "AGE", "WHIPPLE", "LABOR", "MERCHSTUD"), loglog = TRUE, 
            outfile_root = "selection50")

outreg_vars(reg_clean_sample %>% ht_samp(100), 
            c("HEIGHT", "AGE", "WHIPPLE", "LABOR", "MERCHSTUD"), loglog = TRUE,
            outfile_root = "selection100")

outreg_vars(reg_clean_sample %>% ht_samp(500), 
            c("HEIGHT", "AGE", "WHIPPLE", "LABOR", "MERCHSTUD"), loglog = TRUE,
            outfile_root = "selection500")

outreg_vars(reg_clean_sample, 
            c("HEIGHT", "AGE", "WHIPPLE", "LABOR", "MERCHSTUD"), loglog = TRUE)





# OLD STUFF

# HEIGHT
outreg(filter(reg_clean, YRIMM >= regstart & YRIMM <= regend & height_samp == 1 & BIRTHYEAR > 0), 
       yvar = "HEIGHT", ylab = "Height (cm)")

# AGE
outreg(filter(reg_clean, YRIMM >= regstart & YRIMM <= regend & age_samp == 1 & BIRTHYEAR > 0 & AGE > 0) %>% mutate(AGE = AGE_ARR), 
       yvar = "AGE")


heightreg0 <- lm(data = reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & height_samp == 1), 
                 HEIGHT ~ factor(month) + factor(tax))
summary(heightreg0)

heightreg1 <- lm(data = reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & height_samp == 1), 
               HEIGHT ~ factor(month) + factor(tax) + BIRTHYEAR + I(BIRTHYEAR^2))

summary(heightreg1)

heightreg2 <- lm(data = reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & height_samp == 1), 
                 HEIGHT ~ factor(month) + factor(tax) + factor(BIRTHYEAR))

summary(heightreg2)

# elasticity
summary(lm(data = reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & height_samp == 1), 
           log(HEIGHT) ~ factor(month) + log(cost))
)

heightreg3 <- lm(data = reg_clean %>% filter(YRIMM >= regstart & YRIMM <= regend & height_samp == 1), 
                 log(HEIGHT) ~ factor(month) + log(cost) + factor(BIRTHYEAR))
summary(heightreg3)


regmean1 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW),1)
regse1 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$logFLOW)/
                  sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)
regmean0 <- round(mean(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER),1)
regse0 <- round(sd(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)$CHIFLOW_REGISTER)/
                  sqrt(nrow(filter(flow_regress, YRIMM >= regstart & YRIMM <= regend)) - 1),1)

# ht 50
flowreg50 <- lm(data = reg_clean %>% filter(YRIMM >= 1883 & YRIMM <= 1888 & height_samp == 1),  
                log(HEIGHT) ~ factor(month) + log(cost) + factor(BIRTHYEAR))
summary(flowreg50)
regmean50 <- round(mean(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW),1)
regse50 <- round(sd(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888)$logFLOW)/
                   sqrt(nrow(filter(flow_regress, YRIMM >= 1883 & YRIMM <= 1888))-1),1)

# ht 100
flowreg100 <- lm(data = reg_clean %>% filter(YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01") & height_samp == 1),  
                 log(HEIGHT) ~ factor(month) + log(cost) + factor(BIRTHYEAR))
summary(flowreg100)

regmean100 <- round(mean(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW),1)
regse100 <- round(sd(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01"))$logFLOW)/
                    sqrt(nrow(filter(flow_regress, YRIMM >= 1898 & MOIMM <= as.Date("1903-07-01")))-1),1)

# ht 500
flowreg500 <- lm(data = reg_clean %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906 & height_samp == 1),  
                 log(HEIGHT) ~ factor(month) + log(cost) + factor(BIRTHYEAR))
summary(flowreg500)
regmean500 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW),1)
regse500 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$logFLOW)/
                    sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)

flowreg500_v2 <- lm(data = reg_clean %>% filter(MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906 & height_samp == 1), 
                    HEIGHT ~ factor(month) + factor(tax) + factor(BIRTHYEAR))
summary(flowreg500_v2)

regmean500_v2 <- round(mean(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER),1)
regse500_v2 <- round(sd(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906)$CHIFLOW_REGISTER)/
                       sqrt(nrow(filter(flow_regress, MOIMM >= as.Date("1901-06-01") & YRIMM <= 1906))-1),1)

flowreg_means <- c(regmean1, regmean50, regmean100, regmean500, regmean0)
flowreg_ses <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),glue("({regse0})"))

flowreg_means_v2 <- c(regmean1, regmean50, regmean100, regmean500, regmean0, regmean50_v2, regmean100_v2, regmean500_v2)
flowreg_ses_v2 <- c(glue("({regse1})"), glue("({regse50})"),glue("({regse100})"),glue("({regse500})"),
                    glue("({regse0})"), glue("({regse50_v2})"),glue("({regse100_v2})"),glue("({regse500_v2})"))




monthly_stack_height <- all_disconts(reg_clean %>% filter(height_samp==1), 
                                    yvar = "HEIGHT", indiv = TRUE)
height_raw_ma6 <- graph_disconts(monthly_stack_height %>% filter(!is.na(HEIGHT_RAW_MA6)), 
                                 "HEIGHT_RAW_MA6", "6-month MA of Raw Height (cm)", 
                                 sampmean = mean(ifelse(reg_clean$height_samp == 1, reg_clean$HEIGHT, NA), na.rm=TRUE))

ggsave(glue("{git}/output/paper/figures/height_raw_ma6.png"), height_raw_ma6, height = 4, width = 7)


# AGE




height_detr_ma6 <- graph_disconts(monthly_stack_height %>% filter(!is.na(HEIGHT_DETR_MA6)), 
                                  "HEIGHT_DETR_MA6", "6-month MA of Detrended Height")
ggsave(glue("{git}/output/paper/figures/height_detr_ma6.png"), height_detr_ma6, height = 4, width = 7)

height_detr <- graph_disconts(monthly_stack_height, "HEIGHT_DETR", "Mean monthly Detrended Height")
ggsave(glue("{git}/output/paper/figures/height_detr.png"), height_detr, height = 4, width = 7)

height_raw_ma6 <- graph_disconts(monthly_stack_height %>% filter(!is.na(HEIGHT_RAW_MA6)), 
                                 "HEIGHT_RAW_MA6", "6-month MA of Raw Height (cm)", 
               sampmean = mean(ifelse(reg_clean$height_samp == 1, reg_clean$HEIGHT, NA), na.rm=TRUE))

graph_disconts(monthly_stack_height, "HEIGHT_RAW_MA3", "3-month MA of Raw Height", 
               yint = mean(filter(monthly_stack_height, t == 0)$HEIGHT_RAW_MA3))
graph_disconts(monthly_stack_height, "HEIGHT_RAW", "Mean monthly Raw Height", 
               yint = mean(filter(monthly_stack_height, t == 0)$HEIGHT_RAW))

graph_fstats(monthly_stack_height)
graph_fstats(monthly_stack_height, ma3 = TRUE)

# AGE
monthly_stack_age <- all_disconts(reg_clean, 
                                            yvar = "AGE", indiv = TRUE)

graph_disconts(monthly_stack_age, "AGE_RAW_MA3", "3-month MA of Raw Height", yint = NA_real_)
graph_disconts(monthly_stack_age, "AGE_RAW", "Mean monthly Raw Height")

graph_disconts(monthly_stack_age, "AGE_DETR_MA3", "3-month MA of Detrended Height")
graph_disconts(monthly_stack_age, "AGE_DETR", "Mean monthly Detrended Height")

graph_fstats(monthly_stack_age)
graph_fstats(monthly_stack_age, ma3 = TRUE)

# WHIPPLE
monthly_stack_whipple <- all_disconts(reg_clean %>% filter(whipple_samp==1), 
                                    yvar = "WHIPPLE", indiv = TRUE)
graph_disconts(monthly_stack_whipple, "WHIPPLE_DETR_MA3", "3-month MA of Detrended Height")
graph_disconts(monthly_stack_whipple, "WHIPPLE_DETR", "Mean monthly Detrended Height")

graph_fstats(monthly_stack_whipple)
graph_fstats(monthly_stack_whipple, ma3 = TRUE)


ggplot(yearly %>% filter(YRIMM >= regstart & YRIMM <= 1910), 
       aes(x = YRIMM, y = height)) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

ggplot(monthly %>% ungroup() %>% filter(YRIMM >= regstart & YRIMM <= 1924 & !is.na(height) & height > 155), 
       aes(x = MOIMM, y = rollmean(height, 2, na.pad = TRUE))) +
  #geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  geom_line() +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

ggplot(monthly %>% ungroup() %>% filter(YRIMM >= regstart & YRIMM <= 1924), 
       aes(x = MOIMM, y = labor)) +
  #geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  geom_line() +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

## STACKED DESIGN
monthly_stack <- all_disconts(monthly, yvar = "height")
graph_disconts(monthly_stack, "height_DETR", "detrended mean height")
graph_disconts(monthly_stack, "height_DETR", "detrended mean height", rollmean = TRUE)
graph_disconts(monthly_stack, "height", "detrended mean height", yint = 160)

monthly_stack_indiv <- all_disconts(reg_clean %>% filter(height_samp==1), 
                                    yvar = "HEIGHT", indiv = TRUE)
graph_disconts(monthly_stack_indiv, "HEIGHT_DETR", "mean detrended height")
graph_disconts(monthly_stack_indiv, "HEIGHT_DETR", "mean detrended height", rollmean = TRUE)
graph_disconts(monthly_stack_indiv, "HEIGHT_DETR_MA3", "mean detrended height")


ymin = min(monthly_stack_indiv[["HEIGHT_DETR"]], na.rm=TRUE)
ymax = max(monthly_stack_indiv[["HEIGHT_DETR"]], na.rm=TRUE)
lab_vjust = 0.05
lab_hjust = 0.05
yint = 0
ylab=''
ggplot(data = monthly_stack_indiv, aes(x = t, y = rollmean(HEIGHT_DETR,3,na.pad=TRUE), color = group, linetype = group)) + 
  geom_vline(xintercept = 0, color = "#808080", linetype = 1, alpha = 0.5, linewidth = 1) +
  annotate("text", x = 0, y = 5, label = "Head Tax Effective", 
           color = "#808080", hjust = -lab_hjust, size = 3) +
  geom_vline(xintercept = -6, color = "#808080", linetype = 3, alpha = 0.5, linewidth = 1) +
  annotate("text", x = -6, y = -3, label = "Head Tax Announced", 
           color = "#808080", hjust = 1 + lab_hjust, size = 3) +
  geom_hline(yintercept = yint, color = "#808080", linetype = 1, alpha = 0.2) +
  geom_line() +
  scale_color_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(c5, c3, c1)) +
  scale_linetype_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(1, 2, 6)) +
  labs(x = "Month of Immigration Relative to Head Tax Increase", 
       y = ylab,
       linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

monthly_stack_indiv <- all_disconts(reg_clean %>% filter(height_samp==1) %>% mutate(logheight = log(HEIGHT)), 
                                    yvar = "logheight", indiv = TRUE)
graph_disconts(monthly_stack_indiv, "logheight_DETR", "mean detrended log height")


monthly_stack_logs <- all_disconts(monthly, yvar = "logheight")
graph_disconts(monthly_stack_logs, "logheight_DETR", "detrended mean log height")

ggplot(data = monthly_stack, aes(x = t, y = .data[["height_DETR"]], color = group, linetype = group)) + 
  geom_vline(xintercept = 0, color = "#808080", linetype = 1, alpha = 0.5, linewidth = 1) +
  annotate("text", x = 0, y = max(monthly_stack$height_DETR, na.rm=TRUE)*0.95, label = "Head Tax Effective", 
           color = "#808080", hjust = -0.05, size = 3) +
  geom_vline(xintercept = -6, color = "#808080", linetype = 3, alpha = 0.5, linewidth = 1) +
  annotate("text", x = -6, y = min(monthly_stack$height_DETR, na.rm=TRUE)*0.95, label = "Head Tax Announced", 
           color = "#808080", hjust = 1.05, size = 3) +
  geom_hline(yintercept = 0, color = "#808080", linetype = 1, alpha = 0.2) +
  #geom_rect(aes(xmin = -5, xmax = 0, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.1, inherit.aes = FALSE) +
  geom_line() +
  scale_color_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(c5, c3, c1)) +
  scale_linetype_manual(breaks = c("$50 Tax", "$100 Tax", "$500 Tax"), values = c(1, 2, 6)) +
  labs(x = "Month of Immigration Relative to Head Tax Increase", y = "Detrended log(Chinese Immigrant Inflow)",
       linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')


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

window = 3
# 1885
date50 = as.Date("1886-01-01")
moimm_50 <- moimm_reg %>% filter(YRIMM >= year(date50) - window & YRIMM <= year(date50) + window - 1) %>% 
  mutate(group = "$50 Tax", t = interval(date50, MOIMM) %/% months(1))
moimm_50_reg <- lm(height ~ factor(month), data = moimm_50)
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


## FIGURE 2: Raw height plots from register data ----
# all men age 23-50 
height_plot <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  group_by(YRIMM, MOIMM, tax) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE), 
            medheight = median(ifelse(HEIGHT == 0, NA, HEIGHT), na.rm=TRUE),
            height25 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.25,na.rm=TRUE),
            height75 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.75,na.rm=TRUE),
            n= n())
# by taxpayer status
height_plot2 <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(taxgrp = ifelse(FEES == 0, "nofee", "fee")) %>%
  group_by(YRIMM, taxgrp) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE),
            n= n()) %>% filter(n > 1)

# mean heights by tax paid
taxmeanheights = summarize(group_by(height_plot, tax), meanheight = weighted.mean(height, n))$meanheight

# plot for paper
fig_height <- ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  #geom_segment(aes(y = taxmeanheights[1], yend = taxmeanheights[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[2], yend = taxmeanheights[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[3], yend = taxmeanheights[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[4], yend = taxmeanheights[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanheights[4] - 0.2, label = "Mean Height Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

# saving plot
ggsave(glue("{git}/output/paper/figures/height_selection.png"), fig_height, height = 4, width = 7)

# plot for slides
fig_height_slides <- ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  geom_segment(aes(y = taxmeanheights[1], yend = taxmeanheights[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[2], yend = taxmeanheights[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[3], yend = taxmeanheights[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[4], yend = taxmeanheights[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanheights[4] - 0.2, label = "Interval Mean"), inherit.aes=FALSE, color = c3, size = 5) +
  geom_point(aes(size = n), color = c4, alpha = 0.9) + 
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", size = "# of Chinese Immigrants") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

# saving plot
ggsave(glue("{git}/output/slides/height_selection.png"), fig_height_slides, height = 5, width = 8)


# # plot by taxpayer group (deprecated)
# ggplot(height_plot2, aes(x = YRIMM, y = height, color = taxgrp)) +
#   geom_smooth(method = "lm", mapping = aes(weight = n)) +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
#   geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
#   geom_point(aes(size = n), alpha = 0.9) + theme_minimal() + theme(legend.position='bottom') +
#   labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# Chinese Immigrants")

## SLIDES FIGURE: Comparison by YRIMM ----
# crosswalk from chinese height file source to label
height_file_name_crosswalk <- china_height %>% group_by(file_name) %>% summarize(source = first(source))

# combining register with chinese height data (to get predicted heights from various sources)                             
height_plot_compare <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_height %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "height_raw"), 
            by = c("year")) %>%
  group_by(YRIMM, tax) %>%
  summarize(Actual = mean(HEIGHT, na.rm=TRUE), 
            across(ends_with(".csv"), function(.x) mean(.x, na.rm=TRUE)),
            n= n()) %>%
  pivot_longer(Actual:sth_unskilled_rlwy_plot.csv, names_to = "var_raw", values_to = "height") %>%
  left_join(height_file_name_crosswalk, by = c("var_raw" = "file_name")) %>%
  mutate(var = ifelse(var_raw == "Actual", "Actual", source))

# comparison plot with ALL sources (for slides)
fig_height_compare_slides <- ggplot(
  height_plot_compare %>% filter(var != "Actual"), aes(x = YRIMM, y = height, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 168.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_line(aes(linetype = var, color = var)) + 
  scale_color_manual(breaks = c("AUS Melb/Vic Migrants",
                                "Chinese Railway Workers",
                                "AUS QLD Migrants",
                                "US Migrants",
                                "US Prisoners",
                                "AUS NT Migrants",
                                "AUS Prisoners",
                                "Indonesia Migrants"),
                     values = colorRampPalette(colors = c(c3, c5))(8)) +
  scale_shape_manual(breaks = c("AUS Melb/Vic Migrants",
                                "Chinese Railway Workers",
                                "AUS QLD Migrants",
                                "US Migrants",
                                "US Prisoners",
                                "AUS NT Migrants",
                                "AUS Prisoners",
                                "Indonesia Migrants"),
                     values = c(3, 4, 2, 18, 10, 0, 8, 15)) +
  geom_point(data = height_plot_compare %>% filter(var == "Actual"), aes(x = YRIMM, y = height, size = n), shape = 16, color = c1) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", 
       size = "", color = "", shape = "", linetype = "Comparison Population") + guides(color = "none") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/height_compare_selection_all.png"), fig_height_compare_slides, height = 5, width = 8)

# comparison plot with only AUS NT migrants (for slides)
fig_height_compare_ausnt_slides <- ggplot(
  height_plot_compare %>% filter(var %in% c("Actual", "AUS NT Migrants")), 
  aes(x = YRIMM, y = height, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_point(aes(size = n), alpha = 0.9) + 
  scale_color_manual(breaks = c("AUS NT Migrants", "Actual"), values = c(c4,c2)) +
  scale_shape_manual(breaks = c("AUS NT Migrants", "Actual"), values = c(18,16)) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", size = "", color = "", shape = "") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/height_compare_selection_ausnt.png"), fig_height_compare_ausnt_slides, height = 5, width = 8)

## EXTRA FIGURES: Comparison by Birth Cohort ----
# merging register with chinese height data by birth cohort -- separately by tax group (CAN)
height_compare_bc <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  group_by(year, tax) %>%
  summarize(height = mean(HEIGHT), n = n()) %>%
  mutate(dataseries = glue("CA Tax = {tax}")) %>%
  select(c(year, height, dataseries, n)) 

# recreating original graph
height_bybc_original <- ggplot(china_height %>% rename(height = height_raw, dataseries = source),
                               aes(x = year, y = height, color = dataseries, shape = dataseries)) +
  geom_point() + geom_line()

# plotting register data on top of original graph
height_plot_path <- glue("{dbox}/raw/china_height_plot_data")
height_csvs <- list.files(height_plot_path, pattern = ".*csv$")

ggplot(rbind(height_compare_bc %>% filter(n > 100), china_height %>% rename(height = height_raw, dataseries = file_name) %>% select(-c(yr_raw))), 
       aes(x = year, y = height, color = dataseries, shape = dataseries)) + geom_point(aes(size = n)) + 
  scale_color_manual(name = "dataseries", breaks = c(c(height_csvs),"CA Tax = 0", "CA Tax = 1496.19", "CA Tax = 2992.61", "CA Tax = 14115.7"),
                     values = c(rep("grey",8), c5, c4, c2, c1)) + theme_minimal() + theme(legend.position='bottom') + geom_line(alpha = 0.5) 

## REGRESSIONS ----
height_reg_data <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & 
           MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_height %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "height_raw"), 
            by = c("year")) %>%
  mutate(height_diff = HEIGHT - mig_au_melbvic_plot.csv)

height_reg1 <- lm(data = height_reg_data, HEIGHT ~ factor(tax) + AGE)
coeftest(height_reg1, vcov. = vcovHC(height_reg1, vcov = "HC1"))

height_reg2 <- lm(data = height_reg_data, height_diff ~ factor(tax) + AGE)
coeftest(height_reg2, vcov. = vcovHC(height_reg2, vcov = "HC1"))

height_reg3 <- lm(data = height_reg_data, HEIGHT ~ factor(tax) + factor(year))
coeftest(height_reg3, vcov. = vcovHC(height_reg3, vcov = "HC1"))

stargazer(height_reg1, height_reg2, height_reg3,
          dep.var.labels = c("Height (cm)", "Height Diff from Melb/Vic", "Height"),
          keep = c("factor\\(tax\\)*", "AGE"),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax", "Age"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("5-yr Birth Cohort FE", "No", "No", "Yes")))


#_____________________________________________________________
# Age Heaping Analysis ----------------------------------
#_____________________________________________________________
## FIGURE ?: Raw whipple plots from register data ----
# all men age 23-50 
whipple_plot <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = n()) %>% filter(whipple >= 100)

# from census
whipple_plot_cen <- can_imm %>% filter(BPL == "China" & AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = sum(WEIGHT))

# mean whipple by tax paid
taxmeanwhipple = summarize(group_by(whipple_plot, tax), meanwhipple = weighted.mean(whipple, n))$meanwhipple

# plot for paper
fig_whipple <- ggplot(whipple_plot %>% filter(whipple >= 100), aes(x = YRIMM, y = whipple)) + 
  geom_segment(aes(y = taxmeanwhipple[1], yend = taxmeanwhipple[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[2], yend = taxmeanwhipple[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[3], yend = taxmeanwhipple[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[4], yend = taxmeanwhipple[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 160, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanwhipple[4] - 2, label = "Whipple Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Immigrants", size = "# of Chinese Immigrants")

# saving plot
ggsave(glue("{git}/output/paper/figures/whipple_selection.png"), fig_whipple, height = 4, width = 7)

# plot using census data
ggplot(rbind(whipple_plot %>% mutate(source = "Reg"), whipple_plot_cen %>% mutate(source = "Cen")), 
       aes(x = YRIMM, y = whipple, color = source)) + 
  geom_segment(aes(y = taxmeanwhipple[1], yend = taxmeanwhipple[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[2], yend = taxmeanwhipple[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[3], yend = taxmeanwhipple[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[4], yend = taxmeanwhipple[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 160, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanwhipple[4] - 2, label = "Whipple Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Immigrants", size = "# of Chinese Immigrants")


## SLIDES FIGURE: Comparison by YRIMM ----
# crosswalk from chinese age file source to label
age_file_name_crosswalk <- china_age %>% group_by(file_name) %>% summarize(source = first(source))

# combining register with chinese height data (to get predicted heights from various sources)                             
whipple_plot_compare <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1885 & YRIMM < 1924 & MALE == 1 & FEES > 0) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_age %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "age_raw"), 
            by = c("year")) %>%
  group_by(YRIMM, tax) %>%
  summarize(Actual = mean(WHIPPLE, na.rm=TRUE), 
            across(ends_with(".csv"), function(.x) mean(.x, na.rm=TRUE)),
            n= n()) %>%
  pivot_longer(Actual:pris_au_plot.csv, names_to = "var_raw", values_to = "whipple") %>%
  left_join(age_file_name_crosswalk, by = c("var_raw" = "file_name")) %>%
  mutate(var = ifelse(var_raw == "Actual", "Actual", source)) %>%
  filter(whipple < 200)

# comparison plot with ALL sources (for slides)
fig_whipple_compare_slides <- ggplot(
  whipple_plot_compare %>% filter(var != "Actual" & var != "Beijing Military"), aes(x = YRIMM, y = whipple, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 168.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_line(alpha = 0.7) +
  scale_color_manual(breaks = c("AUS Prisoners",
                                "Indonesia Migrants",
                                "US Migrants",
                                "AUS Syd Migrants",
                                "AUS Melb/Bris/Darw Migrants",
                                "Chinese 1953 Census"),
                     values = colorRampPalette(colors = c(c3, c5))(6)) +
  scale_shape_manual(breaks = c("AUS Prisoners",
                                "Indonesia Migrants",
                                "US Migrants",
                                "AUS Syd Migrants",
                                "AUS Melb/Bris/Darw Migrants",
                                "Chinese 1953 Census"),
                     values = c(3, 4, 2, 18, 10, 0)) +
  geom_point(data = whipple_plot_compare %>% filter(var == "Actual" & whipple >= 100), aes(x = YRIMM, y = whipple, size = n), shape = 16, color = c1) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Imm.", size = "", color = "", shape = "") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/whipple_compare_selection_all.png"), fig_whipple_compare_slides, height = 5, width = 8)

## EXTRA FIGURES: Comparison by Birth Cohort ----
# merging register with chinese age data by birth cohort -- separately by tax group (CAN)
whipple_compare_bc <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  group_by(year, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = n()) %>%
  mutate(dataseries = glue("CA Tax = {tax}")) %>%
  select(c(year, whipple, dataseries, n)) 

# recreating original graph
whipple_bybc_original <- ggplot(china_age %>% rename(whipple = age_raw, dataseries = source),
                                aes(x = year, y = whipple, color = dataseries, shape = dataseries)) +
  geom_point() + geom_line()

# plotting register data on top of original graph
whipple_plot_path <- glue("{dbox}/raw/china_age_plot_data")
whipple_csvs <- list.files(whipple_plot_path, pattern = ".*csv$")

ggplot(rbind(whipple_compare_bc %>% filter(whipple >= 100), 
             china_age %>% rename(whipple = age_raw, dataseries = file_name) %>% select(-c(yr_raw)) %>% filter(year <= 1900)), 
       aes(x = year, y = whipple, color = dataseries, shape = dataseries)) + geom_point(aes(size = n)) + 
  scale_color_manual(name = "dataseries", breaks = c(c(whipple_csvs),"CA Tax = 0", "CA Tax = 1496.19", "CA Tax = 2992.61", "CA Tax = 14115.7"),
                     values = c(rep("grey",7), c5, c4, c2, c1)) + theme_minimal() + geom_line(alpha = 0.5) 

## REGRESSIONS ----
whipple_reg_data <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & 
                                         MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_age %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "age_raw"), 
            by = c("year")) %>%
  mutate(whipplediff = WHIPPLE - mig_au_melb_b_d_plot.csv)

whipple_reg1 <- lm(data = whipple_reg_data, WHIPPLE ~ factor(tax) + AGE)
coeftest(whipple_reg1, vcov. = vcovHC(whipple_reg1, vcov = "HC1"))

whipple_reg2 <- lm(data = whipple_reg_data, whipplediff ~ factor(tax) + AGE)
coeftest(whipple_reg2, vcov. = vcovHC(whipple_reg2, vcov = "HC1"))

whipple_reg3 <- lm(data = whipple_reg_data, WHIPPLE ~ factor(tax) + factor(year))
coeftest(whipple_reg3, vcov. = vcovHC(whipple_reg3, vcov = "HC1"))


stargazer(whipple_reg1, whipple_reg2, whipple_reg3,
          dep.var.labels = c("Whipple", "Whipple Diff from Melb/Vic", "Whipple"),
          keep = c("factor\\(tax\\)*", "AGE"),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax", "Age"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("5-yr Birth Cohort FE", "No", "No", "Yes")))

# MORE REGS
age_reg <- lm(data = reg_chi %>% filter(YRIMM > 1885 & YRIMM < 1924 & MALE == 1 & FEES != 0), 
              AGE ~ factor(tax) + YRIMM + I(YRIMM^2))
coeftest(age_reg, vcov. = vcovHC(age_reg, vcov = "HC1"))

age_grp <- reg_chi %>% filter(YRIMM < 1924 & YRIMM > 1885 & MALE == 1 & FEES != 0) %>% group_by(YRIMM) %>% summarize(AGE = mean(AGE))
ggplot(age_grp, aes(x = YRIMM, y = AGE)) + geom_line() + geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) 

for (bw in 3:10){
  print(paste0("BW:", bw))
  yrmin = 1900-bw
  yrmax = 1904+bw
  x1 <- breakpoints(CHIFLOW_REGISTER ~ YRIMM, 
                    data = flow_regress %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3, breaks = 2)
  print(filter(flow_regress, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x1$breakpoints])
  
  x2 <- breakpoints(AGE ~ YRIMM, 
                    data = age_grp %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3, breaks = 2)
  print(filter(age_grp, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x2$breakpoints])
  
  x3 <- breakpoints(height ~ YRIMM, 
                    data = height_plot %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3)
  print(filter(height_plot, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x3$breakpoints])
}

test1 <- flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1923)

sctest(CHIFLOW_REGISTER ~ YRIMM + IMMFLOW_NATL, 
       data = test1, type = "Chow", point = 15)

sctest(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), 
       data = test1, type = "supF", plot = TRUE)

sctest(CHIFLOW_REGISTER ~ YRIMM, 
       data = test1 %>% filter(YRIMM >= 1890 & YRIMM <= 1903), type = "Chow", point = 10, plot = TRUE)

#reg1 <- lm(CHIFLOW_REGISTER ~ IMMFLOW_NATL + log(POPSTOCKLAG_China) + EMIG_TOT, data = chowdata)
#chowdata$resid <- resid(reg1)

chowgraphdata <- function(df, k=2, controls = ""){
  n = nrow(df)
  fstat <- numeric(n)
  pval <- numeric(n)
  critval <- numeric(n)
  yrs <- numeric(n)
  for (i in (k+1):(n-k-1)){
    #print(df$YRIMM[i])
    chowtest <- sctest(as.formula(paste0("CHIFLOW_REGISTER ~ YRIMM",controls)), 
                       data = df, type = "Chow", point = i)
    yrs[i] <- df$YRIMM[i]
    fstat[i] <- chowtest$statistic
    pval[i] <- chowtest$p.value
    critval[i] <- qf(0.95, k, n-(2*k))
    #print(chowtest$statistic)
    #print(chowtest$p.value)
  }
  chow_out <- data.frame(fstat = fstat, pval = pval, critval = critval, YRIMM = yrs) %>% filter(YRIMM > 0)
  return(chow_out)
}

chowdata_all <- flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1921)
chowdata_pre <- flow_regress %>% filter(YRIMM >= 1894 & YRIMM <= 1903)
chowdata_post <- flow_regress %>% filter(YRIMM >= 1901 & YRIMM <= 1910)

chowdata_out <- bind_rows(list(chowgraphdata(chowdata_all, k = 5, controls = "+ IMMFLOW_NATL + log(POPSTOCKLAG_China) + EMIG_TOT") %>% 
                                 mutate(type = "all", subtype = "all"),
                               chowgraphdata(chowdata_pre) %>% mutate(type = "subset", subtype = "pre"),
                               chowgraphdata(chowdata_post) %>% mutate(type = "subset", subtype = "post")))
ggplot(chowdata_out, aes(x = YRIMM, y = fstat, color = subtype)) + 
  geom_point() + geom_line(aes(y = critval)) + facet_wrap(~type, ncol = 1, scales = "free")
#ggplot(chow_out, aes(x = YRIMM, y = pval)) + geom_point()


sctest(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + log(POPSTOCKLAG_China), 
       data = test1, type = "Chow", point = 16)


breakpoints(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), 
            data = test1, breaks = 1, h = 8)

supmz(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), data = test1)
changepoints(segment(test1$CHIFLOW_REGISTER, method = "pelt"))


x2 <- breakpoints(AGE ~ YRIMM, 
                  data = age_grp %>% filter(YRIMM >= 1895 & YRIMM <= 1908),
                  h = 3)
summary(x2)

x3 <- breakpoints(HEIGHT ~ YRIMM, 
                  data = height_plot %>% filter(YRIMM >= 1895 & YRIMM <= 1908),
                  h = 3)
summary(x2)


x2 <- breakpoints(AGE ~ rand, 
                  data = age_grp %>% filter(YRIMM >= 1895 & YRIMM <= 1908) %>%
                    group_by(row_number()) %>%
                    mutate(rand = runif(1)),
                  h = 3)
summary(x2)

flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

#_____________________________________________________________
# DiD Regressions: Can Imm control --------------------------
#_____________________________________________________________
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM >= 1882 & YRIMM <= 1913) %>% #only keeping years with earnings/yrimm data
  mutate(LABOR = ifelse(OCC1950 %in% c(970, 650, 820, 830), 1, 0), #labor: includes general laborer, mine laborer, farm laborer
         BORNCHI = ifelse(BPL == "China", 1, 0),
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
         BORNCHI_twoyearpost500 = BORNCHI*ifelse(twoyearpost == "post500", 1, 0))

did_data_japan <- did_data %>% filter(BORNCHI == 1 | BPL == "Japan") %>% filter(YRIMM >= 1890 & YRIMM <= 1907)# & YEAR != 1921)

# # test
# test <- did_data %>% group_by(BORNCHI, YRIMM) %>% 
#   summarize(n = sum(WEIGHT),
#             across(c(LABOR, SPEAKENG, CANREAD, EARN, EMPLOYED, EMPLOYEE), function(.x) weighted.mean(.x, WEIGHT, na.rm=TRUE)))
# 
# ggplot(test, aes(x = YRIMM, y = LABOR, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = SPEAKENG, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = CANREAD, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EARN, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EMPLOYED, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EMPLOYEE, color = factor(BORNCHI), size = n))+ geom_point()

# normal regs
didlab <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
             data = did_data %>% filter(MALE == 1 & AGE >= 18), #%>% filter(OCC1950 != 290 & OCC1950 != 983), 
             weights = WEIGHT)

dideng <- lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
             data = did_data %>% filter(AGE >= 6 & MALE == 1), 
             weights = WEIGHT)

didread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
              data = did_data %>% filter(AGE >= 18 & MALE == 1), weights = WEIGHT)

didwhip <- lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
              data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT)

didearn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
              data = did_data %>% filter(MALE == 1 & AGE >= 18), weights = WEIGHT)

# japan
didjlab <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
              data = did_data_japan %>% filter(MALE == 1 & AGE >= 18), #%>% filter(OCC1950 != 290 & OCC1950 != 983), 
              weights = WEIGHT)

didjeng <- lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
              data = did_data_japan %>% filter(AGE >= 6 & MALE == 1), 
              weights = WEIGHT)

didjread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
               data = did_data_japan %>% filter(AGE >= 18 & MALE == 1), weights = WEIGHT)

didjwhip <- lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
               data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT)

didjearn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
               data = did_data_japan %>% filter(MALE == 1 & AGE >= 18), weights = WEIGHT)

# summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
# summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
# 
# summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1), weights = WEIGHT))
# summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1), weights = WEIGHT))
# 
# summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
# summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
# 
# 
# did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                       data = did_data, weights = WEIGHT)
# did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                    data = did_data, weights = WEIGHT)
# did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                        data = did_data, weights = WEIGHT)
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

# 
# # normal japan regs
# did_reg_labor_japan <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                           data = did_data_japan, weights = WEIGHT)
# did_reg_canread_japan <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                             data = did_data_japan, weights = WEIGHT)
# did_reg_earn_japan <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                          data = did_data_japan, weights = WEIGHT)
# did_reg_houseown_japan <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                              data = did_data_japan, weights = WEIGHT)

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
# did_data_means <- c(weighted.mean(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
#                     weighted.mean(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
#                     weighted.mean(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
# did_data_japan_means <- c(weighted.mean(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
#                           weighted.mean(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
#                           weighted.mean(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))
# did_data_ses <- c(wtd_se(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
#                     wtd_se(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
#                     wtd_se(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
# did_data_japan_ses <- c(wtd_se(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
#                           wtd_se(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
#                           wtd_se(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))

did_data_means_chi <- c(weighted.mean(filter(did_data, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$LABOR,
                                      filter(did_data, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                        weighted.mean(filter(did_data, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$CANREAD,
                                      filter(did_data, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                        weighted.mean(filter(did_data, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$SPEAKENG,
                                      filter(did_data, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$WEIGHT),
                        weighted.mean(filter(did_data, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WHIPPLE,
                                      filter(did_data, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WEIGHT),
                        weighted.mean(filter(did_data, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$EARN,
                                      filter(did_data, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT)
)

did_data_means_chi_jap <- c(weighted.mean(filter(did_data_japan, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$LABOR,
                                          filter(did_data_japan, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                            weighted.mean(filter(did_data_japan, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$CANREAD,
                                          filter(did_data_japan, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                            weighted.mean(filter(did_data_japan, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$SPEAKENG,
                                          filter(did_data_japan, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$WEIGHT),
                            weighted.mean(filter(did_data_japan, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WHIPPLE,
                                          filter(did_data_japan, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WEIGHT),
                            weighted.mean(filter(did_data_japan, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$EARN,
                                          filter(did_data_japan, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT)
)
# stargazer(did_reg_labor, did_reg_canread, did_reg_houseown,did_reg_labor_japan, did_reg_canread_japan, did_reg_houseown_japan,
#           out = glue("{git}/figs/selection.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           column.labels = c("All Immigrants (1880-1913)","Chinese \\& Japanese Imm. (1890-1907)"),
#           column.separate = c(3,3),
#           dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$",
#                              "$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$"),
#           keep = c("BORNCHI*"),
#           covariate.labels = c("$\\hat{\\beta}_{1}$ ($BORNCHI$)", 
#                                "$\\hat{\\gamma}_{50}^{DD}$ ($BORNCHI \\times$ \\$50 Tax)", 
#                                "$\\hat{\\gamma}_{100}^{DD}$ ($BORNCHI \\times$ \\$100 Tax)", 
#                                "$\\hat{\\gamma}_{500}^{DD}$ ($BORNCHI \\times$ \\$500 Tax)"),
#           keep.stat=c("n","adj.rsq"),
#           table.layout = "=cld#-ta-s-",
#           add.lines = list(c("Dep. Var. Mean (SE)", formatC(did_data_means, format = "f"), formatC(did_data_japan_means, format = "f"), "\\\\"),
#                            c("", paste0("(",c(formatC(did_data_ses, format = "f"), formatC(did_data_japan_ses, format = "f")), ")"), "\\\\")))

# slides: all immig
stargazer(didlab, didread, dideng, didwhip, didearn,
          out = glue("{git}/output/slides/selection_all.tex"), float = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$",
                             "$\\mathbb{P}[\\text{Speaks English}]$", "Age Heaping", "Earnings"),
          keep = c("BORNCHI*"), 
          covariate.labels = c("$\\hat{\\beta}_{1}$ (Born in China)", 
                               "$\\hat{\\gamma}_{50}^{DD}$ ($C_i \\times$ \\$50 Tax)", 
                               "$\\hat{\\gamma}_{100}^{DD}$ ($C_i \\times$ \\$100 Tax)", 
                               "$\\hat{\\gamma}_{500}^{DD}$ ($C_i \\times$ \\$500 Tax)"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "d-t-as",
          add.lines = list(c("Dep. Var. Mean (Chinese)", formatC(did_data_means_chi, format = "f"))))


#slides: japanese immig
stargazer(didjlab, didjread, didjeng, didjwhip, didjearn,
          out = glue("{git}/output/slides/selection_japan.tex"), float = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$",
                             "$\\mathbb{P}[\\text{Speaks English}]$", "Age Heaping", "Earnings"),
          keep = c("BORNCHI*"), 
          covariate.labels = c("$\\hat{\\beta}_{1}$ (Born in China)", 
                               "$\\hat{\\gamma}_{100}^{DD}$ ($C_i \\times$ \\$100 Tax)", 
                               "$\\hat{\\gamma}_{500}^{DD}$ ($C_i \\times$ \\$500 Tax)"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "d-t-as",
          add.lines = list(c("Dep. Var. Mean (Chinese)", formatC(did_data_means_chi_jap, format = "f"))))


#_____________________________________________________________
# DiD Regressions: US Imm control --------------------------
#_____________________________________________________________
did_data_us <- us_imm %>% filter(YEAR >= 1900 & YRIMM >= 1880) %>% #& YRIMM <= 1913) %>% #only keeping years with earnings/yrimm data
  mutate(LABOR = ifelse(OCC1950 %in% c(970, 650, 820, 830), 1, 0), #labor: includes general laborer, mine laborer, farm laborer
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         BORNCHI_notax = BORNCHI * ifelse(tax == 0, 1, 0),
         AGEATIMM = AGE - (YEAR - YRIMM),
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
         BORNCHI_twoyearpost500 = BORNCHI*ifelse(twoyearpost == "post500", 1, 0),
         WEIGHT = 1,
         US = 1) # %>%
#filter(MALE == 1 & AGEATIMM >= 18) 

did_data_compare <- bind_rows(did_data_us, did_data %>% mutate(US = 0))

# test
test <- did_data_compare %>% filter(BORNCHI == 1) %>% group_by(US, YRIMM) %>% 
  summarize(n = sum(WEIGHT),
            across(c(LABOR, CANREAD, EARN), function(.x) weighted.mean(.x, WEIGHT, na.rm=TRUE)))

ggplot(test, aes(x = YRIMM, y = LABOR, color = factor(US), size = n))+ geom_point()
ggplot(test, aes(x = YRIMM, y = CANREAD, color = factor(US), size = n))+ geom_point()
ggplot(test, aes(x = YRIMM, y = EARN, color = factor(US), size = n))+ geom_point()

# normal regs
did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                       data = did_data_us %>% filter(OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1), weights = WEIGHT)

did_reg_labor_compare <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + factor(US)*factor(tax), 
                            data = did_data_compare %>% filter(YRIMM < 1913 & YRIMM >= 1886 & OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1 & BORNCHI == 1), weights = WEIGHT)

did_reg_canread_compare <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + factor(US)*factor(tax), 
                              data = did_data_compare %>% filter(YRIMM < 1913 & OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1 & BORNCHI == 1), weights = WEIGHT)


summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))

summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1), weights = WEIGHT))
summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1), weights = WEIGHT))

summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))

