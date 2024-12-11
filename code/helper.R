#_____________________________________________________________
# FILE DESCRIPTION: Helper functions for Head Tax Project
# CREATED BY: Amy Kim
# CREATED ON: April 2023
# LAST MODIFIED: April 2024
#_____________________________________________________________
#_____________________________________________________________
# DATA CLEANING FUNCTIONS ----
#_____________________________________________________________
## Removing Duplicates from Raw Census Files
# rawdata is full dataset of raw census variables
# idvars are 
remove_duplicates <- function(rawdata, idvars){
  rem_na <- rawdata[, colSums(is.na(rawdata)) == 0]
  print(ncol(rem_na)/ncol(rawdata))
  print(nrow(rawdata))
  print(nrow(distinct(rawdata)))
  print(nrow(distinct(rem_na)))
}


#_____________________________________________________________
# SUMMARY STATS TABLES ----
#_____________________________________________________________
## Calculating Weighted Standard Errors
wtd_se <- function(x, w){
  i <- !is.na(x)
  w <- w[i]
  x <- x[i]
  n_eff = (sum(w)**2)/sum(w**2)
  return(sqrt(wtd.var(x, w)/(n_eff)))
}

## Creating summary stats for a given dataset, given list of variables
summstats <- function(rawdata, vars){
  outdata <- rawdata %>% select(c(source, group, WEIGHT, all_of(vars))) %>%
    group_by(source, group) %>% 
    summarize(across(-c(WEIGHT), 
                     .fns = c(~weighted.mean(.x, WEIGHT, na.rm=TRUE), #mean
                              ~wtd_se(.x, WEIGHT))), #SE
              OBS = n()) #n obs
  return(outdata)
}

#_____________________________________________________________
# COMPUTING COUNTERFACTUAL ----
#_____________________________________________________________
## takes as input a dataframe and a model for inflow, outputs dataframe with 'counterfactual inflow' column
## cen is indicator for census (vs register)

cf_flow_calc <- function(df, model, startyr = 1885, endyr = 1921, 
                         cen = FALSE, lintax = FALSE, logpopstock = FALSE, logflow = FALSE){
  # select relevant columns, calculate total flow and popstock change over total period
  flowregcf_df <- df %>% 
    select(c(CHIFLOW_REGISTER, FLOW_China, POPSTOCKLAG_China, POPSTOCKCHANGE_China, EMIG_TOT, IMMFLOW_NATL, IMMFLOW_CENSUS, YRIMM, tax, DECADE)) %>%
    arrange(YRIMM) %>%
    group_by(DECADE) %>% 
    mutate(totflow = sum(CHIFLOW_REGISTER), 
           outflow = (totflow-POPSTOCKCHANGE_China)/n(),
           outflow_share = ifelse(cen == TRUE, 0, (totflow-POPSTOCKCHANGE_China)/totflow)) %>% ungroup() %>%
    filter(YRIMM >= startyr & YRIMM <= endyr)
  
  # extracting relevant coefficients from model
  intercept <- model$coefficients[["(Intercept)"]]
  coef_emigtot <- model$coefficients[["EMIG_TOT"]]
  if (cen){
    coef_immflownatl <- model$coefficients[["IMMFLOW_CENSUS"]]
  }else{
    coef_immflownatl <- model$coefficients[["IMMFLOW_NATL"]]    
  }
  
  if (logpopstock){
    coef_logpopstock <- model$coefficients[["log(POPSTOCKLAG_China)"]]
  }else{
    coef_popstock <- model$coefficients[["POPSTOCKLAG_China"]]
    coef_popstock2 <- model$coefficients[["I(POPSTOCKLAG_China^2)"]]
  }
  
  if (lintax){
    if ("tax" %in% names(model$coefficients)){
      coef_tax <- model$coefficients[["tax"]]      
    }else{
      coef_tax <- model$coefficients[["log(tax)"]]
    }
  }else{
    if ("factor(tax)1496.19" %in% names(model$coefficients)){
      coef_tax50 <- model$coefficients[["factor(tax)1496.19"]]    
    }else{
      coef_tax50 = 0
    }
    coef_tax100 <- model$coefficients[["factor(tax)2992.61"]]
    coef_tax500 <- model$coefficients[["factor(tax)14115.7"]]
  }
  
  # initializing flow variables
  popstock_cf = flowregcf_df$POPSTOCKLAG_China[1]
  popstock_cf_all <- numeric(length = nrow(flowregcf_df))
  flow_cf_all <- numeric(length= nrow(flowregcf_df))
  flow_pred_all <- numeric(length = nrow(flowregcf_df))
  
  # iterating through years to propogate popstock
  for (i in 1:nrow(flowregcf_df)){
    popstock_cf_all[i] <- popstock_cf
    
    if (cen){
      flow_temp = intercept + coef_emigtot*flowregcf_df$EMIG_TOT[i] + coef_immflownatl*flowregcf_df$IMMFLOW_CENSUS[i]
    }else{
      flow_temp = intercept + coef_emigtot*flowregcf_df$EMIG_TOT[i] + coef_immflownatl*flowregcf_df$IMMFLOW_NATL[i]
      # flow_cf = intercept + coef_emigtot*flowregcf_df$EMIG_TOT[i] + coef_immflownatl*flowregcf_df$IMMFLOW_NATL[i] + coef_popstock*popstock_cf + coef_popstock2*(popstock_cf^2)
      # flow_pred = intercept + coef_emigtot*flowregcf_df$EMIG_TOT[i] + coef_immflownatl*flowregcf_df$IMMFLOW_NATL[i] + 
      #   coef_popstock*flowregcf_df$POPSTOCKLAG_China[i] + coef_popstock2*(flowregcf_df$POPSTOCKLAG_China[i]^2) +
      #   ifelse(flowregcf_df$tax[i] == 1496.19, coef_tax50, 0) + ifelse(flowregcf_df$tax[i] == 2992.61, coef_tax100, 0) + ifelse(flowregcf_df$tax[i] == 14115.7, coef_tax500, 0)
    }
    
    
    if (logpopstock){
      flow_cf = flow_temp + coef_logpopstock*log(popstock_cf)
      flow_pred = flow_temp + coef_logpopstock*log(flowregcf_df$POPSTOCKLAG_China[i])
    }else{
      flow_cf = flow_temp + coef_popstock*popstock_cf + coef_popstock2*(popstock_cf^2)
      flow_pred = flow_temp + coef_popstock*flowregcf_df$POPSTOCKLAG_China[i] + coef_popstock2*(flowregcf_df$POPSTOCKLAG_China[i]^2)
    }
    
    if (lintax){
      flow_pred = flow_pred + coef_tax*ifelse("tax" %in% names(model$coefficients), flowregcf_df$tax[i], log(flowregcf_df$tax[i]))
    }else{
      flow_pred = flow_pred + ifelse(flowregcf_df$tax[i] == 1496.19, coef_tax50, 0) + ifelse(flowregcf_df$tax[i] == 2992.61, coef_tax100, 0) + ifelse(flowregcf_df$tax[i] == 14115.7, coef_tax500, 0)
    }
    # print(glue("YEAR:{flowregcf_df$YRIMM[i]}"))
    # print(glue("ACTUAL:{flowregcf_df$FLOW_China[i]}"))
    # print(glue("CF:{flow_cf}"))
    if (logflow){
      popstock_cf = popstock_cf + (1-flowregcf_df$outflow_share[i])*exp(flow_cf)
      flow_cf_all[i] <- exp(flow_cf)
      flow_pred_all[i] <- exp(flow_pred)
    }else{
      popstock_cf = popstock_cf + (1-flowregcf_df$outflow_share[i])*flow_cf
      flow_cf_all[i] <- flow_cf 
      flow_pred_all[i] <- flow_pred
    }
  }
  
  # mutating df to include cf flow
  if (cen){
    flowregcf_out <- flowregcf_df %>% mutate(`Actual Inflow` = FLOW_China, 
                                             `Counterfactual Inflow` = flow_cf_all, 
                                             diff = `Counterfactual Inflow` - `Actual Inflow`,
                                             `Predicted Inflow` = flow_pred_all)
  }else{
    flowregcf_out <- flowregcf_df %>% mutate(`Actual Inflow` = CHIFLOW_REGISTER, 
                                             `Counterfactual Inflow` = flow_cf_all, 
                                             diff = `Counterfactual Inflow` - `Actual Inflow`,
                                             `Predicted Inflow` = flow_pred_all)
  }

  return(flowregcf_out)
}

cf_flow_out <- function(df){
  print(df %>% group_by(tax) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`), 
                                           tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), 
                                           yrs= n()) %>% 
          mutate(diff= (tot_flow_cf-tot_flow_act)/yrs,
                 pct_change = (tot_flow_cf - tot_flow_act)/tot_flow_cf))
  print(df %>% group_by(1) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`, na.rm=TRUE), 
                                         tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), yrs= n()) %>% 
          mutate(diff= (tot_flow_cf-tot_flow_act)/yrs,
                 pct_change = (tot_flow_cf - tot_flow_act)/tot_flow_cf))
  
  fig_flowregcf <- ggplot(df %>% 
                            pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`), names_to = "cat", values_to = "flow") %>%
                            mutate(flow = flow/1000),
                          aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
    scale_color_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(c4,c1)) +
    scale_linetype_manual(breaks = c("Actual Inflow", "Counterfactual Inflow"), values = c(1, 2)) +
    geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs <= endyr), show.legend = FALSE, color = "#808080", linetype = 3) +
    geom_text(aes(x = yrs, y = max(df$`Counterfactual Inflow`)/1000, label = labs), data = headtaxcuts %>% filter(yrs <= endyr), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
    labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')
  
  return(fig_flowregcf)
}

cf_flow_predout <- function(df){
  print(df %>% group_by(tax) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`), 
                                           tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), 
                                           yrs= n()) %>% 
          mutate(diff= (tot_flow_cf-tot_flow_act)/yrs,
                 pct_change = (tot_flow_cf - tot_flow_act)/tot_flow_cf))
  print(df %>% group_by(1) %>% summarize(tot_flow_cf = sum(`Counterfactual Inflow`, na.rm=TRUE), 
                                         tot_flow_act = sum(`Actual Inflow`, na.rm=TRUE), yrs= n()) %>% 
          mutate(diff= (tot_flow_cf-tot_flow_act)/yrs,
                 pct_change = (tot_flow_cf - tot_flow_act)/tot_flow_cf))
  
  fig_flowregcf <- ggplot(df %>% 
           pivot_longer(c(`Actual Inflow`,`Counterfactual Inflow`, `Predicted Inflow`), names_to = "cat", values_to = "flow") %>%
           mutate(flow = flow/1000),
         aes(x=YRIMM, y = flow, color = cat, linetype = cat)) + geom_line() +
    geom_vline(aes(xintercept = yrs), data = headtaxcuts %>% filter(yrs <= endyr), show.legend = FALSE, color = "#808080", linetype = 3) +
    geom_text(aes(x = yrs, y = max(df$`Counterfactual Inflow`)/1000 - 2, label = labs), data = headtaxcuts %>% filter(yrs <= endyr), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
    labs(x = "Year of Immigration", y = "Chinese Immigrant Inflow (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')
  
  return(fig_flowregcf)
}


