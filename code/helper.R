## HELPER FUNCTIONS

## SUMMARY STATS TABLES
# helper function to calculate weighted standard errors 
wtd_se <- function(x, w){
  i <- !is.na(x)
  w <- w[i]
  x <- x[i]
  n_eff = (sum(w)**2)/sum(w**2)
  return(sqrt(wtd.var(x, w)/(n_eff)))
}

# helper function to create summary stats for a given dataset, given list of variables
summstats <- function(rawdata, vars = c("MALE", "MAR", "AGE", "CANREAD", "LABOR", "EARN", "YRIMM")){
  outdata <- rawdata %>% select(c(source, group, WEIGHT, all_of(vars))) %>%
    group_by(source, group) %>% 
    summarize(across(-c(WEIGHT), 
                     .fns = c(~weighted.mean(.x, WEIGHT, na.rm=TRUE), #mean
                              ~wtd_se(.x, WEIGHT))), #SE
              OBS = n()) #n obs
  return(outdata)
}