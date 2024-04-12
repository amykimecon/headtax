#_____________________________________________________________
# FILE DESCRIPTION: Helper functions for Head Tax Project
# CREATED BY: Amy Kim
# CREATED ON: April 2023
# LAST MODIFIED: April 2024
#_____________________________________________________________

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

