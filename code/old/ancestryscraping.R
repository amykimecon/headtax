########################################################################
### FILE DESCRIPTION: Scraping Port of Entry Data from Ancestry.com
### CREATED BY: Amy Kim
### CREATED ON: Oct 27 2022
### LAST MODIFIED: Oct 27 2022
########################################################################
library(tidyverse)
library(glue)
library(rvest)
library(RSelenium)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

########################################################################
### WEB SCRAPING
########################################################################
rD <- rsDriver(browser = "chrome", chromever = "88.0.4324.96", port = 10L)
remDr <- rD[["client"]]
remDr$navigate()
















