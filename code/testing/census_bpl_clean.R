#_____________________________________________________________
# FILE DESCRIPTION: Canadian Census Birthplace Data Cleaning -- Creating Crosswalks
# CREATED BY: Amy Kim
# CREATED ON: April 2024
# LAST MODIFIED: April 2024
#_____________________________________________________________

#_____________________________________________________________
# IMPORTING RAW CENSUS DATA ----
#_____________________________________________________________
## canadian census data from 1871-1921
raw1871 <- read.spss(glue("{dbox}/raw/census1871.sav")) %>% as.data.frame() #1.8% stratified sample w weights
raw1881 <- read.spss(glue("{dbox}/raw/census1881.sav")) %>% as.data.frame() # 100% sample
raw1891 <- read.spss(glue("{dbox}/raw/census1891.sav")) %>% as.data.frame() # 5% sample
raw1901 <- read.spss(glue("{dbox}/raw/census1901.sav")) %>% as.data.frame() # 5% sample
raw1911 <- read.spss(glue("{dbox}/raw/census1911.sav")) %>% as.data.frame() # 5% sample
raw1921 <- read.spss(glue("{dbox}/raw/census1921.sav")) %>% as.data.frame() # 4% sample

#_____________________________________________________________
# 1871 Canadian Census ----
#_____________________________________________________________






