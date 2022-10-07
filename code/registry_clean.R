########################################################################
### FILE DESCRIPTION: Looking at Chinese Registry Data
### PRIMARY OBJECTIVE: Graphing Trends, etc.
### CREATED BY: Amy Kim
### CREATED ON: Sep 29 2022
### LAST MODIFIED: Sep 29 2022
########################################################################
library(Hmisc)
library(tidyverse)
library(glue)
library(foreign)
library(ggpubr)
library(readxl)
library(gtools)

########################################################################
### DEFINING PATHS
########################################################################
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

########################################################################
### IMPORTING DATA
########################################################################
## chinese registry -- immigration data through ports
# source: https://open.library.ubc.ca/cIRcle/collections/facultyresearchandpublications/52383/items/1.0075988
chiregraw <- read_excel(glue("{dbox}/ChineseRegistry.xlsx"), guess_max = 1000000)

chireg <- chiregraw %>% mutate(COUNTYGRP = case_when(COUNTY_ID == 1 ~ "Taishan",
                                                     COUNTY_ID == 2 ~ "Xinhui",
                                                     COUNTY_ID == 3 ~ "Kaiping",
                                                     COUNTY_ID == 9 ~ "Panyu",
                                                     COUNTY_ID == 6 ~ "Zhongshan",
                                                     COUNTY_ID == 4 ~ "Enping",
                                                     COUNTY_ID == 5 ~ "Heshan",
                                                     TRUE ~ "Other"),
                               YEAR = YEAR_ARRIV,
                               MONTH = str_pad(MONTH_ARRI, 2, "left", pad = "0"),
                               DAY = str_pad(DATE_ARRIV, 2, "left", pad = "0"),
                               DATE = case_when(DAY == "00" & MONTH != "00" ~ as.Date(glue("{YEAR}-{MONTH}-15"), format = "%Y-%m-%d"),
                                                MONTH == "00" & YEAR != 0 ~ as.Date(glue("{YEAR}-07-01"), format = "%Y-%m-%d"),
                                                TRUE ~ as.Date(glue("{YEAR}-{MONTH}-{DAY}"), format = "%Y-%m-%d")),
                               PORT = case_when(ARR_PORT == "Victoria" ~ "Victoria",
                                                ARR_PORT == "Vancouver" ~ "Vancouver",
                                                TRUE ~ "Other"),
                               OCCGRP = case_when(PROFESSION == "Labourer" ~ "Labourer",
                                                  PROFESSION == "Student" | PROFESSION == "Merchant" ~ "Student or Merchant",
                                                  PROFESSION == "Farmer" ~ "Farmer",
                                                  PROFESSION == "Laundryman" | PROFESSION == "Cook" | PROFESSION == "Grocer" ~ "Service",
                                                  TRUE ~ "Other"),
                               WC = case_when(SEX == "Male" & AGE >= 18 ~ "Adult Male",
                                              SEX == "Female" & AGE >= 18 ~ "Adult Female",
                                              AGE < 18 ~ "Child",
                                              TRUE ~ "Unknown"),
                               NAMECLEAN = str_remove(NAME_OF_CH, "\\s*\\(.*\\)$"),
                               LASTNAME = str_to_lower(str_remove(str_extract(NAMECLEAN, "\\s\\S*$"), "^\\s")),
                               FIRSTNAME = str_to_lower(str_remove(NAMECLEAN, "\\s\\S*$")),
                               BIRTHYEAR = REG_Year - AGE,
                               MALE = ifelse(SEX == "Male", 1, 0),
                               REGID = row_number()) %>%
  filter(!is.na(DATE) & YEAR != 0) #excluding entries without valid arrival date (for now) -- only 522 such entries out of ~100k
write_csv(chireg, glue("{dbox}/cleaned/chireg.csv"))

########################################################################
### ATTEMPTING TO MERGE WITH CENSUS DATA
########################################################################
## MUST BE: Census Collection Year >= Registry Arrival Year
## Should be: Birthyears are relatively similar
## Should be: Sexes are same
## May be: Years of immigration are relatively similar
chiregmatch <- chireg %>% filter(!is.na(NAME_OF_CH))
matchnames <- list()
regids <- chiregmatch$REGID
matchnamesraw <- gsub("-", " ",str_remove(str_remove(gsub("[(|)]", "", chiregmatch$NAME_OF_CH), "^\\s"),"\\s$"))
for(i in 1:nrow(chiregmatch)){
  matchnames[[regids[i]]] <- str_trim(str_to_lower(unique(str_split(matchnamesraw[i], " ")[[1]])))
}

searchformatch <- function(lastname, firstname, male, birthyear, yearcensus,
                           matching = matchnames, matchdata = chiregmatch){
  names <- c(str_split(lastname, " ")[[1]], str_split(firstname, " ")[[1]]) %>% str_trim()
  #print(names)
  tosearch <- filter(matchdata, MALE == male & YEAR <= yearcensus & abs(BIRTHYEAR - birthyear) <= 10)$REGID #also birth years within 10 yrs
  matchids <- c()
  maxmatch = -1
  for (searchid in tosearch){
    matchnamei = matching[[searchid]]
    matchnum = 0
    #print(matching[[searchid]])
    for (name in names){
      if (name %in% matchnamei){
        #print(matchnamei)
        matchnum = matchnum + 1
      }
    }
    matchpct = matchnum/length(names)
    if (matchpct > 0.5){ #if over half of names are matched
      if (matchpct > maxmatch){ #if best match, replace previous match
        matchids <- c(searchid)
        maxmatch = matchpct
      }
      else if (matchpct == maxmatch){ #if tied, add to list
        matchids <- append(matchids, searchid) 
      }
    }
  }
  #print(matchids)
  if (length(matchids) == 1){
    return(matchids[1])
  }
  else if (length(matchids) == 0){
    return(NA)
  }
  else{
    closestid = -1
    closestdiff = 11
    for (matchid in matchids){
      regbirthyear <- matchdata$BIRTHYEAR[which(matchdata$REGID == matchid)]
      if(abs(regbirthyear - birthyear) < closestdiff){
        closestid = matchid
        closestdiff = abs(regbirthyear - birthyear)
      }
    }
    return(closestid)
  }
}

clean_chi <- read_csv(glue("{dbox}/cleaned/census_chi.csv")) %>%
  mutate(LASTNAME = str_to_lower(LASTNAME) %>% str_remove("\\(chi.*\\)") %>% str_remove("\\sch$") %>% str_trim(), 
         FIRSTNAME = str_to_lower(FIRSTNAME) %>% str_remove("\\(chi.*\\)") %>% str_remove("\\sch$") %>% str_trim(),
         BIRTHYEAR = YEAR - AGE) %>% 
  filter((!is.na(FIRSTNAME) | !is.na(LASTNAME)) & !is.na(BIRTHYEAR) & !is.na(MALE)) #dropping if missing firstname and lastname

#randomly sample for testing
#clean_chi_test <- clean_chi[sample.int(n = nrow(clean_chi), size = 10),]
merge_raw <- clean_chi %>% rowwise() %>% mutate(regmatchid = searchformatch(LASTNAME, FIRSTNAME, MALE, BIRTHYEAR, YEAR)) %>%
  left_join(chiregmatch, by = c("regmatchid" = "REGID"))

write_csv(merge_raw, glue("{dbox}/cleaned/merge_census_reg.csv"))

# 
# directmerge_chi <- clean_chi %>% filter(!is.na(FIRSTNAME) | !is.na(LASTNAME)) %>% # census data must have at least one of firstname & lastname
#   left_join(chireg, by = c("LASTNAME","FIRSTNAME")) %>%
#   mutate(BIRTHYEARCEN = YEAR.x - AGE.x, BIRTHYEARREG = REG_Year - AGE.y) %>%
#   select(c(LASTNAME, FIRSTNAME, YRIMM, YEAR.y, BIRTHYEARCEN, BIRTHYEARREG, YEAR.x, NAMECLEAN))

########################################################################
### INITIAL ANALYSIS
########################################################################
chinums <- chireg %>% filter(REG_Year != 0) %>% group_by(REG_Year) %>% 
  summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = REG_Year)
chinums_arrive <- chireg %>% group_by(YEAR_ARRIV) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = YEAR_ARRIV)

# entry by year by origin county (largest counties: Taishan = 1, Xinhui = 2, Kaiping = 3, Panyu = 9, Zhongshan = 6, Enping = 4, Heshan = 5)
chiorig <- chireg %>% group_by(COUNTYGRP, YEAR) %>% summarize(n=n()) %>%
  group_by(YEAR) %>% mutate(pct = n/sum(n))
ggplot(chiorig, aes(x = YEAR, y = n, fill = COUNTYGRP)) + geom_area()+ 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry by year by arrival port (victoria, vancouver, other)
ggplot(chireg %>% group_by(PORT, YEAR) %>% summarize(n=n()) %>% group_by(YEAR) %>% mutate(pct = n/sum(n)), 
       aes(x = YEAR, y = pct, fill = PORT)) + geom_area() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry by year by occupation group (for adults)
ggplot(chireg %>% filter(AGE >= 18 & SEX == "Male") %>% group_by(OCCGRP, YEAR) %>% summarize(n=n()) %>% group_by(YEAR) %>% mutate(pct = n/sum(n)), 
       aes(x = YEAR, y = n, fill = OCCGRP)) + geom_area() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923) + geom_vline(xintercept = 1912)

# entry in levels -- comparing registration year to arrival year
ggplot(rbind(select(chinums, c(YEAR, n)) %>% mutate(cat = "Registration"), 
             select(chinums_arrive, c(YEAR, n)) %>% mutate(cat = "Arrival")) %>% filter(YEAR != 0), 
       aes(x = YEAR, y = n, color = factor(cat))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry in levels -- tax payers vs non tax payers
chibytax <- chireg %>% filter(AGE > 18) %>% mutate(taxpay = ifelse(FEES > 0, 1, 0)) %>% group_by(YEAR, taxpay) %>% summarize(n = n())
ggplot(chibytax, aes(x = YEAR, y = n, color = factor(taxpay))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# entry in levels by sex
ggplot(chireg %>% group_by(WC, YEAR) %>% summarize(n=n()), aes(x = YEAR, y = n, color = factor(WC))) + geom_line() + 
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)

# tax
ggplot(chinums_arrive %>% filter(YEAR != 0), aes(x = YEAR, y = tax)) + geom_line() +
  geom_vline(xintercept = 1885) + geom_vline(xintercept = 1900) + geom_vline(xintercept = 1903) + geom_vline(xintercept = 1923)






