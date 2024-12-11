########################################################################
### FILE DESCRIPTION: Cleaning Raw US Census Data
### PRIMARY OBJECTIVE: Standardizing Primary Variables, Creating Single Multi-Year Dataframe
### CREATED BY: Amy Kim
### CREATED ON: Oct 10 2022
### LAST MODIFIED: Oct 10 2022
########################################################################

#_____________________________________________________________
# CREATING DUCKDB ----
#_____________________________________________________________
if (0){
  # creating connection to duckdb database ----
  con <- dbConnect(duckdb(), dbdir = glue("{dbox}/raw/db.duckdb"))
  
  print("*********** Existing tables in database ************")
  dbGetQuery(con, "SHOW TABLES")
  
  tic(glue("Create or replace table usa_fullcount..."))
  dbExecute(con, glue("CREATE OR REPLACE TABLE usa_fullcount AS FROM read_csv_auto ('{dbox}/raw/usa_fullcount.csv')"))
  toc()
}
#us_raw <- read_csv(glue("{dbox}/raw/usa_fullcount.csv"))
#us_raw_1880 <- read_csv(glue("{dbox}/usa_fullcount_1880.csv"))

#_____________________________________________________________
# CLEANING DATA FOR MERGE W CANADIAN CENSUS DATA ----
#_____________________________________________________________
us_clean <- tbl(con, "usa_fullcount") %>% 
  mutate(BORNCHI = ifelse(BPL == 500, 1, 0),
         BORNJAP = ifelse(BPL == 501, 1, 0),
         IMM = ifelse(NATIVITY == 5, 1, 0),
         MALE = ifelse(SEX == 1, 1, 0),
         CANREAD = ifelse(AGE >= 18, ifelse((LIT == 3 | LIT == 4), 1, 0), NA),
         MAR = ifelse(AGE >= 18, ifelse((MARST == 1 | MARST == 2), 1, 0), NA),
         LABOR = ifelse(AGE >= 18, ifelse((OCC1950 <= 970 & OCC1950 >= 900), 1, 0), NA),
         HOUSEOWN = ifelse(AGE >= 18, ifelse(OWNERSHP == 1, 1, 0), NA),
         IDNUM = row_number(),
         EARN = ifelse(AGE >= 18, ERSCOR50, NA)) %>%
  rename(YRIMM = YRIMMIG) %>%
  select(c(IDNUM,YEAR,STATEFIP,BORNCHI,BORNJAP,IMM, YRIMM, AGE, MALE, CANREAD, LABOR, OCC1950, MAR, HOUSEOWN, EARN))
#write_csv(us_clean, glue("{dbox}/cleaned/us_clean.csv"))

# pre-computing summary stats for all of us
us_all_summ <- summstats(us_clean %>% mutate(source = "US Census", group = "All", WEIGHT = 1), c("MALE", "MAR", "AGE", "CANREAD", "LABOR", "EARN"))
write_csv(us_all_summ, glue("{dbox}/cleaned/us_all_summ.csv"))

### CREATING PRIMARY IMMIGRANT SAMPLE ###
us_imm <- us_clean %>% filter(IMM == 1) %>% collect()
write_csv(us_imm, glue("{dbox}/cleaned/us_clean_imm.csv"))

dbDisconnect(con, shutdown = TRUE)



