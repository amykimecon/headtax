## TESTING FASTER DATA ANALYSIS WITH DUCKDB
library(duckdb)

# creating connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

# reading in all ipums census data to create raw tables by year
for (year in seq(1910,1940,10)){
  duckdb_read_csv(con, glue("censusraw{year}"), glue("{rawdata}/census_{year}.csv"))
  #dbExecute(con, glue("ALTER TABLE censusraw{year} ADD COLUMN YEAR_{year} INTEGER DEFAULT 1"))
}

# concatenating into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE censusrawall AS",
                     "SELECT * FROM censusraw1910 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1920 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1930 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1940"))

link_colnames <- c("SERIAL","STATEICP", "COUNTYICP", "SEX", "AGE", "MARST", "RACE", "LABFORCE", "CLASSWKR", "OCC1950")
# reading in all census tree linking files and creating linked tables
for (linkyear in seq(1920,1940,10)){
  dbExecute(con, glue("CREATE TEMP TABLE link{linkyear} AS FROM read_csv_auto('{rawdata}/{linkyear-10}_{linkyear}.csv')"))
  dbExecute(con, glue("CREATE TEMP TABLE linkedtemp{linkyear} AS SELECT * FROM censusraw{linkyear - 10} JOIN link{linkyear} ON (HISTID = histid{linkyear - 10})"))
  dbExecute(con, glue("CREATE TABLE linked{linkyear} AS SELECT * FROM linkedtemp{linkyear} linkedtemp{linkyear} JOIN censusraw{linkyear} ON (histid{linkyear} = censusraw{linkyear}.HISTID)"))
  for (colname in link_colnames){
    dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN {colname} TO {colname}_base"))
    dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN '{colname}:1' TO {colname}_link"))
  }
}

# concatenating into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE linkedall AS",
                     "SELECT * FROM linked1920 UNION ALL BY NAME",
                     "SELECT * FROM linked1930 UNION ALL BY NAME",
                     "SELECT * FROM linked1940"))

dbGetQuery(con, "DESCRIBE linkedall")

## FILTERING INTO DATAFRAMES (note: need to fix years first)
# teacher_sample <- tbl(con, "censusrawall") %>% filter(OCC1950 == 93) %>% as.data.frame()
# linked_teach <- tbl(con, "linkedall") %>% filter(OCC1950_base == 93 | OCC1950_link == 93) %>% as.data.frame()

## LINKED INDIV MANUALLY FOR NOW
linked_teach_list = list()
for (linkyear in seq(1920,1940,10)){
  linked_teach_list[[i]] <- tbl(con,glue("linked{linkyear}")) %>% filter(OCC1950_base == 93 | OCC1950_link == 93) %>% 
    as.data.frame() %>% mutate(YEAR_base = linkyear - 10, YEAR_link = linkyear)
  i = i + 1
}
linked_teach <- bind_rows(linked_teach_list)
