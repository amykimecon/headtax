# CLEANING PARSED CI44 FORMS
library(glue)
library(tidyverse)

# FILE PATHS AND URLS ----
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}

# helper code
source(glue("{git}/code/0_dataclean/ci44_helper.R"))

# declaring constants
startreel = 16164 #usually 16164
endreel = 16184 #usually 16184
CONF_THRESH = 0 # confidence threshold: for now, keep all entries regardless of confidence
MATCH_SCORE_THRESH = 0.75
canadastrings = c("Victoria", "Vancouver", "Ottawa", "Canada", "Ontario", "B\\.C\\.", "Moose Jaw", "Toronto", 
                  "British Columbia", "Cumberland", "New Westminster", "Montreal", "Hamilton")
nativebornstrings = c("Native Born", "Can Born", "Birth Registered", "Birth Certificate")
ci_re = "[C|O|0|c|G]\\.*\\s*[1|I|l|L|T|i]\\.*\\s*" #regex to capture versions of "CI"
month_long = str_to_lower(month.name)
month_short = str_to_lower(month.abb)

# READING DATA ----
# register data
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, 
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))

# parsed ci44 data
#clean_csv_paths <- c(glue("{dbox}/cleaned/ci44/test5/test5out_basemodel_jan25.csv"))
clean_csv_paths <- paste0(dbox, "/cleaned/ci44/t", c(startreel:endreel), "out_basemodel_Jan29.csv")
rawdfs <- list()
i = 1
for (path in clean_csv_paths){
  rawdfs[[i]] <- read_csv(path)
  i = i + 1
}
parsed_raw <- bind_rows(rawdfs)

# CLEANING PARSED DATA ----
## initial tidy ----
parsed_tidy <- parsed_raw %>% filter(confidence >= CONF_THRESH) %>%
  # first: if multiple instances of a type of entity (e.g. physical marks), rename type by number
  group_by(filename, type) %>%
  mutate(rep_type = row_number(), type_mut = paste0(type, rep_type)) %>%
  ungroup() %>%
  # cleaning up names: if max one type of entity across all reels, rename to remove trailing one
  group_by(type) %>%
  mutate(typen = n_distinct(type_mut),
         type = ifelse(typen > 1, type_mut, type)
  ) %>%
  select(c(type, filename, reel, img, mentionText))

## checking missings ----
missing_imgs <- check_raw_imgs(parsed_tidy, outpath = glue("{dbox}/cleaned/missing_ci44_imgs.csv"),
                               missings_compare = TRUE)
# write_csv(missing_imgs, glue("{dbox}/cleaned/missing_ci44_imgs.csv"))
# missing_imgs <- read_csv(glue("{dbox}/cleaned/missing_ci44_imgs.csv"))

## pivot to wide ----
parsed_wide <- parsed_tidy %>% 
  pivot_wider(id_cols = c(reel, img, filename), names_from = type, values_from = mentionText) %>%
  bind_rows(missing_imgs) %>%
  unite(Remarks, starts_with("Remarks"), sep = " ", na.rm=TRUE) %>%
  mutate(ID_ci44 = row_number(),
         NativeBorn = case_when(agrepl("China", BirthplaceCountry, ignore.case=TRUE) ~ FALSE, #detect birthplace china
                                sapply(BirthplaceCountry, agrepls, patterns = c(canadastrings, "Native Born"))|
                                  sapply(BirthplaceDistrict, agrepls, patterns = c(canadastrings, "Native Born"))|
                                  sapply(BirthplaceVillage, agrepls, patterns = c(canadastrings, "Native Born"))~ TRUE, #detect birthplace canada
                                sapply(Remarks, agrepls, patterns = nativebornstrings) | str_detect(str_to_lower(Remarks),"born at") ~ TRUE, #detect keywords in remarks
                                stringdist("Native Born", EntryPort) <= 4 ~ TRUE,
                                TRUE ~ FALSE),
         NativeBornStrict = case_when(agrepl("China", BirthplaceCountry, ignore.case=TRUE) ~ FALSE, #detect birthplace china
                                      sapply(BirthplaceCountry, agrepls, patterns = c(canadastrings, "Native Born"))|
                                        sapply(BirthplaceDistrict, agrepls, patterns = c(canadastrings, "Native Born"))|
                                        sapply(BirthplaceVillage, agrepls, patterns = c(canadastrings, "Native Born"))~ TRUE, #detect birthplace canada
                                      (sapply(Remarks, agrepls, patterns = nativebornstrings) | str_detect(str_to_lower(Remarks),"born at")) &
                                        (is.na(BirthplaceCountry) + is.na(BirthplaceDistrict) + is.na(BirthplaceVillage) +
                                           is.na(EntryPort) + is.na(EntryShip)) >= 4 ~ TRUE, #detect keywords in remarks AND missing at least 4/5 of birthplace/entry
                                      TRUE ~ FALSE))
  
## cleaning fields ----
parsed_clean <- parsed_wide %>%
  mutate(
    # cleaning 'arrivalcert' fields -- separating into ci type, location and number
    across(starts_with("ArrivalCert"),
           list("Type_Clean" = ~sapply(str_extract(.,paste0(ci_re, "([0-9]+|S)")), cert_type_extract, arrivalcert = TRUE),
                "Loc_Clean" = ~str_match(., "([V|v|O][A-Za-z0-9]+)")[,2],
                "Number_Clean" = ~as.numeric(str_match(., "([0-9]{3,6})")[,2]))),
    # cleaning 'endorsedcert' number and type fields
    EndorsedCertNumber_Clean = as.numeric(str_match(str_replace_all(EndorsedCertNumber, "[^0-9]",""), "([0-9]+)")[,2]),
    EndorsedCertType_Clean = sapply(EndorsedCertType, cert_type_extract),
    # cleaning 'cert' number and type fields
    CertNumber_Clean = as.numeric(str_match(str_replace_all(CertNumber, "[^0-9]",""), "([0-9]+)")[,2]),
    CertType_Clean = sapply(CertType, cert_type_extract), 
    # cleaning entry ship, name, aka*, entry port, age fields
    EntryShip_ci44 = sapply(EntryShip, clean_entryship),
    Name_ci44 = str_to_title(trimws(str_replace_all(str_replace_all(Name, "[^a-zA-Z]", " "), "\\s+", " "))),
    across(starts_with("aka"), 
           ~str_to_title(trimws(str_replace_all(str_replace_all(.x, "[^a-zA-Z]", " "), "\\s+", " "))), .names = "{.col}_ci44"),
    across(starts_with("aka"), ~ifelse(. == "", NA, .)),
    EntryPort_ci44 = case_when(
      str_detect(EntryPort, "Van|ancouv|ver") ~ "Vancouver",
      str_detect(EntryPort, "Vict|ict|ria|tor") ~ "Victoria",
      TRUE ~ trimws(str_replace_all(str_to_title(EntryPort), "[^a-zA-Z]", " "))
    ),
    Age_clean = as.numeric(str_extract(Age, "[0-9]+")),
    Age_ci44 = ifelse(Age_clean < 99, Age_clean, NA),
    EntryDateYear_ci44 = str_extract(str_replace_all(EntryDate,"[^a-zA-Z0-9\\s]", " "), "[0-9]{4}"),
    EntryDateMonthLong = sapply(EntryDate, bestpartialmatch, patterns = month_long),
    EntryDateMonthShort = sapply(EntryDate, bestpartialmatch, patterns = month_short),
    EntryDateMonth_ci44 = case_when(!is.na(EntryDateMonthLong) ~ EntryDateMonthLong,
                                    !is.na(EntryDateMonthShort) ~ EntryDateMonthShort,
                                    TRUE ~ sapply(EntryDate, bestpartialmatch, patterns = month_long, max.distance = 0.25)),
    EntryDateDay1 = as.numeric(str_match(EntryDate, "([0-9]{1,2})(?:th|nd|rd|st)")[,2]),
    EntryDateDay2 = as.numeric(str_match(EntryDate, "[^0-9]([0-9]{1,2})(?:,|\\/)")[,2]),
    EntryDateDay = case_when(!is.na(EntryDateDay1) ~ EntryDateDay1,
                             !is.na(EntryDateDay2) ~ EntryDateDay2,
                             str_detect(EntryDate, "^[0-9]{1,2}[^0-9]") ~ as.numeric(str_match(EntryDate, "^([0-9]{1,2})[^0-9]")[,2]),
                             TRUE ~ as.numeric(str_match(EntryDate, "[^0-9]([0-9]{1,2})[^0-9]")[,2])),
    EntryDateDay_ci44 = ifelse(EntryDateDay > 31 | EntryDateDay <= 0, NA, EntryDateDay),
    RegYear_Clean = str_remove_all(RegisteredDate, "[^0-9]"),
    RegYear_ci44 = case_when(
      str_detect(RegYear_Clean, "1924") ~ 1924,
      str_detect(RegYear_Clean, "1923") ~ 1923,
      reel == 16164 & img <= 2597 ~ 1923,
      reel < 16184 ~ 1924,
      reel == 16184 & str_detect(RegYear_Clean, "192[0-9]") ~ as.numeric(str_extract(RegYear_Clean, "192[0-9]")),
      TRUE ~ 1924
    ),
    BirthYear_ci44 = RegYear_ci44-Age_ci44,
  ) 

## harmonizing cert types ----
parsed_bycert <- parsed_clean %>%
  filter(!NativeBorn) %>%
  harmonize_cert_type(5) %>% 
  harmonize_cert_type(28) %>% 
  harmonize_cert_type(30) %>% 
  harmonize_cert_type(36) %>% 
  harmonize_cert_type(4) %>%
  harmonize_cert_type(0) %>%
  select(all_of(c(ends_with("ci44"))))


# CLEANING REGISTER DATA ----
# cleaning reg_chi other_ci column
# to clean and add: profession, village/county/province, conveyance, arr_port
reg_chi_clean <- reg_chi %>%
  mutate(Name_reg = str_to_title(str_remove_all(NAME_OF_CH, "(\\?|\\(.*\\))")),
         aka_reg = str_to_title(str_match(NAME_OF_CH, "\\((.*)\\)")[,2]),
         OtherCI_Number = as.numeric(str_match(Other_CI, "#\\s*([0-9]{4,5})")[,2]),
         CI36_reg = as.numeric(case_when(
           str_detect(Other_CI, paste0(ci_re,"36")) ~ 
             str_match(Other_CI, paste0(ci_re,"36\\s?,?\\s*(?:#|$|No|no|No\\.)\\s*([0-9]{1,6})"))[,2],
           str_detect(Other_CI, "(R|r)eg") ~ str_match(Other_CI, "Reg\\s*(?:#|$|No|no|No\\.)\\s*([0-9]{1,6})")[,2],
           TRUE ~ NA_character_
         )),
         CI28_reg = as.numeric(case_when(
           str_detect(Other_CI, paste0(ci_re,"28")) ~ 
             str_match(Other_CI, paste0(ci_re, "28\\s?,?\\s*(?:#|$|No|no|No\\.)\\s*([0-9]{1,6})"))[,2],
           TRUE ~ NA_character_
         )),
         OtherCI_clean = as.numeric(str_match(Other_CI, "([0-9]{5})")[,2]),
         OtherCI_reg = ifelse(is.na(CI36_reg) & is.na(CI28_reg) & !is.na(Other_CI),
                              case_when(!is.na(OtherCI_clean) ~ OtherCI_clean,
                                        str_detect(Other_CI, "#") ~ as.numeric(str_match(Other_CI, "#\\s*([0-9]{1,6})")[,2]),
                                        str_detect(Other_CI, ci_re) ~ 
                                          as.numeric(str_match(Other_CI, paste0(ci_re, "([0-9]{3,6})"))[,2]),
                                        TRUE ~ NA_integer_), NA),
         CI30_reg = as.numeric(case_when(str_detect(CI5, paste0(ci_re,"30")) ~ 
                                           trimws(str_remove_all(str_remove(CI5, paste0(ci_re, "30")), "[^0-9]")),
                                         str_detect(CI5, "\\(C\\(30\\)") ~ trimws(str_remove(CI5, "\\(C\\(30\\)")),
                                         TRUE ~ NA_character_)),
         CI5_reg = as.numeric(case_when(str_detect(CI5, "[0-9]{5,6}") ~ str_extract(CI5, "[0-9]{5,6}"),
                                        str_detect(CI5, paste0(ci_re, "30")) ~ NA_character_,
                                        str_detect(CI5, "\\(C\\(30\\)") ~ NA_character_,
                                        str_detect(CI5, "#") ~ str_match(CI5, "#([0-9]{3,6})")[,2],
                                        !str_detect(CI5, "[^0-9]") ~ CI5,
                                        TRUE ~ NA_character_,
         )),
         CI6_reg = ifelse(CI6 == 0, NA, CI6),
         EntryDateMonth_reg = ifelse(MONTH_ARRI==0, NA, MONTH_ARRI),
         EntryDateDay_reg = ifelse(DATE_ARRIV==0,NA,DATE_ARRIV),
         EntryDateYear_reg = ifelse(YEAR_ARRIV==0,NA,YEAR_ARRIV),
         BirthYear_reg = BIRTHYEAR,
         EntryPort_reg = ARR_PORT,
         EntryShip_reg = sapply(CONVEYANCE, clean_entryship)) %>%
  rename_with(~ paste0(.x, "_reg", recycle0 = TRUE), c(ID, SEX, AGE)) %>%
  select(all_of(ends_with("reg")))

# MERGING ----
# getting column name lists
names_ci44 <- c("Name_ci44", names(parsed_bycert)[which(str_detect(names(parsed_bycert), "aka"))])
names_reg <- c("Name_reg", "aka_reg")

other_cols_ci44 = names(parsed_bycert)[which(str_detect(names(parsed_bycert), "CI0"))]
other_cols_reg = c("OtherCI_reg")
all_cols_ci44 = names(parsed_bycert)[which(str_detect(names(parsed_bycert), "^CI"))]
all_cols_reg = c(paste0("CI",c(5,6,28,30,36), "_reg"), other_cols_reg)

# main merge (by ci type)
merge_main <- bind_rows(lapply(list(5, 28, 30, 36), merge_dfs))

# secondary merge (ci number with unspecified type)
merge_other <- merge_dfs_other(
  merge_cols_ci44 = other_cols_ci44,
  merge_cols_reg = other_cols_reg,
  match_cols_ci44 = all_cols_ci44,
  match_cols_reg = all_cols_reg,
)

merge_all <- bind_rows(list(merge_main, merge_other))

# finding poorly/well matched
## first: taking best match from all possible matches 
merge_all_post <- merge_all %>% group_by(ID_ci44) %>% 
  arrange(desc(total_score), .by_group = TRUE) %>% 
  mutate(i = row_number()) %>% filter(i == 1)
## poorly matched: totalscore below threshold
poorlymatched_prenames <- merge_all_post %>% filter(total_score < MATCH_SCORE_THRESH)
## well matched
wellmatched_prenames <- merge_all_post %>% filter(total_score >= MATCH_SCORE_THRESH)

# NOW taking unmatched/poorly matched and fuzzy matching on names
merge_namesdf <- merge_dfs_other(
  merge_cols_ci44 = names_ci44,
  merge_cols_reg = names_reg,
  match_cols_ci44 = names_ci44,
  match_cols_reg = names_reg,
  dfci44 = filter(parsed_bycert, !(ID_ci44 %in% wellmatched_prenames$ID_ci44)),
  matchcolname = "namematchcol",
  fuzzymatch = TRUE
)

merge_names_post <- bind_rows(list(merge_namesdf, merge_all)) %>% 
  merge_stats() %>% 
  group_by(ID_ci44) %>%
  mutate(nonname_score = entry_match_score+birth_match_score) %>%
  arrange(desc(nonname_score), desc(namesim), .by_group = TRUE) %>% 
  mutate(i = row_number(), n = n(), 
         certmatches = sum(ifelse(is.na(namedist_osa) & is.na(namedist_cos), 1, 0))) %>% 
  filter(i == 1)

## poorly matched: totalscore below threshold
poorlymatched <- merge_names_post %>% filter(total_score < MATCH_SCORE_THRESH)
## well matched
wellmatched <- merge_names_post %>% filter(total_score >= MATCH_SCORE_THRESH)

# finding unmatched
unmatched <- parsed_clean %>% 
  filter(!(ID_ci44 %in% merge_names_post$ID_ci44))

# reporting matches and match rates
print(glue("{nrow(wellmatched)} good matches {round((100*nrow(wellmatched))/nrow(parsed_bycert),1)}%"))
print(glue("{nrow(poorlymatched)} bad matches {round((100*nrow(poorlymatched))/nrow(parsed_bycert),1)}%"))
print(glue("{nrow(unmatched)} unmatched {round((100*nrow(unmatched))/nrow(parsed_bycert),1)}%"))

# saving matches 
matches <- bind_rows(list(merge_names_post, unmatched))
write_csv(matches, glue("{dbox}/cleaned/matchtest1_jan30.csv"))

# ship matches
x <- merge_names_post %>%
  mutate(entryshipmatch = mapply(ship_name_sim, EntryShip_ci44, EntryShip_reg))
View(x %>% select(c(EntryShip_ci44, EntryShip_reg, entryshipmatch, total_score)))

shipcheck <- merge_names_post %>% 
  mutate(EntryShip_reg = sapply(EntryShip_reg,clean_entryship),
         EntryShip_ci44 = sapply(EntryShip_ci44,clean_entryship)) %>%
  filter(total_score >= 0.8) %>%
  group_by(EntryShip_reg, EntryShip_ci44) %>% summarize(n=n())

bluefunnel <- unique(filter(shipcheck,
                     adist("Blue Funnel", EntryShip_ci44, partial = TRUE)[1,] <= 2 &
                       n >= 5)$EntryShip_reg)

cpr <- unique(filter(shipcheck,
                            adist("C P R", EntryShip_ci44, partial = TRUE)[1,] <= 0 &
                              n >= 5)$EntryShip_reg)

japboat <- unique(filter(shipcheck,
                         adist("Jap Boat", EntryShip_ci44, partial = TRUE)[1,] <= 1 &
                           n >= 5)$EntryShip_reg)

via <- unique(filter(shipcheck,
                         adist("Via", EntryShip_ci44, partial = TRUE)[1,] <= 0 &
                           n >= 2)$EntryShip_reg)

usboat <- unique(filter(shipcheck,
                        adist("Northern Pacific", EntryShip_ci44, partial = TRUE)[1,] <= 3 &
                          n >= 5)$EntryShip_reg)

View(reg_chi %>% filter(YRIMM <= 1923) %>% 
       mutate(CONVEYANCE = sapply(CONVEYANCE, clean_entryship)) %>%
       group_by(CONVEYANCE) %>% summarize(n=n()))

# looking at matches
View(merge_all_post %>% arrange(ID_ci44) %>% #filter(!is.na(Name_reg) & entry_match_score < 3) %>%
       select(c(ID_ci44, ID_reg, 
                total_score, namesim, cert_match, true_match, entry_match_score, birth_match_score, mergedon,
                #CI36_ci44, CI36_reg, CI36EXTRA_ci44,
                #CertNumber, EndorsedCertNumber, ArrivalCert1, ArrivalCert2,
                #CI5, CI6, Other_CI,
                EntryPort_ci44, EntryPort_reg, EntryShip_ci44, EntryShip_reg,
                Name_ci44, Name_reg, BirthYear_ci44, BirthYear_reg,
                EntryDateYear_ci44, EntryDateYear_reg, 
                EntryDateMonth_ci44, EntryDateMonth_reg, 
                EntryDateDay_ci44, EntryDateDay_reg)))

# rewriting unmatched info
unmatched_rewrite <- check_raw_imgs(unmatched,
                                    outpath = glue("{dbox}/cleaned/missing_ci44_imgs.csv"))


