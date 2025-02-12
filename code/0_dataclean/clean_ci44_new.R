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
entryportstrings = c(canadastrings[which(canadastrings != "B\\.C\\.")],
                     "Athelstan", "Cumberland", "Esquimalt", "Halifax", "Quebec", "Sarnia", "St John")
nativebornstrings = c("Native Born", "Can Born", "Birth Registered", "Birth Certificate")
ci_re = "[C|O|0|c|G]\\.*\\s*[1|I|l|L|T|i]\\.*\\s*" #regex to capture versions of "CI"
month_long = str_to_lower(month.name)
month_short = str_to_lower(month.abb)

# READING DATA ----
## register data ----
reg_chi <- read_csv(glue("{dbox}/cleaned/chireg.csv")) %>% 
  mutate(source = "xRegister", group = "Chinese Immigrants", WEIGHT = 1, YRIMM = YEAR, 
         WHIPPLE = 500*ifelse(AGE %% 5 == 0, 1, 0),
         tax = case_when(YRIMM <= 1885 ~ 0,
                         YRIMM <= 1900 ~ 1496.19,
                         YRIMM <= 1903 ~ 2992.61,
                         YRIMM < 1924 ~ 14115.70))

## parsed ci44 data ----
#clean_csv_paths <- c(glue("{dbox}/cleaned/ci44/test5/test5out_basemodel_jan25.csv"))
clean_csv_paths <- paste0(dbox, "/cleaned/ci44/t", c(startreel:endreel), "out_basemodel_Jan29.csv")
rawdfs <- list()
i = 1
for (path in clean_csv_paths){
  rawdfs[[i]] <- read_csv(path)
  i = i + 1
}
parsed_raw <- bind_rows(rawdfs)

## other helpers ----
county_cw <- read_csv(glue("{dbox}/cleaned/reg_county_cw.csv"))
countystrings <- filter(county_cw, n >= 200)$COUNTY_CLEAN

# CLEANING REGISTER DATA ----
# cleaning reg_chi other_ci column
# to clean and add: profession, village/county/province, conveyance, arr_port
reg_chi_clean <- reg_chi %>%
  mutate(Name_reg = str_to_title(str_remove_all(NAME_OF_CH, "(\\?|\\(.*\\))")),
         Name_reg = ifelse(Name_reg == "", NA, Name_reg),
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
         EntryShip_reg = sapply(CONVEYANCE, clean_entryship),
         BplDistrict_reg = str_to_lower(str_remove_all(COUNTY, "[^A-z]")),
         BplDistrictNew_reg = ifelse(str_replace(NEW_COUNTY, "\\?","") %in% unique(county_cw$NEW_COUNTY),
                                     str_to_lower(str_replace(NEW_COUNTY, "\\?","")), NA),
         BplVillage_reg = str_to_title(VILLAGE_NA)) %>%
  rename_with(~ paste0(.x, "_reg", recycle0 = TRUE), c(ID, SEX, AGE)) %>%
  select(all_of(ends_with("reg")))

# pivoting long on cert type
reg_chi_long <- reg_chi_clean %>%
  pivot_longer(c("CI36_reg", "CI28_reg", "CI30_reg", "CI5_reg", "CI6_reg", "OtherCI_reg"),
               names_to = "CertType_reg", values_to = "CertNumber_reg") %>%
  mutate(CertType_reg = as.numeric(str_extract(CertType_reg, "[0-9]{1,2}"))) %>%
  filter(!is.na(CertNumber_reg))

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
# missing_imgs <- check_raw_imgs(parsed_tidy, outpath = glue("{dbox}/cleaned/missing_ci44_imgs.csv"),
#                                missings_compare = TRUE)
# write_csv(missing_imgs, glue("{dbox}/cleaned/missing_ci44_imgs.csv"))
# missing_imgs <- read_csv(glue("{dbox}/cleaned/missing_ci44_imgs.csv"))

## pivot to wide ----
parsed_wide <- parsed_tidy %>% 
  pivot_wider(id_cols = c(reel, img, filename), names_from = type, values_from = mentionText) %>%
  #bind_rows(missing_imgs) %>%
  unite(Remarks, starts_with("Remarks"), sep = " ", na.rm=TRUE) %>%
  mutate(ID_ci44 = row_number(),
         NativeBorn = case_when(agrepl("China", BirthplaceCountry, ignore.case=TRUE) ~ FALSE, #detect birthplace china
                                sapply(BirthplaceCountry, agrepls, patterns = c(canadastrings, "Native Born"))|
                                  sapply(BirthplaceDistrict, agrepls, patterns = c(canadastrings, "Native Born"))|
                                  sapply(BirthplaceVillage, agrepls, patterns = c(canadastrings, "Native Born"))~ TRUE, #detect birthplace canada
                                sapply(Remarks, agrepls, patterns = nativebornstrings) | str_detect(str_to_lower(Remarks),"born at") ~ TRUE, #detect keywords in remarks
                                stringdist("native born", str_to_lower(EntryPort)) <= 4 ~ TRUE,
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
    EntryPort_clean = sapply(str_remove_all(EntryPort, "[^A-z\\s]"), bestpartialmatch, 
                             patterns = entryportstrings, returnval = TRUE, max.distance = 0.2),
    EntryPort_ci44 = case_when(
      !is.na(EntryPort_clean) ~ str_to_title(EntryPort_clean),
      !is.na(EntryPort) ~ str_to_title(str_replace_all(str_remove_all(str_remove(EntryPort, "(?<!^)([B|E|R|N|P|H|3]\\s*\\.*\\s*[C|G|c|e|0|B|Q|Θ|O|S]\\s*\\.*\\s*|Ont|Que|,.*)\\s*\\.*\\s*$"), 
                                         "[^A-z\\s]"), "\\s+", "\\s")),
      TRUE ~ NA_character_
    ),
    Occupation_clean = str_to_lower(str_replace_all(str_replace_all(Occupation, "[^A-z\\s]", " "), "\\s+", " ")),
    Occupation_ci44 = Occupation_clean, #sapply(Occupation_clean, bestpartialmatch, occstrings, returnval = TRUE, max.distance = 0.4),
    Age_clean = as.numeric(str_extract(Age, "[0-9]+")),
    Age_ci44 = ifelse(Age_clean < 99, Age_clean, NA),
    BplDistrict_clean = sapply(str_to_lower(str_remove_all(BirthplaceDistrict,"[^A-z]")),
                               bestpartialmatch, countystrings, returnval = TRUE, max.distance = 0.4),
    BplDistrict_ci44 = case_when(
      !is.na(BplDistrict_clean) ~ BplDistrict_clean,
      !is.na(BirthplaceDistrict) ~ str_to_lower(str_remove_all(BirthplaceDistrict,"[^A-z]")),
      TRUE ~ NA_character_
    ),
    BplVillage_ci44 = str_to_title(trimws(str_replace_all(str_replace_all(BirthplaceVillage, "[^A-z]", " "), "\\s+", " "))),
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
    RegNumber_Clean = as.numeric(str_match(str_replace_all(RegistrationNumber, "[^0-9]",""), "([0-9]+)")[,2]),
    BirthYear_ci44 = RegYear_ci44-Age_ci44,
    across(c(starts_with("aka"), all_of(c("Name_ci44", "EntryPort_ci44", "BplVillage_ci44",
                    "BplDistrict_ci44", "EntryShip_ci44"))), ~ifelse(. == "", NA, .)),
  ) %>%
  left_join(county_cw, by = c("BplDistrict_ci44" = "COUNTY_CLEAN")) %>%
  mutate(BplDistrictNew_ci44 = ifelse(!is.na(NEW_COUNTY), str_to_lower(NEW_COUNTY), NA))

## finding duplicates
dupids <- find_duplicates(parsed_clean)
notdups <- c("ham june",
             "ham tan", 
             "jang chun wong", 
             "jang hoy", 
             "lai dye", 
             "lee hong pong", 
             "lee lung", 
             "low kung yow", 
             "ma kow que", 
             "mah gim goon", 
             "wong bing lee", "wong foo", "wong sam")

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

parsed_long <- parsed_clean %>%
  filter(!NativeBorn) %>%
  harmonize_cert_type(tolong = TRUE) %>%
  select(ends_with("ci44")) %>%
  group_by(ID_ci44, CertNumber_ci44) %>%
  mutate(CertNumberRepeat_ci44 = n()) %>% ungroup() %>%
  group_by(ID_ci44, CertNumber_ci44, CertType_ci44) %>%
  mutate(CertTypeRepeat_ci44 = n(),rownum = row_number()) %>%
  filter(rownum == 1) %>% ungroup()
  
# MERGING ----
## getting column name lists ----
#names_ci44 <- c("Name_ci44", names(parsed_long)[which(str_detect(names(parsed_long), "aka") & 
#                                                          str_detect(names(parsed_long), "ci44"))])
names_ci44 <- c("Name_ci44", "aka1_ci44")
names_reg <- c("Name_reg", "aka_reg")

all_cols_reg = c(paste0("CI",c(5,6,28,30,36), "_reg"), "OtherCI_reg")

## main merge (by ci number) ----
merge_main <- merge_dfs(dfci44 = parsed_long, dfreg = reg_chi_long, 
                        merge_cols_ci44 = c("CertNumber_ci44"), 
                                  merge_cols_reg = c("CertNumber_reg")) %>%
  # calculating bonus score from certs (0.1 if repeated in ci44, 0.2 if matching cert type)
  mutate(certbonus = ifelse(CertType_ci44 == CertType_reg & !is.na(CertType_ci44) & !is.na(CertType_reg) & CertType_ci44 != 0, 0.2, 0) +
           ifelse(CertNumberRepeat_ci44 > 1, 0.1, 0)) %>%
  # if same people are matched more than once, must be because matched on diff cert types/numbers,
  #   take num of distinct cert number matches per pair and take entry with highest certbonus
  group_by(ID_ci44, ID_reg) %>% 
  arrange(desc(certbonus)) %>%
  mutate(rank = row_number(), nmatch = n_distinct(certmatch)) %>% ungroup() %>%
  filter(rank == 1) %>%
  mutate(certbonus = certbonus + (nmatch - 1)) %>%
  merge_stats() 

merge_main_post <- one_to_one_match(merge_main)

merge_main_post_alt <- merge_main  %>% filter(!is.na(total_score)) %>%#this step is temp 
  group_by(ID_ci44) %>% 
  arrange(desc(total_score), .by_group = TRUE) %>% 
  mutate(maxscore_ci44 = max(total_score),
         rank_ci44 = row_number()) %>%
  ungroup() %>% filter(rank_ci44 == 1)

## poorly matched: totalscore below threshold
poorlymatched_prenames <- merge_main_post %>% filter(total_score < 1.5)
## well matched
wellmatched_prenames <- merge_main_post %>% filter(total_score >= 1.5)

# NOW taking unmatched/poorly matched and fuzzy matching on names
merge_namesdf_int <- merge_dfs(
  merge_cols_ci44 = names_ci44,
  merge_cols_reg = names_reg,
  dfci44 = filter(parsed_bycert, !(ID_ci44 %in% wellmatched_prenames$ID_ci44)),
  dfreg = filter(reg_chi_clean, !(ID_reg %in% wellmatched_prenames$ID_reg)),
  matchcolname = "namematchcol",
  fuzzymatch = TRUE
) 
merge_namesdf <- merge_namesdf_int[which(!duplicated(select(merge_namesdf_int, ID_ci44, ID_reg))),] %>% 
  mutate(certbonus = 0) %>% 
  merge_stats(inclnamesim = FALSE) %>%
  filter(nonname_score >= 0.25) %>% merge_stats(inclnamesim = TRUE)

merge_all_raw <- bind_rows(list(merge_namesdf %>% mutate(mainmerge = 0),
                                merge_main %>% mutate(mainmerge = 1))) %>%
  mutate(total_score_old = total_score,
         total_score = total_score + mainmerge*0.5,
         total_score_raw_old = total_score_raw,
         total_score_raw = total_score_raw + mainmerge*0.5) #0.5 bonus for cert match

# merge_names_post <- merge_all_raw %>% 
#   one_to_one_match()

merge_names_post <- merge_all_raw[which(!duplicated(select(merge_all_raw, ID_ci44, ID_reg, total_score))),] %>%
  group_by(ID_ci44) %>% arrange(desc(total_score)) %>%
  mutate(n = n(),
         medscore = median(total_score),
         maxscore = max(total_score),
         secondscore = max(total_score*ifelse(row_number() == 2, 1, 0)),
         scorediff = maxscore - secondscore) 

allmaxes <- merge_names_post %>% filter(row_number() == 1)
  
match1 <- merge_names_post %>%
  filter(maxscore == total_score & scorediff >= 0.5 & total_score >= 1.5 & (total_score > 2 | total_score_raw > 1))

match2 <- merge_names_post %>% 
  filter(row_number() == 1 & 
           ((scorediff < 0.5 & total_score >= 2 & total_score_raw > 1)|
              (scorediff >= 0.5 & total_score < 1.5 & total_score >= 1.4 & total_score_raw > 1)))

match3 <- merge_names_post %>% filter(row_number() == 1) %>%
  filter(!(ID_ci44 %in% match1$ID_ci44) & !(ID_ci44 %in% match2$ID_ci44)) %>%
  filter(total_score >= 1.5 & total_score_raw > 1 & nonname_score > 0.5)

wellmatched <- bind_rows(list(match1, match2, match3))

poorlymatched <- merge_names_post %>% filter(row_number() == 1) %>%
  filter(!(ID_ci44 %in% wellmatched$ID_ci44))

# finding unmatched
unmatched <- parsed_bycert %>% 
  filter(!(ID_ci44 %in% merge_names_post$ID_ci44))

# reporting matches and match rates
print(glue("{nrow(wellmatched)} good matches {round((100*nrow(wellmatched))/nrow(parsed_bycert),1)}%"))
print(glue("{nrow(poorlymatched)} bad matches {round((100*nrow(poorlymatched))/nrow(parsed_bycert),1)}%"))
print(glue("{nrow(unmatched)} unmatched {round((100*nrow(unmatched))/nrow(parsed_bycert),1)}%"))

# saving matches 
matches <- bind_rows(list(wellmatched %>% mutate(matchstat = "wellmatched"), 
                          poorlymatched %>% mutate(matchstat = "poorlymatched"), 
                          unmatched %>% mutate(matchstat = "unmatched"))) %>%
  left_join(parsed_bycert %>% select(c(ID_ci44, Occupation_ci44)))

write_csv(matches, glue("{dbox}/cleaned/matches_feb11.csv"))

write_csv(merge_names_post, glue("{dbox}/cleaned/merge_names_post_feb11.csv"))


# # rewriting unmatched info
# unmatched_rewrite <- check_raw_imgs(unmatched,
#                                     outpath = glue("{dbox}/cleaned/missing_ci44_imgs.csv"))


