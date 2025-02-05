# File Description: Helper Functions for Cleaning Parsed DocAI code (CI44)
# Author: Amy Kim
# Date Created: Thu Jan 30 10:14:22 2025

# IMPORTING PACKAGES ----
library(tidyverse)
library(glue)
library(jpeg)
library(stringdist)
library(fuzzyjoin)

# SETTING WORKING DIRECTORIES ----
root <- "/Users/amykim/Princeton Dropbox/Amy Kim/head_tax/head_tax_data"
code <- "/Users/amykim/Documents/GitHub/<FILL IN>/code"

# compares raw image files with parsed output and displays missing images if true
# df MUST be of type parsed_tidy -- need to be long but have unique fieldnames for duplicate fields
check_raw_imgs <- function(df, outpath,
                           imgroot = glue("{dbox}/raw/ci44"), 
                           missings_compare = FALSE){
  if (missings_compare){
    # get list of images corresp to reels in df
    imglist_raw <- bind_rows(lapply(unique(df$reel), 
                                    function(.x){
                                      data.frame(reel = .x,
                                                 img = as.numeric(str_match(list.files(glue("{imgroot}/t{.x}")), "img([0-9]+)\\.jpg$")[,2]))}))
    
    # list of imgs from df
    imglist_df <- df %>% group_by(reel, img) %>% summarize(parsed = TRUE)
    if (nrow(imglist_raw) <= nrow(imglist_df)){
      print("Error: fewer images in raw source folder than in parsed output. File path misspecified?")
      return(NA)
    }
    
    # merge with df
    merge_imglists <- left_join(imglist_raw, imglist_df) %>% 
      mutate(filepath = glue("{imgroot}/t{reel}/reel{reel}_img{img}.jpg")) %>%
      filter(is.na(parsed))
    
    # iterating through unmatched imgs
    fieldnamesdf <- unique(df$type)
    colnamesdf <- c(fieldnamesdf[which(!str_detect(fieldnamesdf, "PhysicalMarks") & 
                                         !str_detect(fieldnamesdf, "DependentInfo") &
                                         !str_detect(fieldnamesdf, "Remarks"))],
                    "Remarks1")
  }
  else{
    merge_imglists <- df %>% 
      mutate(filepath = glue("{imgroot}/t{reel}/reel{reel}_img{img}.jpg"))
    
    colnamesdf <- c(names(df)[which(!str_detect(names(df), "PhysicalMarks|DependentInfo|Remarks|ArrivalCert|aka|reel|img|filename") & 
                                      !str_detect(names(df), "_Clean|_clean|_ci44|EntryDateMonth|EntryDateDay|NativeBornStrict"))],
                    "Remarks1", "aka1", "ArrivalCert1", "ArrivalCert2")
  }
  
  makenewplot()
  first = TRUE
  for (file in merge_imglists$filepath){
    if (first){
      tempdata <- disp_rawimg(file, colnames = colnamesdf) 
      first = FALSE
    }
    else{
      tempdata <- bind_rows(list(tempdata, disp_rawimg(file, colnames = colnamesdf)))
    }
    write_csv(tempdata, outpath)
  }
  # df_out <- bind_rows(lapply(merge_imglists$filepath, 
  #                                 disp_rawimg,
  #                                 colnames = colnamesdf))
  return(tempdata)
}

# making new plot to display image
makenewplot <- function(){
  # close old plots
  while(dev.cur() != 1){
    dev.off()
  }
  # create new plot
  dev.new(noRStudioGD = TRUE)
  plot(0:11, 0:11, type = "n", ann = FALSE, axes = FALSE)
}

# displays raw image and allows user to input info
disp_rawimg <- function(imgpath, colnames, makeplot = FALSE, userinput = TRUE){
  if (makeplot){
    makenewplot()
  }
  # show image
  img <- readJPEG(imgpath, native = TRUE)
  rasterImage(img, 0, 0, 8.5, 11)
  
  if (!userinput){
    return(NA)
  }
  
  # check if valid form
  validform <- readline("Is this image a valid form? Enter 'no' if not valid: ")
  if (str_to_lower(validform) == "no"){
    return(data.frame())
  }
  
  # if valid form, iterate through column names of interest
  imgdata <- character(length(colnames))
  i = 1
  while (i <= length(colnames)){
    imgread <- readline(glue("Enter {colnames[i]} (enter 'undo' to go back): "))
    if (str_to_lower(imgread) == "undo"){
      i = i - 1
    }
    else{
      imgdata[i] <- imgread
      i = i + 1
    }
  }
  outdata <- data.frame(t(imgdata))
  names(outdata) <- colnames
  return(outdata)
}

# fuzzy search over multiple possible strings
agrepls <- function(str, patterns, max.distance = 0.1){
  for (pattern in patterns){
    if (agrepl(pattern, str, ignore.case= TRUE, max.distance = max.distance)){
      return(TRUE)
    }
  }
  return(FALSE)
}

# fuzzy replace: looks for fuzzy match of 
fuzzy_replace <- function(str, pattern, replace, max.distance = 2){
  bestmatch = NA
  bestmatchdist = nchar(pattern)
  for (i in 1:(nchar(str)-nchar(pattern)+1)){
    substr = substr(str,i,i+nchar(pattern)-1)
    substrdist = stringdist(substr, pattern)
    # print(substr)
    # print(substrdist)
    if (substrdist < bestmatchdist){
      bestmatch = substr
      bestmatchdist = stringdist(substr, pattern)
    }
  }
  if (bestmatchdist <= max.distance){
    return(str_replace(str, bestmatch, replace))
  }
  else{
    return(str)
  }
}

detect_month <- function(str, low_tol = 0.25){
  month_out <- case_when(
    # FIRST: trying fuzzy matching w default distance + exact match for abbrev
    agrepl("January", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "jan") ~ "01",
    agrepl("February", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "feb") ~ "02",
    agrepl("August", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "aug") ~ "08",
    agrepl("September", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "sep") ~ "09",
    agrepl("October", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "oct") ~ "10",
    agrepl("November", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "nov") ~ "11",
    agrepl("December", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "dec") ~ "12",
    agrepl("March", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "mar") ~ "03",
    agrepl("April", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "apr") ~ "04",
    agrepl("June", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "jun") ~ "06",
    agrepl("July", str, ignore.case=TRUE) | str_detect(str_to_lower(str), "jul") ~ "07",
    agrepl("May", str_replace(str_to_lower(str), "day.of", ""), ignore.case=TRUE) ~ "05",
    # SECOND: trying fuzzy matching w higher max distance + default fuzzy matching for abbrev
    agrepl("January", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("jan", str, ignore.case=TRUE) ~ "01",
    agrepl("February", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("feb", str, ignore.case=TRUE) ~ "02",
    agrepl("August", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("aug", str, ignore.case=TRUE) ~ "08",
    agrepl("September", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("sep", str, ignore.case=TRUE) ~ "09",
    agrepl("October", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("oct", str, ignore.case=TRUE) ~ "10",
    agrepl("November", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("nov", str, ignore.case=TRUE) ~ "11",
    agrepl("December", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("dec", str, ignore.case=TRUE) ~ "12",
    agrepl("March", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("mar", str, ignore.case=TRUE) ~ "03",
    agrepl("April", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("apr", str, ignore.case=TRUE) ~ "04",
    agrepl("June", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("jun", str, ignore.case=TRUE) ~ "06",
    agrepl("July", str, ignore.case=TRUE, max.distance = low_tol) | agrepl("jul", str, ignore.case=TRUE) ~ "07",
    agrepl("May", str_replace(str_to_lower(str), "day.of", ""), ignore.case=TRUE, max.distance = low_tol) ~ "05",
    TRUE ~ NA_character_)
  return(month_out)
}

# looks for best partial match for str in vector patterns
bestpartialmatch <- function(str, patterns, max.distance = 0.1, returnval = FALSE){
  if (is.na(str)){
    return(NA)
  }
  str = str_to_lower(str)
  bestmatch = NA
  bestmatchind = NA
  lowestdist = 1
  for (i in 1:length(patterns)){
    pattern = patterns[i]
    maxdist = ceiling(str_length(pattern)*max.distance)
    d = adist(pattern, str, partial = TRUE)
    if((d/str_length(pattern) < lowestdist) & (d <= maxdist)){
      lowestdist = d/str_length(pattern)
      bestmatch = pattern
      bestmatchind = i
    }
  }
  if(returnval){
    return(bestmatch)
  }
  return(bestmatchind)
}

# # cleans arrival certs from parsed_wide
# clean_arrivalcerts <- function(df){
#   df_mut <- df %>% 
#     mutate(across(starts_with("ArrivalCert"),
#          list("Type_Clean" = ~sapply(str_extract(.,paste0(ci_re, "[0-9]+")), cert_type_extract, arrivalcert = TRUE),
#               "Loc_Clean" = ~str_match(., "([V|v|O][A-Za-z0-9]+)")[,2],
#               "Number_Clean" = ~as.numeric(str_match(., "([0-9]{3,6})")[,2]))))
#   
#   df_mut$ArrivalCert1 <- NA
#   df_mut$arrivalmatch2 <- NA
#   
#   for (arrname in names(df)[which(str_detect(names(df), "ArrivalCert"))]){
#     # for each arrivalcert column, if 
#     df_mut$arrivalmatch1 <- ifelse(!is.na(df_mut$arrivalmatch1), df_mut$arrivalmatch1,
#                                    )
#   }
#   
#   ArrivalCert1_Type_Clean = ifelse(!is.na(ArrivalCert1_Number_Clean), ArrivalCert1_Type_Clean, 0),
#   ArrivalCert2_Type_Clean = ifelse(!is.na(ArrivalCert2_Number_Clean), ArrivalCert2_Type_Clean, 0),
#   
# }

# coerces string of form "ci ##" into valid cert type (4, 5, 28, 30, 36)
cert_type_extract <- function(str, arrivalcert = FALSE){
  #print(str)
  if(is.na(str)){
    return(0)
  }
  str_clean <- str_replace(str, ci_re, " ")
  #print(str_clean)
  if(str_detect(str_clean, "36")){
    return(36)
  }
  if(str_detect(str_clean, "30")){
    return(30)
  }
  if(str_detect(str_clean, "28")){
    return(28)
  }
  if(trimws(str_clean) == "5"|trimws(str_clean) == "S"|
     str_detect(str_clean, "^(5|S)[^0-9]")|str_detect(str_clean, "[^0-9](5|S)$")|
     str_detect(str_clean, "[^0-9](5|S)[^0-9]")){
    return(5)
  }
  if(arrivalcert & (trimws(str_clean) == "4"|
                    str_detect(str_clean, "^4[^0-9]")|str_detect(str_clean, "[^0-9]4$")|
                    str_detect(str_clean, "[^0-9]4[^0-9]"))){
    return(4)
  }
  return(0)
}

# cleans entryship strings
clean_entryship <- function(str){
  if (is.na(str)){
    return(NA)
  }
  
  # generic clean
  str_clean <- str_replace_all(str, "[^A-z\\s]", " ")
  
  # # detecting generic boat
  # if (str_detect(str_to_lower(str_clean), "boat") | str_detect(str_to_lower(str_clean), "ship") |
  #     adist("steamer", str_clean, ignore.case=TRUE, partial = TRUE) <= 1 |
  #     (str_detect(str_to_lower(str_clean), "jap") & !str_detect(str_to_lower(str_clean), "japan") &
  #      adist("empress", str_clean, ignore.case=TRUE, partial=TRUE) > 3)){
  #   return("Boat")
  # }
  
  # removing leading 'ss'
  str_clean <- str_remove(str_clean, "^(S|s)\\.?\\s?(S|s)\\.?\\s?")
  
  # detecting doesn't know/unknown
  if (trimws(str_clean) == "" | 
      adist("unknown", str_clean, ignore.case=TRUE, partial = TRUE) <= 1 |
      adist("does not", str_clean, ignore.case=TRUE, partial=TRUE) <= 1 |
      adist("not know", str_clean, ignore.case=TRUE, partial=TRUE) <= 1 |
      adist("unable", str_clean, ignore.case=TRUE, partial=TRUE) <= 1 |
      adist("remember", str_clean, ignore.case=TRUE, partial=TRUE) <= 2 |
      adist("uncertain", str_clean, ignore.case=TRUE, partial=TRUE) <= 1 |
      adist("forget", str_clean, ignore.case=TRUE, partial=TRUE) <= 1 | 
      str_detect(str_to_lower(str_clean), "know")){
    return(NA)
  }
  
  # final clean
  str_out <- str_to_title(str_replace_all(trimws(str_clean), "\\s+", " "))
  return(str_out)
}

# looks across all cert type fields for cert type matching certnum and returns column of corresp numbers
harmonize_cert_type <- function(df, certnum){
  df[[paste0("CI",certnum,"_ci44")]] <- case_when(
      # CASE 1: Cert type is 5 and (other cert types not 5 OR at least one other cert number same)
      df$CertType_Clean == certnum & 
        ((df$EndorsedCertType_Clean != certnum & df$ArrivalCert1_Type_Clean != certnum & df$ArrivalCert2_Type_Clean != certnum) |
           (df$EndorsedCertNumber_Clean == df$CertNumber_Clean) | 
           (df$ArrivalCert1_Number_Clean == df$CertNumber_Clean) |
           (df$ArrivalCert2_Number_Clean == df$CertNumber_Clean)) ~ as.character(df$CertNumber_Clean),
      # CASE 2: Endorsed Cert Type is certnum and (other cert types not certnum OR at least one other cert number same)
      df$EndorsedCertType_Clean == certnum & 
        ((df$CertType_Clean != certnum & df$ArrivalCert1_Type_Clean != certnum & df$ArrivalCert2_Type_Clean != certnum) |
           (df$ArrivalCert1_Number_Clean == df$EndorsedCertNumber_Clean) | 
           (df$ArrivalCert2_Number_Clean == df$EndorsedCertNumber_Clean)) ~ as.character(df$EndorsedCertNumber_Clean),
      # CASE 3: Arrival Cert 1 Type is certnum and other cert types not certnum
      df$ArrivalCert1_Type_Clean == certnum & 
        ((df$CertType_Clean != certnum & df$EndorsedCertType_Clean != certnum & df$ArrivalCert2_Type_Clean != certnum) | 
           (df$ArrivalCert1_Number_Clean == df$ArrivalCert2_Number_Clean))~ as.character(df$ArrivalCert1_Number_Clean),
      # CASE 4: Arrival Cert 2 Type is certnum and other cert types not certnum
      df$ArrivalCert2_Type_Clean == certnum & 
        (df$CertType_Clean != certnum & df$EndorsedCertType_Clean != certnum & df$ArrivalCert1_Type_Clean != certnum) ~ as.character(df$ArrivalCert2_Number_Clean),
      # CASE 5: Conflicting Cert and Endorsed Cert numbers (both type certnum)
      df$CertType_Clean == certnum & df$EndorsedCertType_Clean == certnum ~ paste0(df$CertNumber_Clean, "|", df$EndorsedCertNumber_Clean),
      # CASE 6: Conflicting Cert and Arrival numbers
      df$CertType_Clean == certnum & df$ArrivalCert1_Type_Clean == certnum & !is.na(df$ArrivalCert1_Number_Clean) ~
        paste0(df$CertNumber_Clean, "|", df$ArrivalCert1_Number_Clean),
      df$CertType_Clean == certnum & df$ArrivalCert2_Type_Clean == certnum & !is.na(df$ArrivalCert2_Number_Clean) ~
        paste0(df$CertNumber_Clean, "|", df$ArrivalCert2_Number_Clean),
      # CASE 7: Conflicting Endorsed and Arrival numbers
      df$EndorsedCertType_Clean == certnum & df$ArrivalCert1_Type_Clean == certnum ~
        paste0(df$EndorsedCertNumber_Clean, "|", df$ArrivalCert1_Number_Clean),
      df$EndorsedCertType_Clean == certnum & df$ArrivalCert2_Type_Clean == certnum ~
        paste0(df$EndorsedCertNumber_Clean, "|", df$ArrivalCert2_Number_Clean),
      # CASE 8: Conflicting Arrival numbers
      df$ArrivalCert1_Type_Clean == certnum & df$ArrivalCert2_Type_Clean == certnum ~ 
        paste0(df$ArrivalCert1_Number_Clean, "|", df$ArrivalCert2_Number_Clean),
      TRUE ~ NA_character_
    )
  
  df_out <- df %>% 
    separate_wider_delim(paste0("CI",certnum,"_ci44"), "|", 
                         names = c(paste0("CI",certnum,"_ci44"), paste0("CI",certnum,"EXTRA_ci44")),
                         too_few = "align_start") %>%
    mutate(across(starts_with(paste0("CI",certnum)), ~as.numeric(.)))
  return(df_out)
}

# ## NEW VERSION OF HARMONIZE
# harmonize_cert_type <- function(df, certnum){
#   certstrings <- rep("", nrow(df))
# 
#   # look for cert column roots
#   cert_cols <- str_remove(names(df)[which(str_detect(names(df), "Type") & str_detect(names(df), "Clean"))],
#                           "Type_Clean")
# 
#   # iterate through columns
#   for (col in cert_cols){
#     # if type matches certnum AND nonmissing number AND number not already in certstrings, add to certstrings
#     certstrings <- ifelse(df[[paste0(col,"Type_Clean")]] == certnum &
#                             !is.na(df[[paste0(col,"Number_Clean")]]) &
#                             !str_detect(certstrings, paste0("\\|",df[[paste0(col, "Number_Clean")]])),
#                           paste0(certstrings, "|", df[[paste0(col, "Number_Clean")]]),
#                           certstrings)
#   }
# 
#   df[[paste0("CI",certnum)]] <- str_remove(certstrings, "^\\|")
#   df_out <- df %>%
#     separate_wider_delim(paste0("CI",certnum), "|",
#                          names_sep = "_",
#                          too_few = "align_start") %>%
#     mutate(across(starts_with(paste0("CI",certnum)), ~as.numeric(.))) %>%
#     rename_with(.fn = ~paste0(.,"_ci44"), starts_with(paste0("CI",certnum)))
# 
#   return(df_out)
# }


# after merging: gets max similarity between ci44 name and reg name (including akas)
max_name_sim <- function(df, method = "cosine", default = 0, 
                         namecols_ci44 = names_ci44, namecols_reg = names_reg,
                         outcol_suff = ""){
  df_temp <- df[,!str_detect(names(df), "namesim")]
  i = 1
  for (ci44colname in namecols_ci44){
    ci44col = df_temp[[ci44colname]]
    for (regcolname in namecols_reg){
      regcol = df_temp[[regcolname]]
      df_temp[[paste0("namesim",i)]] = ifelse(is.na(ci44col) | is.na(regcol), 
                                              default, stringsim(ci44col, regcol, method = method))
      i = i + 1
    }
  }
  df_temp <- df_temp %>% rowwise() %>%
    mutate(namesim = max(c_across(starts_with("namesim"))))
  df_out <- df
  df_out[[paste0("namesim", outcol_suff)]] <- df_temp$namesim
  
  return(df_out)
}

# after merging: checks similarity of ship names
## returns -1 if one or both strings is missing/genericly boat/ship etc.
## returns -2 if one of the strings names a line not a ship and the other string is a ship on that line
## otherwise, returns best match score out of a variety of cleaning/matching attempts
ship_name_sim <- function(str1, str2, goodmatchcutoff = 0.7){
  # nas
  if (is.na(str1) | is.na(str2) | nchar(str_remove_all(str1, "[^A-z]")) <= 3 | 
      nchar(str_remove_all(str2, "^[A-z]")) <= 3){
    return(-1)
  }
  
  # ATTEMPT ONE: adist (partial adist on each side)
  sim = min(adist(str1, str2, partial = TRUE), adist(str2, str1, partial = TRUE))/min(nchar(str1), nchar(str2))
  
  # ATTEMPT TWO: removing 'empress of'
  str1_2 = str_remove(str1, "Em[A-z]*(\\s?O.?)?\\s")
  str2_2 = str_remove(str2, "Em[A-z]*(\\s?O.?)?\\s")
  sim2 = min(adist(str1_2, str2_2, partial = TRUE),
             adist(str2_2, str1_2, partial = TRUE))/min(nchar(str1_2), nchar(str2_2))
  if (sim2 < sim | str_detect(str1, "Em[A-z]*(\\s?O.?)?\\s") | str_detect(str2, "Em[A-z]*(\\s?O.?)?\\s")){
    # if empress match is better or if empress-type string detected in either
    sim = sim2
  }
  
  # ATTEMPT THREE: removing 'SS'
  str1_3 = str_remove(str1, "^(S|s)\\.?\\s?(S|s)\\.?\\s?")
  str2_3 = str_remove(str2, "^(S|s)\\.?\\s?(S|s)\\.?\\s?")
  sim3 = min(adist(str1_3, str2_3, partial = TRUE),
             adist(str2_3, str1_3, partial = TRUE))/min(nchar(str1_3), nchar(str2_3))
  if (sim2 < sim){
    # if ss match is better
    sim = sim3
  }
  
  # ATTEMPT FOUR: splitting on 'ex'
  if (str_detect(str1, " Ex ")){
    splits <- str_split(str1, " Ex ")[[1]]
    sim4_1 = min(
      min(adist(splits[1], str2, partial = TRUE),
                 adist(str2, splits[1], partial = TRUE))/min(nchar(splits[1]), nchar(str2)),
      min(adist(splits[2], str2, partial = TRUE),
                 adist(str2, splits[2], partial = TRUE))/min(nchar(splits[2]), nchar(str2)))
    if (sim4_1 < sim){
      sim = sim4_1
    }
  }
  if (str_detect(str2, " Ex ")){
    splits <- str_split(str2, " Ex ")[[1]]
    sim4_2 = min(
      min(adist(splits[1], str1, partial = TRUE),
                 adist(str1, splits[1], partial = TRUE))/min(nchar(splits[1]), nchar(str1)),
      min(adist(splits[2], str1, partial = TRUE),
                 adist(str1, splits[2], partial = TRUE))/min(nchar(splits[2]), nchar(str1)))
    if (sim4_2 < sim){
      sim = sim4_2
    }
  }
  
  # detecting generic ships
  boats <- c("Boat", "Ship", "Vessel", "Line", "Steamer", "Liner")
  
  ## blue funnel line
  bluefunnel <- c("Antilochus", "Bellerophon",  "Bellerophone" ,"Cyclops"  ,   
                  "Keemun"     ,  "Ning Chow"    ,"Oanfa"      ,  "Peleus"      ,
                  "Protesilaus"  ,"Talthybius" ,  "Teucer"  ,     "Titan"       )
  if (((adist("Blue Funnel", str1, partial = TRUE) <= 2 & agrepls(str2, bluefunnel)) |
      (adist("Blue Funnel", str2, partial = TRUE) <= 2 & agrepls(str1, bluefunnel)))
      & 1-sim < goodmatchcutoff){
    return(-2)
  }
  
  ## cpr
  cpr <- c("Empress Of China" ,"Empress Of India", "Empress of Japan", "Monteagle", "Tartar")
  if (((adist("C P R", str1, partial = TRUE) <= 1 & agrepls(str2, cpr)) |
       (adist("C P R", str2, partial = TRUE) <= 1 & agrepls(str1, cpr)))
      & 1-sim < goodmatchcutoff){
    return(-2)
  }
  
  ## empress boat
  if (((adist("Empress Boat", str1, partial = TRUE) <= 2) | 
      adist("Empress Boat", str2, partial = TRUE) <= 2) & (1-sim < goodmatchcutoff)){
    return(-2)
  }
  
  ## northern pacific
  npacific <- c("Braemar"        ,  "City Of Kingston" ,"Columbia"      ,  
                "Duke Of Fire"  ,   "Glenogle"       ,  "Olympia"        , 
                "Shawmut"       ,   "Sikh"          ,   "Tacoma"          ,
                "Victoria"        , "Victorian" )
  if (((adist("Northern Pacific", str1, partial = TRUE) <= 3 & agrepls(str2, npacific)) |
       (adist("Northern Pacific", str2, partial = TRUE) <= 3 & agrepls(str1, npacific)))
      & 1-sim < goodmatchcutoff){
    return(-2)
  }
  
  ## japanese boat
  japboatnames <- c()
  i = 1
  for (boat in boats){
    for (prefix in c("Jap", "Japanese", "Japan")){
      japboatnames[i] = paste0(prefix, " ", boat)
      i = i + 1
    }
  }
  if (((agrepls(str1, japboatnames) & agrepl("Maru", str2)) |
       (agrepls(str2, japboatnames) & agrepl("Maru", str1)))
      & 1-sim < goodmatchcutoff){
    return(-2)
  }
  
  ## via
  via <- c("City Of Kingston", "City Of Puebla" ,  "G T Ry"     ,     
           "Queen"          ,  "Umatilla"    , "Walla Walla")
  if (((adist("Via", str1, partial = TRUE) <= 0 & agrepls(str2, via)) |
       (adist("Via", str2, partial = TRUE) <= 0 & agrepls(str1, via)))
      & 1-sim < goodmatchcutoff){
    return(-2)
  }
  
  ## generic ships
  prefixes <- c("", "Sail", "Sailing", "American", "U S A")
  for (boat in boats){
    for(prefix in prefixes){
      if ((adist(paste0(prefix, " ", boat), str1) <= 2 |
          adist(paste0(prefix, " ", boat), str2) <= 2) & (1-sim < goodmatchcutoff)){
        return(-1)
      }
    }
  }
  
  return(1-sim)
}

# after merging: cleans up and evaluates matches
merge_stats <- function(df, certtype=0, name_method="cosine"){
  # checking for extra certtype matches
  certmatch = numeric(nrow(df)) 
  if (certtype != 0){
    for (cert in c(5, 28, 30, 36)){
      if (cert != certtype){
        ci44nums <- df[[paste0("CI", cert, "_ci44")]]
        ci44extranums <- df[[paste0("CI", cert, "EXTRA_ci44")]]
        regnums <- df[[paste0("CI", cert, "_reg")]]
        
        certmatch = ifelse((!is.na(ci44nums) & !is.na(regnums) & ci44nums == regnums) |
                             (!is.na(ci44extranums) & !is.na(regnums) & ci44extranums == regnums),
                           1, certmatch)
      }
    }
  }
  
  df_out <- df %>%
    max_name_sim(method=name_method) %>%
    mutate(shipsim = mapply(ship_name_sim, EntryDateYear_ci44, EntryDateYear_reg),
           cert_match = certmatch,
           year_match = case_when(!is.na(EntryDateYear_ci44) & EntryDateYear_ci44 == EntryDateYear_reg ~ 1, #match
                                  !is.na(EntryDateYear_ci44) & !is.na(EntryDateYear_reg) & stringdist(EntryDateYear_ci44, EntryDateYear_reg) == 1 ~ 
                                    0.5, #small mismatch
                                  !is.na(EntryDateYear_ci44) & !is.na(EntryDateYear_reg) ~ 0, #big mismatch
                                  TRUE ~ 0.25 #missing
           ),
           month_match = case_when(!is.na(EntryDateMonth_ci44) & EntryDateMonth_ci44 == EntryDateMonth_reg ~ 1, #match
                                   !is.na(EntryDateMonth_ci44) & !is.na(EntryDateMonth_reg) ~ 0, #any mismatch (Months are strings so no room for error)
                                   TRUE ~ 0.25 #missing
           ),
           day_match = case_when(!is.na(EntryDateDay_ci44) & EntryDateDay_ci44 == EntryDateDay_reg ~ 1, #match
                                 !is.na(EntryDateDay_ci44) & !is.na(EntryDateDay_reg) & stringdist(EntryDateDay_ci44, EntryDateDay_reg) == 1 ~
                                   0.5, # small mismatch 
                                 !is.na(EntryDateDay_ci44) & !is.na(EntryDateDay_reg) ~ 0, #big mismatch
                                 TRUE ~ 0.25 #missing
           ),
           entry_ship_match = stringsim(EntryShip_ci44, EntryShip_reg, method = "cosine"),
           entry_match_score = year_match + month_match + day_match,
           birth_match_score = case_when(is.na(BirthYear_reg) | is.na(BirthYear_ci44) ~ 0,
                                         abs(BirthYear_reg - BirthYear_ci44) > 5 ~ 0,
                                         abs(BirthYear_reg - BirthYear_ci44) > 2 ~ 0.5,
                                         abs(BirthYear_reg - BirthYear_ci44) <= 2 ~ 1),
           total_score = ifelse(cert_match, 1, 0.5*namesim + (entry_match_score + birth_match_score)/8),
           true_match = case_when(
             #If exact match with another certificate, return TRUE
             cert_match == 1 ~ TRUE,
             #If name similarity over 95%, return TRUE
             namesim > 0.95 ~ TRUE,
             #If name similarity over 90% and date exactly right or date almost right and birth year right, return TRUE
             namesim > 0.85 & 
               (entry_match_score >= 2.5 | (entry_match_score >= 1.5 & birth_match_score == 1)) ~ TRUE,
             # If date exactly right and birth year right, return TRUE
             namesim > 0.8 & 
               entry_match_score + birth_match_score >= 3 ~ TRUE,
             TRUE ~ FALSE
           ),
           mergedon = certtype
    ) 
  return(df_out)
}

# merging parsed_by_cert and reg_chi_clean based on certtype
merge_dfs <- function(certtype, dfci44 = parsed_bycert, dfreg = reg_chi_clean){
  # df1[paste0("CI", certtype)] <- df1[[paste0("CI",certtype,"_ci44")]]
  # df2[paste0("CI", certtype)] <- df2[[paste0("CI",certtype,"_reg")]]
  # df1[paste0("CI", certtype, "EXTRA")] <- df1[[paste0("CI",certtype,"EXTRA_ci44")]]
  # df2[paste0("CI", certtype, "EXTRA")] <- df2[[paste0("CI",certtype,"_reg")]]
  # df_out <- bind_rows(
  #   list(
  #     inner_join( #first joining on main cert number from ci44 
  #       df1[which(!is.na(df1[[paste0("CI", certtype)]])),],
  #       df2[which(!is.na(df2[[paste0("CI", certtype)]])),],
  #       by = paste0("CI", certtype)) %>%
  #       merge_stats(certtype),
  #     inner_join( #then joining if ci44 also has a different number of the same cert type
  #       df1[which(!is.na(df1[[paste0("CI", certtype, "EXTRA")]])),],
  #       df2[which(!is.na(df2[[paste0("CI", certtype, "EXTRA")]])),],
  #       by = paste0("CI", certtype, "EXTRA")) %>%
  #       merge_stats(certtype)
  #   )
  # )
  # return(df_out)
  merge_names_ci44 <- names(dfci44)[which(str_detect(names(dfci44), paste0("CI",certtype)))]
  merge_names_reg <- names(dfreg)[which(str_detect(names(dfreg), paste0("CI",certtype)))]
  
  df_out <- merge_dfs_other(merge_names_ci44, merge_names_reg, dfci44 = dfci44, dfreg = dfreg, matchcolname = paste0("CI", certtype))
  return(df_out)
}

# merging ci44 df (maybe filtered) and reg_chi_clean on many columns:
#   - dfci44 is ci44 df
#   - dfreg is reg df
#   - merge_cols_ci44 are names of columns in dfci44 to try matching
#   - merge_cols_reg are names of columns in dfreg to try matching
#   - match_cols_ci44 are names of columns in dfci44 to try matching with
#   - match_cols_reg are names of columns in dfreg to try matching with
#   - matchcolname is name for new column created for matching (in output df, the thing that was matched on)
merge_dfs_other <- function(merge_cols_ci44, merge_cols_reg,
                            match_cols_ci44 = merge_cols_ci44, match_cols_reg = merge_cols_reg,
                            dfci44 = parsed_bycert, dfreg = reg_chi_clean,
                            matchcolname = "othermatchcol",
                            fuzzymatch = FALSE){
  if (fuzzymatch){
    mergefunc = function(x, y){merge_names(x, y, matchcolname = matchcolname)}
  }
  else{
    mergefunc = inner_join
  }
  
  allmatches = list()
  i = 1
  
  # matching mergecols from ci44
  for (col in merge_cols_ci44){
    # create new column in ci44 called `matchcolname` equal to column from ci44 to merge on
    dfci44[[matchcolname]] <- dfci44[[col]]
    # filter such that no NAs in merge column
    matchdfci44 <- dfci44[which(!is.na(dfci44[[matchcolname]])),] 
    for (matchcol in match_cols_reg){
      # create new column in reg called `matchcolname` equal to column from reg to try matching on
      dfreg[[matchcolname]] <- dfreg[[matchcol]]
      # merge (inner join) 
      allmatches[[i]] <- mergefunc(matchdfci44, dfreg[which(!is.na(dfreg[[matchcolname]])),])
      i = i + 1
    }
  }
  
  # do the same thing in reverse (matching mergecols from reg to matchcols from ci44)
  if (!identical(merge_cols_ci44, match_cols_ci44) |
      !identical(merge_cols_reg, match_cols_reg)){
    for (col in merge_cols_reg){
      dfreg[[matchcolname]] <- dfreg[[col]]
      matchdfreg <- dfreg[which(!is.na(dfreg[[matchcolname]])),]
      for (matchcol in match_cols_ci44){
        dfci44[[matchcolname]] <- dfci44[[matchcol]]
        allmatches[[i]] <- mergefunc(matchdfreg, dfci44[which(!is.na(dfci44[[matchcolname]])),])
        i = i + 1
      }
    }  
  }
  
  return(bind_rows(allmatches) %>% merge_stats())
}

# helper function to fuzzy merge on names
merge_names <- function(df1, df2, matchcolname, osadist = 2, cosdist = 0.1){
  if (sum(is.na(df1[[matchcolname]]), is.na(df2[[matchcolname]])) > 0){
    print(glue("Error in merge_names: should not be any NA values of {matchcolname}"))
  }
  # osa merge
  merge1 <- stringdist_inner_join(df1, df2, by = matchcolname,
                                  method = "osa", max_dist = osadist,    
                                  distance_col = "namedist_osa")
  
  # cosine merge
  merge2 <- stringdist_inner_join(df1, df2, by = matchcolname,
                                  method = "cosine", max_dist = cosdist,    
                                  distance_col = "namedist_cos")
  
  return(bind_rows(list(merge1, merge2)))
}