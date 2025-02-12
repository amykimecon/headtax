# File Description: Helper Functions for Cleaning Parsed DocAI code (CI44)
# Author: Amy Kim
# Date Created: Thu Jan 30 10:14:22 2025

# IMPORTING PACKAGES ----
library(tidyverse)
library(glue)
library(jpeg)
library(stringdist)
library(fuzzyjoin)
library(gtools)
library(matrixStats)

# SETTING WORKING DIRECTORIES ----
root <- "/Users/amykim/Princeton Dropbox/Amy Kim/head_tax/head_tax_data"
code <- "/Users/amykim/Documents/GitHub/headtax/code"

# helper to easily compare matched data
view_matched <- function(df, extracolnames = c()){
  View(df %>% select(c(ID_ci44, ID_reg,
                       total_score, total_score_raw, score_denom, 
                       nonname_score, nonname_score_raw, nonname_denom, scorediff, namesim, shipsim, 
                       mainmerge, certbonus, bonus_score,
                       #CI36_ci44, CI36_reg, CI36EXTRA_ci44,
                       #CertNumber, EndorsedCertNumber, ArrivalCert1, ArrivalCert2,
                       #CI5, CI6, Other_CI,
                       Name_ci44, Name_reg, EntryPort_ci44, EntryPort_reg, EntryShip_ci44, EntryShip_reg,
                       BirthYear_ci44, BirthYear_reg,
                       BplVillage_ci44, BplVillage_reg,
                       BplDistrict_ci44, BplDistrict_reg,
                       EntryDateYear_ci44, EntryDateYear_reg,
                       EntryDateMonth_ci44, EntryDateMonth_reg,
                       EntryDateDay_ci44, EntryDateDay_reg, all_of(extracolnames))))
}

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

# computing lev distance (partial on either side)
adist_partial <- function(str1, str2){
  if(is.na(str1) | is.na(str2) | str1 == "" | str2 == ""){
    return(NA)
  }
  return(min(adist(str1, str2, partial = TRUE),
      adist(str2, str1, partial = TRUE))/min(nchar(str1), nchar(str2)))
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
  order = order(nchar(patterns), decreasing = TRUE)
  patterns = patterns[order]
  for (i in 1:length(patterns)){
    pattern = str_to_lower(patterns[i])
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
  return(order[bestmatchind])
}

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

# looks across all cert type fields for cert type matching certnum and returns df with 
#   all possible matches
harmonize_cert_type <- function(df, certnum = 0, tolong = FALSE){
  certstrings <- rep("", nrow(df))

  # look for cert column roots
  cert_cols <- str_remove(names(df)[which(str_detect(names(df), "Type") & str_detect(names(df), "Clean"))],
                          "Type_Clean")
  
  # paste type and number and pivot longer
  if (tolong){
    for (col in cert_cols){
      df[[col]] <- ifelse(!is.na(df[[paste0(col,"Number_Clean")]]),
                                           paste0(df[[paste0(col,"Type_Clean")]], "|", df[[paste0(col,"Number_Clean")]]),
                                           NA)
    }
    df_out <- df %>% pivot_longer(all_of(cert_cols),
                                  names_to = "CertSource",
                                  values_to = "CertInfo") %>%
      filter(!is.na(CertInfo)) %>%
      separate_wider_delim("CertInfo", "|", names = c("CertType_ci44", "CertNumber_ci44")) %>%
      mutate(across(c("CertType_ci44", "CertNumber_ci44"), ~as.numeric(.)))
  }
  else{ #otherwise just harmonize normally (keep wide)
    # iterate through columns
    for (col in cert_cols){
      # if type matches certnum AND nonmissing number AND number not already in certstrings, add to certstrings
      certstrings <- ifelse(df[[paste0(col,"Type_Clean")]] == certnum &
                              !is.na(df[[paste0(col,"Number_Clean")]]) &
                              !str_detect(certstrings, paste0("\\|",df[[paste0(col, "Number_Clean")]])),
                            paste0(certstrings, "|", df[[paste0(col, "Number_Clean")]]),
                            certstrings)
    }
    # separating all matches wider on delimiter |
    df[[paste0("CI",certnum)]] <- str_remove(certstrings, "^\\|")
    df_out <- df %>%
      separate_wider_delim(paste0("CI",certnum), "|",
                           names_sep = "_",
                           too_few = "align_start") %>%
      mutate(across(starts_with(paste0("CI",certnum)), ~as.numeric(.))) %>%
      rename_with(.fn = ~paste0(.,"_ci44"), starts_with(paste0("CI",certnum)))
  }

  return(df_out)
}

# find duplicates in dataset (need to have exact matches for now)
find_duplicates <- function(df){
  df[["nonmissings"]] <- rowSums(!is.na(df %>% select(c(Age_ci44, BplDistrict_ci44, BplVillage_ci44,
                    EntryDateYear_ci44, EntryDateMonth_ci44, EntryDateDay_ci44,
                    RegYear_ci44, RegNumber_Clean, ArrivalCert1_Number_Clean,
                    ArrivalCert2_Number_Clean, CertNumber_Clean, EndorsedCertNumber_Clean,
                    Name_ci44, EntryShip_ci44, EntryPort_ci44))))
  
  maincrit <- c(names(df)[which(str_detect(names(df), "Number") & str_detect(names(df), "Clean"))])
  adtlcrit <- c("Name_ci44")
  dups = c()
  for (col in maincrit){
    df_filt <- df[which(!is.na(df[[col]])),]
    df_filt[["dupind"]] <- duplicated(df_filt[[col]])
    alldup <- df_filt %>% group_by(.data[[col]]) %>% mutate(dupindgrp = max(dupind)) %>%
      filter(dupindgrp == 1) #%>%
    #   group_by(.data[[col]], EntryDateDay_ci44,
    #            EntryDateMonth_ci44, EntryDateYear_ci44) %>%
    #   mutate(dupcheck = ifelse(n() > 1 & !is.na(EntryDateDay_ci44) & !is.na(EntryDateMonth_ci44) &
    #                              !is.na(EntryDateYear_ci44), 1, 0),
    #          ) %>% ungroup()
    # dups <- c(dups, filter(alldup %>% filter(dupcheck == 1) %>% group_by() %>%
    #                          arrange(desc(nonmissings)) %>%
    #                          mutate(rank = row_number()), rank==1 & dupcheck == 1))

    for (col2 in adtlcrit){
      if (col != col2){
        alldup[["tempcol"]] <- paste0(alldup[[col]], "|", alldup[[col2]])
        alldup <- alldup %>% group_by(tempcol) %>% 
          mutate(dupcheck = ifelse(n() > 1 & !is.na(.data[[col2]]), 1, 0)) %>% ungroup() 
        dups <- c(dups, filter(alldup %>% filter(dupcheck == 1) %>% group_by(tempcol) %>%
                                 arrange(desc(nonmissings)) %>%
                                 mutate(rank = row_number()), rank==1)$ID_ci44)
      }
    }
  }
  return(unique(dups))
}

# # merging parsed_by_cert and reg_chi_clean based on certtype
# merge_dfs_certtype <- function(certtype, dfci44 = parsed_bycert, dfreg = reg_chi_clean){
#   merge_names_ci44 <- names(dfci44)[which(str_detect(names(dfci44), paste0("CI",certtype)))]
#   merge_names_reg <- names(dfreg)[which(str_detect(names(dfreg), paste0("CI",certtype)))]
#   
#   df_out <- merge_dfs(merge_names_ci44, merge_names_reg, dfci44 = dfci44, dfreg = dfreg, 
#                       matchcolname = paste0("CI", certtype), certtype = certtype)
#   return(df_out)
# }

# merging ci44 df (maybe filtered) and reg_chi_clean on many columns:
#   - dfci44 is ci44 df
#   - dfreg is reg df
#   - merge_cols_ci44 are names of columns in dfci44 to try matching
#   - merge_cols_reg are names of columns in dfreg to try matching
#   - match_cols_ci44 are names of columns in dfci44 to try matching with
#   - match_cols_reg are names of columns in dfreg to try matching with
#   - matchcolname is name for new column created for matching (in output df, the thing that was matched on)
merge_dfs <- function(dfci44, dfreg, merge_cols_ci44, merge_cols_reg,
                      match_cols_ci44 = merge_cols_ci44, match_cols_reg = merge_cols_reg,
                      matchcolname = "certmatch", fuzzymatch = FALSE){
  if (fuzzymatch){
    mergefunc = function(x, y){merge_names(x, y, matchcolname = matchcolname)}
  }
  else{
    mergefunc = function(x, y){inner_join(x,y,relationship="many-to-many")}
  }
  
  nmatches = length(merge_cols_ci44)*length(match_cols_reg) + 
    ifelse(!identical(merge_cols_ci44, match_cols_ci44) |
             !identical(merge_cols_reg, match_cols_reg),
           length(merge_cols_reg)*length(match_cols_ci44), 0)
  allmatches = vector("list", length = nmatches)
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
  
  return(bind_rows(allmatches))
}

# helper function to fuzzy merge on names
merge_names <- function(df1, df2, matchcolname, lvdist = 2, cosdist = 0.1){
  if (sum(is.na(df1[[matchcolname]]), is.na(df2[[matchcolname]])) > 0){
    print(glue("Error in merge_names: should not be any NA values of {matchcolname}"))
  }
  # osa merge
  merge1 <- stringdist_inner_join(df1, df2, by = matchcolname,
                                  method = "lv", max_dist = lvdist,    
                                  distance_col = "namedist_lv")
  
  # cosine merge
  merge2 <- stringdist_inner_join(df1, df2, by = matchcolname,
                                  method = "cosine", max_dist = cosdist,    
                                  distance_col = "namedist_cos")
  
  return(bind_rows(list(merge1, merge2)))
}

# helper: similarity between two name strings (lv)
name_sim <- function(str1, str2){
  if (is.na(str1) | is.na(str2)){
    return(NA)
  }
  # ATTEMPT ONE: direct match
  sim = adist_partial(str1, str2)
  if(sim == 0){
    return(1)
  }
  
  # ATTEMPT TWO: permutations
  if (str_detect(str1, " ") & nchar(str1) < nchar(str2)){
    splits = unique(str_split(str1, " ")[[1]])
    if (length(splits) > 1){
      perms = permutations(n = length(splits), r = length(splits), v = splits)
      sim2 = min(apply(perms, 1, function(.x){adist_partial(paste(.x, collapse = " "), str2)}))
      if (sim2 < sim){
        sim = sim2
      }
    }
  }
  
  if (str_detect(str2, " ")){
    splits = unique(str_split(str2, " ")[[1]])
    if (length(splits) > 1){
      perms = permutations(n = length(splits), r = length(splits), v = splits)
      sim2 = min(apply(perms, 1, function(.x){adist_partial(paste(.x, collapse = " "), str1)}))
      if (sim2 < sim){
        sim = sim2
      }  
    }
  }
  
  if (sim==0){
    return(1)
  }
  
  return(1-sim)
}

# after merging: gets max similarity between ci44 name and reg name (including akas)
max_name_sim <- function(df, default = 0, 
                         namecols_ci44 = names_ci44, namecols_reg = names_reg,
                         outcol_suff = ""){
  df_temp <- df[,!str_detect(names(df), "namesim")]
  i = 1
  for (ci44colname in namecols_ci44){
    ci44col = df_temp[[ci44colname]]
    for (regcolname in namecols_reg){
      regcol = df_temp[[regcolname]]
      df_temp[[paste0("namesim",i)]] = ifelse(is.na(ci44col) | is.na(regcol), 
                                              default, mapply(name_sim, ci44col, regcol))
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
ship_name_sim <- function(str1, str2, goodmatchcutoff = 0.75){
  # nas
  if (is.na(str1) | is.na(str2) | nchar(str_remove_all(str1, "[^A-z]")) <= 3 | 
      nchar(str_remove_all(str2, "^[A-z]")) <= 3){
    return(-1)
  }
  
  # ATTEMPT ONE: adist (partial adist on each side)
  sim = min(adist_partial(str1, str2), 
            adist_partial(str_to_lower(str_remove_all(str1, "\\s+")), str_to_lower(str_remove_all(str2, "\\s+"))))
  if (sim == 0){ #if exact match
    return(1)
  }
  
  # ATTEMPT TWO: removing 'empress of'
  str1_2 = str_remove(str1, "Em[A-z]*(\\s?O.?)?\\s")
  str2_2 = str_remove(str2, "Em[A-z]*(\\s?O.?)?\\s")
  sim2 = adist_partial(str1_2, str2_2)
  if (sim2 < sim | str_detect(str1, "Em[A-z]*(\\s?O.?)?\\s") | str_detect(str2, "Em[A-z]*(\\s?O.?)?\\s")){
    # if empress match is better or if empress-type string detected in either
    sim = sim2
  }
  if (sim == 0){
    return(1)
  }
  
  # ATTEMPT THREE: removing 'SS'
  sim3 = adist_partial(str_remove(str1, "^(S|s)\\.?\\s?(S|s)\\.?\\s?"),
                       str_remove(str2, "^(S|s)\\.?\\s?(S|s)\\.?\\s?"))
  if (sim2 < sim){
    # if ss match is better
    sim = sim3
  }
  if (sim == 0){
    return(1)
  }
  
  # ATTEMPT FOUR: splitting on 'ex'
  if (str_detect(str1, " Ex ")){
    splits <- str_split(str1, " Ex ")[[1]]
    sim4_1 = min(adist_partial(splits[1], str2),
                 adist_partial(splits[2], str2))
    if (sim4_1 < sim){
      sim = sim4_1
    }
  }
  if (str_detect(str2, " Ex ")){
    splits <- str_split(str2, " Ex ")[[1]]
    sim4_2 = min(adist_partial(splits[1], str1),
                 adist_partial(splits[2], str1))
    if (sim4_2 < sim){
      sim = sim4_2
    }
  }  
  if (sim == 0){
    return(1)
  }
  
  # detecting railway
  if (str_detect(str_to_lower(str1), " ry| rly|c p r|g t r|rail|cpry|train") &
      str_detect(str_to_lower(str2), " ry| rly|c p r|g t r|rail|cpry|train")){
    return(1)
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
merge_stats <- function(df, strcutoff_high = 0.8, strcutoff_low = 0.7,
                        shipmatchcutoff_high = strcutoff_high, shipmatchcutoff_low = strcutoff_low,
                        bplvillcutoff_high = strcutoff_high, bplvillcutoff_low = strcutoff_low,
                        entryportcutoff_high = strcutoff_high, entryportcutoff_low = strcutoff_low,
                        inclnamesim = TRUE){
  if (inclnamesim){
    df <- df %>% max_name_sim()
  }
  else{
    df <- df %>% rowwise() %>%
      mutate(namesim = 1 - namedist_lv/max(min(nchar(namematchcol.x), nchar(namematchcol.y)), 2)) %>% 
      ungroup()
  }
  df_out <- df %>%
    mutate(name_score = ifelse(is.na(Name_ci44) | is.na(Name_reg), NA, namesim),
           shipsim = mapply(ship_name_sim, EntryShip_ci44, EntryShip_reg, goodmatchcutoff = shipmatchcutoff_high),
           ship_score = case_when(shipsim >= shipmatchcutoff_high ~ 1,
                                 shipsim == -2 | shipsim >= shipmatchcutoff_low ~ 0.5,
                                 shipsim < shipmatchcutoff_low & shipsim >= 0 ~ 0,
                                 shipsim == -1 ~ NA_real_),
           year_match = case_when(!is.na(EntryDateYear_ci44) & EntryDateYear_ci44 == EntryDateYear_reg ~ 1, #match
                                  !is.na(EntryDateYear_ci44) & !is.na(EntryDateYear_reg) & stringdist(EntryDateYear_ci44, EntryDateYear_reg) == 1 ~ 
                                    0.5, #small mismatch
                                  !is.na(EntryDateYear_ci44) & !is.na(EntryDateYear_reg) ~ 0, #big mismatch
                                  TRUE ~ NA_real_ #missing
           ),
           month_match = case_when(!is.na(EntryDateMonth_ci44) & EntryDateMonth_ci44 == EntryDateMonth_reg ~ 1, #match
                                   !is.na(EntryDateMonth_ci44) & !is.na(EntryDateMonth_reg) ~ 0, #any mismatch (Months are strings so no room for error)
                                   TRUE ~ NA_real_ #missing
           ),
           day_match = case_when(!is.na(EntryDateDay_ci44) & EntryDateDay_ci44 == EntryDateDay_reg ~ 1, #match
                                 !is.na(EntryDateDay_ci44) & !is.na(EntryDateDay_reg) ~ 0, #big mismatch
                                 TRUE ~ NA_real_ #missing
           ),
           birthyear_score = case_when(is.na(BirthYear_reg) | is.na(BirthYear_ci44) ~ NA_real_,
                                         abs(BirthYear_reg - BirthYear_ci44) > 5 ~ 0,
                                         abs(BirthYear_reg - BirthYear_ci44) > 1 ~ 0.5,
                                         abs(BirthYear_reg - BirthYear_ci44) <= 1 ~ 1),
           bpl_village_sim = 1 - mapply(adist_partial, BplVillage_ci44, BplVillage_reg),
           bpl_village_score = case_when(is.na(BplVillage_ci44) | is.na(BplVillage_reg) ~ NA_real_,
                                         bpl_village_sim >= bplvillcutoff_high ~ 1,
                                         bpl_village_sim >= bplvillcutoff_low ~ 0.5,
                                         TRUE ~ 0),
           bpl_district_score = case_when(is.na(BplDistrictNew_ci44) | is.na(BplDistrictNew_reg) ~ NA_real_,
                                          BplDistrictNew_ci44 == BplDistrictNew_reg ~ 1,
                                          TRUE~ 0),
           entryport_sim = 1 - mapply(adist_partial, EntryPort_ci44, EntryPort_reg),
           entryport_score = case_when(is.na(EntryPort_ci44) | is.na(EntryPort_reg) ~ NA_real_,
                                       entryport_sim >= entryportcutoff_high ~ 1,
                                       entryport_sim >= entryportcutoff_low ~ 0.5,
                                       TRUE ~ 0),
           nonname_denom = ifelse(is.na(bpl_village_score), 0, 1)*0.4 +
             ifelse(is.na(ship_score), 0, 1)*0.25 +
             ifelse(is.na(year_match), 0, 1)*0.1 +
             ifelse(is.na(month_match), 0, 1)*0.1 +
             ifelse(is.na(day_match), 0, 1)*0.1 +
             ifelse(is.na(bpl_district_score), 0, 1)*0.1 +
             ifelse(is.na(birthyear_score), 0, 1)*0.1 +
             ifelse(is.na(entryport_score), 0, 1)*0.05,
           score_denom = ifelse(is.na(name_score), 0, 1)*0.8 + nonname_denom,
           nonname_score_raw = ifelse(is.na(bpl_village_score), 0, bpl_village_score)*0.4 +
             ifelse(is.na(ship_score), 0, ship_score)*0.25 +
             ifelse(is.na(year_match), 0, year_match)*0.1 +
             ifelse(is.na(month_match), 0, month_match)*0.1 +
             ifelse(is.na(day_match), 0, day_match)*0.1 +
             ifelse(is.na(bpl_district_score), 0, bpl_district_score)*0.1 +
             ifelse(is.na(birthyear_score), 0, birthyear_score)*0.1 +
             ifelse(is.na(entryport_score), 0, entryport_score)*0.05,
           total_score_raw = ifelse(is.na(name_score), 0, name_score)*0.8 +
             nonname_score_raw,
           # bonus score: 0.2 if cert type matching, 0.1 if cert number repeat > 1, 
           #    1 for each additional cert match, 0.2 if entry port matching and not vic/van
           bonus_score = certbonus + ifelse(!is.na(entryport_score) & entryport_score == 1 & EntryPort_reg != "Vancouver" & EntryPort_reg != "Victoria", 0.2, 0),
           total_score = ifelse(score_denom > 0, total_score_raw*(2/score_denom) + bonus_score, 0),
           nonname_score = ifelse(nonname_denom > 0, nonname_score_raw*(1.2/nonname_denom) + bonus_score, 0),
    ) 
  return(df_out)
}

one_to_one_match <- function(df){
  # alg to get best 1 to 1 matching
  round <- df %>% filter(!is.na(total_score)) %>%#this step is temp 
    group_by(ID_ci44) %>%
    # first step: each ci44 entry 'proposes' to top score among matches
    arrange(desc(total_score), .by_group = TRUE) %>%
    mutate(rank_ci44 = row_number(),
           propose = ifelse(rank_ci44 == 1, 1, 0)) %>% 
    ungroup() %>%
    group_by(ID_reg) %>%
    # each reg entry 'accepts' the highest scoring 'proposal' and rejects others
    arrange(desc(total_score), .by_group = TRUE) %>%
    mutate(reject = ifelse(propose & row_number() != 1, 1, 0)) %>%
    ungroup() 
  
  while (sum(round$reject) > 0){
    notreject <- round %>% filter(!reject)
    print(nrow(notreject))
    round <- notreject %>%
      group_by(ID_ci44) %>%
      # now propose to highest scoring match not already rejected by
      arrange(desc(total_score), .by_group = TRUE) %>%
      mutate(rank_ci44 = row_number(),
             propose = ifelse(rank_ci44 == 1, 1, 0)) %>% 
      ungroup() %>%
      group_by(ID_reg) %>%
      # each reg entry 'accepts' the highest scoring 'proposal' and rejects others
      arrange(desc(total_score), .by_group = TRUE) %>%
      mutate(reject = ifelse(propose & row_number() != 1, 1, 0)) %>%
      ungroup() 
  }
  
  return(round %>% filter(propose==1))
}
