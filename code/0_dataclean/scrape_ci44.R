## WEB SCRAPING: DOWNLOADING CHINESE IMMIGRATION RECORDS CI44
library(rvest)
library(glue)
library(RSelenium)
library(tidyverse)
library(httr)

# FILE PATHS AND URLS ----
if (Sys.getenv("USER") == "amykim"){
  dbox = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data"
  git = "/Users/amykim/Documents/GitHub/headtax"
}
ci44_raw_out <- glue("{dbox}/raw/ci44")
#ci44_raw_out <- glue("{dbox}/test_ci44/raw/ci44")

## getting urls to all reels
all_reels <- 'https://heritage.canadiana.ca/view/oocihm.lac_mikan_4350360'
reelcards <- read_html(all_reels) %>% html_elements(".stretched-link") %>% html_attr("href")

# HELPER FUNCTIONS ----
## helper function to download single image from ci44 reel to appropriate folder
download_img <- function(remDr, reel_str, outfolder = ci44_raw_out, k = 2, maxpg = 100000, verbose = FALSE){
  # current image number (extract from url)
  imgno <- str_extract(remDr$getCurrentUrl()[[1]], "[0-9]+$")
  reelno = str_extract(reel_str, "[0-9]{5}")
  
  # checking that image number does not exceed maxpg
  stopifnot(as.numeric(imgno) < maxpg)
  
  # getting url of full-size image for download
  img_url <- remDr$findElement(using = "css selector", value = "#pvFullImage")$getElementAttribute("href")[[1]]
  
  # try at least k times to download image to proper folder
  dlcode = 1
  attempt = 1
  while(dlcode != 0 & attempt <= k){
    attempt = attempt + 1
    try(
      dlcode <- download.file(img_url, destfile = glue("{outfolder}/{reel_str}/reel{reelno}_img{imgno}.jpg"), quiet = TRUE)
    )
  }
  
  if (verbose){
    print(glue("File {imgno} from Reel {reel_str} downloaded successfully!"))
  }
}

## helper function to download all images from a reel given link to first page
download_reel <- function(remDr, reel_url, testsample = 0, fromscratch = FALSE, outfolder = ci44_raw_out, maxpg = 100000, verbose = FALSE){
  # get string of reel number
  reel_str <- str_extract(reel_url, "t[0-9]{5}$")
  
  if (verbose){
    print(glue("Downloading Reel Number: {reel_str}..............."))
  }
  
  # create output dir if doesn't already exist 
  dir.create(glue("{ci44_raw_out}/{reel_str}"), showWarnings = FALSE)
  
  # extracting random sample from 1 to 2000
  if (testsample > 0){
    testpgs <- round(runif(testsample, 1, 2000))
    
    # iterate through sampled pages
    for (pg in testpgs){
      # navigate to page
      remDr$navigate(glue("{reel_url}/{pg}"))
      
      # download image
      download_img(remDr, reel_str, outfolder = outfolder, maxpg = maxpg, verbose = verbose)
    }
    return(TRUE)
  }
  
  # if not fromscratch, check for last image in folder and navigate to corresponding page
  if (!fromscratch){
    imgnums <- as.numeric(str_extract(str_extract(list.files(path = glue("{ci44_raw_out}/{reel_str}")), 
                                                  "img[0-9]{1,5}.jpg"),
                                      "[0-9]{1,5}"))
    if (length(imgnums) != 0){
      remDr$navigate(glue("{reel_url}/{max(imgnums)}"))
    }
  }
  
  # download first image
  download_img(remDr, reel_str)
  
  # finding next page button to check if it exists 
  nextbutton <- remDr$findElement(using = "css selector", value = "#pvNext")
  
  # keep iterating until we reach the last page of the reel
  while(!str_detect(nextbutton$getElementAttribute("class")[[1]], "disabled")){
    # navigate to next page
    nextbutton$clickElement()
    
    # download image
    download_img(remDr, reel_str, outfolder = outfolder, maxpg = maxpg, verbose = verbose)
    
    # find new next page button
    nextbutton <- remDr$findElement(using = "css selector", value = "#pvNext")
  }
}

# SETTING UP DRIVER ----
driver <- rsDriver(port = 4444L, browser = "firefox", chromever = NULL)
remote_driver <- driver[["client"]]
remote_driver$open()

# ITERATING THROUGH REELS ----
for (reel_url in reelcards){
  reelno <- as.numeric(str_extract(reel_url, "[0-9]+$"))
  if (reelno >= 16164){ # >= 64 for actual forms
    remote_driver$navigate(reel_url)
    download_reel(remote_driver, reel_url, verbose = TRUE)
  }
}

# CLOSING REMOTE DRIVER ----
driver$server$stop()


## TEMP STUFF (comment out after use)
# # Renaming Reel Imgs (Jan 13)
# for (dir in list.dirs(ci44_raw_out)){
#   reel_name = str_extract(dir, "[0-9]{5}")
#   print(reel_name)
#   img_list = list.files(dir, full.names = TRUE)
#   new_names = str_replace(img_list, "img", glue("reel{reel_name}_img"))
#   #print(head(new_names))
#   file.rename(img_list, new_names)
# }

# CREATING NEW TEST FOLDER ----
testdirname = "test6"
reelfolders = TRUE
ci44_raw_source <- glue("{dbox}/raw/ci44")
ci44_raw_out <- glue("{dbox}/test_ci44/raw/ci44")
dir.create(glue("{ci44_raw_out}/{testdirname}"), showWarnings = FALSE)

for (dir in list.dirs(ci44_raw_source)){
  if (str_detect(dir, "t[0-9]{5}$") & as.numeric(str_match(dir, "t([0-9]{5})")[,2]) >= 16164){
    reelno = str_match(dir, "(t[0-9]{5})")[,2]
    if (reelfolders){
      dir.create(glue("{ci44_raw_out}/{testdirname}/{reelno}"), showWarnings = FALSE)  
      testdirout = glue("{testdirname}/{reelno}")
    }
    else{
      testdirout = testdirname
    }
    files <- list.files(dir)
    i = round(runif(10, 1, length(files)))
    for (file in files[i]){
      print(file)
      file.copy(from=paste0(dir,"/",file), to=paste0(ci44_raw_out,"/", testdirout),
                overwrite = TRUE, recursive = FALSE,
                copy.mode = TRUE)
    }
  }
}
