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

## getting urls to all reels
all_reels <- 'https://heritage.canadiana.ca/view/oocihm.lac_mikan_4350360'
reelcards <- read_html(all_reels) %>% html_elements(".stretched-link") %>% html_attr("href")

# HELPER FUNCTIONS ----
## helper function to download single image from ci44 reel to appropriate folder
download_img <- function(remDr, reel_str, outfolder = ci44_raw_out, maxpg = 100000, verbose = FALSE){
  # current image number (extract from url)
  imgno <- str_extract(remDr$getCurrentUrl()[[1]], "[0-9]+$")
  
  # checking that image number does not exceed maxpg
  stopifnot(as.numeric(imgno) < maxpg)
  
  # getting url of full-size image for download
  img_url <- remDr$findElement(using = "css selector", value = "#pvFullImage")$getElementAttribute("href")[[1]]
  
  # downloading image to proper folder
  download.file(img_url, destfile = glue("{outfolder}/{reel_str}/img{imgno}.jpg"), quiet = TRUE)
  
  if (verbose){
    print(glue("File {imgno} from Reel {reel_str} downloaded successfully!"))
  }
}

## helper function to download all images from a reel given link to first page
download_reel <- function(remDr, reel_url, fromscratch = FALSE, outfolder = ci44_raw_out, maxpg = 100000, verbose = FALSE){
  # get string of reel number
  reel_str <- str_extract(reel_url, "t[0-9]{5}$")
  
  if (verbose){
    print(glue("Downloading Reel Number: {reel_str}..............."))
  }
  
  # create output dir if doesn't already exist 
  dir.create(glue("{ci44_raw_out}/{reel_str}"), showWarnings = FALSE)
  
  # if not fromscratch, check for last image in folder and navigate to corresponding page
  if (!fromscratch){
    imgnums <- as.numeric(str_extract(list.files(path = glue("{ci44_raw_out}/{reel_str}")), "[0-9]+"))
    remDr$navigate(glue("{reel_url}/{max(imgnums)}"))
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
  if (reelno >= 16164){
    remote_driver$navigate(reel_url)
    download_reel(remote_driver, reel_url)
  }
}

# CLOSING REMOTE DRIVER ----
# driver$server$stop()

