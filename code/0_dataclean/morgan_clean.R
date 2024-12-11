#_____________________________________________________________
# FILE DESCRIPTION: Reading in all data from plots in baten et al. 
# CREATED BY: Amy Kim
# CREATED ON: June 2024
#_____________________________________________________________

### READING DATA FROM PLOTS
#HEIGHT
height_plot_path <- glue("{dbox}/raw/china_height_plot_data")
height_csvs <- list.files(height_plot_path, pattern = ".*csv$")
height_files <- list()
i = 1
for (height_file in height_csvs){
  height_files[[i]] <- read_csv(glue("{height_plot_path}/{height_file}"), col_names = c("yr_raw", "height_raw")) %>% mutate(file_name = height_file)
  i = i + 1
}
height_raw <- bind_rows(height_files) %>% mutate(year = round(yr_raw))
write_csv(height_raw, glue("{dbox}/cleaned/china_height_plotdata.csv"))

#AGE HEAPING
age_plot_path <- glue("{dbox}/raw/china_age_plot_data")
age_csvs <- list.files(age_plot_path, pattern = ".*csv$")
age_files <- list()
i = 1
for (age_file in age_csvs){
  age_files[[i]] <- read_csv(glue("{age_plot_path}/{age_file}"), col_names = c("yr_raw", "age_raw")) %>% mutate(file_name = age_file)
  i = i + 1
}
age_raw <- bind_rows(age_files) %>% mutate(year = round(yr_raw))
write_csv(age_raw, glue("{dbox}/cleaned/china_age_plotdata.csv"))
# 
# #_____________________________________________________________
# # FILE DESCRIPTION: Initial Cleaning of Stephen Morgan Data
# # CREATED BY: Amy Kim
# # CREATED ON: June 2024
# #_____________________________________________________________
# 
# 
# morgan_raw_path <- glue("{dbox}/raw/morgan_aus_data")
# 
# list.files(morgan_raw_path)
# # seems like the main file (NAA merged + prisoners)
# cedt_prisoners_spss <- read.spss(glue("{morgan_raw_path}/Morgan_CEDT & Prisoners basic  data.sav")) %>% as.data.frame()
# 
# # all NAA files -- certificates of exemption from dictation test?
# naa_ar_merged <- read.spss(glue("{morgan_raw_path}/NAA_CEDT-AR merged.sav")) %>% as.data.frame()
# 
# # NAA separately by location?
# naa_alien <- read_xls(glue("{morgan_raw_path}/NAA_Alien SPSS master.xls"))
# naa_brisbane <- read_xls(glue("{morgan_raw_path}/NAA_Brisbane SPSS master.xls"))
# naa_darwin <- read_xls(glue("{morgan_raw_path}/NAA_Darwin SPSS master.xls"))

