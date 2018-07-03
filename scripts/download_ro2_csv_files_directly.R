# Download risk factor files directly

# The current query: risk factor by SDI (not countries/other geographies)

# RO2 - risk factors by SDI

library(tidyverse)
# First download the files 
# 1.1 store the links


# then a number from 1 onwards 

read_csv_directly <- function(url_loc, csv_name){
  temp <- tempfile()
  download.file(url_loc, temp)
  output_csv <- read_csv(unz(temp, csv_name))
  unlink(temp)
  return(output_csv)
}


# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2016-permalink/bd2083c498a4b2a3bd628e8d4d886d99


num <- 1
out_file_base <- "raw_data/ro2/csvs/"
url_root <- "http://s3.healthdata.org/gbd-api-2016-production/49fc21f9fe89ce5686dfeccdc713d00d_files/IHME-GBD_2016_DATA-49fc21f9-"


files_already_downloaded <- list.files(path = "raw_data/ro2/csvs/")

nums_already_downloaded <- stringr::str_replace(files_already_downloaded, "\\.csv$", "") %>% as.numeric()
rm(files_already_downloaded)
while(1){
  cat("file number: ", num, "\n")  
  if (num %in% nums_already_downloaded) {
    num <- num + 1
    next
  }
  
  this_url <- paste0(url_root, num, ".zip")
  this_csv_name <- paste0("IHME-GBD_2016_DATA-49fc21f9-", num, ".csv")
  this_csv_file <- try(read_csv_directly(this_url, this_csv_name), silent = TRUE)
  
  
  
  print(head(this_csv_file))
  
  if(!is.data.frame(this_csv_file)) {break}
  
  try(
    write_csv(
      this_csv_file,
      paste0(out_file_base, num, ".csv")        
    ),
    silent = TRUE
  )
  num <- num + 1    
}
