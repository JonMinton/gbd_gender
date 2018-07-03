

library(tidyverse)
# First download the files 
# 1.1 store the links

# url root as follows:

url_root <- "http://s3.healthdata.org/gbd-api-2016-production/1378517769c519e775ba860b1cc102d0_files/IHME-GBD_2016_DATA-13785177-"

# then a number from 1 onwards 

read_csv_directly <- function(url_loc, csv_name){
  temp <- tempfile()
  download.file(url_loc, temp)
  output_csv <- read_csv(unz(temp, csv_name))
  unlink(temp)
  return(output_csv)
}


num <- 1
out_file_base <- "raw_data/ro1/csvs/"
url_root <- "http://s3.healthdata.org/gbd-api-2016-production/1378517769c519e775ba860b1cc102d0_files/IHME-GBD_2016_DATA-13785177-"

files_already_downloaded <- list.files(path = "raw_data/ro1/csvs/")

nums_already_downloaded <- stringr::str_replace(files_already_downloaded, "\\.csv$", "") %>% as.numeric()
rm(files_already_downloaded)
while(1){
  cat("file number: ", num, "\n")  
  if (num %in% nums_already_downloaded) {
    num <- num + 1
    next
  }
  
  this_url <- paste0(url_root, num, ".zip")
  this_csv_name <- paste0("IHME-GBD_2016_DATA-13785177-", num, ".csv")
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
