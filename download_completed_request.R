# Download request function

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

download_completed_request <-function(url_before_filenm, url_filenm_root, outdir, flush = FALSE){
  # flush: Should the outdir files be cleared before downloading?
  
  
  
  num <- 1
  url_root <- paste(url_before_filenm, url_filenm_root, sep = "/")

  if(!dir.exists(outdir)){
    dir.create(outdir, recursive = T)
  }
  
  if(flush){
    files_already_downloaded <- list.files(path = outdir, full.names = TRUE)
    unlink(files_already_downloaded)
  }
  
  files_already_downloaded <- list.files(path = outdir)
  
  nums_already_downloaded <- stringr::str_replace(files_already_downloaded, "\\.csv$", "") %>% as.numeric()
  rm(files_already_downloaded)
  while(1){
    cat("file number: ", num, "\n")  
    if (num %in% nums_already_downloaded) {
      num <- num + 1
      next
    }
    
    this_url <- paste0(url_root, num, ".zip")
    this_csv_name <- paste0(url_filenm_root, num, ".csv")
    this_csv_file <- try(read_csv_directly(this_url, this_csv_name), silent = TRUE)
    
    
    
    print(head(this_csv_file))
    
    if(!is.data.frame(this_csv_file)) {break}
    
    try(
      write_csv(
        this_csv_file,
        paste0(outdir, "/", num, ".csv")        
      ),
      silent = TRUE
    )
    num <- num + 1    
  }
  
  
}

