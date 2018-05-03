# Permuting combinations 

require(tidyverse)

df_loc_lookup <- readRDS(file = "data/ro1/metadata.rds")

# I want a list of ALL unique measures, locations, causes and metrics 
df_loc_lookup %>% 
  mutate(measures = map(metadata, "measures")) %>% 
  mutate(locations = map(metadata, "locations")) %>% 
  mutate(ages = map(metadata, "ages")) %>% 
  mutate(causes = map(metadata, "causes")) %>% 
  mutate(metrics = map(metadata, "metrics")) -> df_loc_lookup


# now to combine all measures and work out what's unique

all_unique_measures <- flatten_chr(df_loc_lookup$measures) %>% unique()
all_unique_locs <- flatten_chr(df_loc_lookup$locations) %>% unique()
all_unique_ages <- flatten_chr(df_loc_lookup$ages) %>% unique()
all_unique_causes <- flatten_chr(df_loc_lookup$causes) %>% unique()
all_unique_metrics <- flatten_chr(df_loc_lookup$metrics) %>% unique()

all_unique_measures  
all_unique_locs
all_unique_ages
all_unique_causes
all_unique_metrics

##

# For each combinatin of measures, locs, ages, causes and metrics 

# 1 ) find which files contain these combinations 
# 2 ) load and merge these files
# 3 ) calculate relative and absolute differences (male/female; male - female)
# 4 ) Save as two new columns in a new row whose first column is each combination of other attributes 

# When the total number of rows == 50 000
#   i ) save file
#   ii ) begin new data_frame object 


## To start with, use only broad wealth groups as locs
# 
# write_csv(data_frame(raw_locs = all_unique_locs), path = "data/ro1/all_locs.csv")
# 

# SDI locs ----------------------------------------------------------------

locs_grouped <- read_csv("data/ro1/all_locs_categorised.csv")

sdi_groups <- locs_grouped %>% 
  filter(wealth_group == 1) %>% 
  pull(raw_locs)

# now to search within df_loc_lookup$locations to see if they contain sdi_groups

df_loc_lookup %>% 
  mutate(has_sdi = map_lgl(locations, function(x) {any(x %in% sdi_groups)})) %>% 
  filter(has_sdi)

# This doesn't limit the number of files that need to be accessed.
# But it will limit the number of rows in each file to be accessed.

# This should be used to create a new version of the files that only contain the SDI groups 

extract_only_sdi <- function(input_file){
  root_file_loc <- "raw_data/ro1/csvs/"
  this_file <- read_csv(file.path(root_file_loc, input_file))
  subfile <- this_file %>% filter(location_name %in% sdi_groups)
  cat("File: ", input_file, "\t from ", nrow(this_file), "to ", nrow(subfile), "\n")
  return(subfile)
}

df_loc_lookup %>% 
  mutate(subset_of_file = map(files, extract_only_sdi)) -> df_loc_sdi_subset

# now to turn subset_of_file into data_frame 

sdi_df <- bind_rows(df_loc_sdi_subset$subset_of_file)

write_csv(x = sdi_df, path = "data/ro1/sdi_subset.csv")



