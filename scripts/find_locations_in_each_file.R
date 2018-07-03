library(tidyverse)

files_to_open <- list.files("raw_data/ro1/csvs")

# read file
# get unique locations
# save as list

df_loc_lookup <- data_frame(files = files_to_open)

df_loc_lookup %>%
  mutate(
    metadata = map(
      files,
      function(x){

        cat("file: ", x, "\n")

        this_csv <- read_csv(file.path("raw_data/ro1/csvs", x))

        unique_measures <- unique(this_csv$measure_name)
        unique_locs <- unique(this_csv$location_name)
        unique_ages <- unique(this_csv$age_name)
        unique_causes <- unique(this_csv$cause_name)
        unique_metrics <- unique(this_csv$metric_name)
        list(measures = unique_measures, locations = unique_locs, ages = unique_ages, causes = unique_causes, metrics = unique_metrics)
      }
    )
  ) -> df_loc_lookup

saveRDS(df_loc_lookup, file = "data/ro1/metadata.rds")


