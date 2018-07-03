rm(list = ls())
# Analyse by country 
require(tidyverse)

# Now to use the lookup properly

locs_hier <- readxl::read_excel("raw_data/IHME_GBD_2016_CODEBOOK/IHME_GBD_2016_ALL_LOCATION_HIERARCHIES_Y2018M04D26.XLSX")

countries_idname <- locs_hier %>% 
  filter(level == 3) %>% 
  select(location_id, location_name)


# Want to know,

# which rows refer to countries 

# Relative and absolute overall mort diff by age group and country
# relative and absolute NCD mort diff by age group and country 

df_loc_lookup <- readRDS("data/ro1/metadata.rds")


extract_rel_abs_country <- function(input_file){
  root_file_loc <- "raw_data/ro1/csvs/"
  this_file <- read_csv(file.path(root_file_loc, input_file))
  subfile <- this_file %>% semi_join(countries_idname)

  cat("Trying file: ", input_file, "\n")
  
  subfile %>% 
    filter(measure_name == "Deaths") %>% 
    filter(metric_name == "Rate") -> subfile
  
  if(nrow(subfile) == 0) return(NULL) 
  
  subfile %>% 
    select(country = location_name, location_id, sex = sex_name, age = age_name, 
           cause = cause_name, year, val) %>% 
    filter(cause %in% c("All causes", "Non-communicable diseases")) %>% 
    spread(sex, val) %>% 
    mutate(rel = Male / Female, abs = Male - Female) %>% 
    select(country, location_id, age, year, cause, rel, abs) -> output
  cat("File: ", input_file, "\t from ", nrow(this_file), "to ", nrow(output), "\n")
  
  return(output)
}

df_loc_lookup %>% 
  mutate(subset_of_file = map(files, extract_rel_abs_country)) -> df_loc_countries_subset



countries_ncd_all <- bind_rows(df_loc_countries_subset$subset_of_file)

countries_ncd_all %>% 
  mutate(age = factor(age, ordered = T, levels = c("Under 5", "5-14 years", "15-49 years", "50-69 years", "70+ years"))) %>% 
  arrange(country, year, age, cause) -> countries_ncd_all


# join continent to country 

countries_ncd_all %>% 
  left_join(
    locs_hier %>% filter(level == 3) %>% select(location_id, parent_id)
  ) %>% 
  left_join(
    locs_hier %>% select(location_id, continent = location_name),
    by = c("parent_id" = "location_id")
  ) %>% 
  select(location_id, country, continent, age, year, cause, rel, abs) -> countries_ncd_all


write_csv(countries_ncd_all, "data/ro1/countries_ncd_all.csv")
write_rds(countries_ncd_all, "data/ro1/countries_ncd_all.rds")
