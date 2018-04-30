rm(list = ls())

library(tidyverse)


dta_file_locations <- dir("raw_data",
    full.names = T,
    recursive = T,
    pattern = "\\.csv$")


tmp <- lapply(dta_file_locations, FUN = read_csv)
tmp2 <- reduce(.x = tmp, .f = bind_rows)

tmp2 %>% 
  filter(sex != "Both") %>% 
  filter(metric == "Rate") %>% 
  filter(!(age %in% c(
    "Age-standardized", "All Ages", "Early Neonatal", "Late Neonatal", "Post Neonatal" ,"Under 5",
    "80 plus", "<20 years", "70+ years", "10 to 19", "10 to 54", "10 to 24", "15-49 years",
    "4-14 years", "50-69 years"
    ))
  ) %>% 
  filter(location == "Wales") %>% 
  ggplot(aes(x = year, y = age, fill = log(val))) + 
  geom_tile() + 
  facet_wrap(~sex)



