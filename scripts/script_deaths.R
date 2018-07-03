# Script - high level 

# permalink location here: 
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2016-permalink/5ad7dff5463eb6decae159de076ba89a

# location by income level
# year - all
# context - cause
# age - smallest age groups except < 1
# metric - rate
# sex - male & female
# measure - dalys, incidence, prevalence
# cause - A B C 

library(tidyverse)

dta <- read_csv("raw_data/IHME-GBD_2016_DATA-a00058b1-1/IHME-GBD_2016_DATA-a00058b1-1.csv")

dta %>% 
  select(location = location_name, measure = measure_name, sex = sex_name, age = age_name, year = year, cause = cause_name, 
         type = metric_name, val, lower, upper)  %>% 
  mutate(age = factor(age, ordered = T, 
                      levels = c(
                        "<1 year", "1 to 4", "5 to 9",
                        "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", 
                        "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69",
                        "70 to 74", "75 to 79", "80 to 84", "85 to 89"
                      )
    )
  ) %>% 
  mutate(
    location = factor(location, ordered = T,
                      levels = c("Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI", "Global")
                      
    )
  ) -> dta_prepped

dta_prepped %>% 
  filter(age == "<1 year") %>% 
  filter(cause == "All causes") %>% 
  filter(type  == "Rate") %>%  
  filter(measure == "Deaths") %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = val, linetype = sex)) +
  geom_line() + 
  facet_wrap(~location)

dta_prepped %>% 
  filter(cause == "All causes") %>% 
  filter(type  == "Rate") %>%  
  filter(measure == "Deaths") %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = val, linetype = sex, colour = age)) +
  geom_line() + 
  facet_wrap( ~location)


dta_prepped %>% 
  filter(cause == "All causes") %>% 
  filter(type  == "Rate") %>%  
  filter(measure == "Deaths") %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = age, fill = log(val, base = 10))) +
  geom_tile() + 
  facet_grid(sex ~location) + 
  scale_fill_distiller("Log mortality", palette = "Paired", type = "qual", direction = 1)

# Would like to do: 
# - interpolate between more colours (currently 6, so the three pairs)

dta_prepped %>% 
  filter(cause == "All causes") %>% 
  filter(type  == "Rate") %>%  
  filter(measure == "Incidence") %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = age, fill = val)) +
  geom_tile() + 
  facet_grid(sex ~location) + 
  scale_fill_distiller("Incidence", palette = "Paired", type = "qual", direction = 1)

dta_prepped %>% 
  filter(cause == "All causes") %>% 
  filter(type  == "Rate") %>%  
  filter(measure == "Prevalence") %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = age, fill = val)) +
  geom_tile() + 
  facet_grid(sex ~location) + 
  scale_fill_distiller("Prevalence", palette = "Paired", type = "qual", direction = 1)

# Now to plot differences 


