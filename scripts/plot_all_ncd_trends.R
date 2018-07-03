rm(list = ls())

require(tidyverse)

country_death_relabs <- read_rds(path = "data/ro1/countries_ncd_all.rds")

country_death_relabs %>%
  filter(cause == "All causes") %>% 
  ggplot(aes(x = year, y = rel, group = country)) +
  geom_line(aes(colour = continent), alpha = 0.5) + 
  facet_wrap(~age, nrow = 1) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) + 
  geom_hline(yintercept = 1) + 
  labs(x = "Year", y = "Relative mortality rate", title = "Relative mortality rates - All causes", subtitle = "By age group and country",
       caption = "Source: GBD")

country_death_relabs %>%
  filter(cause == "Non-communicable diseases") %>% 
  ggplot(aes(x = year, y = rel, group = country)) +
  geom_line(aes(colour = continent), alpha = 0.5) + 
  facet_wrap(~age, nrow = 1) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) + 
  geom_hline(yintercept = 1) + 
  labs(x = "Year", y = "Relative mortality rate", title = "Relative mortality rates - NCDs", subtitle = "By age group and country",
       caption = "Source: GBD")



country_death_relabs %>%
  filter(cause == "All causes") %>% 
  ggplot(aes(x = year, y = abs, group = country)) +
  geom_line(aes(colour = continent), alpha = 0.5) + 
  facet_wrap(~age, nrow = 1) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Year", y = "Absolute mortality rate", title = "Absolute mortality rate differences - All causes", subtitle = "By age group and country",
       caption = "Source: GBD")

country_death_relabs %>%
  filter(cause == "Non-communicable diseases") %>% 
  ggplot(aes(x = year, y = abs, group = country)) +
  geom_line(aes(colour = continent), alpha = 0.5) + 
  facet_wrap(~age, nrow = 1) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Year", y = "Absolute mortality rate", title = "Absolute mortality rates - NCDs", subtitle = "By age group and country",
       caption = "Source: GBD")


