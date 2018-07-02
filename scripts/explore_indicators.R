simple_models <- mort_rate_data_joined %>% 
  filter(cause == "All causes") %>% 
  select(country, year, sex, std_mort_rate, indicator_id, indicator, indicator_value) %>% 
  filter(!is.na(indicator_value)) %>% 
  group_by(indicator_id) %>% 
  nest() %>% 
  mutate(male_model = map(
    data,
    function(x) {
      x %>%
      filter(sex == "Male") %>%
      lm(std_mort_rate ~ year + indicator_value, data = .) 
    }
      )
    ) %>%
  mutate(female_model = map(
    data, 
    function(x) {
      x %>% 
        filter(sex == "Female") %>% 
        lm(std_mort_rate ~ year + indicator_value, data = .) 
        }
      )
    )



# What  I want 

# std mort rate by year and gender 
# indicator ide by year and gender 
# line for each country 
# poly line for each country - indicator val vs std mort rate 
mort_rate_data_joined %>%
  select(country, year, cause, sex,std_mort_rate) %>% 
  filter(sex != "Both") %>% 
  distinct() %>% 
  ggplot(., aes(x = year, y = std_mort_rate, group = country)) + 
  geom_line(alpha = 0.1) +
  facet_grid(cause~sex)

mort_rate_data_joined %>% 
  select(year, country, indicator_id, indicator, indicator_value) %>% 
  distinct() %>% 
  ggplot(aes(x = year, y = indicator_value)) + 
  geom_line(aes(group = country), alpha = 0.2) +
  stat_smooth(method = "loess") + 
  facet_wrap(~indicator, scale = "free_y")


# Definitely issues with the youth employment rate indicators. For some countries these are over 100%

mort_rate_data_joined %>% 
  group_by(indicator_id,indicator) %>% 
  tally() %>% print(n = 40)

# Indicator name is 
# Ratio of female to male youth unemployment rates, so this is plausible

mort_rate_data_joined %>% 
  filter(indicator_id == 'SL.UEM.1524.FM.NE.ZS') %>% 
  select(country, year, indicator_value) %>% 
  distinct() %>% 
  arrange(desc(indicator_value)) 




