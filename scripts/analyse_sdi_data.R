rm(list = ls())
require(tidyverse)

sdi_dta <- read_csv("data/ro1/big/sdi_subset.csv")

sdi_dta


order_age_sdi <- function(x, age_var, location_var){
  
  # enquo finds the value associated with the var name and makes sure that 
  # when !! is used, the value is used as the name
  
  # quo_name used on LHS before special asignment operator := 
  # for naming columns 
  
  age_var <- enquo(age_var)
  location_var <- enquo(location_var)
  
  x %>% 
    mutate(!!quo_name(age_var) := factor(
    !!age_var, ordered = T, 
    levels = c("Under 5", "5-14 years", "15-49 years", "50-69 years", "70+ years")
    )
  ) %>% 
    mutate(!!quo_name(location_var) := factor(
      !!location_var, ordered = T,
      levels = c("Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI")
    )
  ) -> out
  out

}


# Function for adding consistent aesthetics to comparisons between SDI groups

sdi_scaling <- function(){
  list(
    scale_colour_manual("SDI", values = c("red", "red", "black", "blue", "blue")), 
    scale_linetype_manual("SDI", values = c("solid", "dashed", "solid", "dashed", "solid")), 
    scale_size_manual("SDI", values = c(1.5, 1, 1, 1, 1.5)) 
  )
}

# to start. let's 
# 1) make age_name ordered 
# 2) filter on cause_name == "all causes")
# 3) calc relative and absolute 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "All causes") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Deaths") -> sdi_deaths


sdi_deaths %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Death Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2.8, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel


sdi_deaths %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Death Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-1000, 2000, by = 200)) -> gg_allcause_abs
  

png("figures/ro1/all_cause_sdi_rel_abs.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2) 
dev.off()

# DALYs 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "All causes") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "DALYs (Disability-Adjusted Life Years)") -> sdi_dalys


sdi_dalys %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative DALYs", subtitle = "All causes", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2.8, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel


sdi_dalys %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute DALY differences", subtitle = "All causes", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-10000, 45000, by = 2000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/dalys_all_cause_sdi_rel_abs.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2) 
dev.off()


# YLLs 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "All causes") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "YLLs (Years of Life Lost)") -> sdi_ylls


sdi_ylls %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative YLLs", subtitle = "All causes", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2.8, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel


sdi_ylls %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute YLL differences", subtitle = "All causes", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-10000, 40000, by = 2000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/ylls_all_cause_sdi_rel_abs.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2) 
dev.off()


# DALYs - NCDs

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "DALYs (Disability-Adjusted Life Years)") -> sdi_dalys


sdi_dalys %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative DALYs", subtitle = "NCDs", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2.0, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel


sdi_dalys %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute DALY differences", subtitle = "NCDs", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-10000, 30000, by = 2000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/dalys_NCDs_sdi_rel_abs.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2) 
dev.off()


# YLLs 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "YLLs (Years of Life Lost)") -> sdi_ylls


sdi_ylls %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative YLLs", subtitle = "NCDs", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.9, 2.0, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel


sdi_ylls %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute YLL differences", subtitle = "NCDs", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-10000, 30000, by = 2000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/ylls_ncds_sdi_rel_abs.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2) 
dev.off()

# This seems to work

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "All causes") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Incidence") -> sdi_incidence



sdi_incidence %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Incidence Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.7, 1.1, by = 0.02)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel

# This shows the higher morbidity for females 
# May need to think about how to label rel < 1 (1/x ?)



# now to do the same with absolute

sdi_incidence %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Incidence Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::comma) -> gg_allcause_abs

# I'm not sure how this is possible if what's being compared is incidence rates per 
# 100,000 and the abs max > 100,000

# Thinking about it again: people can have onset of more than one condition! 


png("figures/ro1/all_cause_sdi_rel_abs_incidence.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2)
dev.off()

# What about prevalence? 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "All causes") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Prevalence") -> sdi_prevalence



sdi_prevalence %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Prevalence Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.7, 1.1, by = 0.02)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel

# This shows the higher morbidity for females 
# May need to think about how to label rel < 1 (1/x ?)



# now to do the same with absolute

sdi_prevalence %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Prevalence Rates", subtitle = "All causes", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::comma) -> gg_allcause_abs



png("figures/ro1/all_cause_sdi_rel_abs_prevalence.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2)
dev.off()


# NCD only ----------------------------------------------------------------


# Now let's find the proportion due to NCDs


sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Deaths") -> sdi_ncd_deaths


sdi_ncd_deaths %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Death Rates", subtitle = "NCDs", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel

# This shows the higher morbidity for females 
# May need to think about how to label rel < 1 (1/x ?)



# now to do the same with absolute

sdi_ncd_deaths %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Death Rates", subtitle = "NCDs", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-600, 2000, by = 200)) -> gg_allcause_abs


png("figures/ro1/ncd_sdi_rel_abs_deaths.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2)
dev.off()


# NCDs, incidence and prevalence

# Incidence

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Incidence") -> sdi_ncd_incidence


sdi_ncd_incidence %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Incidence", subtitle = "NCDs", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 2, by = 0.1)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel

# This shows the higher morbidity for females 
# May need to think about how to label rel < 1 (1/x ?)



# now to do the same with absolute

sdi_ncd_incidence %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Incidence", subtitle = "NCDs", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-65000, 25000, by = 5000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/ncd_sdi_rel_abs_incidence.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2)
dev.off()


# Prevalence

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  select(measure = measure_name, sdi, sex = sex_name, age, year, val) %>% 
  spread(sex, val) %>% 
  mutate(rel = Male / Female, abs = Male - Female) %>% 
  select(-Female, -Male) %>% 
  filter(measure == "Prevalence") -> sdi_ncd_prevalence


sdi_ncd_prevalence %>% 
  ggplot(aes(x = year, y = rel, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Relative Prevalence", subtitle = "NCDs", caption = "Source: GBD",
       y = "Relative rate", x = "Year") +
  sdi_scaling() + 
  scale_y_continuous(breaks = seq(0.8, 1, by = 0.01)) +
  geom_hline(yintercept = 1.0) -> gg_allcause_rel

# This shows the higher morbidity for females 
# May need to think about how to label rel < 1 (1/x ?)



# now to do the same with absolute

sdi_ncd_incidence %>% 
  ggplot(aes(x = year, y = abs, colour = sdi, linetype = sdi, size = sdi)) + 
  facet_wrap(~age, nrow = 1) + 
  geom_line() +
  labs(title = "Absolute Prevalence", subtitle = "NCDs", caption = "Source: GBD",
       y = "Absolute rate", x = "Year") +
  sdi_scaling() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-65000, 25000, by = 5000), labels = scales::comma) -> gg_allcause_abs


png("figures/ro1/ncd_sdi_rel_abs_prevalence.png", height = 20, width = 20, units = "cm", res = 300)
gridExtra::grid.arrange(gg_allcause_rel, gg_allcause_abs, nrow = 2)
dev.off()

# Want to know


sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name %in% c("All causes", "Non-communicable diseases")) %>% 
  select(
    measure = measure_name, sdi, sex = sex_name, age,
    year = year, cause = cause_name, rate = val
  ) %>% 
  filter(measure == "Deaths") %>% 
  mutate(cause = case_when(
    cause == "Non-communicable diseases" ~ "NCD",
    cause == "All causes" ~ "Total"
    )
  ) %>% 
  spread(cause, rate) %>% 
  mutate(prop_total = NCD / Total) -> tmp 

tmp %>% 
  ggplot(aes(x  = year, y = prop_total, group = sdi, linetype = sdi, size = sdi, colour = sdi)) + 
  geom_line() + 
  sdi_scaling() + 
  facet_grid(sex ~ age) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + 
  labs(title = "Proportion of Deaths that are NCDs", x = "Year", y = "Proportion", caption = "Source: GBD")

ggsave("figures/ro1/prop_deaths_ncds.png", height = 20, width = 20, dpi = 300, units = "cm")

# swap facets 

tmp %>% 
  ggplot(aes(x = year, y = prop_total, group = sex, linetype = sex)) + 
  geom_line() + 
  facet_grid(sdi ~ age) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + 
  labs(title = "Proportion of Deaths that are NCDs", x = "Year", y = "Proportion", caption = "Source: GBD")
ggsave("figures/ro1/prop_deaths_ncds_diff_facet.png", height = 20, width = 20, dpi = 300, units = "cm")

# Gender Ratio of proportions 

sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name %in% c("All causes", "Non-communicable diseases")) %>% 
  select(
    measure = measure_name, sdi, sex = sex_name, age,
    year = year, cause = cause_name, rate = val
  ) %>% 
  filter(measure == "Deaths") %>% 
  mutate(cause = case_when(
    cause == "Non-communicable diseases" ~ "NCD",
    cause == "All causes" ~ "Total"
  )
  ) %>% 
  spread(cause, rate) %>% 
  mutate(prop_total = NCD / Total) %>%
  select(sdi, age, year, sex, prop_total) %>% 
  spread(sex, prop_total) %>% 
  mutate(rel = Male / Female) %>% 
  ggplot(aes(x  = year, y = rel, group = sdi, linetype = sdi, size = sdi, colour = sdi)) + 
  geom_line() + 
  sdi_scaling() + 
  facet_wrap( ~ age, nrow = 1) + 
  scale_y_continuous(limits = c(0.7, 1.3), breaks = seq(0.6, 1.4, by = 0.02)) + 
  geom_hline(yintercept = 1) + 
  labs(title = "Gender Ratio of Proportion of Deaths that are NCDs", 
       subtitle = "Male proportion NCDs over Female proportion NCDs", x = "Year", y = "Ratio of proportions", caption = "Source: GBD")
ggsave("figures/ro1/gender_ratio_proportion_ncds.png", height = 12, width = 20, units = "cm", dpi = 300)

# Abs diff in proportions 
sdi_dta %>% 
  rename(age = age_name, sdi = location_name) %>% 
  order_age_sdi(age_var = age, location_var = sdi) %>% 
  filter(cause_name %in% c("All causes", "Non-communicable diseases")) %>% 
  select(
    measure = measure_name, sdi, sex = sex_name, age,
    year = year, cause = cause_name, rate = val
  ) %>% 
  filter(measure == "Deaths") %>% 
  mutate(cause = case_when(
    cause == "Non-communicable diseases" ~ "NCD",
    cause == "All causes" ~ "Total"
  )
  ) %>% 
  spread(cause, rate) %>% 
  mutate(prop_total = NCD / Total) %>%
  select(sdi, age, year, sex, prop_total) %>% 
  spread(sex, prop_total) %>% 
  mutate(abs = Male - Female) %>% 
  ggplot(aes(x  = year, y = abs, group = sdi, linetype = sdi, size = sdi, colour = sdi)) + 
  geom_line() + 
  sdi_scaling() + 
  facet_wrap( ~ age, nrow = 1) + 
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, by = 0.01)) + 
  geom_hline(yintercept = 0) + 
  labs(
    title = "Difference in Proportion of Deaths that are NCDs", 
    x = "Year", 
    y = "Difference in proportions", 
    caption = "Source: GBD")
