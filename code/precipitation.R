source('functions.R')
library(tidyverse)
library(janitor)
library(evd)
precipitation <- read.csv("../data/precipitation.csv") %>% 
  clean_names() %>% 
  filter(station_name == "JFK INTERNATIONAL AIRPORT NY US", hpcp != 999.99) %>% 
  mutate(date = strptime(date, format = "%Y%m%d %H:%M"))

annual_max_hpcp <- precipitation %>% 
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

monthly_max_dpcp <- precipitation %>%
  group_by(year = as.numeric(format(date, "%Y")),
           month = as.numeric(format(date, "%m")),
           day = as.numeric(format(date, "%d"))) %>% 
  summarize(dpcp = sum(hpcp)) %>% 
  ungroup(day) %>% 
  summarize(monthly_max_dpcp = max(dpcp)) %>% 
  ungroup()

set.seed(123)
nyc_pvals <- myapp(annual_max_hpcp$annual_max_hpcp, 10000, 'gev', pgev)

saveRDS(nyc_pvals, "../data/nyc_pvals.RDS")

