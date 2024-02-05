source('functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
precipitation <- read.csv("../data/precipitation.csv") %>% 
  clean_names() %>% 
  mutate(date = strptime(date, format = "%Y%m%d %H:%M"))

mdw <- precipitation %>% 
  filter(station == "COOP:111577", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

auto.arima(mdw$annual_max_hpcp)
set.seed(123)
mdw_pvals <- myapp(mdw$annual_max_hpcp, 10000, 'gev', evd::pgev, df = NULL)


lga <- precipitation %>% 
  filter(station == "COOP:305811", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

auto.arima(lga$annual_max_hpcp)
set.seed(123)
lga_pvals <- myapp(lga$annual_max_hpcp, 10000, 'gev', evd::pgev)


lax <- precipitation %>% 
  filter(station == "COOP:045114", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

auto.arima(lga$annual_max_hpcp)
set.seed(123)
lax_pvals <- myapp(lax$annual_max_hpcp, 10000, 'gev', evd::pgev)

write(print(xtable(data.frame(MDW = mdw_pvals, LGA = lga_pvals, LAX =
                                lax_pvals),
                   caption = "P-values for testing that annual maximums of hourly 
            precipitation from the three airports follows the GEV distribution.",
            label = "table:precipitation"), 
            caption.placement = "top"), 
        file = "../manuscript/tables/precipitation_pvals.tex")

