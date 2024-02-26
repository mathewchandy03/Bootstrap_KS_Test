source('functions.R')
source('alt_functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
precipitation <- read.csv("../data/precipitation.csv") %>% 
  clean_names() %>% 
  mutate(date = strptime(date, format = "%Y%m%d %H:%M"))

mdw <- precipitation %>% 
  filter(station == "COOP:111577", hpcp != 999.99) %>%
  group_by(day = as.numeric(format(date, "%Y%m%d"))) %>% 
  summarize(dpcp = sum(hpcp)) %>% 
  ungroup() %>% 
  mutate(year = substr(day, start = 1, stop = 4)) %>% 
  group_by(year) %>% 
  summarize(annual_max_dpcp = max(dpcp))
  

mdw_fit <- auto.arima(mdw$annual_max_dpcp)

mdw_pvals <- myapp(mdw$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL)
mdw_pvals <- c(mdw_pvals, my_babu(mdw$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL))
mdw_pvals <- c(mdw_pvals, my_param(mdw$annual_max_dpcp, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))


lga <- precipitation %>% 
  filter(station == "COOP:305811", hpcp != 999.99) %>%
  group_by(day = as.numeric(format(date, "%Y%m%d"))) %>% 
  summarize(dpcp = sum(hpcp)) %>% 
  ungroup() %>% 
  mutate(year = substr(day, start = 1, stop = 4)) %>% 
  group_by(year) %>% 
  summarize(annual_max_dpcp = max(dpcp))

lga_fit <- auto.arima(lga$annual_max_dpcp)

lga_pvals <- myapp(lga$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL)
lga_pvals <- c(lga_pvals, my_babu(lga$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL))
lga_pvals <- c(lga_pvals, my_param(lga$annual_max_dpcp, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))

lax <- precipitation %>% 
  filter(station == "COOP:045114", hpcp != 999.99) %>%
  group_by(day = as.numeric(format(date, "%Y%m%d"))) %>% 
  summarize(dpcp = sum(hpcp)) %>% 
  ungroup() %>% 
  mutate(year = substr(day, start = 1, stop = 4)) %>% 
  group_by(year) %>% 
  summarize(annual_max_dpcp = max(dpcp))

lax_fit <- auto.arima(lax$annual_max_dpcp)

lax_pvals <- myapp(lax$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL)
lax_pvals <- c(lax_pvals, my_babu(lax$annual_max_dpcp, 10000, 'gev', evd::pgev, df = NULL))
lax_pvals <- c(lax_pvals, my_param(lax$annual_max_dpcp, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))

pvals <- rbind(mdw_pvals, lga_pvals, lax_pvals)

colnames(pvals) <- c("block", "basic", "param")

rownames(pvals) <- c("MDW", "LGA", "LAX")


write(print(xtable(data.frame(pvals),
                   caption = "P-values for testing that annual maximums of hourly 
            precipitation from the three airports follows the GEV distribution.",
            label = "table:precipitation"), 
            caption.placement = "top"), 
        file = "../manuscript/tables/precipitation_pvals.tex")

