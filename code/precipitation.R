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
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

mdw_fit <- auto.arima(mdw$annual_max_hpcp)
mdw_resid <- residuals(mdw_fit)

mdw_pvals <- myapp(mdw_resid, 10000, 'gev', evd::pgev, df = NULL)
mdw_pvals <- c(mdw_pvals, my_babu(mdw_resid, 10000, 'gev', evd::pgev, df = NULL))
mdw_pvals <- c(mdw_pvals, my_param(mdw_resid, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))


lga <- precipitation %>% 
  filter(station == "COOP:305811", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

lga_fit <- auto.arima(lga$annual_max_hpcp)
lga_resid <- residuals(lga_fit)

lga_pvals <- myapp(lga_resid, 10000, 'gev', evd::pgev, df = NULL)
lga_pvals <- c(lga_pvals, my_babu(lga_resid, 10000, 'gev', evd::pgev, df = NULL))
lga_pvals <- c(lga_pvals, my_param(lga_resid, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))

lax <- precipitation %>% 
  filter(station == "COOP:045114", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

lax_fit <- auto.arima(lax$annual_max_hpcp)
lax_resid <- residuals(lax_fit)

lax_pvals <- myapp(lax_resid, 10000, 'gev', evd::pgev, df = NULL)
lax_pvals <- c(lax_pvals, my_babu(lax_resid, 10000, 'gev', evd::pgev, df = NULL))
lax_pvals <- c(lax_pvals, my_param(lax_resid, 10000, 'gev', evd::pgev, evd::rgev, 
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

