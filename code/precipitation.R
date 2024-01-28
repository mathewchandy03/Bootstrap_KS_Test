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

acf(mdw$annual_max_hpcp)
pacf(mdw$annual_max_hpcp)
arima0(mdw$annual_max_hpcp, order = c(1, 0, 1))
# Coefficients:
#   ar1     ma1  intercept
# -0.7886  0.6723     1.3957
# s.e.   0.0966  0.5436     0.0635
#
# sigma^2 estimated as 0.3086:  log likelihood = -55.71,  aic = 119.41
set.seed(123)
mdw_pvals <- myapp(mdw$annual_max_hpcp, 10000, 'gev', evd::pgev, df = NULL)


lga <- precipitation %>% 
  filter(station == "COOP:305811", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

acf(lga$annual_max_hpcp)
pacf(lga$annual_max_hpcp)
arima0(lga$annual_max_hpcp, order = c(1, 0, 1))
# Coefficients:
#   ar1     ma1  intercept
# -0.4624  0.3793     1.1507
# s.e.   0.1123  0.6820     0.0473
#
# sigma^2 estimated as 0.1656:  log likelihood = -34.32,  aic = 76.64
set.seed(123)
lga_pvals <- myapp(lga$annual_max_hpcp, 10000, 'gev', evd::pgev)


lax <- precipitation %>% 
  filter(station == "COOP:045114", hpcp != 999.99) %>%
  group_by(year = as.numeric(format(date, "%Y"))) %>% 
  summarize(annual_max_hpcp = max(hpcp)) %>% 
  ungroup()

acf(lax$annual_max_hpcp)
pacf(lax$annual_max_hpcp)
arima0(lax$annual_max_hpcp, order = c(1, 0, 1))
# Coefficients:
#   ar1     ma1  intercept
# -0.8726  0.8123     0.5946
# s.e.   0.0824  0.8140     0.0311
#
# sigma^2 estimated as 0.06766:  log likelihood = -4.78,  aic = 17.56
set.seed(123)
lax_pvals <- myapp(lax$annual_max_hpcp, 10000, 'gev', evd::pgev)

write(print(xtable(data.frame(MDW = mdw_pvals, LGA = lga_pvals, LAX =
                                lax_pvals),
                   caption = "P-values for testing that annual maximums of hourly 
            precipitation from the three airports follows the GEV distribution.",
            label = "table:precipitation"), 
            caption.placement = "top"), 
        file = "../manuscript/tables/precipitation_pvals.tex")

