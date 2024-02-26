source('functions.R')
source('alt_functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
ny_min_temp <- read.csv("../data/ny_min_temp.csv") %>% 
  clean_names()
colnames(ny_min_temp) <- ny_min_temp[4,]
ny_min_temp <- ny_min_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

ny_min_temp <- as.vector(ny_min_temp)[[1]]


ny_min_temp_fit <- auto.arima(ny_min_temp)

ny_min_temp_pvals <- myapp(ny_min_temp, 10000, 'gev', evd::pgev, df = NULL)
ny_min_temp_pvals <- c(ny_min_temp_pvals, my_babu(ny_min_temp, 10000, 'gev', evd::pgev, df = NULL))
ny_min_temp_pvals <- c(ny_min_temp_pvals, my_param(ny_min_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                           df = NULL))

la_min_temp <- read.csv("../data/la_min_temp.csv") %>% 
  clean_names()
colnames(la_min_temp) <- la_min_temp[4,]
la_min_temp <- la_min_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

la_min_temp <- as.vector(la_min_temp)[[1]]


la_min_temp_fit <- auto.arima(la_min_temp)

la_min_temp_pvals <- myapp(la_min_temp, 10000, 'gev', evd::pgev, df = NULL)
la_min_temp_pvals <- c(la_min_temp_pvals, my_babu(la_min_temp, 10000, 'gev', evd::pgev, df = NULL))
la_min_temp_pvals <- c(la_min_temp_pvals, my_param(la_min_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                           df = NULL))

chi_min_temp <- read.csv("../data/chi_min_temp.csv") %>% 
  clean_names()
colnames(chi_min_temp) <- chi_min_temp[4,]
chi_min_temp <- chi_min_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

chi_min_temp <- as.vector(chi_min_temp)[[1]]


chi_min_temp_fit <- auto.arima(chi_min_temp)

chi_min_temp_pvals <- myapp(chi_min_temp, 10000, 'gev', evd::pgev, df = NULL)
chi_min_temp_pvals <- c(chi_min_temp_pvals, my_babu(chi_min_temp, 10000, 'gev', evd::pgev, df = NULL))
chi_min_temp_pvals <- c(chi_min_temp_pvals, my_param(chi_min_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                             df = NULL))