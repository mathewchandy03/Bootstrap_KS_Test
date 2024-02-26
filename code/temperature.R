source('functions.R')
source('alt_functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
ny_temp <- read.csv("../data/ny_temp.csv") %>% 
  clean_names()
colnames(ny_temp) <- ny_temp[4,]
ny_temp <- ny_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

ny_temp <- as.vector(ny_temp)[[1]]


ny_temp_fit <- auto.arima(ny_temp)

ny_temp_pvals <- myapp(ny_temp, 10000, 'gev', evd::pgev, df = NULL)
ny_temp_pvals <- c(ny_temp_pvals, my_babu(ny_temp, 10000, 'gev', evd::pgev, df = NULL))
ny_temp_pvals <- c(ny_temp_pvals, my_param(ny_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                   df = NULL))

la_temp <- read.csv("../data/la_temp.csv") %>% 
  clean_names()
colnames(la_temp) <- la_temp[4,]
la_temp <- la_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

la_temp <- as.vector(la_temp)[[1]]


la_temp_fit <- auto.arima(la_temp)

la_temp_pvals <- myapp(la_temp, 10000, 'gev', evd::pgev, df = NULL)
la_temp_pvals <- c(la_temp_pvals, my_babu(la_temp, 10000, 'gev', evd::pgev, df = NULL))
la_temp_pvals <- c(la_temp_pvals, my_param(la_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                           df = NULL))

chi_temp <- read.csv("../data/chi_temp.csv") %>% 
  clean_names()
colnames(chi_temp) <- chi_temp[4,]
chi_temp <- chi_temp[-(1:4),] %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(Value) 

chi_temp <- as.vector(chi_temp)[[1]]


chi_temp_fit <- auto.arima(chi_temp)

chi_temp_pvals <- myapp(chi_temp, 10000, 'gev', evd::pgev, df = NULL)
chi_temp_pvals <- c(chi_temp_pvals, my_babu(chi_temp, 10000, 'gev', evd::pgev, df = NULL))
chi_temp_pvals <- c(chi_temp_pvals, my_param(chi_temp, 10000, 'gev', evd::pgev, evd::rgev, 
                                           df = NULL))