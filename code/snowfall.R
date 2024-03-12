source('functions.R')
source('alt_functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
load(file = "../data/SnowfallData.RData")

series <- annmax.1
pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                             df = NULL))

series <- annmax.2
pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                           df = NULL))

series <- annmax.3
pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                           df = NULL))

series <- annmax.4
pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                           df = NULL))
