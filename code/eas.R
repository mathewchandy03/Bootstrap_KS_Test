source('functions.R')
source('alt_functions.R')
library(tidyverse)
library(janitor)
library(evd)
library(xtable)
library(forecast)
load(file = "../data/EAS.txx.RData")

txx_df <- data.frame(matrix(NA, nrow = 2, ncol = 3))

for (col in 1:ncol(EAS.txx)) {
  series <- EAS.txx[, col]
  pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
  pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
  pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                                             df = NULL))
  txx_df <- rbind(txx_df, pvals)
}

load(file = "../data/EAS.txn.RData")

txn_df <- data.frame(matrix(NA, nrow = 2, ncol = 3))

for (col in 1:ncol(EAS.txn)) {
  series <- -EAS.txn[, col]
  pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
  pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
  pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                             df = NULL))
  txn_df <- rbind(txn_df, pvals)
}

load(file = "../data/EAS.tnx.RData")

tnx_df <- data.frame(matrix(NA, nrow = 2, ncol = 3))

for (col in 1:ncol(EAS.tnx)) {
  series <- EAS.tnx[, col]
  pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
  pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
  pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                             df = NULL))
  tnx_df <- rbind(tnx_df, pvals)
}

load(file = "../data/EAS.tnn.RData")

tnn_df <- data.frame(matrix(NA, nrow = 2, ncol = 3))

for (col in 1:ncol(EAS.tnn)) {
  series <- -EAS.tnn[, col]
  pvals <- myapp(series, 10000, 'gev', evd::pgev, df = NULL)
  pvals <- c(pvals, my_babu(series, 10000, 'gev', evd::pgev, df = NULL))
  pvals <- c(pvals, my_param(series, 10000, 'gev', evd::pgev, evd::rgev, 
                             df = NULL))
  tnn_df <- rbind(tnn_df, pvals)
}
