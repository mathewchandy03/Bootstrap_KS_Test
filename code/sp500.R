source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

sp500_pval_5y <- app_scheme("2018-01-01", "2022-12-31", 5, "^gspc")

sp500_pval_4y <- app_scheme("2019-01-01", "2022-12-31", 4, "^gspc")

sp500_pval_3y <- app_scheme("2020-01-01", "2022-12-31", 3, "^gspc")

sp500_pval_2y <- app_scheme("2021-01-01", "2022-12-31", 2, "^gspc")

sp500_pval_1y <- app_scheme("2022-01-01", "2022-12-31", 1, "^gspc")

sp500_df <- t(rbind(sp500_pval_1y, sp500_pval_2y, sp500_pval_3y, sp500_pval_4y, 
                        sp500_pval_5y
))

saveRDS(sp500_df, "../data/sp500_df")


