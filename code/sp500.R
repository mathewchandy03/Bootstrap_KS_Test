source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

#sp500_pval_5y <- app_scheme("2019-01-01", "2023-12-31", 5, "^gspc")

sp500_pval_4y <- app_scheme("2020-01-01", "2023-12-31", 4, "^gspc")

#sp500_pval_3y <- app_scheme("2021-01-01", "2023-12-31", 3, "^gspc")

#sp500_pval_2y <- app_scheme("2022-01-01", "2023-12-31", 2, "^gspc")

#sp500_pval_1y <- app_scheme("2023-01-01", "2023-12-31", 1, "^gspc")

#sp500_df <- t(rbind(sp500_pval_1y, sp500_pval_2y, sp500_pval_3y, sp500_pval_4y, 
#                        sp500_pval_5y
#))

saveRDS(sp500_pval_4y, "../data/sp500_pval_4y")

my_format <- function(x) {
  format(round(x, 4), nsmall = 4)
}

sp500_pval_4y[,2] <- sapply(as.numeric(sp500_pval_4y[,2]), my_format)

sp500_pval_4y[,3] <- sapply(as.numeric(sp500_pval_4y[,3]), my_format)

sp500_pval_4y[,4] <- sapply(as.numeric(sp500_pval_4y[,4]), my_format)

sp500_pval_4y[,5] <- sapply(as.numeric(sp500_pval_4y[,5]), my_format)

colnames(sp500_pval_4y) <- c("$v$", "Our Method", "Babu's Method",
                             "Zeimbekakis' Method", "Semiparametric Method")

sp500_pval_4y[1, 1] <- "$\\infty$"

write(print(xtable(sp500_pval_4y,
                   caption = "P-values for 4 years of S\\&P 500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5004",
  digits = 3
), caption.placement = "top", sanitize.text.function=function(x){x},
include.rownames = FALSE), 
file = "../manuscript/tables/sp500_pval_4y.tex")


