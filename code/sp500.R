source("functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

# This codewas used to get the p-values, but the table was reformatted manually
# in LaTeX

sp500_pval_4y <- app_scheme("2020-01-01", "2023-12-31", 4, "^gspc")

options(scipen = 999)

my_format <- function(x) {
  format(round(x, 4), nsmall = 4)
}

sp500_pval_4y[,2] <- sapply(as.numeric(sp500_pval_4y[,2]), my_format)

sp500_pval_4y[,3] <- sapply(as.numeric(sp500_pval_4y[,3]), my_format)

sp500_pval_4y[,4] <- sapply(as.numeric(sp500_pval_4y[,4]), my_format)

sp500_pval_4y[,5] <- sapply(as.numeric(sp500_pval_4y[,5]), my_format)

sp500_pval_4y[1, 1] <- "$\\infty$"


saveRDS(sp500_pval_4y, "../data/sp500_pval_4y.RDS")

write(print(xtable(sp500_pval_4y,
                   caption = "P-values for 4 years of S\\&P 500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5004",
  digits = 3
), caption.placement = "top", sanitize.text.function=function(x){x},
include.rownames = FALSE), 
file = "../manuscript/tables/sp500_pval_4y.tex")


