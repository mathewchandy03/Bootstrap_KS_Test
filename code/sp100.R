source("functions.R")
source("alt_functions.R")
source("ksfitted.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

#sp100_pval_5y <- app_scheme("2019-01-01", "2023-12-31", 5, "^sp100")

sp100_pval_4y <- app_scheme("2020-01-01", "2023-12-31", 4, "^sp100")

#sp100_pval_3y <- app_scheme("2021-01-01", "2023-12-31", 3, "^sp100")

#sp100_pval_2y <- app_scheme("2022-01-01", "2023-12-31", 2, "^sp100")

#sp100_pval_1y <- app_scheme("2023-01-01", "2023-12-31", 1, "^sp100")

#sp100_df <- t(rbind(sp100_pval_1y, sp100_pval_2y, sp100_pval_3y, sp100_pval_4y, 
#                    sp100_pval_5y
#))
options(scipen = 999)

saveRDS(sp100_pval_4y, "../data/sp100_pval_4y")

my_format <- function(x) {
  format(round(x, 4), nsmall = 4)
}

sp100_pval_4y[,2] <- sapply(as.numeric(sp100_pval_4y[,2]), my_format)

sp100_pval_4y[,3] <- sapply(as.numeric(sp100_pval_4y[,3]), my_format)

sp100_pval_4y[,4] <- sapply(as.numeric(sp100_pval_4y[,4]), my_format)

sp100_pval_4y[,5] <- sapply(as.numeric(sp100_pval_4y[,5]), my_format)

colnames(sp100_pval_4y) <- c("$v$", "Our Method", "Babu's Method",
                             "Zeimbekakis' Method", "Semiparametric Method")
 
sp100_pval_4y[1, 1] <- "$\\infty$"



series <- 
  diff(log(get.hist.quote(instrument = "^sp100", start = "2020-01-01",
                          end = "2023-12-31")))

series <- as.vector(series[, "Close"])
#  fit <- auto.arima(as.vector(series[, "Close"]))
#  resid <- residuals(fit)

resid <- series

set.seed(123)
semi <- ks.test.fitted(resid, "norm", B = 10000, serial = TRUE, 
                       param = c(mean = 0, sd = 1))$p.value

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  set.seed(123)
  semi <- c(semi, 
            ks.test.fitted.lst(resid, "lst", B = 10000, serial = TRUE, 
                       param = c(mu = 0, sigma = 1), df = v)$p.value)
}

sp100_pval_4y[,5] <- semi

non <- sp100_pval_4y[,3]

param <- sp100_pval_4y[,4]

sp100_pval_4y[,3] <- sp100_pval_4y[,5]

sp100_pval_4y[,4] <- non

sp100_pval_4y[,5] <- param

colnames(sp100_pval_4y) <- c("$v$", "Our Method", "Semiparametric Method",
                             "Nonparametric Method", "Parametric Method")

saveRDS(sp100_pval_4y, "../data/sp100_pval_4y")

write(print(xtable(sp100_pval_4y,
                   caption = "P-values for 4 years of S\\&P 100 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1004",
  digits = 3
), caption.placement = "top", sanitize.text.function=function(x){x},
include.rownames = FALSE), 
file = "../manuscript/tables/sp100_pval_4y.tex")




