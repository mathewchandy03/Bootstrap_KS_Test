source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)
microsoft_5y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2018-01-01",
                          end = "2022-12-31")))

mc5_fit <- auto.arima(as.vector(microsoft_5y[, "Close"]))
mc5_resid <- residuals(mc5_fit)

set.seed(123)
mc_pval_5y <- c(myapp(mc5_resid, 1000, "normal", 
                         pnorm),
                     my_babu(mc5_resid, 1000, "normal", 
                         pnorm))

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  mc_pval_5y <- rbind(mc_pval_5y,
                        c(myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                                df = v),
                          my_babu(mc5_resid, 1000, "normal", 
                                  pnorm)))
}

mc_pval_5y <- cbind(rep(5, dim(mc_pval_5y)[1]), 
                     c("norm", 30, 20, 10, 5, 4, 3, 2, 1), mc_pval_5y)
colnames(mc_pval_5y) <- c("duration", "df", "block", "basic")
rownames(mc_pval_5y) <- NULL


microsoft_4y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2019-01-01",
                          end = "2021-12-31")))

mc4_fit <- auto.arima(as.vector(microsoft_4y[, "Close"]))
mc4_resid <- residuals(mc4_fit)

set.seed(123)
mc_pval_4y <- c(myapp(mc4_resid, 1000, "normal", 
                      pnorm),
                my_babu(mc4_resid, 1000, "normal", 
                        pnorm))

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  mc_pval_4y <- rbind(mc_pval_4y,
                      c(myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                              df = v),
                        my_babu(mc4_resid, 1000, "normal", 
                                pnorm)))
}

mc_pval_4y <- cbind(rep(4, dim(mc_pval_4y)[1]), 
                    c("norm", 30, 20, 10, 5, 4, 3, 2, 1), mc_pval_4y)
colnames(mc_pval_4y) <- c("duration", "df", "block", "basic")
rownames(mc_pval_4y) <- NULL


microsoft_3y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2020-01-01",
                          end = "2022-12-31")))

mc3_fit <- auto.arima(as.vector(microsoft_3y[, "Close"]))
mc3_resid <- residuals(mc3_fit)

set.seed(123)
mc_pval_3y <- c(myapp(mc3_resid, 1000, "normal", 
                      pnorm),
                my_babu(mc3_resid, 1000, "normal", 
                        pnorm))

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  mc_pval_3y <- rbind(mc_pval_3y,
                      c(myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                              df = v),
                        my_babu(mc3_resid, 1000, "normal", 
                                pnorm)))
}

mc_pval_3y <- cbind(rep(3, dim(mc_pval_3y)[1]), 
                    c("norm", 30, 20, 10, 5, 4, 3, 2, 1), mc_pval_3y)
colnames(mc_pval_3y) <- c("duration", "df", "block", "basic")
rownames(mc_pval_3y) <- NULL


microsoft_2y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2021-01-01",
                          end = "2022-12-31")))

mc2_fit <- auto.arima(as.vector(microsoft_2y[, "Close"]))
mc2_resid <- residuals(mc5_fit)

set.seed(123)
mc_pval_2y <- c(myapp(mc2_resid, 1000, "normal", 
                      pnorm),
                my_babu(mc2_resid, 1000, "normal", 
                        pnorm))

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  mc_pval_2y <- rbind(mc_pval_2y,
                      c(myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                              df = v),
                        my_babu(mc2_resid, 1000, "normal", 
                                pnorm)))
}

mc_pval_2y <- cbind(rep(2, dim(mc_pval_2y)[1]), 
                    c("norm", 30, 20, 10, 5, 4, 3, 2, 1), mc_pval_2y)
colnames(mc_pval_2y) <- c("duration", "df", "block", "basic")
rownames(mc_pval_2y) <- NULL


microsoft_1y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2022-01-01",
                          end = "2022-12-31")))

mc1_fit <- auto.arima(as.vector(microsoft_1y[, "Close"]))
mc1_resid <- residuals(mc1_fit)

set.seed(123)
mc_pval_1y <- c(myapp(mc1_resid, 1000, "normal", 
                      pnorm),
                my_babu(mc1_resid, 1000, "normal", 
                        pnorm))

for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
  mc_pval_1y <- rbind(mc_pval_1y,
                      c(myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                              df = v),
                        my_babu(mc1_resid, 1000, "normal", 
                                pnorm)))
}

mc_pval_1y <- cbind(rep(1, dim(mc_pval_1y)[1]), 
                    c("norm", 30, 20, 10, 5, 4, 3, 2, 1), mc_pval_1y)
colnames(mc_pval_1y) <- c("duration", "df", "block", "basic")
rownames(mc_pval_1y) <- NULL


microsoft_df <- rbind(mc_pval_1y, mc_pval_2y, mc_pval_3y, mc_pval_4y, 
                      mc_pval_5y
)

saveRDS(microsoft_df, "../data/microsoft_df")
write(print(xtable(microsoft_df,
  caption = "P-values for Microsoft stock return data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft"
  ), caption.placement = "top"), 
      file = "../manuscript/tables/mc_pvals.tex")
