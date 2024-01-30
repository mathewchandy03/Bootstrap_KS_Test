source("functions.R")
library(tseries)
library(extraDistr)
library(xtable)
microsoft <- 
  diff(log(get.hist.quote(instrument = "msft", start = "1998-01-01", 
                          end = "2022-12-31")))

microsoft_5y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2018-01-01",
                          end = "2022-12-31")))

acf(as.vector(microsoft_5y[, "Close"]))
pacf(as.vector(microsoft_5y[, "Close"]))
arima0(as.vector(microsoft_5y[, "Close"]), order = c(1, 0, 1))

set.seed(123)
mc_norm_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                     df = 30)

set.seed(123)
mc_t20_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                     df = 20)

set.seed(123)
mc_t10_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                     df = 10)

set.seed(123)
mc_t5_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                    df = 5)

set.seed(123)
mc_t2_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                    df = 2)

set.seed(123)
mc_t2.5_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                      df = 2.5)

set.seed(123)
mc_t2.2_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                      df = 2.2)

set.seed(123)
mc_t2.15_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                       df = 2.15)
# We fail to reject that the degrees of freedom is 2.15 at alpha = 0.1 using
# 5 years data

microsoft_4y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2019-01-01",
                          end = "2022-12-31")))

acf(as.vector(microsoft_4y[, "Close"]))
pacf(as.vector(microsoft_4y[, "Close"]))
arima0(as.vector(microsoft_4y[, "Close"]), order = c(1, 0, 1))

set.seed(123)
mc_norm_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                        df = 30)

set.seed(123)
mc_t20_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                        df = 20)

set.seed(123)
mc_t10_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                        df = 10)

set.seed(123)
mc_t5_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t2_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 2)

set.seed(123)
mc_t2.5_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                         df = 2.5)

set.seed(123)
mc_t2.2_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                         df = 2.2)

set.seed(123)
mc_t2.15_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                          df = 2.15)
# 

microsoft_3y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2020-01-01",
                          end = "2022-12-31")))

acf(as.vector(microsoft_3y[, "Close"]))
pacf(as.vector(microsoft_3y[, "Close"]))
arima0(as.vector(microsoft_3y[, "Close"]), order = c(1, 0, 1))

set.seed(123)
mc_norm_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                        df = 30)

set.seed(123)
mc_t20_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                        df = 20)

set.seed(123)
mc_t10_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                        df = 10)

set.seed(123)
mc_t5_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t2_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 2)

set.seed(123)
mc_t2.5_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                         df = 2.5)

set.seed(123)
mc_t2.2_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                         df = 2.2)

set.seed(123)
mc_t2.15_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                          df = 2.15)
# 

microsoft_2y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2021-01-01",
                          end = "2022-12-31")))

set.seed(123)
mc_norm_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                        df = 30)

set.seed(123)
mc_t20_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                        df = 20)

set.seed(123)
mc_t10_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                        df = 10)

set.seed(123)
mc_t5_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t2_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 2)

set.seed(123)
mc_t2.5_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                         df = 2.5)

set.seed(123)
mc_t2.2_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                         df = 2.2)

set.seed(123)
mc_t2.15_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                          df = 2.15)

#

microsoft_1y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2022-01-01",
                          end = "2022-12-31")))

acf(as.vector(microsoft_1y[, "Close"]))
pacf(as.vector(microsoft_1y[, "Close"]))
arima0(as.vector(microsoft_1y[, "Close"]), order = c(1, 0, 1))

set.seed(123)
mc_norm_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                        df = 30)

set.seed(123)
mc_t20_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                        df = 20)

set.seed(123)
mc_t10_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                        df = 10)

set.seed(123)
mc_t5_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t2_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                       df = 2)

set.seed(123)
mc_t2.5_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                         df = 2.5)

set.seed(123)
mc_t2.2_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                         df = 2.2)

set.seed(123)
mc_t2.15_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                          df = 2.15)

# We fail to reject at alpha = 0.1 using
# the last year of data


microsoft_df <- rbind(
  data.frame(Duration = "5 Years", Normal = mc_norm_pval_5y, 
             `t, v = 30` = mc_t30_pval_5y, 
             `t, v = 20` = mc_t20_pval_5y, 
             `t, v = 10` = mc_t10_pval_5y, 
             `t, v = 5` = mc_t5_pval_5y, 
             `t, v = 2` = mc_t2_pval_5y, 
             `t, v = 2.5` = mc_t2.5_pval_5y, 
             `t, v = 2.2` = mc_t2.2_pval_5y, 
             `t, v = 2.15` = mc_t2.15_pval_5y),
  data.frame(Duration = "4 Years", Normal = mc_norm_pval_4y, 
             `t, v = 30` = mc_t30_pval_4y, 
             `t, v = 20` = mc_t20_pval_4y, 
             `t, v = 10` = mc_t10_pval_4y, 
             `t, v = 5` = mc_t5_pval_4y, 
             `t, v = 2` = mc_t2_pval_4y, 
             `t, v = 2.5` = mc_t2.5_pval_4y, 
             `t, v = 2.2` = mc_t2.2_pval_4y, 
             `t, v = 2.15` = mc_t2.15_pval_4y),
  data.frame(Duration = "3 Years", Normal = mc_norm_pval_3y, 
             `t, v = 30` = mc_t30_pval_3y, 
             `t, v = 20` = mc_t20_pval_3y, 
             `t, v = 10` = mc_t10_pval_3y, 
             `t, v = 5` = mc_t5_pval_3y, 
             `t, v = 2` = mc_t2_pval_3y, 
             `t, v = 2.5` = mc_t2.5_pval_3y, 
             `t, v = 2.2` = mc_t2.2_pval_3y, 
             `t, v = 2.15` = mc_t2.15_pval_3y),
  data.frame(Duration = "2 Years", Normal = mc_norm_pval_2y, 
             `t, v = 30` = mc_t30_pval_2y, 
             `t, v = 20` = mc_t20_pval_2y, 
             `t, v = 10` = mc_t10_pval_2y, 
             `t, v = 5` = mc_t5_pval_2y, 
             `t, v = 2` = mc_t2_pval_2y, 
             `t, v = 2.5` = mc_t2.5_pval_2y, 
             `t, v = 2.2` = mc_t2.2_pval_2y, 
             `t, v = 2.15` = mc_t2.15_pval_2y),
  data.frame(Duration = "1 Year", Normal = mc_norm_pval_1y, 
             `t, v = 30` = mc_t30_pval_1y, 
             `t, v = 20` = mc_t20_pval_1y, 
             `t, v = 10` = mc_t10_pval_1y, 
             `t, v = 5` = mc_t5_pval_1y, 
             `t, v = 2` = mc_t2_pval_1y, 
             `t, v = 2.5` = mc_t2.5_pval_1y, 
             `t, v = 2.2` =mc_t2.2_pval_1y, 
             `t, v = 2.15` = mc_t2.15_pval_1y)
)

write(print(xtable(microsoft_df,
  caption = "P-values for Microsoft stock return data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft"
  ), caption.placement = "top"), 
      file = "../manuscript/tables/mc_pvals.tex")
