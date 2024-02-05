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

mc5_fit <- auto.arima(as.vector(microsoft_5y[, "Close"]))
mc5_resid <- residuals(fit)
plot(mc5_resid)

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
mc_t7_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                    df = 5)

set.seed(123)
mc_t4_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                       df = 4)

set.seed(123)
mc_t3_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                       df = 3)

set.seed(123)
mc_t2_pval_5y <- myapp(as.vector(microsoft_5y[, "Close"]), 1000, "t", plst, 
                    df = 2)


microsoft_4y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2019-01-01",
                          end = "2022-12-31")))

mc4_fit <- auto.arima(as.vector(microsoft_4y[, "Close"]))
mc4_resid <- residuals(fit)
plot(mc4_resid)

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
mc_t7_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t4_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 4)

set.seed(123)
mc_t3_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 3)

set.seed(123)
mc_t2_pval_4y <- myapp(as.vector(microsoft_4y[, "Close"]), 1000, "t", plst, 
                       df = 2)


microsoft_3y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2020-01-01",
                          end = "2022-12-31")))

mc3_fit <- auto.arima(as.vector(microsoft_3y[, "Close"]))
mc3_resid <- residuals(fit)
plot(mc3_resid)

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
mc_t7_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t4_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 4)

set.seed(123)
mc_t3_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 3)

set.seed(123)
mc_t2_pval_3y <- myapp(as.vector(microsoft_3y[, "Close"]), 1000, "t", plst, 
                       df = 2)


microsoft_2y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2021-01-01",
                          end = "2022-12-31")))

mc2_fit <- auto.arima(as.vector(microsoft_2y[, "Close"]))
mc2_resid <- residuals(fit)
plot(mc2_resid)

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
mc_t7_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t4_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 4)

set.seed(123)
mc_t3_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 3)

set.seed(123)
mc_t2_pval_2y <- myapp(as.vector(microsoft_2y[, "Close"]), 1000, "t", plst, 
                       df = 2)


microsoft_1.5y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2021-07-02",
                          end = "2022-12-31")))

mc1.5_fit <- auto.arima(as.vector(microsoft_1.5y[, "Close"]))
mc1.5_resid <- residuals(fit)
plot(mc1.5_resid)

set.seed(123)
mc_norm_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "normal", 
                         pnorm)

set.seed(123)
mc_t30_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                        df = 30)

set.seed(123)
mc_t20_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                        df = 20)

set.seed(123)
mc_t10_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                        df = 10)

set.seed(123)
mc_t7_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t4_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                       df = 4)

set.seed(123)
mc_t3_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                       df = 3)

set.seed(123)
mc_t2_pval_1.5y <- myapp(as.vector(microsoft_1.5y[, "Close"]), 1000, "t", plst, 
                       df = 2)


microsoft_1y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2022-01-01",
                          end = "2022-12-31")))

mc1_fit <- auto.arima(as.vector(microsoft_1y[, "Close"]))
mc1_resid <- residuals(fit)
plot(mc1_resid)

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
mc_t7_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                       df = 7)

set.seed(123)
mc_t5_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                       df = 5)

set.seed(123)
mc_t4_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                         df = 4)

set.seed(123)
mc_t3_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                         df = 3)

set.seed(123)
mc_t2_pval_1y <- myapp(as.vector(microsoft_1y[, "Close"]), 1000, "t", plst, 
                       df = 2)


microsoft_df <- rbind(
  data.frame(Duration = "5 Years", Normal = mc_norm_pval_5y, 
             `t, v = 30` = mc_t30_pval_5y, 
             `t, v = 20` = mc_t20_pval_5y, 
             `t, v = 10` = mc_t10_pval_5y, 
             `t, v = 7` = mc_t7_pval_5y, 
             `t, v = 5` = mc_t5_pval_5y, 
             `t, v = 4` = mc_t4_pval_5y, 
             `t, v = 3` = mc_t3_pval_5y,
             `t, v = 2` = mc_t2_pval_5y),
  data.frame(Duration = "4 Years", Normal = mc_norm_pval_4y, 
             `t, v = 30` = mc_t30_pval_4y, 
             `t, v = 20` = mc_t20_pval_4y, 
             `t, v = 10` = mc_t10_pval_4y, 
             `t, v = 7` = mc_t7_pval_4y, 
             `t, v = 5` = mc_t5_pval_4y, 
             `t, v = 4` = mc_t4_pval_4y, 
             `t, v = 3` = mc_t3_pval_4y, 
             `t, v = 2` = mc_t2_pval_4y),
  data.frame(Duration = "3 Years", Normal = mc_norm_pval_3y, 
             `t, v = 30` = mc_t30_pval_3y, 
             `t, v = 20` = mc_t20_pval_3y, 
             `t, v = 10` = mc_t10_pval_3y, 
             `t, v = 7` = mc_t7_pval_3y,
             `t, v = 5` = mc_t5_pval_3y, 
             `t, v = 4` = mc_t4_pval_3y, 
             `t, v = 3` = mc_t3_pval_3y, 
             `t, v = 2` = mc_t2_pval_3y),
  data.frame(Duration = "2 Years", Normal = mc_norm_pval_2y, 
             `t, v = 30` = mc_t30_pval_2y, 
             `t, v = 20` = mc_t20_pval_2y, 
             `t, v = 10` = mc_t10_pval_2y, 
             `t, v = 7` = mc_t7_pval_2y,
             `t, v = 5` = mc_t5_pval_2y, 
             `t, v = 4` = mc_t4_pval_2y,
             `t, v = 3` = mc_t3_pval_2y, 
             `t, v = 2` = mc_t2_pval_2y),
  data.frame(Duration = "1.5 Years", Normal = mc_norm_pval_1.5y, 
             `t, v = 30` = mc_t30_pval_1.5y, 
             `t, v = 20` = mc_t20_pval_1.5y, 
             `t, v = 10` = mc_t10_pval_1.5y, 
             `t, v = 7` = mc_t7_pval_1.5y,
             `t, v = 5` = mc_t5_pval_1.5y, 
             `t, v = 4` = mc_t4_pval_1.5y,
             `t, v = 3` = mc_t3_pval_1.5y, 
             `t, v = 2` = mc_t2_pval_1.5y),
  data.frame(Duration = "1 Year", Normal = mc_norm_pval_1y, 
             `t, v = 30` = mc_t30_pval_1y, 
             `t, v = 20` = mc_t20_pval_1y, 
             `t, v = 10` = mc_t10_pval_1y, 
             `t, v = 7` = mc_t7_pval_1y,
             `t, v = 5` = mc_t5_pval_1y, 
             `t, v = 4` = mc_t4_pval_1y,
             `t, v = 3` = mc_t3_pval_1y,
             `t, v = 2` = mc_t2_pval_1y)
)

write(print(xtable(microsoft_df,
  caption = "P-values for Microsoft stock return data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft"
  ), caption.placement = "top"), 
      file = "../manuscript/tables/mc_pvals.tex")
