source("functions.R")
library(tseries)
library(extraDistr)
library(xtable)
microsoft <- 
  diff(log(get.hist.quote(instrument = "msft", start = "1998-01-01", 
                          end = "2022-12-31")))
#log difference
set.seed(123)
mc_norm_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "normal", pnorm)

set.seed(123)
mc_t30_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                      df = 30)

set.seed(123)
mc_t20_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                     df = 20)

set.seed(123)
mc_t10_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                     df = 10)

set.seed(123)
mc_t5_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                     df = 5)

set.seed(123)
mc_t2_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                    df = 2)

set.seed(123)
mc_t2.5_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                    df = 2.5)

set.seed(123)
mc_t2.2_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                      df = 2.2)

set.seed(123)
mc_t2.15_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "t", plst, 
                      df = 2.15)

# We fail to reject that the degrees of freedom is 2.15 at alpha = 0.1 using
# the entire data (about 25 years)

microsoft_5y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2018-01-01",
                          end = "2022-12-31")))

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
# the entire data (about 25 years)

microsoft_1y <- 
  diff(log(get.hist.quote(instrument = "msft", start = "2022-01-01",
                          end = "2022-12-31")))

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


write(print(xtable(
  rbind(data.frame(Duration = "25 Years", Normal = mc_norm_pval, 
                   `t(30)` = mc_t30_pval, `t(20)` = mc_t20_pval, 
                   `t(10)` = mc_t10_pval, 
                   `t(5)` = mc_t5_pval, `t(2)` = mc_t2_pval, 
                   `t(2.5)` = mc_t2.5_pval, 
                   `t(2.2)` = mc_t2.2_pval, 
                   `t(2.15)` = mc_t2.15_pval),
        data.frame(Duration = "5 Years", Normal = mc_norm_pval_5y, 
                   `t(30)` = mc_t30_pval_5y, 
                   `t(20)` = mc_t20_pval_5y, 
                   `t(10)` = mc_t10_pval_5y, 
                   `t(5)` = mc_t5_pval_5y, 
                   `t(2)` = mc_t2_pval_5y, 
                   `t(2.5)` = mc_t2.5_pval_5y, 
                   `t(2.2)` = mc_t2.2_pval_5y, 
                   `t(2.15)` = mc_t2.15_pval_5y),
        data.frame(Duration = "1 Year", Normal = mc_norm_pval_1y, 
                   `t(30)` = mc_t30_pval_1y, 
                   `t(20)` = mc_t20_pval_1y, 
                   `t(10)` = mc_t10_pval_1y, 
                   `t(5)` = mc_t5_pval_1y, 
                   `t(2)` = mc_t2_pval_1y, 
                   `t(2.5)` = mc_t2.5_pval_1y, 
                   `t(2.2)` =mc_t2.2_pval_1y, 
                   `t(2.15)` = mc_t2.15_pval_1y)
        ),
  caption = "P-values for Microsoft stock return data using different durations
  and different degrees of freedom for Student's t distribution."
  )), 
      file = "../manuscript/tables/mc_pvals.tex")
