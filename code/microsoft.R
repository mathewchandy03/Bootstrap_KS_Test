source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

mc_pval_5y <- microsoft_app("2018-01-01", "2022-12-31", 5)

mc_pval_4y <- microsoft_app("2019-01-01", "2022-12-31", 4)

mc_pval_3y <- microsoft_app("2020-01-01", "2022-12-31", 3)

mc_pval_2y <- microsoft_app("2021-01-01", "2022-12-31", 2)

mc_pval_1y <- microsoft_app("2022-01-01", "2022-12-31", 1)

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
