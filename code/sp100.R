source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

sp100_pval_5y <- app_scheme("2019-01-01", "2023-12-31", 5, "^sp100")

sp100_pval_4y <- app_scheme("2020-01-01", "2023-12-31", 4, "^sp100")

sp100_pval_3y <- app_scheme("2021-01-01", "2023-12-31", 3, "^sp100")

sp100_pval_2y <- app_scheme("2022-01-01", "2023-12-31", 2, "^sp100")

sp100_pval_1y <- app_scheme("2023-01-01", "2023-12-31", 1, "^sp100")

sp100_df <- t(rbind(sp100_pval_1y, sp100_pval_2y, sp100_pval_3y, sp100_pval_4y, 
                    sp100_pval_5y
))

saveRDS(sp100_df, "../data/sp100_df")

write(print(xtable(sp100_pval_1y,
                   caption = "P-values for 1 year of SP100 stock return data using different 
  methods
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1001"
), caption.placement = "top"), 
file = "../manuscript/tables/sp100_pval_1y.tex")

write(print(xtable(sp100_pval_2y,
                   caption = "P-values for 2 years of SP100 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1002"
), caption.placement = "top"), 
file = "../manuscript/tables/sp100_pval_2y.tex")

write(print(xtable(sp100_pval_3y,
                   caption = "P-values for 3 years of SP100 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1003"
), caption.placement = "top"), 
file = "../manuscript/tables/sp100_pval_3y.tex")

write(print(xtable(sp100_pval_4y,
                   caption = "P-values for 4 years of SP100 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1004"
), caption.placement = "top"), 
file = "../manuscript/tables/sp100_pval_4y.tex")

write(print(xtable(sp100_pval_5y,
                   caption = "P-values for 5 years of SP100 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP1005"
), caption.placement = "top"), 
file = "../manuscript/tables/sp100_pval_5y.tex")

