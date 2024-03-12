source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

sp500_pval_5y <- app_scheme("2019-01-01", "2023-12-31", 5, "^gspc")

sp500_pval_4y <- app_scheme("2020-01-01", "2023-12-31", 4, "^gspc")

sp500_pval_3y <- app_scheme("2021-01-01", "2023-12-31", 3, "^gspc")

sp500_pval_2y <- app_scheme("2022-01-01", "2023-12-31", 2, "^gspc")

sp500_pval_1y <- app_scheme("2023-01-01", "2023-12-31", 1, "^gspc")

sp500_df <- t(rbind(sp500_pval_1y, sp500_pval_2y, sp500_pval_3y, sp500_pval_4y, 
                        sp500_pval_5y
))

saveRDS(sp500_df, "../data/sp500_df")

write(print(xtable(sp500_pval_1y,
                   caption = "P-values for 1 year of SP500 stock return data using different 
  methods
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5001"
), caption.placement = "top"), 
file = "../manuscript/tables/sp500_pval_1y.tex")

write(print(xtable(sp500_pval_2y,
                   caption = "P-values for 2 years of SP500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5002"
), caption.placement = "top"), 
file = "../manuscript/tables/sp500_pval_2y.tex")

write(print(xtable(sp500_pval_3y,
                   caption = "P-values for 3 years of SP500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5003"
), caption.placement = "top"), 
file = "../manuscript/tables/sp500_pval_3y.tex")

write(print(xtable(sp500_pval_4y,
                   caption = "P-values for 4 years of SP500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5004"
), caption.placement = "top"), 
file = "../manuscript/tables/sp500_pval_4y.tex")

write(print(xtable(sp500_pval_5y,
                   caption = "P-values for 5 years of SP500 stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:SP5005"
), caption.placement = "top"), 
file = "../manuscript/tables/sp500_pval_5y.tex")

