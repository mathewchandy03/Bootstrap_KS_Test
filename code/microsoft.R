source("functions.R")
source("alt_functions.R")
library(forecast)
library(tseries)
library(extraDistr)
library(xtable)

mc_pval_5y <- app_scheme("2018-01-01", "2022-12-31", 5, "msft")

mc_pval_4y <- app_scheme("2019-01-01", "2022-12-31", 4, "msft")

mc_pval_3y <- app_scheme("2020-01-01", "2022-12-31", 3, "msft")

mc_pval_2y <- app_scheme("2021-01-01", "2022-12-31", 2, "msft")

mc_pval_1y <- app_scheme("2022-01-01", "2022-12-31", 1, "msft")

microsoft_df <- t(rbind(mc_pval_1y, mc_pval_2y, mc_pval_3y, mc_pval_4y, 
                      mc_pval_5y
))

saveRDS(microsoft_df, "../data/microsoft_df")


write(print(xtable(mc_pval_1y,
  caption = "P-values for 1 year of Microsoft stock return data using different 
  methods
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft1"
  ), caption.placement = "top"), 
      file = "../manuscript/tables/mc_pval_1y.tex")

write(print(xtable(mc_pval_2y,
                   caption = "P-values for 2 years of Microsoft stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft2"
), caption.placement = "top"), 
file = "../manuscript/tables/mc_pval_2y.tex")

write(print(xtable(mc_pval_3y,
                   caption = "P-values for 3 years of Microsoft stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft3"
), caption.placement = "top"), 
file = "../manuscript/tables/mc_pval_3y.tex")

write(print(xtable(mc_pval_4y,
                   caption = "P-values for 4 years of Microsoft stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft4"
), caption.placement = "top"), 
file = "../manuscript/tables/mc_pval_4y.tex")

write(print(xtable(mc_pval_5y,
                   caption = "P-values for 5 years of Microsoft stock return 
                   data using different durations
  and different degrees of freedom for Student's t distribution.",
  label = "table:microsoft5"
), caption.placement = "top"), 
file = "../manuscript/tables/mc_pval_5y.tex")
