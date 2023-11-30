source("functions.R")
library(tseries)
microsoft <- get.hist.quote(instrument = "msft", start = "1998-01-01")
#log difference
set.seed(123)
mc_norm_pval <- myapp(as.vector(microsoft[, "Close"]), 1000, "normal", pnorm)
saveRDS(mc_norm_pval, "../data/mc_norm_pval.RDS")