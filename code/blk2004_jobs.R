source("hpc_functions.R")
library(parallel)

mclapply(1:20, my_job, mc.cores = 20)
