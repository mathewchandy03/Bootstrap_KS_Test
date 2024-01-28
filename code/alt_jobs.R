source("hpc_functions.R")
# library(copula)
i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
B <- 1000
nrep <- 5
phis <- c()
# for (i in c(-.75, -.5, -.25, 0, .25, .5, .75)) {
#   phis <- c(phis, iTau(normalCopula(), i))
# }
phis <- 
  c(-0.9238795, -0.7071068, -0.3826834, 0, 0.3826834, 0.7071068, 0.9238795)
i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (true_dist in c("normal", "gamma")) {
      set.seed(as.integer(i))
      if (true_dist == "normal") {
        h0_dist <- "gamma"
        f0 <- pgamma
        f <- qnorm
        theta <- c(8, sqrt(8))
      }
      if (true_dist == "gamma") {
        h0_dist <- "normal"
        f0 <- pnorm
        f <- qgamma
        theta <- c(8,1)
      }
      df <- data.frame(matrix(NA,    
                              nrow = nrep,
                              ncol = 1))
      blksize <- ceiling(n^(1/3))
      p <- replicate(nrep, mysim(n, blksize, B, h0_dist, true_dist, f0, f, 
                                 theta, phi, 0))
      df$p <- p
      saveRDS(df, paste("../data/alt_", n, "_", true_dist, "_",
                        phi, "_", 
                        "0", "_",  i, ".RDS", sep = ""))
    }
  }
}


