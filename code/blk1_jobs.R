source("hpc_functions.R")
# library(copula)
B <- 1000
nrep <- 4
phis <- c()
# for (i in c(-.75, -.5, -.25, 0, .25, .5, .75)) {
#   phis <- c(phis, iTau(normalCopula(), i))
# }
phis <- 
  c(-0.9238795, -0.7071068, -0.3826834, 0, 0.3826834, 0.7071068, 0.9238795)
#i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
i <- 6
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    set.seed(as.integer(i))
    for (true_dist in c("normal", "gamma")) {
      for (truth in c("alt")) {
        if (true_dist == "normal" & truth == "null") {
          h0_dist <- "normal"
          f0 <- pnorm
          f <- qnorm
          theta <- c(8, sqrt(8))
          rgen <- rnorm
        }
        if (true_dist == "gamma" & truth == "alt") {
          h0_dist <- "normal"
          f0 <- pnorm
          f <- qgamma
          theta <- c(8,1)
          rgen <- rnorm
        }
        if (true_dist == "gamma" & truth == "null") {
          h0_dist <- "gamma"
          f0 <- pgamma
          f <- qgamma
          theta <- c(8,1)
          rgen <- rgamma
        } 
        if (true_dist == "normal" & truth == "alt") {
          h0_dist <- "gamma"
          f0 <- pgamma
          f <- qnorm
          theta <- c(8, sqrt(8))
          rgen <- rgamma
        }
        blksize <- ceiling(n^(1/3))
        p <- replicate(nrep, mysim(n, blksize, B, h0_dist, true_dist, f0, f, 
                                   theta, phi, rgen))
        saveRDS(p, paste("../data/", truth, "_", n, "_", true_dist, "_",
                          phi, "_", 
                          "0", "_",  i, ".RDS", sep = ""))
      }
    }
  }
}

