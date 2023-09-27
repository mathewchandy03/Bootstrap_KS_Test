source("functions.R")
i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    nrep = 100
    n = n
    blksize = 10
    B = 100
    dist = 'normal'
    f = pnorm
    phi <- -0.4
    theta <- 0
    sim_ar1_n.4 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
    hist(sim_ar1_n.4)
  }
}
saveRDS(norm_results, paste('../Data/norm_results', '_', i, '.rds', sep = ''))
saveRDS(exp_results, paste('../Data/exp_results', '_', i, '.rds', sep = ''))
