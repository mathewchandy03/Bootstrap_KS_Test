library(tidyverse)
library(janitor)
setwd("../data")

file_names <- list.files(pattern = "^sim")
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      list <- file_names[which(grepl(paste("_", n, "_", dist, "_", phi, "_0_", sep = ''), 
                                     file_names))]
      tosave <- c()
      for (file in list) {
        tosave <- c(tosave, readRDS(file)$p)
      }
      saveRDS(tosave, file = paste("sim_", n, "_", dist, "_",
                                   phi, '_', 
                                   '0', ".RDS", sep = ''))
    }
  }
}

file_names <- list.files(pattern = "^alt")
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      list <- file_names[which(grepl(paste("_", n, "_", dist, "_", phi, "_0_", sep = ''), 
                                     file_names))]
      tosave <- c()
      for (file in list) {
        tosave <- c(tosave, readRDS(file)$p)
      }
      saveRDS(tosave, file = paste("alt_", n, "_", dist, "_",
                                   phi, '_', 
                                   '0', ".RDS", sep = ''))
    }
  }
}

df <- data.frame(matrix(ncol = 8, nrow = 0))
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      for(alpha in c(.01, .05, .10)) {
        pvals <- readRDS(
          paste("../data/", "alt_", n, "_", dist, "_", phi, "_0", ".RDS", 
                sep = ''))
        R <- length(pvals)
        if (R <= 0) next
        x <- sum(pvals < alpha)
        pt <- prop.test(x, R, correct=FALSE)
        rr_lb <- pt$conf.int[1]
        rr_ub <- pt$conf.int[2]
        rr <- mean(c(rr_lb, rr_ub))
        df <- rbind(df, c(phi, n, dist, alpha, rr_lb, rr, rr_ub, R))
      }
    }
  }
}
colnames(df) <- c("phi", "n", "dist", "alpha", "rr_lb", "rr", "rr_ub", "R")
saveRDS(df, file = "../data/rejection_rates.RDS")

df <- data.frame(matrix(ncol = 4, nrow = 0))
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      pvals <- readRDS(
        paste("../data/", "sim_", n, "_", dist, "_", phi, "_0", ".RDS", 
              sep = ''))
      R <- length(pvals)
      tobind <- cbind(rep(phi, R), rep(n, R), rep(dist, R), pvals)
      df <- rbind(df, tobind)
    }
  }
}
colnames(df) <- c("phi", "n", "dist", "pval")
saveRDS(df, file = "../data/pvals.RDS")
