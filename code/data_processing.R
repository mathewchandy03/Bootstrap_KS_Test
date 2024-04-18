library(tidyverse)
library(janitor)
setwd("../data/simulation")

file_names <- list.files(pattern = "^sim")
phis <- 
  c(-0.9238795, -0.7071068, -0.3826834, 0, 0.3826834, 0.7071068, 0.9238795)
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (dist in c("normal", "gamma")) {
      list <- file_names[which(grepl(paste("_", n, "_", dist, "_", phi, "_0_", sep = ''), 
                                     file_names))]
      tosave <- c()
      for (file in list) {
        tosave <- c(tosave, readRDS(file)$p)
      }
      saveRDS(tosave, file = paste("../sim_", n, "_", dist, "_",
                                   phi, '_', 
                                   '0', ".RDS", sep = ''))
    }
  }
}

file_names <- list.files(pattern = "^alt")
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (dist in c("normal", "gamma")) {
      list <- file_names[which(grepl(paste("_", n, "_", dist, "_", phi, "_0_", sep = ''), 
                                     file_names))]
      tosave <- c()
      for (file in list) {
        tosave <- c(tosave, readRDS(file)$p)
      }
      saveRDS(tosave, file = paste("../alt_", n, "_", dist, "_",
                                   phi, '_', 
                                   '0', ".RDS", sep = ''))
    }
  }
}

df <- data.frame(matrix(ncol = 8, nrow = 0))
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (dist in c("normal", "gamma")) {
      for(alpha in c(.01, .05, .10)) {
        pvals <- readRDS(
          paste("../", "sim_", n, "_", dist, "_", phi, "_0", ".RDS", 
                sep = ''))
        R <- length(pvals)
        if (R <= 0) next
        x <- sum(pvals < alpha)
        prop_test <- prop.test(x, R, correct=TRUE)
        rr_lb <- prop_test$conf.int[1]
        rr_ub <- prop_test$conf.int[2]
        rr <- x / R
        df <- rbind(df, c(phi, n, dist, alpha, rr_lb, rr, rr_ub, R))
      }
    }
  }
}
colnames(df) <- c("phi", "n", "dist", "alpha", "rr_lb", "rr", "rr_ub", "R")
saveRDS(df, file = "../rejection_rates1.RDS")

df <- data.frame(matrix(ncol = 8, nrow = 0))
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (dist in c("normal", "gamma")) {
      for(alpha in c(.01, .05, .10)) {
        pvals <- readRDS(
          paste("../", "alt_", n, "_", dist, "_", phi, "_0", ".RDS", 
                sep = ''))
        R <- length(pvals)
        if (R <= 0) next
        x <- sum(pvals < alpha)
        prop_test <- prop.test(x, R, correct=FALSE)
        rr_lb <- prop_test$conf.int[1]
        rr_ub <- prop_test$conf.int[2]
        rr <- mean(c(rr_lb, rr_ub))
        df <- rbind(df, c(phi, n, dist, alpha, rr_lb, rr, rr_ub, R))
      }
    }
  }
}
colnames(df) <- c("phi", "n", "dist", "alpha", "rr_lb", "rr", "rr_ub", "R")
saveRDS(df, file = "../rejection_rates2.RDS")

df <- data.frame(matrix(ncol = 4, nrow = 0))
for (phi in phis) {
  for (n in c(100, 200, 400, 800)) {
    for (dist in c("normal", "gamma")) {
      pvals <- readRDS(
        paste("../", "sim_", n, "_", dist, "_", phi, "_0", ".RDS", 
              sep = ''))
      R <- length(pvals)
      tobind <- cbind(rep(phi, R), rep(n, R), rep(dist, R), pvals)
      df <- rbind(df, tobind)
    }
  }
}
colnames(df) <- c("phi", "n", "dist", "pval")
saveRDS(df, file = "../pvals.RDS")

setwd("../../code")
