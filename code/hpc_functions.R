.libPaths("/gpfs/homefs1/mac18033/rlibs")
library(tidyverse)
library(truncdist)
mystat <- function(x, dist) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  c(c(x),  
    MASS::fitdistr(x, dist)$estimate)
}

mysim <- function(n, blksize, B, h0_dist, true_dist, f0, f, theta, phi, rho) {
  # phi cannot be set to 0, but rho can
  if (mean(phi == 0)) {
    y <- arima.sim(list(ma = rho), n = n) / sqrt(1 + sum(rho^2))
  } else {
    y <- arima.sim(list(ar = phi, ma = rho), n = n) * sqrt(1 - sum(phi^2) / 1 + sum(rho^2))
  }
  if (h0_dist == 'gamma' & true_dist == 'normal') {
    y <- qtrunc(pnorm(y), "norm", a = 0, mean = theta[1], sd = theta[2])
  } else {
    y <- do.call(f, c(list(pnorm(y)), as.list(theta)))
  }
  
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  fit_theta <- MASS::fitdistr(y, h0_dist)$estimate # observed fitted values
  fit <- do.call(f0, c(list(sort(y)), as.list(fit_theta))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  # Block Bootstrap
  bts <- boot::tsboot(y, mystat, l = blksize, sim = "fixed", R = B, 
                      dist = h0_dist)
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = length(fit_theta)) 
  
  sum_theta <- 0
  
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts$t[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts$t[i, -(1:n)]))
  }
  exp_emp <- colMeans(list_emp) # exp empirical distribution
  exp_theta <- colMeans(list_theta) # exp fitted values
  exp_fit <- do.call(f0, c(list(sort(y)), as.list(exp_theta))) # distribution with exp fitted values
  bias_term <- exp_emp - exp_fit # bias term
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f0, c(list(sort(y)), as.list(boot_theta))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit - bias_term)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}
