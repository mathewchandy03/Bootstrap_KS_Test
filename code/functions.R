library(tidyverse)
#library(qqplotr)
mystat <- function(x, dist) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  c(c(x),  
    MASS::fitdistr(x, dist)$estimate)
}

mysim <- function(n, blksize, B, dist, f0, f, theta, phi, rho) {
  # phi cannot be set to 0, but rho can
  if (mean(phi == 0)) {
    y <- arima.sim(list(ma = rho), n = n) / sqrt(1 + sum(rho^2))
  } else {
    y <- arima.sim(list(ar = phi, ma = rho), n = n) * sqrt(1 - sum(phi^2) / 1 + sum(rho^2))
  }
  y <- do.call(f, c(list(pnorm(y)), as.list(theta)))
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  fit_theta <- MASS::fitdistr(y, dist)$estimate # observed fitted values
  fit <- do.call(f0, c(list(sort(y)), as.list(fit_theta))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  # Block Bootstrap
  bts <- boot::tsboot(y, mystat, l = blksize, sim = "fixed", R = B, dist = dist)
  
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

plot_p_vals <- function(filename) {
  pvals <- readRDS(paste("../data/", filename, ".RDS", sep = ''))
  nrep <- length(pvals)
  df <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = nrep,
                          ncol = 1))
  df$p <- pvals
  gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
    scale_x_continuous(breaks=c(0, 1)) +
    scale_y_continuous(breaks=c(0, 1)) + 
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .1) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)
  ggsave(filename = paste(filename, ".pdf", sep = ''), plot = gg.f, 
         path = "../manuscript/figures", height = 4, width = 4)
  gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
    scale_x_continuous(breaks=c(0, 0.1)) +
    scale_y_continuous(breaks=c(0, 0.1)) + 
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .1) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_cartesian(ylim = c(0, 0.1), xlim = c(0, 0.1))
  ggsave(filename = paste("zoom_", filename, ".pdf", sep = ''), plot = gg.f, 
         path = "../manuscript/figures", height = 4, width = 4)
  gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
    geom_histogram(aes(x = p), binwidth = .05) +
    labs(x = "Probability Points", y = "Cumulative Probability")
  ggsave(filename = paste("hist_", filename, ".pdf", sep = ''), plot = gg.f, 
         path = "../manuscript/figures", height = 4, width = 4)
  gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
    geom_histogram(aes(x = p), binwidth = .01) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_cartesian(xlim = c(0, 0.1))
  ggsave(filename = paste("zoom_hist_", filename, ".pdf", sep = ''), plot = gg.f, 
         path = "../manuscript/figures", height = 4, width = 4)
}

