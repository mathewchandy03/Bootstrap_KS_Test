mystat <- function(x, dist) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  c(c(x),  
    MASS::fitdistr(x, dist)$estimate)
}

mysim <- function(n, blksize, B, dist, f, phi = 0, theta = 0) {
  # phi cannot be set to 0, but theta can
  if (mean(phi == 0)) {
    y <- arima.sim(list(ma = theta), n = n) / sqrt(1 + sum(theta^2))
  } else {
    y <- arima.sim(list(ar = phi, ma = theta), n = n) * sqrt(1 - sum(phi^2) / 1 + sum(theta^2))
  }
  
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  theta <- MASS::fitdistr(y, dist)$estimate # observed fitted values
  fit <- do.call(f, c(list(sort(y)), as.list(theta))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  # Block Bootstrap
  bts <- boot::tsboot(y, mystat, l = blksize, sim = "fixed", R = B, dist = dist)
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = 2) 

  sum_theta <- 0
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts$t[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts$t[i, -(1:n)]))
  }
  exp_emp <- colMeans(list_emp) # exp empirical distribution
  exp_theta <- colMeans(list_theta) # exp fitted values
  exp_fit <- do.call(f, c(list(sort(y)), as.list(exp_theta))) # distribution with exp fitted values
  bias_term <- exp_emp - exp_fit # bias term
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f, c(list(sort(y)), as.list(boot_theta))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit - bias_term)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}

plot_p_vals <- function(n, blksize, B, dist, f, phi, theta, nrep = 1000) {
  df <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = nrep,
                          ncol = 1))
  p <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
  df$p <- p
  ## Section 2: Fitted parameters
  gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
    scale_x_continuous(breaks=c(0, 1)) +
    scale_y_continuous(breaks=c(0, 1)) + 
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .1) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)
  ggsave(filename = paste("sim_", n, '_norm_', 
                          phi, '_', 
                          theta, ".pdf", sep = ''), plot = gg.f, 
         path = "../manuscript", height = 4, width = 4)
}
