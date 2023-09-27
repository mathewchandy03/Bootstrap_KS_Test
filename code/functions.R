mystat <- function(x, dist) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  c(c(x),  
    MASS::fitdistr(x, dist)$estimate)
}

mysim <- function(n, blksize, B, dist, f, phi = 0, theta = 0) {
  # phi cannot be set to 0, but theta can
  if (phi == 0) {
    x <- arima.sim(list(ma = theta), n = n) / sqrt(1 + sum(theta^2))
  } else {
    x <- arima.sim(list(ar = phi, ma = theta), n = n) * 
      sqrt(1 - sum(phi^2) / 1 + sum(theta^2))
  }
  
  f_n <- ecdf(x)(sort(x)) # observed empirical distribution
  theta_n <- MASS::fitdistr(x, dist)$estimate # observed fitted values
  fit_obsv <- do.call(f, c(list(sort(x)), as.list(theta_n))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(f_n - fit_obsv)) 
  
  # Block bootstrap 
  bts <- boot::tsboot(x, mystat, l = blksize, sim = "fixed", R = B, dist = dist)
  
  exp <- colMeans(bts$t[, (1:n)])
  
  exp.f_n.star <- ecdf(exp)(sort(x))
  
  exp.theta_n.star <- colMeans(bts$t[, -(1:n)])
  
  fit_exp <- do.call(f, c(list(sort(x)), as.list(exp.theta_n.star)))
  
  bias_term <- exp.f_n.star - f.exp
  
  ks_values <- c()
  for (b in 1:B) {
    bt <- bts$t[b, (1:n)]
    f_n.star <- ecdf(bt)(sort(x))
    theta_n.star <- bts$t[b, -(1:n)]
    fit_boot <- do.call(f, c(list(sort(x)), as.list(theta_n.star)))
    ks_values <- c(ks_values, sqrt(n) * max(abs((f_n.star - fit_boot - bias_term))))
  }
  c(mean(ks_values > obsv_ks))
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
