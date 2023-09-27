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
  
  exp <- colMeans(bts$t[,c(1:n)])
  
  exp.f_n.star <- ecdf(exp)(sort(exp))
  
  exp.theta_n.star <- colMeans(bts$t[,c((n+1):(n+2))])
  
  fit_exp <- do.call(f, c(list(sort(x)), as.list(exp.theta_n.star)))
  
  bias_term <- exp.f_n.star - f.exp
  
  ks_values <- c()
  for (b in 1:B) {
    bt <- bts$t[b, 1:n]
    f_n.star <- ecdf(bt)(sort(bt))
    theta_n.star <- bts$t[b, (n+1):(n+2)]
    fit_boot <- do.call(f, c(list(sort(x)), as.list(theta_n.star)))
    ks_values <- c(ks_values, sqrt(n) * max(abs((f_n.star - theta_n.star - bias_term))))
  }
  c(mean(ks_values > obsv_ks))
}