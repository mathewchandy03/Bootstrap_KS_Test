library(tidyverse)
library(evd)
library(truncdist)
library(qqplotr)
library(copula)
library(tseries)
mystat <- function(x, dist, df = NULL) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  if (dist == 'gev') {
    c(c(x), 
      evd::fgev(x, std.err = FALSE)$estimate)
  } else {
    c(c(x),  
      MASS::fitdistr(x, dist, df = df)$estimate)
  }
}

my_babu <- function(y, B, h0_dist, f0, df = NULL) {
  n <- length(y)
  blksize <- 1
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  if (h0_dist == 'gev') {
    fit_theta <- evd::fgev(y, std.err = FALSE)$estimate # gev not recognised by fitdistr
  } else {
    fit_theta <- MASS::fitdistr(y, h0_dist, df = df)$estimate # observed fitted values
  }
  
  fit <- do.call(f0, c(list(sort(y)), as.list(c(df, fit_theta)))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  # Basic Bootstrap
  bts <- boot::tsboot(y, mystat, l = blksize, sim = "fixed", R = B, 
                      dist = h0_dist, df = df)
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = length(fit_theta)) 
  
  sum_theta <- 0
  
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts$t[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts$t[i, -(1:n)]))
  }
  
  bias_term <- emp - fit # bias term
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f0, c(list(sort(y)), as.list(c(df, boot_theta)))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit - bias_term)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}

my_param <- function(y, B, h0_dist, f0, rgen, df = NULL) {
  n <- length(y)
  blksize <- 1
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  if (h0_dist == "gev") {
    fit_theta <- evd::fgev(y, std.err = FALSE)$estimate # gev not recognised by fitdistr
  } else {
    fit_theta <- MASS::fitdistr(y, h0_dist, df = df)$estimate # observed fitted values
  }
  
  fit <- do.call(f0, c(list(sort(y)), as.list(c(df, fit_theta)))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  bts <- t(replicate(B, c(
    do.call(rgen, c(n, as.list(c(df, fit_theta)))))))
  
  theta <- c()
  if (h0_dist == "gev") {
    for(row in 1:nrow(bts)) {
      theta <- rbind(theta, evd::fgev(bts[row,], std.err = FALSE)$estimate)
    }
  } else {
    for(row in 1:nrow(bts)) {
      theta <- rbind(theta, MASS::fitdistr(bts[row,], h0_dist, df = df)$estimate)
    }
  }
 
  bts <- cbind(bts, theta)
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = length(fit_theta)) 
  
  sum_theta <- 0
  
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts[i, -(1:n)]))
  }
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f0, c(list(sort(y)), as.list(c(df, boot_theta)))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}

ks.sb <- function(x, B = 1000, q0, f0, df = NULL) {
  stat <- ks.test(x, f0, df = df)$statistic
  stat.b <- rep(0, B)
  n <- length(x)
  rk <- rank(x)
  for (i in 1:B) {
    u <- runif(n)
    x.b <- q0(sort(u)[rk], df, mean(x), sd(x))
    ## The ks stat remained the same as the one before sorting
    stat.b[i] <- ks.test(x.b, f0, df = df, mean(x.b), sd(x.b))$statistic
  }
  p.value <-  (sum(stat.b >= stat) + 0.5) / (B + 1)
  list(p.value = p.value,
       statistic = stat, stat.b = stat.b)
}

normal.ks.sb <- function(x, B = 1000, q0, f0, df = NULL) {
  stat <- ks.test(x, f0)$statistic
  stat.b <- rep(0, B)
  n <- length(x)
  rk <- rank(x)
  for (i in 1:B) {
    u <- runif(n)
    x.b <- q0(sort(u)[rk], mean(x), sd(x))
    ## The ks stat remained the same as the one before sorting
    stat.b[i] <- ks.test(x.b, f0, mean(x.b), sd(x.b))$statistic
  }
  p.value <-  (sum(stat.b >= stat) + 0.5) / (B + 1)
  list(p.value = p.value,
       statistic = stat, stat.b = stat.b)
}

my_sb <- function(x, B = 1000, q0, f0, df = NULL) {
  if(is.null(df)) {
    normal.ks.sb(x, B = 1000, q0, f0)$p.value
  } else {
    ks.sb(x, B = 1000, q0, f0, df = df)$p.value
  }
}

app_scheme <- function(start, end, years, stock) {
  series <- 
    diff(log(get.hist.quote(instrument = stock, start = start,
                            end = end)))
  
  series <- as.vector(series[, "Close"])
#  fit <- auto.arima(as.vector(series[, "Close"]))
#  resid <- residuals(fit)
  
  resid <- series
  
  set.seed(123)
  pval <- myapp(resid, 10000, "normal", 
                      pnorm)
  set.seed(123)
  pval <- c(pval, my_babu(resid, 10000, "normal", 
                                      pnorm))
  set.seed(123)
  pval <- c(pval, my_param(resid, 10000, "normal", 
                                pnorm, rnorm))
  set.seed(123)
  pval <- c(pval, my_sb(resid, 10000, qnorm, pnorm, df = NULL))
  
  for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
    set.seed(123)
    np <- myapp(resid, 10000, "t", plst, 
                df = v)
    set.seed(123)
    babu <- my_babu(resid, 10000, "t", plst, 
                    df = v)
    set.seed(123)
    param <- my_param(resid, 10000, "t", plst, rlst,
                    df = v)
    
    set.seed(123)
    sb <- my_sb(resid, 10000, qlst, rlst, df = v)
    pval <- rbind(pval,
                        c(np,
                          babu,
                          param,
                          sb))
  }
  
  pval <- cbind(c("normal", 30, 20, 10, 5, 4, 3, 2, 1), pval)
  colnames(pval) <- c("$v$", "Our Method", "Babu", "Zeimbekakis", 
                      "Semiparametric")
  rownames(pval) <- NULL
  return(pval)
}

