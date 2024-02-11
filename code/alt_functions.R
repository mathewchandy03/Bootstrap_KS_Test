library(tidyverse)
library(evd)
library(truncdist)
library(qqplotr)
library(copula)
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

my_param_app <- function(y, B, h0_dist, f0, rgen = NULL, df = NULL) {
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
  
  # Parametric Bootstrap
  bts <- NA
  if (h0_dist == 'gev') {
    bts <- replicate(B, c(evd::rgev(length(y)), 
                          evd::fgev(y, std.err = FALSE)$estimate)) # gev not recognised by fitdistr
  } else {
    bts <- replicate(B, c(rgen(length(y)),
                     MASS::fitdistr(y, dist, df = df)$estimate)) # observed fitted values
  }
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = length(fit_theta)) 
  
  sum_theta <- 0
  
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts$t[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts$t[i, -(1:n)]))
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



