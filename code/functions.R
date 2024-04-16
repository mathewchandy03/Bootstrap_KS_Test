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

myapp <- function(y, B, h0_dist, f0, df = NULL) {
  n <- length(y)
  blksize <- ceiling(n^(1/3))
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  if (h0_dist == 'gev') {
    fit_theta <- evd::fgev(y, std.err = FALSE)$estimate # gev not recognised by fitdistr
  } else {
    fit_theta <- MASS::fitdistr(y, h0_dist, df = df)$estimate # observed fitted values
  }
  
  fit <- do.call(f0, c(list(sort(y)), as.list(c(df, fit_theta)))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  
  # Block Bootstrap
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
  exp_emp <- colMeans(list_emp) # exp empirical distribution
  exp_theta <- colMeans(list_theta) # exp fitted values
  exp_fit <- do.call(f0, c(list(sort(y)), as.list(c(df, exp_theta)))) # distribution with exp fitted values
  bias_term <- exp_emp - exp_fit # bias term
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f0, c(list(sort(y)), as.list(c(df, boot_theta)))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit - bias_term)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}

plot_p_vals <- function(df, filename) {
  
  df$tau <- factor(case_when(df$phi == "-0.9238795" ~ "tau == -0.75",
                      df$phi == "-0.7071068" ~ "tau == -0.5",
                      df$phi == "-0.3826834" ~ "tau == -0.25",
                      df$phi == "0" ~ "tau == 0",
                      df$phi == "0.9238795" ~ "tau == 0.75",
                      df$phi == "0.7071068" ~ "tau == 0.5",
                      df$phi == "0.3826834" ~ "tau == 0.25"),
                   levels = c("tau == -0.75",
                              "tau == -0.5",
                              "tau == -0.25",
                              "tau == 0",
                              "tau == 0.25",
                              "tau == 0.5",
                              "tau == 0.75"))
  
  df$dist <- factor(case_when(df$dist == "normal" ~ "N(8,8)",
                              df$dist == "gamma" ~ "Gamma(8,1)"))
  
  df$n <- factor(case_when(df$n == 100 ~ "n == 100",
                           df$n == 200 ~ "n == 200",
                           df$n == 400 ~ "n == 400",
                           df$n == 800 ~ "n == 800"))
  
  gg.f <- ggplot(data = df, mapping = aes(sample = pval)) +
    scale_x_continuous(breaks=c(0, 1)) +
    scale_y_continuous(breaks=c(0, 1)) + 
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .1) +
    facet_grid(rows = vars(n), cols = vars(tau),
               labeller = label_parsed) +
    labs(x = "Theoretical Cumulative Distribution", 
         y = "Empirical Cumulative Distribution") +
    coord_fixed() +
    theme(strip.text.x = element_text(size = 8))
  ggsave(filename = filename, plot = gg.f, path = "../manuscript/figures", 
         height = 4, width = 6)
  
  gg.f <- ggplot(data = df, mapping = aes(sample = pval)) +
    scale_x_continuous(breaks=c(0, 0.1)) +
    scale_y_continuous(breaks=c(0, 0.1)) + 
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .1) +
    facet_grid(rows = vars(n), cols = vars(tau), 
               labeller = label_parsed) +
    labs(x = "Theoretical Cumulative Distribution", 
         y = "Empirical Cumulative Distribution") +
    coord_fixed(ylim = c(-0.01, 0.11), xlim = c(-0.01, 0.11)) +
    theme(strip.text.x = element_text(size = 8))
  ggsave(filename = paste('zoom_', filename, sep = ''), plot = gg.f, 
         path = "../manuscript/figures", height = 4, width = 6)
}

plot_rr <- function(df, filename) {
  
  df$dist <- factor(case_when(df$dist == "normal" ~ "N(8,8)",
                       df$dist == "gamma" ~ "Gamma(8,1)"),
                    levels = c("N(8,8)",
                               "Gamma(8,1)"))
  
  df$tau <- case_when(df$phi == "-0.9238795" ~ -0.75,
                      df$phi == "-0.7071068" ~ -0.5,
                      df$phi == "-0.3826834" ~ -0.25,
                      df$phi == "0" ~ 0,
                      df$phi == "0.9238795" ~ 0.75,
                      df$phi == "0.7071068" ~ 0.5,
                      df$phi == "0.3826834" ~ 0.25)
  
  gg.f <- ggplot(data = df, mapping = aes(x = tau, y = as.numeric(rr), 
                                          color = n, 
                                          linetype = n)) +
    geom_line() +
    geom_point() +
    facet_grid(cols = vars(dist), labeller = label_parsed) +
    theme(
          legend.position = "bottom") +
    labs(x = latex2exp::TeX("$\\tau$"), y = latex2exp::TeX("Rejection Rate")) +
    coord_fixed(ylim = c(0, 1), xlim = c(-1, 1))
  ggsave(filename = filename, plot = gg.f, path = "../manuscript/figures", 
         height = 3, width = 6)
    
    
}

