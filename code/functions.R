library(tidyverse)
library(evd)
library(truncdist)
library(qqplotr)
library(copula)
library(tseries)
library(latex2exp)
source("ksfitted.R")


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
    coord_fixed(ylim = c(0, 1), xlim = c(-0.6, 0.6))
  ggsave(filename = filename, plot = gg.f, path = "../manuscript/figures", 
         height = 3, width = 6)
    
    
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
  pval <- c(pval, ks.test.fitted(resid, "norm", B = 10000, serial = TRUE, 
                                 param = c(mean = 0, sd = 1))$p.value)
  
  set.seed(123)
  pval <- c(pval, my_babu(resid, 10000, "normal", 
                          pnorm))
  
  set.seed(123)
  pval <- c(pval, my_param(resid, 10000, "normal", 
                           pnorm, rnorm))
  
  for (v in c(30, 20, 10, 5, 4, 3, 2, 1)) {
    set.seed(123)
    np <- myapp(resid, 10000, "t", plst, 
                df = v)
    set.seed(123)
    sb <- ks.test.fitted.lst(resid, "lst", B = 10000, serial = TRUE, 
                             param = c(mu = 0, sigma = 1), df = v)$p.value
    set.seed(123)
    babu <- my_babu(resid, 10000, "t", plst, 
                    df = v)
    set.seed(123)
    param <- my_param(resid, 10000, "t", plst, rlst,
                      df = v)
    
    pval <- rbind(pval,
                  c(np,
                    babu,
                    param,
                    sb))
  }
  
  pval <- cbind(c("normal", 30, 20, 10, 5, 4, 3, 2, 1), pval)
  colnames(pval) <- c("$v$", "NPBB", "SPB", "NPB", 
                      "PB")
  rownames(pval) <- NULL
  return(pval)
}


R <- function(x, k) {
  N <- length(x)
  a <- x[-(1:abs(k))]
  b <- x[-((N - abs(k) + 1):N)]
  sum = 0
  for (i in 1:length(a)) {
    sum = sum + (a[i] - mean(x)) * (b[i] - mean(x))
  }
  return(sum / N)
}

rho <- function(x, k) {
  R(x, k) / R(x, 0)
}

politis_test <- function(x, C = 2, K = NULL) {
  N <- length(x)
  if (is.null(K)) {
    K = max(c(5, sqrt(log10(N))))
  } 
  
  condition = TRUE
  m = 1
  while (condition == TRUE) {
    condition = FALSE
    for (k in 1:K) {
      if (abs(rho(x, m + k)) >= C * sqrt(log(N) / N)) {
        condition = TRUE
        break
      }
    }
    m = m + 1
  }
  return(m)
}

lambda <- function(t) {
  if (abs(t) >= 0 & abs(t) <= 1/2) {
    return(1)
  } else if (abs(t) >= 1/2 & abs(t) <= 1) {
    return(2 * (1 - abs(t)))
  } else return(0)
}

g <- function(x, w, M) {
  sum = 0
  for (k in (-M):M) {
    sum = sum + lambda(k / M) * R(x, k) * cos(w * k)
  }
  return(sum)
}

G <- function(x, M) {
  sum = 0
  for (k in (-M):M) {
    sum = sum + lambda(k / M) * abs(k) * R(x, k)
  }
  return(sum)
}
politis2004blksize <- function(x) {
  N <- length(x)
  m <- politis_test(x)
  M <- 2*m
  D <- 4 / 3 * g(x, 0, M) ^ 2
  G <- G(x, M)
  return( (2 * G ^ 2 / D) ^ (1 / 3) * N ^ (1 / 3) )
}

# stochastic_approximation <- function(x, b, B) {
#   n <- length(x)
#   N_n <- chooose(n, b)
#   I <- sample.int(N_n, B)
#   sum = 0
#   for (t in 1:N) {
#     sum = sum + ((tau * (theta1 - theta2)) <= x)
#   }
#   return((1 / B) * sum)
# }
# 
# minimum_volatility_blksize <- function(x, b_small = 1, b_big = 30) {
#   
#   for (b in b_small:b_big) {
#     subsampling(x, b)
#   }
#   
# }
