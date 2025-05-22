library(tidyverse)
library(truncdist)
library(doRNG)
library(foreach)
library(doSNOW)
doFuture::registerDoFuture()
future::plan("multisession", workers=20)
doRNG::registerDoRNG(123)
# cl <- makeCluster(20)
# registerDoSNOW(cl)
# pb <- txtProgressBar(max = 40, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
mystat <- function(x, dist, type = 1) {
  if (type == 1) {
    vec <- x
  } else if (type == 2) {
    vec <- c()
  }
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  if (dist == 'normal') {
    n <- length(x)
    sd0 <- sqrt((n - 1) / n) * sd(x)
    mx <- mean(x)
    c(c(vec), c(mx, sd0))
  } else if (dist == 'gamma') {
    mx <- mean(x)
    a <- 0.5 / (log(mx) - mean(log(x)))
    a <- 1 / (1/a + (mean(log(x)) - log(mx) + log(a) - digamma(a)) /
                (a - a^2 * trigamma(a)))
    rate <- a / mx
    
    c(c(vec), c(a, rate))
  } else {
    c(c(vec), MASS::fitdistr(x, dist)$estimate)
  }
}



bs_ks <- function(y, B, h0_dist, f0, blk, method, rgen = NULL) {
  n <- length(y)
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  fit_theta <- mystat(y, h0_dist, type = 2) # observed fitted values
  fit <- do.call(f0, c(list(sort(y)), as.list(fit_theta))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  # print(blk)
  if (method == "npbb") {
    bts <- cbb(y, mystat, l = blk, R = B, dist = h0_dist)
  } else if (method == "npb") {
    bts <- boot::tsboot(y, mystat, l = 1, sim = "fixed", R = B, 
                        dist = h0_dist)$t
  } else if (method == "pb") {
    bts <- t(replicate(B, c(
      do.call(rgen, c(n, as.list(c(fit_theta)))))))
    theta <- c()
    for(row in 1:nrow(bts)) {
      theta <- rbind(theta, MASS::fitdistr(bts[row,], h0_dist)$estimate)
    }
    bts <- cbind(bts, theta)
  }
  
  
  # Data must be restructured in the following ways, so the expected value
  # of the cdfs can be computed
  list_emp <- matrix(, nrow = 0, ncol = n) 
  list_theta <- matrix(, nrow = 0, ncol = length(fit_theta)) 
  
  sum_theta <- 0
  
  for (i in 1:B) {
    list_emp <- rbind(list_emp, c(ecdf(bts[i, (1:n)])(sort(y))))
    list_theta <- rbind(list_theta, c(bts[i, -(1:n)]))
  }
  
  if (method == "npbb") {
    exp_emp <- colMeans(list_emp) # exp empirical distribution
    exp_theta <- colMeans(list_theta) # exp fitted values
    exp_fit <- do.call(f0, c(list(sort(y)), as.list(exp_theta))) # distribution with exp fitted values
    bias_term <- exp_emp - exp_fit # bias term
  } else if (method == "npb") {
    bias_term <- emp - fit # bias term
  } else if (method == "pb") {
    bias_term <- 0
  }
  
  ks_values <- c()
  for (i in 1:B) {
    boot_emp <- list_emp[i,] # bootstrap empirical distribution
    boot_theta <- list_theta[i, ] # bootstrap fitted values
    boot_fit <- do.call(f0, c(list(sort(y)), as.list(boot_theta))) # distribution with bootstrap fitted values
    ks_values <- c(ks_values, sqrt(n) * max(abs((boot_emp - boot_fit - bias_term)))) # bootstrap ks statistics
  }
  c(mean(ks_values > obsv_ks)) # Which are greater than the observed ks statistic
}

mysim <- function(n, blksize, B, h0_dist, true_dist, f0, f, theta, phi,  
                  rgen) {
  y <- arima.sim(list(ar = phi), n = n) * 
    sqrt(1 - sum(phi^2))
  if (h0_dist == 'gamma' & true_dist == 'normal') {
    y <- qtrunc(pnorm(y), "norm", a = 0, mean = theta[1], sd = theta[2])
  } else {
    y <- do.call(f, c(list(pnorm(y)), as.list(theta)))
  }
  if (blksize == 'a') {
    blk <- politis2004blksize(y)
  } else blk <- blksize
  npbb <- bs_ks(y, B, h0_dist, f0, blk, "npbb")
  #  npb <- bs_ks(y, B, h0_dist, f0, blk, "npb")
  #  pb <- bs_ks(y, B, h0_dist, f0, blk, "pb", rgen)
  
  #  list(npbb = npbb, npb = npb, pb = pb)
  list(npbb = npbb, blk = blk)
  
}

R <- function(x, k) {
  x <- do.call(c, lapply(x, pmin, 10e6))
  N <- length(x)
  a <- x[1:pmax(N - abs(k), 1)]
  b <- x[pmin(1+abs(k), N):N]
  bar_x <- mean(x)
  return(sum((a - bar_x) * (b - bar_x)) / N)
}

# rho <- function(x, k) {
#   R(x, k) / R(x, 0)
# }

politis_test <- function(x, C = 2, K = NULL) {
  N <- length(x)
  if (is.null(K)) {
    K = max(c(5, sqrt(log10(N))))
  } 
  
  condition = TRUE
  m = 1
  R_0 <- R(x, 0)
  while (condition == TRUE) {
    # print(m)
    condition = FALSE
    for (k in 1:K) {
      # print(k)
      rho <- R(x, m + k) / R_0
      if (abs(rho) >= C * sqrt(log(N) / N)) {
        condition = TRUE
        m = m + 1
        break
      }
    }
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
  s = 0
  for (k in (-M):M) {
    s = s + lambda(k / M) * R(x, k) * cos(w * k)
  }
  return(s)
}

G <- function(x, M) {
  s = 0
  for (k in (-M):M) {
    s = s + lambda(k / M) * abs(k) * R(x, k)
  }
  return(s)
}
politis2004blksize <- function(x) {
  N <- length(x)
  m <- politis_test(x)
  M <- 2*m
  D <- 4 / 3 * g(x, 0, M) ^ 2
  blk <- round((2 * G(x, M) ^ 2 / D) ^ (1 / 3) * N ^ (1 / 3) )
  return(pmax(pmin(blk, N), 1))
}

my_job <- function(i, nrep = 1, blksize = 'a') {
  B <- 1000
  # If you cannot run it on HPC, you can try running it locally, but 10,000
  # replicates will take a long time, try nrep <- 10 first
  phis <- c()
  
  # my_data <- data.frame(p = numeric(), blk = integer(), truth = character(), 
  #                  n = integer(), true_dist = character(), phi = numeric(),
  #                  i = integer(), rep = integer(), stringsAsFactors = FALSE)
  phis <- 
    c(-0.9238795, -0.7071068, -0.3826834, 0, 0.3826834, 0.7071068, 0.9238795)
  # 1-2500, to run it locally select one seed
  my_data <- foreach(rep = 1:nrep, .packages=c('rpm'), .combine = 'rbind') %dorng% {
    # if (rep %% 4 == 0) print(rep)
    foreach(phi = phis, .packages=c('rpm'), .combine = 'rbind') %dorng% {
      foreach(n = c(100, 200, 400, 800), .packages=c('rpm'), .combine = 'rbind') %dorng% {
        set.seed(as.integer(i))
        foreach(true_dist = c("normal", "gamma"), .packages=c('rpm'), .combine = 'rbind') %dorng% {
          foreach(truth = c("null", "alt"), .packages=c('rpm'), .combine = 'rbind') %dorng% {
            if (true_dist == "normal" & truth == "null") {
              h0_dist <- "normal"
              f0 <- pnorm
              f <- qnorm
              theta <- c(8, sqrt(8))
              rgen <- rnorm
            }
            if (true_dist == "gamma" & truth == "alt") {
              h0_dist <- "normal"
              f0 <- pnorm
              f <- qgamma
              theta <- c(8,1)
              rgen <- rnorm
            }
            if (true_dist == "gamma" & truth == "null") {
              h0_dist <- "gamma"
              f0 <- pgamma
              f <- qgamma
              theta <- c(8,1)
              rgen <- rgamma
            } 
            if (true_dist == "normal" & truth == "alt") {
              h0_dist <- "gamma"
              f0 <- pgamma
              f <- qnorm
              theta <- c(8, sqrt(8))
              rgen <- rgamma
            }
            # print(i)
            # print(phi)
            # print(n)
            # print(true_dist)
            # print(truth)
            sim <- mysim(n, blksize, B, h0_dist, true_dist, f0, f, 
                         theta, phi, rgen)
            p <- sim[['npbb']]
            blk <- sim[['blk']]
            # saveRDS(p, paste("../data/simulation/", truth, "_", n, "_", true_dist, "_",
            #                  phi, "_", 
            #                  "0", "_",  i, ".RDS", sep = ""))
            new_row <- data.frame(
              p = p,
              blk = blk,
              truth = truth,
              n = n,
              true_dist = true_dist,
              phi = phi,
              i = i,
              rep = rep,
              stringsAsFactors = FALSE
            )
            new_row
          }
        }
      }
    }
 }

  return(my_data)
}

cbb <- function(data, statistic, R, l, dist, nparam = 2) {
  n <- length(data)
  bootstrap_samples <- matrix(NA, nrow = R, ncol = n + nparam)
  
  for (i in 1:R) {
    resampled_series <- c()
    
    while (length(resampled_series) < n) {
      start_index <- sample(1:n, 1)
      
      if (start_index + l - 1 <= n) {
        block <- data[start_index:(start_index + l - 1)]
      }
      else {
        diff <- start_index + l - 1 - n
        block <- c(data[start_index:n], data[1:diff])
      }
      if (length(resampled_series) + l > n) {
        remaining_length <- n - length(resampled_series)
        block <- block[1:remaining_length]
      }
      
      resampled_series <- c(resampled_series, block)
    }
    
    bootstrap_samples[i, ] <- statistic(resampled_series, dist)
  }
  
  return(bootstrap_samples)
}

start.time <- Sys.time()
my_data <- data.frame(
  p = numeric(),
  blk = numeric(),
  truth = character(),
  n = numeric(),
  true_dist = character(),
  phi = numeric(),
  i = numeric(),
  rep = integer(),
  stringsAsFactors = FALSE
)
for(i in 1:10) {
  print(i)
  my_data <- rbind(my_data, my_job(i, 1000))
}
# close(pb)
# stopCluster(cl) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(my_data, '../data/blk2004_results_10000.RDS')
