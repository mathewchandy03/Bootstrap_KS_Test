.libPaths("/gpfs/homefs1/mac18033/rlibs") # replace with actual path
library(tidyverse)
library(truncdist)
mystat <- function(x, dist) {
  # Bootstrap will return actual pseudo-sample, mean, and standard deviation
  c(c(x),  
    MASS::fitdistr(x, dist)$estimate)
}

bs_ks <- function(y, B, h0_dist, f0, blk, method, rgen = NULL) {
  n <- length(y)
  emp <- ecdf(y)(sort(y)) # observed empirical distribution
  fit_theta <- MASS::fitdistr(y, h0_dist)$estimate # observed fitted values
  fit <- do.call(f0, c(list(sort(y)), as.list(fit_theta))) # observed fitted distribution
  obsv_ks <- sqrt(n) * max(abs(emp - fit)) # observed ks statistic
  print(blk)
  if (method == "npbb") {
    bts <- boot::tsboot(y, mystat, l = blk, sim = "fixed", R = B, 
                        dist = h0_dist)$t
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
  c(npbb)
  
}

R <- function(x, k) {
  N <- length(x)
  a <- x[1:(N - abs(k))]
  b <- x[(1+abs(k)):N]
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
  return(pmin(blk, 100))
}

my_job <- function(i, nrep = 500, blksize = 'a') {
  B <- 1000
  # If you cannot run it on HPC, you can try running it locally, but 10,000
  # replicates will take a long time, try nrep <- 10 first
  phis <- c()
  
  phis <- 
    c(-0.9238795, -0.7071068, -0.3826834, 0, 0.3826834, 0.7071068, 0.9238795)
  # 1-2500, to run it locally select one seed
  
  for (phi in phis) {
    for (n in c(100, 200, 400, 800)) {
      set.seed(as.integer(i))
      for (true_dist in c("normal", "gamma")) {
        for (truth in c("null", "alt")) {
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
          print(phi)
          print(n)
          print(true_dist)
          print(truth)
          p <- replicate(nrep, mysim(n, blksize, B, h0_dist, true_dist, f0, f, 
                                     theta, phi, rgen))
          saveRDS(p, paste("../data/simulation/", truth, "_", n, "_", true_dist, "_",
                           phi, "_", 
                           "0", "_",  i, ".RDS", sep = ""))
        }
      }
    }
  }
}

subsampling_interval <- function(b, x, B) {
  theta_n <- rep(mean(x), B)
  Y <- replicate(B, sample(x, b))
  if (is.null(dim(Y))) Y <- t(as.matrix(Y))
  theta_b <- as.vector(apply(Y, 2, mean))
  C <- sort(abs(theta_b - theta_n)) # I just took the absolute value for the
  # normalizing sequence but I am not sure if this is correct
  c(C[.05 * B], C[.95 * B])
}

politis1999subsampling <- function(x, B = 100) {
  b_small <- 1
  b_big <- sqrt(length(x))
  bs <- seq(b_small, b_big, 3)
  intervals <- lapply(bs, subsampling_interval, x, B)
  intervals <- t(do.call(rbind, intervals))
  num_bs <- ncol(intervals)
  smooth_intervals <- matrix(NA, nrow(intervals), num_bs)
  volatility <- rep(NA, num_bs)
  # The book recommends to skip some b's, but I am not sure if that also
  # means to skip them in the following smoothing step. I am also not
  # sure how to smooth for b_small and b_big, because the algorithm says to 
  # take the averages of {I_{b-m, low}, ..., I_{b+m, low}} and
  # {I_{b-m, up}, ..., I_{b+m, up}} 
  for (i in 1:num_bs) {
    smooth_intervals[,i] <- rowMeans(intervals[,(pmax(1, i-1):pmin(num_bs, i+1))]) 
  }
  for (i in 1:num_bs) {
    volatility[i] <- sum(as.vector(apply(intervals[,(pmax(1, i-1):pmin(num_bs, i+1))], 1, sd, 
                                         na.rm=TRUE)))
  }
  return(bs[which.min(volatility)])
}