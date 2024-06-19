## Need packages copula and pracma

library(copula)
library(pracma)

mymvd <- copula::mvdc(normalCopula(0.4), margins = "exp", paramMargins = list(rate=1), marginsIdentical = TRUE)

## numerical integration to get 
EXY <- pracma::dblquad(function(x, y) x * y * copula::dMvdc(cbind(x, y), mymvd),
                       0, 20, 0, 20)
## approximate true rho
rho <- (EXY - 1) / 1

## empirical check
z <- rMvdc(100000, mymvd)
cor(z[,1], z[,2])



rho2phi <- function(rho) {
  mymvd <- copula::mvdc(copula::normalCopula(rho), margins = "gamma",
                        paramMargins = list(rate=8, scale=1), marginsIdentical = TRUE)
  EXY <- pracma::dblquad(function(x, y) x * y * copula::dMvdc(cbind(x, y), mymvd),
                         0, 20, 0, 20)
  phi <- (EXY - 1) / 1
  phi
}

uniroot(function(x) rho2phi(x) - 0.707, interval = c(0, 0.9))

uniroot(function(x) rho2phi(x) + 0.707, interval = c(-.56, 0))

# lag-1 AR for normal series
for (i in c(-.75, -.5, -.25, 0, .25, .5, .75)) { 
  print(iTau(normalCopula(), i))
}

# approximation of lag-1 AR for gamma series
set.seed(123)
for(phi in c(-0.924, -0.707, -0.383, 0, 0.383, 0.707, 0.924)) {
  y <- arima.sim(list(ar = phi), n = 1000000) * 
    sqrt(1 - phi^2)
  y <- qgamma(pnorm(y), 8, 1)
  print(cor(y[-1], y[-length(y)]))
}
