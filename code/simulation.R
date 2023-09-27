nrep = 1000
n = 100
blksize = 10
B = 1000
dist = 'normal'
f = pnorm
phi <- -0.4
theta <- 0
sim_ar1_n.3 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_n.3)