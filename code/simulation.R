nrep = 100
n = 100
blksize = 10
B = 100
dist = 'normal'
f = pnorm
phi <- -0.4
theta <- 0
sim_ar1_n.4 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_n.4)


nrep = 100
n = 100
blksize = 10
B = 100
dist = 'normal'
f = pnorm
phi <- -0.2
theta <- 0
sim_ar1_n.2 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_n.2)


nrep = 100
n = 100
blksize = 10
B = 100
dist = 'normal'
f = pnorm
phi <- 0
theta <- 0
sim_ar1_0 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_0)


nrep = 100
n = 100
blksize = 10
B = 100
dist = 'normal'
f = pnorm
phi <- 0.2
theta <- 0
sim_ar1_.2 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_.2)


nrep = 100
n = 100
blksize = 10
B = 100
dist = 'normal'
f = pnorm
phi <- 0.4
theta <- 0
sim_ar1_.4 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
hist(sim_ar1_.4)