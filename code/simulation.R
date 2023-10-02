source('functions.R')
library(tidyverse)
library(qqplotr)
set.seed(1234)
nrep = 1000
n = 200
blksize <- ceiling(n^(1/3))
B = 1000
dist = 'normal'
f = pnorm
phi <- 0
theta <- 0
df <- data.frame(matrix(NA,    # Create empty data frame
                        nrow = nrep,
                        ncol = 1))
sim_200_norm_0_0 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
df$p <- sim_200_norm_0_0
## Section 2: Fitted parameters
gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  coord_fixed() # theme(aspect.ratio=1)
ggsave(filename = "sim_200_norm_0_0.pdf", plot = gg.f, path = "../manuscript", 
       height = 4, width = 4)

phi <- 0.2
df <- data.frame(matrix(NA,    # Create empty data frame
                        nrow = nrep,
                        ncol = 1))
sim_200_norm_0.2_0 <- replicate(nrep, mysim(n, blksize, B, dist, f, phi, theta))
df$p <- sim_200_norm_0.2_0
## Section 2: Fitted parameters
gg.f <- ggplot(data = df, mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  coord_fixed() # theme(aspect.ratio=1)
ggsave(filename = "sim_200_norm_0.2_0.pdf", plot = gg.f, path = "../manuscript", 
       height = 4, width = 4)


