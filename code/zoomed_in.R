source('functions.R')

sim_800_normal_n0.4_0 <- readRDS('../data/sim_800_normal_-0.4_0.RDS')
zoomed_in(sim_800_normal_n0.4_0, 800, 'normal', -0.4, 0)
hist(sim_800_normal_n0.4_0$p)

sim_800_normal_n0.2_0 <- readRDS('../data/sim_800_normal_-0.2_0.RDS')
zoomed_in(sim_800_normal_n0.2_0, 800, 'normal', -0.2, 0)
hist(sim_800_normal_n0.2_0$p)

sim_800_normal_0_0 <- readRDS('../data/sim_800_normal_0_0.RDS')
zoomed_in(sim_800_normal_0_0, 800, 'normal', 0, 0)
hist(sim_800_normal_0_0$p)

sim_800_normal_0.2_0 <- readRDS('../data/sim_800_normal_0.2_0.RDS')
zoomed_in(sim_800_normal_0.2_0, 800, 'normal', 0.2, 0)
hist(sim_800_normal_0.2_0$p)

sim_800_normal_0.4_0 <- readRDS('../data/sim_800_normal_0.4_0.RDS')
zoomed_in(sim_800_normal_0.4_0, 800, 'normal', 0.4, 0)
hist(sim_800_normal_0.4_0$p)
