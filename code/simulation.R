source('functions.R')
library(tidyverse)
library(qqplotr)


set.seed(0123)
plot_p_vals(400, 20, 1000, 'normal', pnorm, phi = -0.4, theta = 0)


set.seed(1234)
plot_p_vals(400, 20, 1000, 'normal', pnorm, phi = -0.2, theta = 0)


set.seed(2345)
plot_p_vals(400, 20, 1000, 'normal', pnorm, phi = 0, theta = 0)


set.seed(3456)
plot_p_vals(400, 20, 1000, 'normal', pnorm, phi = 0.2, theta = 0)


set.seed(4567)
plot_p_vals(400, 20, 1000, 'normal', pnorm, phi = 0.4, theta = 0)


set.seed(0123)
plot_p_vals(400, 20, 1000, 'exponential', pexp, qexp, phi = -0.4, theta = 0)


set.seed(1234)
plot_p_vals(400, 20, 1000, 'exponential', pexp, qexp, phi = -0.2, theta = 0)


set.seed(2345)
plot_p_vals(400, 20, 1000, 'exponential', pexp, qexp, phi = 0, theta = 0)


set.seed(3456)
plot_p_vals(400, 20, 1000, 'exponential', pexp, qexp, phi = 0.2, theta = 0)


set.seed(4567)
plot_p_vals(400, 20, 1000, 'exponential', pexp, qexp, phi = 0.4, theta = 0)