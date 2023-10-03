source('functions.R')
library(tidyverse)
library(qqplotr)


set.seed(0123)
plot_p_vals(200, 20, 1000, 'normal', pnorm, -0.4, 0)


set.seed(1234)
plot_p_vals(200, 20, 1000, 'normal', pnorm, -0.2, 0)


set.seed(2345)
plot_p_vals(200, 20, 1000, 'normal', pnorm, 0, 0)


set.seed(3456)
plot_p_vals(200, 20, 1000, 'normal', pnorm, 0.2, 0)


set.seed(4567)
plot_p_vals(200, 20, 1000, 'normal', pnorm, 0.4, 0)

set.seed(0123)
plot_p_vals(400, 20, 1000, 'normal', pnorm, -0.4, 0)


set.seed(1234)
plot_p_vals(400, 20, 1000, 'normal', pnorm, -0.2, 0)


set.seed(2345)
plot_p_vals(400, 20, 1000, 'normal', pnorm, 0, 0)


set.seed(3456)
plot_p_vals(400, 20, 1000, 'normal', pnorm, 0.2, 0)


set.seed(4567)
plot_p_vals(400, 20, 1000, 'normal', pnorm, 0.4, 0)


