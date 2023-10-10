source('functions.R')

set.seed(0123)
plot_p_vals(200, 1, 1000, 'normal', pnorm, qnorm, c(8,8), 0, 0)

set.seed(1234)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.4, 0)

set.seed(2345)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.2, 0)

set.seed(3456)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.2, 0)

set.seed(4567)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.4, 0)

set.seed(1234)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.4, 0)

set.seed(2345)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.2, 0)

set.seed(3456)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.2, 0)

set.seed(4567)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.4, 0)