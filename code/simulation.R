source('functions.R')

# Independent Data Basic Bootstrap
set.seed(0123)
plot_p_vals(200, 1, 1000, 'normal', pnorm, qnorm, c(8,8), 0, 0)

# Dependent Data Basic Bootstrap n = 200 Normal(8,8)
set.seed(1234)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.4, 0)

set.seed(2345)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.2, 0)

set.seed(3456)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0, 0)

set.seed(4567)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.2, 0)

set.seed(5678)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.4, 0)

# Dependent Data Basic Bootstrap n = 200 Gamma(8,1)
set.seed(1234)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.4, 0)

set.seed(2345)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.2, 0)

set.seed(3456)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0, 0)

set.seed(4567)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.2, 0)

set.seed(5678)
plot_p_vals(200, ceiling(200^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.4, 0)

# Dependent Data Basic Bootstrap n = 400 Normal(8,8)
set.seed(1234)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.4, 0)

set.seed(2345)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), -0.2, 0)

set.seed(3456)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0, 0)

set.seed(4567)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.2, 0)

set.seed(5678)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'normal', pnorm, qnorm, 
            c(8,8), 0.4, 0)

# Dependent Data Basic Bootstrap n = 400 Gamma(8,1)
set.seed(1234)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.4, 0)

set.seed(2345)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), -0.2, 0)

set.seed(3456)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0, 0)

set.seed(4567)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.2, 0)

set.seed(5678)
plot_p_vals(400, ceiling(400^(1/3)), 1000, 'gamma', pgamma, qgamma, 
            c(8,1), 0.4, 0)