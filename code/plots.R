setwd("../code")
source("functions.R")

df <- readRDS("../data/pvals.RDS") %>% 
#  filter(phi == -0.2 | phi == 0 | phi == 0.2, n == 400) %>% 
  mutate(pval = as.numeric(pval))

normal <- df %>% filter(dist == 'normal')
plot_p_vals(normal, 'normal.pdf')

gamma <- df %>% filter(dist == 'gamma')
plot_p_vals(gamma, 'gamma.pdf')


df <- readRDS("../data/rejection_rates.RDS") %>% filter(alpha == 0.05)
plot_rr(df, 'rr.pdf')
