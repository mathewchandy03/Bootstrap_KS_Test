library(xtable)
df <- readRDS("../data/sim_rejection_rates.RDS") %>% 
  mutate(rr_lb = round(as.numeric(rr_lb), 3),
         rr = round(as.numeric(rr), 3),
         rr_ub = round(as.numeric(rr_ub), 3),
         alpha = format(round(as.numeric(alpha), 2), nsmall = 2))

df$tau <- factor(case_when(df$phi == "-0.9238795" ~ "$\\tau = -0.75$",
                           df$phi == "-0.7071068" ~ "$\\tau = -0.5$",
                           df$phi == "-0.3826834" ~ "$\\tau = -0.25$",
                           df$phi == "0" ~ "$\\tau = 0$",
                           df$phi == "0.9238795" ~ "$\\tau = 0.75$",
                           df$phi == "0.7071068" ~ "$\\tau = 0.5$",
                           df$phi == "0.3826834" ~ "$\\tau = 0.25$"),
                 levels = c("$\\tau = -0.75$",
                            "$\\tau = -0.5$",
                            "$\\tau = -0.25$",
                            "$\\tau = 0$",
                            "$\\tau = 0.25$",
                            "$\\tau = 0.5$",
                            "$\\tau = 0.75$"))

library(reshape2)
df_norm <- df %>% filter(dist == "normal") %>% 
  dplyr::select(n, alpha, rr, tau) %>% 
  dcast(n + alpha ~ tau, value.var = "rr") %>% 
  rename(`$n$` = n, `$\\alpha$` = alpha)

write(print(xtable(df_norm, 
                   caption = "Rejection rates for test that $N(8, 8)$ is indeed
                   normally distributed for
                   different values of AR(1) coefficient and for different 
                   significance levels.",
                   label = "table:rr_norm", digits = 3), caption.placement = "top",
            sanitize.text.function=function(x){x}, include.rownames = FALSE), 
      file = "../manuscript/tables/rr_norm.tex")  

df_gamma <- df %>% filter(dist == "gamma") %>% 
  dplyr::select(n, alpha, rr, tau) %>% 
  dcast(n + alpha ~ tau, value.var = "rr") %>% 
  rename(`$n$` = n, `$\\alpha$` = alpha)

write(print(xtable(df_gamma, 
                   caption = "Rejection rates for test that $\\Gamma(8, 8)$ is 
                   indeed
                   gamma-distributed for
                   different values of AR(1) coefficient and for different 
                   significance levels.",
                   label = "table:rr_gamma", digits = 3), caption.placement = "top",
            sanitize.text.function=function(x){x}, include.rownames = FALSE), 
      file = "../manuscript/tables/rr_gamma.tex") 
