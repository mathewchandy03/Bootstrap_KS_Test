library(xtable)
df <- readRDS("../data/rejection_rates.RDS") %>% 
  mutate(rr_lb = round(as.numeric(rr_lb), 3),
         rr = round(as.numeric(rr), 3),
         rr_ub = round(as.numeric(rr_ub), 3)) %>% 
  filter(alpha == 0.05)
df_100 <- df %>% 
  filter(n == 100) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_100, 
                   caption = "Rejection rates for test when n = 100 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_100"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_100.tex")  

df_200 <- df %>% 
  filter(n == 200) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_200,
                   caption = "Rejection rates for test when n = 200 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_200"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_200.tex")

df_400 <- df %>% 
  filter(n == 400) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_400,
                   caption = "Rejection rates for test when n = 400 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_400"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_400.tex")

df_800 <- df %>% 
  filter(n == 800) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_800,
                   caption = "Rejection rates for test when n = 800 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_800"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_800.tex")

df_1600 <- df %>% 
  filter(n == 1600) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_1600,
                   caption = "Rejection rates for test when n = 1600 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_1600"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_1600.tex")

df_3200 <- df %>% 
  filter(n == 3200) %>% 
  select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df_3200,
                   caption = "Rejection rates for test when n = 3200 for 
                   different values of AR(1) coefficient and for different 
                   significance levels. Lower and upper bounds are also 
                   included.",
                   label = "table:rr_3200"), caption.placement = "top"), 
      file = "../manuscript/tables/rr_3200.tex")

