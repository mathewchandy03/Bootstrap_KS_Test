library(xtable)
df <- readRDS("../data/rejection_rates.RDS")
df <- df %>% filter(phi == 0.2, n == 800) %>% select(-R) %>% 
  rename(distribution = dist, `lower bound` = rr_lb, `rejection rate` = rr, 
         `upper bound` = rr_ub)
write(print(xtable(df)), file = "../manuscript/tables/rr.tex")      
