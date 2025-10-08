source('functions.R')
my_data <- readRDS('../data/results_10000_C_n.RDS')
my_data_50 <- readRDS('../data/results_10000_C_n_50.RDS')

# my_data <- rbind(my_data, my_data_50)


my_data$tau <- case_when(my_data$phi == -0.9238795 ~ -0.75,
                         my_data$phi == -0.7071068 ~ -0.5,
                         my_data$phi == -0.3826834 ~ -0.25,
                         my_data$phi == 0 ~ 0,
                         my_data$phi == 0.9238795 ~ 0.75,
                         my_data$phi == 0.7071068 ~ 0.5,
                         my_data$phi == 0.3826834 ~ 0.25)

my_data$dist <- factor(case_when(my_data$true_dist == "normal" ~ "N(8,8)",
                                 my_data$true_dist == "gamma" ~ "Gamma(8,1)"))

my_data$my_n <- factor(case_when(my_data$n == 100 ~ "n == 100",
                                 my_data$n == 200 ~ "n == 200",
                                 my_data$n == 400 ~ "n == 400",
                                 my_data$n == 800 ~ "n == 800"))


gg.f <- ggplot(data = my_data %>% 
                 filter(truth == 'null', true_dist == 'normal'), 
               mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  facet_grid(rows = vars(n), cols = vars(tau),
             labeller = label_parsed) +
  labs(x = "Theoretical Cumulative Distribution", 
       y = "Empirical Cumulative Distribution") +
  coord_fixed() +
  theme(strip.text.x = element_text(size = 8))
ggsave(filename = 'normal_C_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 4, width = 6)

gg.f <- ggplot(data = my_data %>% 
                 filter(truth == 'null', true_dist == 'gamma'), 
               mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  facet_grid(rows = vars(n), cols = vars(tau),
             labeller = label_parsed) +
  labs(x = "Theoretical Cumulative Distribution", 
       y = "Empirical Cumulative Distribution") +
  coord_fixed() +
  theme(strip.text.x = element_text(size = 8))
ggsave(filename = 'gamma_C_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 4, width = 6)

alt_data <- my_data %>% 
  filter(truth == 'alt', abs(tau) <= 0.5) %>%
  mutate(n = factor(n), dist = factor(dist, levels = c("N(8,8)", "Gamma(8,1)"))) %>%
  group_by(dist, n, tau) %>%
  summarise(rr = mean(p < 0.05))


gg.f <- ggplot(data = alt_data, mapping = aes(x = tau, y = as.numeric(rr), 
                                              color = n, 
                                              linetype = n)) +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(dist), labeller = label_parsed) +
  theme(
    legend.position = "bottom") +
  labs(x = latex2exp::TeX("$\\tau$"), y = latex2exp::TeX("Rejection Rate")) +
  coord_fixed(ylim = c(0, 1), xlim = c(-0.6, 0.6))
ggsave(filename = 'rr_C_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 3, width = 6)




my_data <- readRDS('../data/results_10000_K_n.RDS')


my_data$tau <- case_when(my_data$phi == -0.9238795 ~ -0.75,
                         my_data$phi == -0.7071068 ~ -0.5,
                         my_data$phi == -0.3826834 ~ -0.25,
                         my_data$phi == 0 ~ 0,
                         my_data$phi == 0.9238795 ~ 0.75,
                         my_data$phi == 0.7071068 ~ 0.5,
                         my_data$phi == 0.3826834 ~ 0.25)

my_data$dist <- factor(case_when(my_data$true_dist == "normal" ~ "N(8,8)",
                                 my_data$true_dist == "gamma" ~ "Gamma(8,1)"))

my_data$my_n <- factor(case_when(my_data$n == 100 ~ "n == 100",
                                 my_data$n == 200 ~ "n == 200",
                                 my_data$n == 400 ~ "n == 400",
                                 my_data$n == 800 ~ "n == 800"))


gg.f <- ggplot(data = my_data %>% 
                 filter(truth == 'null', true_dist == 'normal'), 
               mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  facet_grid(rows = vars(n), cols = vars(tau),
             labeller = label_parsed) +
  labs(x = "Theoretical Cumulative Distribution", 
       y = "Empirical Cumulative Distribution") +
  coord_fixed() +
  theme(strip.text.x = element_text(size = 8))
ggsave(filename = 'normal_K_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 4, width = 6)

gg.f <- ggplot(data = my_data %>% 
                 filter(truth == 'null', true_dist == 'gamma'), 
               mapping = aes(sample = p)) +
  scale_x_continuous(breaks=c(0, 1)) +
  scale_y_continuous(breaks=c(0, 1)) + 
  stat_pp_band(distribution = "unif") +
  stat_pp_line() +
  stat_pp_point(distribution = "unif", cex = .1) +
  facet_grid(rows = vars(n), cols = vars(tau),
             labeller = label_parsed) +
  labs(x = "Theoretical Cumulative Distribution", 
       y = "Empirical Cumulative Distribution") +
  coord_fixed() +
  theme(strip.text.x = element_text(size = 8))
ggsave(filename = 'gamma_K_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 4, width = 6)

alt_data <- my_data %>% 
  filter(truth == 'alt', abs(tau) <= 0.5) %>%
  mutate(n = factor(n), dist = factor(dist, levels = c("N(8,8)", "Gamma(8,1)"))) %>%
  group_by(dist, n, tau) %>%
  summarise(rr = mean(p < 0.05))


gg.f <- ggplot(data = alt_data, mapping = aes(x = tau, y = as.numeric(rr), 
                                              color = n, 
                                              linetype = n)) +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(dist), labeller = label_parsed) +
  theme(
    legend.position = "bottom") +
  labs(x = latex2exp::TeX("$\\tau$"), y = latex2exp::TeX("Rejection Rate")) +
  coord_fixed(ylim = c(0, 1), xlim = c(-0.6, 0.6))
ggsave(filename = 'rr_K_n.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 3, width = 6)