source('functions.R')
my_data <- readRDS('../data/blk2004_results_10000.RDS')

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

my_freq <- my_data %>% 
  group_by(my_n, dist, tau) %>% 
  summarise(freq = mean(blk > sqrt(n)))

gg.f <- ggplot(my_freq, aes(x = tau, y = freq)) +
  geom_point() +
  facet_grid(rows = vars(my_n), cols = vars(dist), labeller = label_parsed) +
  xlab(TeX("\\tau")) +
  ylab(TeX('Frequency of Selected Block Size Greater than \\sqrt{n}')) +
  scale_x_continuous(breaks=c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75))
ggsave(filename = 'large_block.pdf', plot = gg.f, 
       path = "../manuscript/figures", height = 3, width = 6)


my_data$large_block <- ifelse(my_data$blk > sqrt(my_data$n), 'T', 'F')
table(my_data$large_block)




)




  




normal_null <- normal %>% filter(truth == 'null')

ggplot(data = normal_null, mapping = aes(sample = p)) +
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

