setwd("../code")
source("functions.R")

for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      plot_p_vals(paste("sim_", n, "_", dist, "_", phi, "_0", sep = ""))
    }
  }
}
