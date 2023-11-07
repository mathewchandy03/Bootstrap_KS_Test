setwd("../data")

#precipitation <- read.csv("precipitation.txt", header = TRUE, sep = ' ')

microsoft <- read.csv('microsoft.csv')

file_names <- list.files(pattern = "^sim")
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    for (dist in c("normal", "gamma")) {
      list <- file_names[which(grepl(paste("_", n, "_", dist, "_", phi, "_0_", sep = ''), 
                                     file_names))]
      tosave <- c()
      for (file in list) {
        tosave <- c(tosave, readRDS(file)$p)
      }
      saveRDS(tosave, file = paste("sim_", n, "_", dist, "_",
                                   phi, '_', 
                                   '0', ".RDS", sep = ''))
    }
  }
}