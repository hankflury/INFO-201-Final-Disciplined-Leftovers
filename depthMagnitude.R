library(dplyr)
library(ggplot2)
quakeData <- read.csv("pnw-cat-recent-processed.csv", stringsAsFactors = FALSE)

depthMagnitude <- function(earthquakes) {
  trimmedQuakes <-  earthquakes %>% select(lat:mag)
  depthMagCorrelate <- trimmedQuakes %>% ggplot() +
                        geom_point(aes(depth, mag), position = "jitter", color = "red", size = 1) +
                        geom_smooth(aes(depth, mag), size = 2, color = "purple", level = .99) +
                        theme_classic() +
                        labs(x = "Depth (Metres)", y = "Magnitude (Richter)",
                             title = "Depth vs. Magnitude")
  print(depthMagCorrelate)
}

depthMagnitude(quakeData)
