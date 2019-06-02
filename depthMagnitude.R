library(dplyr)
library(ggplot2)
quakeData <- read.csv("pnw-cat-recent-processed.csv", stringsAsFactors = FALSE)

depthMagnitude <- function(earthquakes) {
  trimmedQuakes <-  earthquakes %>% select(lat:mag)
  depthMagCorrelate <- trimmedQuakes %>% ggplot() +
                        geom_point(aes(depth, mag), position = "jitter", color = "yellow", size = 1) +
                        scale_color_continuous() +
                        geom_smooth(aes(depth, mag), size = 1.5, color = "purple", level = .99) +
                        theme_minimal() +
                        labs(x = "Depth (Metres)", y = "Magnitude (Richter)",
                             title = "Depth vs. Magnitude", subtitle = 
                             "Shaded area represents a 99% Conidence Interval")
  print(depthMagCorrelate)
}

depthMagnitude(quakeData)
