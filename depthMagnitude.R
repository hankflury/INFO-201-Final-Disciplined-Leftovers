library(dplyr)
library(ggplot2)
quakeData <- read.csv("pnw-cat-recent-processed.csv", stringsAsFactors = FALSE)

depthMagnitude <- function(earthquakes) {
  trimmedQuakes <-  earthquakes %>% select(lat:mag)
  depthMagCorrelate <- trimmedQuakes %>% ggplot() +
                        geom_smooth(aes(mag, depth)) +
                        theme_light()
  print(depthMagCorrelate)
}

depthMagnitude(quakeData)