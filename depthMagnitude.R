library(dplyr)
library(ggplot2)

depthMagnitude <- function(earthquakes) {
  trimmedQuakes <-  earthquakes %>% select(lat:mag)
  depthMagCorrelate <- trimmedQuakes %>% ggplot() +
                        geom_point(aes(depth, mag), position = "jitter", color = "red", size = 1) +
                        scale_color_continuous() +
                        geom_smooth(aes(depth, mag), size = 1.5, color = "purple", level = .99) +
                        theme_minimal() +
                        labs(x = "Depth (Metres)", y = "Magnitude (Richter)",
                             title = paste0("Depth vs. Magnitude", " (n = ", nrow(trimmedQuakes),")"), 
                             subtitle = 
                             "Shaded area represents a 99% Conidence Bound")
  print(depthMagCorrelate)
}