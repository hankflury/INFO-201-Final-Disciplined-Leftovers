library(dplyr)
library(ggplot2)

#Creates the depth vs magnitude graph for the earthquake viz app
#Utilizes geom point and geom smooth to draw the line on top of the individual data points
depthMagnitude <- function(earthquakes) {
  trimmedQuakes <-  earthquakes %>% select(lat:mag)
  depthMagCorrelate <- trimmedQuakes %>% ggplot() +
                        geom_point(aes(depth, mag), position = "jitter", color = "red", size = 1) + #Red points for visibility
                        scale_color_continuous() +
                        geom_smooth(aes(depth, mag), size = 1.5, color = "purple", level = .99) + #Use a 99 confidence interval
                        theme_minimal() +
                        labs(x = "Depth (Metres)", y = "Magnitude (Richter)",
                             title = paste0("Depth vs. Magnitude", " (n = ", nrow(trimmedQuakes),")"), #To show the current count based on the parameters
                             subtitle = 
                             "Shaded area represents a 99% Conidence Bound")
  print(depthMagCorrelate)
}