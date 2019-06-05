library(ggplot2)
library(leaflet)
library(dplyr)
mag.plot <- function(data) {
  plot <- ggplot(data) +
    geom_point(aes(x = lon, y =lat, size = mag), shape = 1, col = "purple") +
    labs(x = "Longitude", y = "Latitude", size = "Magnitude") +
    ggtitle(paste0("Magnitude of Earthquakes (n=", nrow(data), ")")) +
    theme_bw()
  plot
}

mag.plot.leaf <- function(data) {
  leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(lng = data$lon, lat = data$lat, radius = (data$mag - 2.5) * 10, color = "purple", fill = "white")
}

time.series <- function(data) {
  plot <- ggplot(data) +
    geom_density(aes(x = date), fill = "blue", alpha = .5) +
    ggtitle(paste0("Earthquake Time Series (n=", nrow(data), ")")) +
    labs(x = "Date", y = "Density") +
    theme_bw()
  print(plot)
}
