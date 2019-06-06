library(ggplot2)
library(leaflet)
library(dplyr)

#Plots the magnitude of earthquakes on a blank graph by latitude and longitude
mag.plot <- function(data) {
  plot <- ggplot(data) +
    geom_point(aes(x = lon, y =lat, size = mag), shape = 1, col = "purple") +
    labs(x = "Longitude", y = "Latitude", size = "Magnitude") +
    ggtitle(paste0("Magnitude of Earthquakes (n=", nrow(data), ")")) + #To show counts based on parameters
    theme_bw()
  plot
}

#Plots the magnitude of earthquakes but as a leaflet by longitude and latitiude 
mag.plot.leaf <- function(data) {
  leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(lng = data$lon, lat = data$lat, radius = (data$mag - 2.5) * 10, color = "purple", fill = "white")
}

#Plots earthquake frequency over time as a geom desnity graph (so a 'filled' curve)
time.series <- function(data) {
  plot <- ggplot(data) +
    geom_density(aes(x = date), fill = "blue", alpha = .5) +
    ggtitle(paste0("Earthquake Time Series (n=", nrow(data), ")")) + #To show counts based on parameters
    labs(x = "Date", y = "Density") +
    theme_bw()
  print(plot)
}
