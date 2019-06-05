#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
source("graph functions.R")
source("depthMagnitude.R")
cat <- read.csv("pnw-cat-recent-processed.csv")
cat$date <- as.Date(cat$date)

# Define UI for application that draws a histogram
depthChunk <- function() {
  mainPanel(
    strong(h1("Tremors: A Reckoning with Data")),
    h3(p("What we're looking at here is a distribution of earthquake data over a\n
      smoothed and fitted line. Each red dot represnets a single earthquake event\n
      across varying depths and magnitudes. With this graph, a larger depth indicates\n
      earthquakes that were detected deeper into the Earth. The shaded area represents,\n
      to a 99% confidence interval, an extremely strong likelihood that earthquakes with any\n
      specific depth will have a magnitude within that shaded area. As you can see, the spread\n
      of the confidence interval is based off of similar data occurences; the accuracy increases\n
      the more repetition there is! This graph clealry displays that, the farther down into the Earth\n
      the quake is detected, the less predictable it is to ascertain the magnitude of the tremor!")
    )
  )
}

timeChunk <- function() {
  mainPanel(
    strong(h1("Whose fault is it anyways?")),
    h3(p("What we're looking at here is a distribution of earthquake data over a\n
      smoothed and fitted line. Each red dot represnets a single earthquake event\n
      across varying depths and magnitudes. With this graph, a larger depth indicates\n
      earthquakes that were detected deeper into the Earth. The shaded area represents,\n
      to a 99% confidence interval, an extremely strong likelihood that earthquakes with any\n
      specific depth will have a magnitude within that shaded area. As you can see, the spread\n
      of the confidence interval is based off of similar data occurences; the accuracy increases\n
      the more repetition there is! This graph clealry displays that, the farther down into the Earth\n
      the quake is detected, the less predictable it is to ascertain the magnitude of the tremor!")
    )
  )
}

ui <- navbarPage(theme = shinytheme("superhero"),
                 "Earthquake Visualizations",
                 tabPanel("Depth vs. Magnitude", fluidPage(
                   
                   # Application title
                   titlePanel(strong("Depth vs. Magnitude")),
                   
                   # Sidebar range for lat, lon, and date
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("lat", label = h3("Latitude Range"), min = 42, 
                                   max = 49, value = c(42, 49)),
                       sliderInput("lon", label = h3("Slider Range"), min = -125, 
                                   max = -116.5, value = c(-125, -116.5)),
                       dateRangeInput("date", label = h3("Date Range"), min = "1970-01-01",
                                      max = "2012-01-01", start = "1970-01-01", end = "2012-01-01")
                       ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("dep.mag")
                     )
                   ),
                   depthChunk()
                 )),
                 tabPanel("Earthquakes Over Time", fluidPage(
                   
                   # Application title
                   titlePanel("Earthquakes Over Time"),
                   
                   # Sidebar range for lat, lon, and date
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("lat2", label = h3("Latitude Range"), min = 42, 
                                   max = 49, value = c(42, 49)),
                       sliderInput("lon2", label = h3("Longitude Range"), min = -125, 
                                   max = -116.5, value = c(-125, -116.5)),
                       dateRangeInput("date2", label = h3("Date Range"), min = "1970-01-01",
                                   max = "2012-01-01", start = "1970-01-01", end = "2012-01-01"),
                       sliderInput("mag", label = h3("Magnitude Range"), min = 2.5,
                                   max = 10, value = c(2.5, 10))
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("time.series")
                     )
                   ),
                   timeChunk()
                 )),
                 tabPanel("Earthquakes in Time and Space", fluidPage(
                   
                   # Application title
                   titlePanel("Earthquakes in Time and Space"),
                   
                   # Sidebar range for lat, lon, and date
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("lat3", label = h3("Latitude Range"), min = 42, 
                                   max = 49, value = c(42, 49)),
                       sliderInput("lon3", label = h3("Longitude Range"), min = -125, 
                                   max = -116.5, value = c(-125, -116.5)),
                       dateRangeInput("date3", label = h3("Date Range"), min = "1970-01-01",
                                      max = "2012-01-01", start = "1970-01-01", end = "2012-01-01"),
                       sliderInput("mag2", label = h3("Magnitude Range"), min = 2.5,
                                   max = 10, value = c(2.5, 10))
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("mag.plot"),
                       leafletOutput("mag.plot.leaf")
                     )
                   )
                 ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$dep.mag <- renderPlot({
     data <- cat
     data <- data[data$lat >= input$lat[1] & data$lat <= input$lat[2], ]
     data <- data[data$lon >= input$lon[1] & data$lon <= input$lon[2], ]
     data <- data[data$date >= input$date[1] & data$date <= input$date[2], ]
     depthMagnitude(data)
   })
   
   output$time.series <- renderPlot({
     data <- cat
     data <- data[data$lat >= input$lat2[1] & data$lat <= input$lat2[2], ]
     data <- data[data$lon >= input$lon2[1] & data$lon <= input$lon2[2], ]
     data <- data[data$date >= input$date2[1] & data$date <= input$date2[2], ]
     data <- data[data$mag >= input$mag[1] & data$mag <= input$mag[2], ]
     time.series(data)
   })
   
   output$mag.plot <- renderPlot({
     data <- cat
     data <- data[data$lat >= input$lat3[1] & data$lat <= input$lat3[2], ]
     data <- data[data$lon >= input$lon3[1] & data$lon <= input$lon3[2], ]
     data <- data[data$date >= input$date3[1] & data$date <= input$date3[2], ]
     data <- data[data$mag >= input$mag2[1] & data$mag <= input$mag2[2], ]
     mag.plot(data)
   })
   
   output$mag.plot.leaf <- renderLeaflet({
     data <- cat
     data <- data[data$lat > input$lat3[1] & data$lat < input$lat3[2], ]
     data <- data[data$lon > input$lon3[1] & data$lon < input$lon3[2], ]
     data <- data[data$date > input$date3[1] & data$date < input$date3[2], ]
     data <- data[data$mag > input$mag2[1] & data$mag < input$mag2[2], ]
     mag.plot.leaf(data)
   })
}



# Run the application 
shinyApp(ui = ui, server = server)

