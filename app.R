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

introChunk <- function() {
  mainPanel(
    h2("'What I am looking at here?' Quakes for days.."),
    h3(p("The dataset we're working with is a compilation of recorded earthquake statistics from the\n
      Pacific Northwest Seismic Network (PSSN). This dataset ranges from 1969 to 2018, with information\n
      collected from over 1000 recording stations. While the data begins in 1969, the equipment used to\n
      collect the information was somewhat archaic until 1970. That system  was based upn individuals calling\n
      in to report earthquakes and then state on a scale of 1-10 how severe it was. From 1970 onwards, actual\n
      recording equipment was utilized to collect the earthquake information."))
  )
}
depthChunk <- function() {
  mainPanel(
    strong(h1("Tremors: A Reckoning with Data")),
    h3(p("What we're looking at here is a distribution of earthquake data over a\n
      smoothed and fitted line. Each red dot represnets a single earthquake event\n
      across varying depths and magnitudes. With this graph, a larger depth indicates\n
      earthquakes that were detected deeper into the Earth. The shaded area represents,\n
      to a 99% confidence interval, an extremely strong likelihood that earthquakes with any\n
      specific depth will have a magnitude within that shaded area.The curve basically predicts\n
      the magnitude (y) given the depth (x), along with a 'margin of errror'. As you can see, the spread\n
      of the confidence interval is based off of similar data occurences; the accuracy increases\n
      the more repetition there is! This graph clealry displays that, the farther down into the Earth\n
      the quake is detected, the less predictable it is to ascertain the magnitude of the tremor!")
    )
  )
}

timeChunk <- function() {
  mainPanel(
    h1("Whose fault is it anyways?"),
    h3(p("Here we are seeing a fairly straightforward representation of earthquake frequency\n
         over time in the Pacific Northwest! This graph displays weighted averages for the frequency\n
         of earthquakes over time: the density on the left (y) literally repersents a proportion of\n 
         all the earthquakes that occurred. Since this is a 'filled' graph that represents density proportions,\n
         if you took the integral of the entire graph you would get a total of 1! Now that was maybe too much\n
         calculus for your liking, just understand that the increase in density does represent an increase\n
         in the frequency of earthquakes!")),
    strong(em("Fun Fact: The two large spikes in 1980 and 2004 respectively are when Mt. St. Helens erupted!\n 
        While the 1980 eruption most definitely changed the shape of the mountain itself, by 2004 there were\n
        more 'detected' earthquakes due to the additional sensors installed since 1980!"))
  )
}

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("united"),
                 "'The Most Dangerous Quake'",
                 
                 tabPanel(h2("Earthquakes in Time and Space"), fluidPage(
                   
                   h1("There's always a bigger quake..."),
                   
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
                       plotOutput("mag.plot", click = "plot_click"),
                       verbatimTextOutput("info"),
                       leafletOutput("mag.plot.leaf")
                    )
                   ),
                   introChunk()
                 )),
                 
                 tabPanel("Depth vs. Magnitude", fluidPage(
                   
                   # Application title
                   titlePanel(h1(strong("I'm quaking!"))),
                   
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
                   titlePanel(h1(strong("It's not my fault!"))),
                   
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

                 tabPanel("Contributors", fluidPage(
                   
                   # Application title
                   titlePanel("Contributors"),
                   mainPanel(
                     img("Pratibha Kharel, Computer Science", src = 'pratibha.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Hank Flury, Statistics", src='hank.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Abdul Mohomed, Informatics", src='abdul.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Shri Sharma, Informatics ", src='shri.png', width = 150, height = 200),
                     verbatimTextOutput("")
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
   output$info <- renderText({
     xy_str <- function(e) { 
       if(is.null(e)) return("Click on a point above\n")
       paste0("The longitude and latitude is (", round(e$x, 0),",", round(e$y, 0), ")")
     }
     paste0(xy_str(input$plot_click))
     
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

