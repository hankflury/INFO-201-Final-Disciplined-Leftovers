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
    h3(p("The dataset we're working with is a compilation of recorded earthquake statistics from the\n",
      a("Pacific Northwest Seismic Network (PSSN)", href = "https://pnsn.org"), ". This dataset ranges from 1969 to 2018, with information\n
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
                 
                 tabPanel(h2("Back to Bedrock: What's that noise?"), fluidPage(
                          titlePanel("Why care about earthquake analysis?"),
                          mainPanel(
                            h1(p("Seismic activity is a constant and ongoing event that many can tend to overlook given all\n
                              that's happening in the world. The fact of the matter is, understanding the potential patterns\n
                              behind seisimc activity can enable us to better handle (possibly) catastrophic events!"),
                            h2("IF we can understanding the when and where, and to what extent in relation to other seismic events,\n
                               then just maybe we can determine the why!")),
                            h2("This application is designed to be an informative and interactive exploration into earthquake activity\n
                               within the Pacific Northwest! Today we aim to present:"),
                            h1(strong("-A visualization of earthquake acivity across time and space from 1970 to 2012")),
                            h1(strong("-An analysis of the possible correlation between depth of a seismic event and magnitude")),
                            h1(strong("-A visualiztion of earthquake frequency during the same time period of 1970 to 2012"))
                          ),
                          sidebarPanel(
                            img("Earthquake jokes :)", src='quakePun.jpg', width = 400, height = 400)
                          )
                 
                 )),
                 
                 tabPanel("Earthquakes in Time and Space", fluidPage(
                   
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
                       sliderInput("lon", label = h3("Longitude Range"), min = -125, 
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

                 tabPanel("Speaking to our faults", fluidPage(
                   
                   # Application title
                   titlePanel("Closing thoughts and Credits"),
                   mainPanel(
                     h1("Things to remember..."),
                     h2("1.Correlation does not equal causation! While we make claims about earthquake activity in the PNW, we are\n
                        severely limited in some areas but lack relevant data to make definitive claims!"),
                     h2("2.While it may seem that the frequency of earthquakes have increased over the past 40 odd some years,\n
                        it's important to realize that more accurate and distributed recording equipment has been utilized!"),
                     h2("3.Realize that we live nearby a (somtimes active) volcano! The frequency graph is a clear presentation\n
                        of the effect Mt. St. Helens can have on surrounding seismic activity!"),
                     h1("Contributors:"),
                     img("Pratibha Kharel, Computer Science", src = 'pratibha.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Hank Flury, Statistics", src='hank.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Abdul Mohamed, Political Science and Informatics", src='abdul.png', width = 150, height = 200),
                     verbatimTextOutput(""),
                     img("Shri Sharma, Informatics ", src='shri.png', width = 150, height = 200),
                     verbatimTextOutput("")
                   ),
                   sidebarPanel(
                     img("Earthquake jokes pt. 2 :)", src = 'quakeJoke2.png', width = 300, height = 300)
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

