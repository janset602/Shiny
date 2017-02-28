#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library( shiny )
library( NCStats )
library( varhandle )
library( ggplot2 )
library( dplyr )

setwd( "E:/")

mo<- c("Jul" , "Aug" , "Sept" , "Oct" , "Nov" , "Dec" , "Jan" , "Feb" ,
        "Mar" , "Arp" , "May" , "Jun" )

getc <- function( m , cvec , last ) {
  f <- m[( cvec[1] - 1) %% 12 + 1]
  for ( i in 2:length( cvec )) {
    f <- c( f , m[( cvec[i] %% 12 ) + 1])
  }
  return( f )
}

df <- read.csv( "ETest.csv" )
totaltest <- df
totaltest$Month <- totaltest$Month + (( as.numeric( totaltest$Year ) - 1) * 12 )

# Define UI for application that draws a histogram

ui <- fluidPage(
  fluidRow(
  sidebarPanel( sliderInput("Inp", "Gas Slider", 1 , max = 
  max( totaltest$Month ), value = c( 1 , max( totaltest$Month ))),
  checkboxInput( "togtot" , "Show Total" , value = FALSE ),
  radioButtons( "type" , label = "Graph" , choices = c( "Gas" , "Electric" ,
  "Cost of Gas" , "Cost of Electric") , selected = "Gas" ),
  selectInput("build" , label = "Building" , multiple = TRUE ,  
  choice = levels( df$Building ) , selected = "Ponzio Campus Center " )),
  mainPanel( plotOutput( "MainPlot"))
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$MainPlot <- renderPlot({
    
    totaltest <- filterD( df , Building %in% input$build & Meter.Numbers != 6332524 )

    totaltest$Month <- totaltest$Month + (( as.numeric( totaltest$Year ) - 1) * 12 )
    
    if ( input$type[1] == "Gas" | input$type[1] == "Cost of Gas" )
      totaltest <- filterD( totaltest , Electric.Gas == "G" )
    
    else if ( input$type[1] == "Electric" | input$type[1] == "Cost of Electric" )
      totaltest <- filterD( totaltest , Electric.Gas == "E" )
    
    if ( input$type[1] == "Cost of Gas" | input$type[1] == "Cost of Electric") 
      totaltest$var <- totaltest$Dollar
    
    else 
      totaltest$var <- totaltest$kwh.ccf
    
    tempd <- filterD( totaltest , totaltest$Month >= input$Inp[1] &
    totaltest$Month <= input$Inp[2])
    
    
    f <- group_by(tempd ,  Month ) %>% summarise( sum = sum( var ))
    
    graph1 <- ggplot( data = tempd , aes( x = Month , y = var , color = Building )) +
    geom_line() + geom_point() + 
    scale_x_continuous( breaks = seq( input$Inp[1] , input$Inp[2] , by = 6 ) ,
    label = getc( mo , seq( input$Inp[1] , input$Inp[2] , by = 6 ))) +
    scale_y_continuous( name = input$type )
    
    if ( input$togtot == TRUE ) {
      graph1 <- graph1 + geom_line( data = f , aes( y = sum , color = "Total")) +
      geom_point( data = f , aes( y = sum , color = "Total"))
    }
    graph1
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

