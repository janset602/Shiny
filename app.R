#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library( NCStats )
library( varhandle )
library( ggplot2 )
library( dplyr )
setwd( "E:/")

mo<- c("Jul" , "Aug" , "Sept" , "Oct" , "Nov" , "Dec" , "Jan" , "Feb" ,
        "Mar" , "Arp" , "May" , "Jun" )

getc <- function( m , lower , upper ) {
  f <- m[(lower-1)%%12 + 1]
  for ( i in lower:( upper - 1)) {
   f <- c( f , m[( i %% 12 ) + 1 ])
  }
  return( f )
}

df <- read.csv( "ETest.csv" )

totaltest <- filterD( df , df$Electric.Gas == "G" &
!is.na( kwh.ccf ))

totaltest$Month <- totaltest$Month + (( as.numeric( totaltest$Year ) - 1) * 12 )

f <- group_by(totaltest ,  Month ) %>% summarise( sum = sum( kwh.ccf ))

# Define UI for application that draws a histogram

ui <- fluidPage(
  sliderInput("Inp", "Gas Slider", 1, max = 
  max( totaltest$Month ), value = c( 1 , max( totaltest$Month ))),#defines input$num
  checkboxGroupInput("build" , label = "Building" , 
  choice = c("Science Center" , "ELLC") , inline = FALSE ) , 
  plotOutput( "Gas") 
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$Gas <- renderPlot({
    tempd <- filterD( totaltest , totaltest$Month >= input$Inp[1] &
    totaltest$Month <= input$Inp[2])
    ggplot( data = tempd , aes( x = Month , y = kwh.ccf , color = Building )) +
    geom_line() + geom_point() + 
    scale_x_continuous( breaks = seq( input$Inp[1] , input$Inp[2] , by = 1 ) ,
    label = getc( mo , input$Inp[1] , input$Inp[2]))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

