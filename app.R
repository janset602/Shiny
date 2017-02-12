#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(NCStats)

# Define UI for application that draws a histogram

ui <- fluidPage(
  sliderInput("num", "Random Numbers", 1, 100, 30),#defines input$num
         checkboxGroupInput("checkGroup", 
                            label = h3("Checkbox group"), 
                            choices = list("Choice 1" = 1, 
                                           "Choice 2" = 2, "Choice 3" = 3),
                            selected = 1),#numericInput("inp" , label = h3("Input") , value = 1 , min = 1 , max = 2),
  textOutput("box" ), plotOutput(("Plot2")), plotOutput("distPlot")
)

# Define server logic required to draw a histogram
rd1 <-matrix( floor(runif(200,0,100)), 100 , 2) #outside server fcn so that input does 
# not re rand info
rd<-faithful[,2]
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
    
      num <- seq(min(rd), max(rd), length.out = input$num + 1)
      
      
      # draw the histogram with the specified number of bins
        hist(rd, breaks = num, col = 'black', border = 'white')
        
   })
   
   f.f <- function(x) {
     if ( length(input$checkGroup) == 0 )
       return(FALSE)
     for( i in 1:length(input$checkGroup)) {
       if( input$checkGroup[i] == x )
         return(TRUE)
     }
     return(FALSE)
   }
   output$Plot2 <- renderPlot({
     if ( f.f(1) == TRUE)
       plot( rd1[,1] , rd1[,1], col = "red",type = 'l')
     if ( f.f(2) == TRUE)
      lines( rd1[,1]+10 , rd1[,1] , col="blue" )
     if ( f.f(3) == TRUE )
      lines( rd1[,1] - 10 , rd1[,1] , col="green")
     
   })
   
   
   
   output$box<-renderText({input$checkGroup})
}
# Run the application 
shinyApp(ui = ui, server = server)

