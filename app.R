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
   
   # Application title
   titlePanel("Rand"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("num",
                     "Random Numbers:",
                     min = 1,
                     max = 100,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
rd1 <-matrix( floor(runif(200,0,100)), 100 , 2) #outside server fcn so that input does 
# not re rand info
rd<-faithful[,2]
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
    
      num <- seq(min(rd), max(rd), length.out = input$num + 1)
      plot( rd1[,1] , rd1[,2] )
      
      
      # draw the histogram with the specified number of bins
      #hist(rd, breaks = num, col = 'black', border = 'white')

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

