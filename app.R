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

setwd( "E:/Capstone")
load( "tempws.RData" )
df <- read.csv( "ETest.csv" )
df <- df[-6014,]
#Filter out bad meters
temperature <- read.csv( "atemp.csv" )
studs<-read.csv("Students.csv" )

df.var <- c( "Gas","Electricity","Dollar E","Dollar G","Year","Month","Month_OT")

mplot <- filterD( df , Meter.Numbers != 6332524 , Meter.Numbers != 6254626  )
mplot$Month <- convMonth( mplot$Month , mplot$Year )

mplot.years <- mplot$Year
#parameters for MainPlot
#take the mean of var in month through 11 years
all_build <- df 
all_build$Month <- getc( mo , all_build$Month )
all_build$Month <- ordermonth( all_build$Month )
all_build$Month <- as.numeric( all_build$Month )
#Sets January as first Month and allows ggplot
#to draw lines between points

ui <- fluidPage(
  fluidRow( 
    sidebarPanel( sliderInput("mslide", "Month", 1 , max = 
    max( mplot$Month ), value = c( 1 , max( mplot$Month ))),
    
    selectInput("build" , label = "Building" , multiple = TRUE ,  
    choice = levels( df$Building ) , selected = "Ponzio Campus Center " ),
    
    checkboxInput( "total" , "Show Total" , value = FALSE ),
    
    radioButtons( "type" , label = "Y Variable" , choices = c( "Gas" , "Electric" ,
    "Cost of Gas" , "Cost of Electric") , selected = "Gas" , inline = TRUE ),
    
    textOutput( "Years" ),
    
    width = 4 ),
    
    mainPanel( plotOutput( "MainPlot") , width = 8 )
    ),
  
  fluidRow(
    sidebarPanel( selectInput( "vbyv1" , label = "Building X" ,
    choices = levels( df$Building ) , selected = "Science Center" ),
    selectInput( "vbyv2" , label = "Building Y" , choices = levels(df$Building),
    selected = "Ponzio Campus Center " ),
    radioButtons( "vbyvvar" , label = "Gas or Electric" ,
    choices = c( "Gas" , "Electric") , selected = "Electric" ),
    width = 4 ),
    
    mainPanel( plotOutput( "vbyv" ) , width = 7 )
  )#,
  
  #fluidRow(
  #  sidebarPanel( radioButtons("userx",label="X-Variable",
  #                choices = df.var ,selected = "Month_OT", inline = TRUE),
  #                radioButtons("usery",label=
  #                "Y-Variable",choices = df.var,selected = "Gas",inline=TRUE),
  #                radioButtons("userg",label="Grouping",
  #                choices = c("Building","Year","Month"),selected = "Building"),inline=TRUE)
  #)
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$MainPlot <- renderPlot({
    
    mplot <- filterD( mplot , Building %in% input$build )
    
    if ( input$type == "Gas" | input$type == "Cost of Gas" )
      mplot <- filterD( mplot , Electric.Gas == "G" )
    
    else if ( input$type == "Electric" | input$type == "Cost of Electric" )
      mplot <- filterD( mplot , Electric.Gas == "E" )
    
    if ( input$type == "Cost of Gas" | input$type == "Cost of Electric") 
      mplot$var <- mplot$Dollar
    
    else 
      mplot$var <- mplot$kwh.ccf
    
    mplotadj <- filterD( mplot , mplot$Month >= input$mslide[1] &
    mplot$Month <= input$mslide[2])
    
    mplot.years <- mplotadj$Year
    
    mplotadj <- mplotadj %>% group_by( Month , Building ) %>%
      summarise( var = sum( var ))
    
    diff = input$mslide[2] - input$mslide[1] + 1
    diff <- floor( diff / 24 ) + 1
    incvec <- func( input$mslide[1] , input$mslide[2] , diff )
    
    
    f <- group_by( mplotadj ,  Month ) %>% summarise( sum = sum( var ,na.rm = TRUE ))
    
    graph1 <- ggplot( data = mplotadj , aes( x = Month , y = var , color = Building )) +
    geom_line() + geom_point()
    
    
    if ( input$total == TRUE ) {
      graph1 <- graph1 + geom_line( data = f , aes( y = sum , color = "Total")) +
      geom_point( data = f , aes( y = sum , color = "Total"))
    }
    
    graph1 + scale_y_continuous( name = input$type ) + 
    scale_x_continuous( breaks = incvec ,
    labels = getc( mo , incvec )) + ggtitle( "Interactive Plot" ) +
      theme(plot.title = element_text(hjust = .5))
  })
  
  output$vbyv <- renderPlot({
    if ( input$vbyvvar == "Gas" )
      parm <- "G"
    else
      parm <- "E"
    
    inp1 <- filterD( df , Electric.Gas == parm , Building == input$vbyv1 ) %>%
      group_by( Year , Month ) %>% summarise( kwh.ccf = sum( kwh.ccf , na.rm = TRUE ))
    inp2 <- filterD( df , Electric.Gas == parm , Building == input$vbyv2 ) %>%
      group_by( Year , Month ) %>% summarise( kwh.ccf = sum( kwh.ccf , na.rm = TRUE ))

    ndf <- data.frame( inp1$kwh.ccf , inp2$kwh.ccf , inp1$Month )
    ndf <- ndf[-131,]
    ndf <- ndf[-4,]
    
    ndf$inp1.Month <- getc( mo , ndf$inp1.Month )
    ndf$inp1.Month <- ordermonth( ndf$inp1.Month )
    #Bad data^^^
    ggplot( data = ndf , aes( x = inp1.kwh.ccf , y = inp2.kwh.ccf , color = inp1.Month )) +
      geom_point() + scale_x_continuous( name = input$vbyv1 ) +
      scale_y_continuous( name = input$vbyv2 ) +
      scale_colour_hue( "Month" , h.start = 180 )
  })
  
  output$Years <- renderText({
    ylvl <- levels( mplot$Year )
    c( "Fiscal Year " , ylvl[ceiling(input$mslide[1]/11)] ,
        " to " ,ylvl[ceiling(input$mslide[2]/12)] )
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

