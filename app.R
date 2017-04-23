library( shiny )
library( NCStats )
library( ggplot2 )
library( dplyr )

#Defining Useful Functions
#Using Month and Year to Convert a series of Years in Months to one long timeframe
convMonth <- function( month , years ) {
  month <- month + (( as.numeric( years ) - 1) * 12 )
  return( month )
}

#Wrapping Seq Function but including the Max value if not included
nseq <- function( min , max , inc ) {
  s <- seq( min , max , by = inc )
  if ( s[ length( s )] != max )
    s <- c( s , max )
  return( s )
}

#Converts numers to months given 1 == July
getc <- function( m , cvec ) {
  f <- m[( cvec[1] - 1) %% 12 + 1]
  for ( i in 2:length( cvec )) {
    f <- c( f , m[(( cvec[i] - 1 ) %% 12 ) + 1])
  }
  return( f )
}

#Factors months to January first
ordermonth <- function( months ) {
  nmonths <- factor( months , levels = c(
    "Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" , "Jul" ,
    "Aug" , "Sept" , "Oct" , "Nov" , "Dec"
  ))
  return(nmonths)
}

#Defining Month Vector for above Functions
GMonths <- c(
  "Jul","Aug","Sept","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"
)

#Change WD if ETest elsewhere
setwd( "E:/Capstone")
df <- read.csv( "ETest.csv" )
#Dollar = -6610.62 , kwh.ccf = -95640 
#Removed as assumed mistake
df <- df[-6014,]
#Main Plot Adjustment
#Full data set except two bad meters
mplot <- filterD( df , Meter.Numbers != 6332524 , Meter.Numbers != 6254626  )
#Convert Month Variable to full time line, to be manipulated by input 1-max data
mplot$Month <- convMonth( mplot$Month , mplot$Year )

#Defining X and Y axis choices for user plot
uplot.choices <- c(
  "Building","Month","Month_OT","Year","Energy","Dollar","Gas","Electricity","na"
)
#Grouping Choices
uplot.groups <- c(
  "Building","Month","Year","Electric.Gas","na"
)
#Filtering Weird Data
#I don't know why Dollar would be negative
uplot <- filterD( df , kwh.ccf >= 0 & Dollar >= 0 )

#Setting Variables to appropriate information
uplot$Energy <- uplot$kwh.ccf
uplot$Month_OT <- convMonth( uplot$Month , uplot$Year )
uplot$Month <- ordermonth( getc( GMonths , uplot$Month ))

#Define the Shiny Page User Interface
ui <- fluidPage(
  #Define Contents of first row for Main Plot
  fluidRow(
    #Define Side Panel of Main Plot
    #Slider Input goes from 1 to the Max Month in data frame
    sidebarPanel( sliderInput("mslide", "Month", 1 , max = 
    max( mplot$Month ), value = c( 1 , max( mplot$Month ))),
    
    #Define Input for Building to be read as Character Vector
    selectInput("build" , label = "Building" , multiple = TRUE ,  
    choice = levels( df$Building ) , selected = "Ponzio Campus Center " ),
    
    #Define Button for Total
    checkboxInput( "total" , "Show Total" , value = FALSE ),
    
    #Define Buttons for Y Axis Variable
    radioButtons( "type" , label = "Y Variable" , choices = c( "Gas" , "Electric" ,
    "Cost of Gas" , "Cost of Electric") , selected = "Gas" , inline = TRUE ),
    
    #Show Years covered
    textOutput( "Years" ),
    
    #Total Side Panel Width 
    width = 4 ),
    
    #Define Plot and Width of Main Plot
    mainPanel( plotOutput( "MainPlot") , width = 8 )
    ),
  
  #Second Row for Energy by Energy of Two Buildings Plot
  fluidRow(
    #Building Input Multiple = False only one building per Axis
    #Input for X Axis
    sidebarPanel( selectInput( "vbyv1" , label = "Building X" ,
    choices = levels( df$Building ) , selected = "Science Center" ),
    #Building Input for Y Axis
    selectInput( "vbyv2" , label = "Building Y" , choices = levels(df$Building),
    selected = "Ponzio Campus Center " ),
    #Select either Gas or Electricity
    radioButtons( "vbyvvar" , label = "Gas or Electric" ,
    choices = c( "Gas" , "Electric") , selected = "Electric" ),
    #Uses Same Width as All other Side Panels
    width = 4 ),
    
    #Defines Plot and Width for aesthetics
    mainPanel( plotOutput( "vbyv" ) , width = 7 )
  ),
  
  fluidRow(
    sidebarPanel(
      #X and Y axis choices same, grouping different
      radioButtons("userx","X Axis",uplot.choices,selected="Month_OT",inline=TRUE),
      radioButtons("usery","Y Axis",uplot.choices,selected = "Energy",inline=TRUE),
      radioButtons("userg","Grouping",uplot.groups,selected="Building",inline=TRUE)
    ),
    #Width undefined as plot changes
    mainPanel( plotOutput("userp"))
  )
)

#Defines Server with Input and Output defined in UI
server <- function(input, output) {
  #Outputs Main Plot
  output$MainPlot <- renderPlot({
    #Logic for Filtering Either Gas or Electricity
    if ( input$type == "Gas" | input$type == "Cost of Gas" )
      mplot <- filterD( mplot , Electric.Gas == "G" )
    else if ( input$type == "Electric" | input$type == "Cost of Electric" )
      mplot <- filterD( mplot , Electric.Gas == "E" )
    
    #Logic for Variable, either energy or money
    if ( input$type == "Cost of Gas" | input$type == "Cost of Electric") 
      mplot$var <- mplot$Dollar
    else 
      mplot$var <- mplot$kwh.ccf
    
    #Adjusting Main Plot data frame with specified inputs
    #input$mslide is a number vecter length 2 from slider
    mplotadj <- filterD( mplot , mplot$Month >= input$mslide[1] ,
                         mplot$Month <= input$mslide[2] , Building %in% input$build ) %>%
      #New data frame includes Month and Building
      #and the summation of inputted variable
      group_by( Month , Building ) %>%
      summarise( var = sum( var ))
    
    #Logic to adjust the sequence of breaks and labels
    #As difference decreases, step decreases, showing more breaks and labels
    #Lables and breaks use same vector
    diff = input$mslide[2] - input$mslide[1] + 1
    diff <- floor( diff / 24 ) + 1
    incvec <- nseq( input$mslide[1] , input$mslide[2] , diff )
    
    #Creates the Total data frame that summates all buildings included in mplotadj
    f <- group_by( mplotadj ,  Month ) %>% summarise( sum = sum( var , na.rm = TRUE ))
    
    #Creates graphical object of main plot grouping by Building, uses lines and points
    graph1 <- ggplot( data = mplotadj , aes( x = Month , y = var , color = Building )) +
    geom_line() + geom_point()
    
    #Logic to include total data frame to the graphical object
    if ( input$total == TRUE ) {
      graph1 <- graph1 + geom_line( data = f , aes( y = sum , color = "Total")) +
      geom_point( data = f , aes( y = sum , color = "Total"))
    }
    
    #Renders graphical object with appropriate labels
    graph1 + scale_y_continuous( name = input$type ) + 
    scale_x_continuous( breaks = incvec ,
    labels = getc( GMonths , incvec )) + ggtitle( "Interactive Plot" ) +
      theme(plot.title = element_text(hjust = .5))
  })
  
  #Output for Years in Main Plot
  output$Years <- renderText({
    ylvl <- levels( mplot$Year )
    #Logic of converting total months to years using sliders
    c( "Fiscal Year " , ylvl[ceiling(input$mslide[1]/11)] ,
        " to " ,ylvl[ceiling(input$mslide[2]/12)] )
  })
  
  output$vbyv <- renderPlot({
    #Logic for type of Energy 
    if ( input$vbyvvar == "Gas" )
      parm <- "G"
    else
      parm <- "E"
    
    #Creates two data frames from each input parameter
    inp1 <- filterD( df , Electric.Gas == parm , Building == input$vbyv1 ) %>%
      group_by( Year , Month ) %>% summarise( kwh.ccf = sum( kwh.ccf , na.rm = TRUE ))
    inp2 <- filterD( df , Electric.Gas == parm , Building == input$vbyv2 ) %>%
      group_by( Year , Month ) %>% summarise( kwh.ccf = sum( kwh.ccf , na.rm = TRUE ))

    #Creates data frame to be used by graph
    ndf <- data.frame( inp1$kwh.ccf , inp2$kwh.ccf , inp1$Month )
    
    #Removes data collection errors Bad Months
    ndf <- ndf[-131,]
    ndf <- ndf[-4,]
    
    #Converts Month to Factor for Legend
    ndf$inp1.Month <- ordermonth( getc( GMonths , ndf$inp1.Month ))
    
    #Graphical Object for 
    ggplot( data = ndf , aes( x = inp1.kwh.ccf , y = inp2.kwh.ccf , color = inp1.Month )) +
      geom_point() + scale_x_continuous( name = input$vbyv1 ) +
      scale_y_continuous( name = input$vbyv2 ) +
      scale_colour_hue( "Month" , h.start = 180 )
  })
  
  output$userp <- renderPlot({
    #Logic for filtering Gas or electricity
    #Sets adjusted df variable to corresponding name so that input can be used
    if ( input$usery == "Gas" | input$userx == "Gas" ) {
      uplot.adj <- filterD( uplot , Electric.Gas == "G" )
      uplot.adj$Gas <- uplot.adj$Energy
    }
    else if ( input$usery == "Electricity" | input$userx == "Electricity" ) {
      uplot.adj <- filterD( uplot , Electric.Gas == "E" )
      uplot.adj$Electricity <- uplot.adj$Energy
    }
    else
      uplot.adj <- uplot
    
    #Defines user.graph as a null graph in case inputs do not fit logic
    user.graph <- ggplot( data = uplot.adj )
    
    #Histogram
    #Also prevents x and y input as na and userx being na as no graphs are usefull
    #without an x axis Also prevents y axis based histogram. But that is silly
    if ( input$usery == "na" & input$userx != "na" ) {
      if ( !is.factor( uplot.adj[,input$userx])) {
        user.graph <- ggplot( data = uplot.adj , aes( x = uplot.adj[,input$userx])) +
          geom_histogram( binwidth = mean( uplot.adj[,input$userx]) / 2 ) +
          scale_y_continuous( name = "Frequency" ) +
          scale_x_continuous( name = input$userx )
        
        #Histogram looks better with fill aesthetic instead of color
        if ( input$userg != "na" )
          user.graph <- user.graph + aes( fill = uplot[,input$userg])
      }
    }
    #Normal two variable plot 
    #Only points no lines
    else if ( input$userx != "na" & input$usery != "na" ) {
      if ( !is.factor( uplot.adj[,input$userx]) | !is.factor( uplot.adj[,input$usery])) {
        user.graph <- ggplot( data = uplot.adj , aes( x = uplot.adj[,input$userx] ,
                                                      y = uplot.adj[,input$usery])) +
          geom_point()
        
        #Grouping Info and Label
        if ( input$userg != "na" )
          user.graph <- user.graph + aes( color = uplot.adj[,input$userg]) +
            scale_color_discrete( name = input$userg )
        
        #Logic for axis labels
        if ( is.factor( uplot.adj[,input$userx]))
          user.graph <- user.graph + scale_x_discrete( name = input$userx )
        else
          user.graph <- user.graph + scale_x_continuous( name = input$userx )
        
        if ( is.factor( uplot.adj[,input$usery]))
          user.graph <- user.graph + scale_y_discrete( name = input$usery )
        else
          user.graph <- user.graph + scale_y_continuous( name = input$usery )
      }
    }
    
    #If Month, start cold months with cold colors
    if ( input$userg == "Month" )
      user.graph <- user.graph + scale_color_hue( h.start = 180 , name = "Month" )
    
    user.graph
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

