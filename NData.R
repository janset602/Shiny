library( NCStats )
library( varhandle )
library( ggplot2 )
library( dplyr )
#df1<- filterD( df , df$Electric.Gas == "E" & df$Year == "2005-2006" )

#ggplot( data = df1 , aes( x = Month , y = kwh.ccf , 
#     color = Building )) + geom_line( na.rm = TRUE ) +
#       scale_x_continuous( name = "Month" ,
#                breaks = seq( 1 , 12 , by = 1 ) , label = mo )
setwd( "E:/")


mo<- c("Jul" , "Aug" , "Sept" , "Oct" , "Nov" , "Dec" , "Jan" , "Feb" ,
        "Mar" , "Arp" , "May" , "Jun" )

df <- read.csv( "ETest.csv" )

t <- matrix( df$Dollar , df$Year)

t <- group_by(df ,  Month , Year ) %>% 
  summarise( sum = sum(Dollar , na.rm = TRUE ))

ggplot( data = t , aes( x = Month , y = sum , color = Year )) + 
  geom_line( na.rm = TRUE ) +  scale_x_continuous( name = "Month" ,
     breaks = seq( 1 , 12 , by = 1 ) , label = mo )
