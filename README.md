
# Uber Data Analysis 🚘

In this project I am going to analyse the data of Uber to see their pattern according to month, day and time. I will also use visualization which I will make using ggplot2 and shinny to see the pattern of their data. 
 


## Library
    1.  tidyverse
    2.  tidyr
    3.  lubridate
    4.  ggplot2
    5.  scales
    6.  data.table
    7.  ggthemes
    8.  dplyr
    9.  DT
    10. leaflet
    11. leaflet.extras
    12. shiny
    13. rsconnect

## R Code

    #for reading data faster
    df_april <- fread('uber-raw-data-apr14.csv', data.table = F) #read april excel
    df_may <- fread('uber-raw-data-may14.csv', data.table = F) #read may excel
    df_june <- fread('uber-raw-data-jun14.csv', data.table = F) #read june excel
    df_july <- fread('uber-raw-data-jul14.csv', data.table = F) #read july excel
    df_aug <- fread('uber-raw-data-aug14.csv', data.table = F) #read august excel
    df_sep <- fread('uber-raw-data-sep14.csv', data.table = F) #read september excel

    #Pivot table to show Trips by the hour
    trip_hour <- df_merged%>%
    group_by(Hour)%>%
    dplyr::summarise(Total=n())

    #Graph representation of Trips Every Hour
    ggplot(trip_hour,aes(Hour,Total))+
    geom_bar(stat = "identity",fill="pink",color="gray")+
    ggtitle("Trips by the Hour")+
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels=comma)+xlab("Hour")+ylab("Total Trips")

    #Heat map Bases and Day of Week
    trip_base_day <- df_merged %>%
    group_by(Base, dayofweek) %>%
    dplyr::summarize(Total = n())

    #For converting table to csv file
    trip_hour <- df_merged%>%
    group_by(Hour)%>%
    dplyr::summarise(Total=n())
    fwrite(trip_hour, file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip Per Hour.csv")


## Shinny Code

    #read the data that we converted from df to csv
    df_base_day <- read.csv("Trip By Base and Day.csv")
    df_base_month <- read.csv("Trip By Base and Month.csv")
    df_day_hour <- read.csv("Trip By Day and Hour.csv")
    df_dayofweek_month <- read.csv("Trip by DayofWeek And Month.csv")
    df_hour_month <- read.csv("Trip By Hour And Month.csv")
    df_month_day <- read.csv("Trip By Month and Day.csv")
    df_month <- read.csv("Trip by Month.csv")
    df_hour <- read.csv("Trip Per Hour.csv")

    #Bar chart using if else condition
    output$bar_chart <- renderPlot({
    #using if and else statement as per users selection
    #if the user selects base and day data then it display x and y depending on the table value
    if (input$data_select == "df_base_day") {
    ggplot(selected_data(), aes(x = dayofweek, y = Total, fill = Base)) +
    geom_bar(stat = "identity") +
    labs(title = "Interactive Bar Chart", x = "Day of Week", y = "Total") +
    theme_minimal()
    }
    #if the user selects base and month data then it display x and y depending on the table value
    else if (input$data_select == "df_base_month") {
    ggplot(selected_data(), aes(x = Month, y = Total, fill = Base)) +
    geom_bar(stat = "identity") +
    labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
    theme_minimal()
    }
## Geo Spatial leaflet
    #if the user selects map then we display the map
    else if (input$data_select == "map") {
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%
            addTiles() %>%
            addMarkers(lng = -87.6298, lat = 41.8781, popup = "Chicago")
      })
    }
## Prediction ride model
    #Prediction to know which month has the highest number of people use uber after or at 6 pm. 
    ggplot(trip_hour_month, aes(Month, Total)) +
    geom_point(
    data = filter(trip_hour_month, rank(Hour) >= 18),
    size = 4, color = "red" )+
    geom_point(aes(colour = Hour))        
## Links

[github](https://github.com/stutishrestha21/UberProject)

[shinnyApp](http://stutishrestha21.shinyapps.io/ShinnyUber)


