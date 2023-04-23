#libraries
library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(rsconnect)
library(DT)
library(leaflet)
library(leaflet.extras)

getwd()
#Read data that we fwrote in uber project
df_base_day <- read.csv("Trip By Base and Day.csv")
df_base_month <- read.csv("Trip By Base and Month.csv")
df_day_hour <- read.csv("Trip By Day and Hour.csv")
df_dayofweek_month <- read.csv("Trip by DayofWeek And Month.csv")
df_hour_month <- read.csv("Trip By Hour And Month.csv")
df_month_day <- read.csv("Trip By Month and Day.csv")
df_month <- read.csv("Trip by Month.csv")
df_hour <- read.csv("Trip Per Hour.csv")

# Title and selection part
ui <- fluidPage(
  titlePanel("Uber Trip Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_select", "Select Data", choices = c("df_base_day", "df_base_month", "df_day_hour", "df_dayofweek_month", "df_hour_month", "df_month_day", "df_month", "df_hour","prediction","map"), selected = "df_base_day")
    ),
    mainPanel(
      plotOutput("bar_chart"),
      DT::dataTableOutput("table"),
      leafletOutput("map")
    )
  )
)

# Making our shinny interactive
server <- function(input, output) {
 
  
  # Reactive data based on selected data
  selected_data <- reactive({
    switch(input$data_select,
           "df_base_day" = df_base_day,
           "df_base_month" = df_base_month,
           "df_day_hour" = df_day_hour,
           "df_dayofweek_month" = df_dayofweek_month,
           "df_hour_month" = df_hour_month,
           "df_month_day" = df_month_day,
           "df_month" = df_month,
           "df_hour" = df_hour,
           "prediction" = df_hour_month)
  })
  
  # Render bar chart
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
    #if the user selects day and hour data then it display x and y depending on the table value
    else if (input$data_select == "df_day_hour") {
      ggplot(selected_data(), aes(x = Hour, y = Total, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #if the user selects day of week and month data then it display x and y depending on the table value
    else if (input$data_select == "df_dayofweek_month") {
      ggplot(selected_data(), aes(x = dayofweek, y = Total, fill = Month)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "dayofweek", y = "Total") +
        theme_minimal()
    }
    #if the user selects base and day data then it display x and y depending on the table value
    else if (input$data_select == "df_hour_month") {
      ggplot(selected_data(), aes(x = Hour, y = Total, fill = Month)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #if the user selects month and day data then it display x and y depending on the table value
    else if (input$data_select == "df_month_day") {
      ggplot(selected_data(), aes(x = Month, y = Total, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
        theme_minimal()
    }
    #if the user selects month then it display x and y depending on the table value
    else if (input$data_select == "df_month") {
      ggplot(selected_data(), aes(x = Month, y = Total)) +
        geom_bar(stat = "identity", fill= "red") +
        labs(title = "Interactive Bar Chart", x = "Month", y = "Total") +
        theme_minimal()
    }
    #if the user selects hour then it display x and y depending on the table value
    else if (input$data_select == "df_hour") {
      ggplot(selected_data(), aes(x = Hour, y = Total)) +
        geom_bar(stat = "identity", fill="forestgreen") +
        labs(title = "Interactive Bar Chart", x = "Hour", y = "Total") +
        theme_minimal()
    }
    #Prediction to know which month has the highest number of people use uber after or at 6 pm.
    else if (input$data_select == "prediction") {
      ggplot(df_hour_month, aes(x = Month, y = Total, color = factor(Hour > 18))) +
        geom_point(size = 4) +
        scale_color_manual(values = c("black", "red"), guide = FALSE) +
        labs(color = "Hour > 18")
    }
    #map to show chicago data
    else if (input$data_select == "map") {
      output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%
            addTiles() %>%
            addMarkers(lng = -87.6298, lat = 41.8781, popup = "Chicago")
      })
    }
    
  })
  
  # This gives the table and rows till 10
  output$table <- DT::renderDataTable({
    DT::datatable(selected_data(), options = list(pageLength = 5))
  })
  
  
  
}
# Run the Shiny app
shinyApp(ui = ui, server = server)






