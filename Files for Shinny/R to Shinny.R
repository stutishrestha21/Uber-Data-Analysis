library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(rsconnect)
library(readxl)

rm(list = ls())

setwd("~/Desktop/DATA/Data 332/UberProject/Files for Shinny")
#Read data
data_df1 <- read_excel("trip_by_base_day.xlsx")
data_df2 <- read_excel("trip_by_base_month.xlsx")
data_df3<- read_excel("trip_by_day_hour.xlsx")
data_df4<- read_excel("trip_by_dayofWeek_month.xlsx")
data_df5 <- read_excel("trip_by_hour_month.xlsx")
data_df6 <- read_excel("trip_by_month_day.xlsx")
data_df7 <- read_excel("trip_by_month.xlsx")
data_df8 <- read_excel("trip_per_hour.xlsx")

column_names<-colnames(data_df1) #for input selections of first df table

#Define ui to draw a histogram

ui <- fluidPage(
  # App title ----
  titlePanel(title = "Trip According to Base and Day of Week"),
  fluidRow(
    column(2,
           selectInput('X', 'Choose Base',column_names,column_names[1]),
           selectInput('Y', 'Choose Day of Week',column_names,column_names[2]),
    ),
    column(4,plotOutput('plotTrips')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
)


server<-function(input,output){
  
  output$plotTrips <- renderPlot({
    
    ggplot(data_df1, aes_string(x=input$X, y= input$Y))+
      geom_point()+
      geom_smooth()
  })
  output$table_01<-DT::renderDataTable({
    data_df1%>%
      select(input$X, input$Y)%>%
      slice(1:4)},
    options= list(pageLength=))
  
}

shinyApp(ui=ui, server=server)
