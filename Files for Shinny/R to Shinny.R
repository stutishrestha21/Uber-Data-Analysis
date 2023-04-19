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


column_names<-colnames(data_df1) #for input selections

#Define ui to draw a histogram

ui <- fluidPage(
  # App title ----
  titlePanel(title = "March Madness 2023 Dataset"),
  fluidRow(
    column(2,
           selectInput('X', 'Choose Seed',column_names,column_names[2]),
           selectInput('Y', 'Choose Kenpom Adjusted Efficiency',column_names,column_names[6]),
           selectInput('Z', 'Choose Barttorvik Adjusted Efficiency',column_names,column_names[10]),
           selectInput('Splitby', 'Split By', column_names,column_names[3])
    ),
    column(4,plotOutput('plotMarchMadness')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
)


server<-function(input,output){
  
  output$plotMarchMadness <- renderPlot({
    
    ggplot(data_df, aes_string(x=input$X, y= input$Y, z= input$Z))+
      geom_point()+
      geom_smooth()
  })
  output$table_01<-DT::renderDataTable(data_df[,c(input$X,input$Y,input$Z, input$Splitby)],options = list(pageLength = 4))
  
}

shinyApp(ui=ui, server=server)