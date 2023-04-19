library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(lubridate)
library(shiny)    
library(ggplot2)
library(scales)

rm(list=ls())

setwd("~/Desktop/DATA/Data 332/Project 3")

April_table <- read.csv('uber-raw-data-apr14.csv') #read april excel
May_table <- read.csv('uber-raw-data-may14.csv') #read may excel
June_table <- read.csv('uber-raw-data-jun14.csv') #read june excel
July_table <- read.csv('uber-raw-data-jul14.csv') #read july excel
August_table <- read.csv('uber-raw-data-aug14.csv') #read august excel
Sept_table <- read.csv('uber-raw-data-sep14.csv') #read september excel

combined_table <-rbind(April_table, May_table, June_table, July_table, August_table,Sept_table)%>% #Binding all months data
  rename(Date_Time = Date.Time) #rename date so that date and time could be seperated

#formatting date
combined_table$Date <- as.POSIXct(paste(combined_table$Date_Time), format = "%m/%d/%Y")

#convert time in 24 hour format
combined_table$Time<- format (as.POSIXct(strptime(combined_table$Date_Time,"%m/%d/%Y %H:%M:%S")), format = "%H:%M:%S")

#seperated day, date, month and year
combined_table$Day <- factor(day(combined_table$Date))
combined_table$Month <- factor(month(combined_table$Date, label = TRUE))
combined_table$Year <- factor(year(combined_table$Date))
combined_table$dayofweek <- factor(wday(combined_table$Date, label = TRUE))

#seperated hour, minutes and seconds
combined_table$Hour <- factor(hour(hms(combined_table$Time)))
combined_table$Minute <- factor(minute(hms(combined_table$Time)))
combined_table$Second <- factor(second(hms(combined_table$Time)))

#stacked and grouped graphs when asking for months
#1. Pivot table to show Trips by the hour
trip_hour <- combined_table%>%
  group_by(Hour)%>%
  dplyr::summarise(Total=n())

# 3. Graph representation of Trips Every Hour
ggplot(trip_hour,aes(Hour,Total))+
  geom_bar(stat = "identity",fill="pink",color="gray")+
  ggtitle("Trips by the Hour")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels=comma)+xlab("Hour")+ylab("Total Trips")

#2. Trips by Hour and Month
trip_hour_month<- combined_table%>%
  group_by(Month, Hour)%>%
  dplyr::summarise(Total=n())

#Graph Representation(Chart)
ggplot(trip_hour_month,aes(Hour, Total, fill = Month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by Hour and Month")+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = comma)+xlab("Day of Week")+ylab("Total Trips")+
  scale_fill_manual(values = c("bisque4", "antiquewhite3", "cornsilk4", "darkolivegreen", "cadetblue4", "burlywood4"))


#5. Table of trips taken during every day of the month
trip_dayofweek_Month <- combined_table%>%
  group_by(Day)%>%
  dplyr::summarise(Total=n())

#4. Graph Representation
ggplot(trip_dayofweek_Month,aes(x = Day,y= Total))+
  geom_point(size = 2, color= "deeppink3")+
  labs(title = "Trips taken every day", x= "Day of week", y = "Total Trip")

#6. Trips by month
trip_month <- combined_table%>%
  group_by(Month)%>%
  dplyr::summarise(Total=n())

#Graph Represenataion
ggplot(trip_month,aes(Month, Total))+
  geom_bar(stat = "identity", fill= "cyan4", color="peachpuff3")+
  ggtitle("Trips by the Month")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = comma)+xlab("Month")+ylab("Total Trips")

#6. Trips by Day and Month (bar chart with each day of the week, x axis as the month)
trip_dayofweek_Month <- combined_table%>%
  group_by(dayofweek, Month)%>%
  dplyr::summarise(Total=n())

#Graph Representation
ggplot(trip_dayofweek_Month, aes(Month, Total, fill= dayofweek))+
  geom_bar(stat = "identity",aes(fill = dayofweek), position = "dodge")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = c("coral4", "seagreen", "goldenrod", "bisque2", "gray", "darkkhaki", "aquamarine4"))


#7. Trips by Bases and Month (Base is the X axis and Month is your label)
trip_base_month <- combined_table%>%
  group_by(Base, Month)%>%
  dplyr::summarise(Total=n())

#Graph Representation
ggplot(trip_base_month, aes(Base, Total, fill= Month))+
  geom_bar(stat = "identity",aes(fill = Month), position = "dodge")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = c("orchid2", "orange4", "plum3", "seashell4", "skyblue4", "aquamarine4"))

#8. Heat map that displays by hour and day
trip_day_hour <- combined_table %>%
  group_by(Day, Hour) %>%
  dplyr::summarize(Total = n())

#HeatMap
ggplot(trip_day_hour, aes(Day, Hour, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "pink", high = "red") +
  ggtitle("Heat Map by Hour and Day")

#9. Heat map by month and day
trip_month_day <- combined_table %>%
  group_by(Month, Day) %>%
  dplyr::summarize(Total = n())

#HeatMap
ggplot(trip_month_day, aes(Month, Day, fill= Total))+
  geom_tile(color="white")+
  scale_fill_gradient(low="rosybrown", high="rosybrown4")+
  ggtitle("Heat Map by Month and Day")

#10. Heat map by month and week
ggplot(trip_dayofweek_Month, aes(Month, dayofweek, fill= Total))+
  geom_tile(color="white")+
  scale_fill_gradient(low="palegreen", high="seagreen4")+
  ggtitle("Heat Map by Month and Week")

#11. Heat map Bases and Day of Week
trip_base_day <- combined_table %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(trip_base_day, aes(Base, dayofweek, fill= Total))+
  geom_tile(color="white")+
  scale_fill_gradient(low="deeppink", high="deeppink4")+
  ggtitle("Heat Map by Month and Day")




