#import necessary packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)
library(ggthemes)
library(dplyr)
library(DT)
library(leaflet)
library(leaflet.extras)


#clear the environment
rm(list=ls())

#setting the working directly in the folder
setwd("~/Desktop/DATA/Data 332/UberProject/Files for Analysis")

#reading the data using fread for faster processing
df_april <- fread('uber-raw-data-apr14.csv', data.table = F) #read april excel
df_may <- fread('uber-raw-data-may14.csv', data.table = F) #read may excel
df_june <- fread('uber-raw-data-jun14.csv', data.table = F) #read june excel
df_july <- fread('uber-raw-data-jul14.csv', data.table = F) #read july excel
df_aug <- fread('uber-raw-data-aug14.csv', data.table = F) #read august excel
df_sep <- fread('uber-raw-data-sep14.csv', data.table = F) #read september excel


#combing all the data in one table
df_merged <-rbind(df_april, df_may, df_june, df_july, df_aug,df_sep) %>%
  rename("Date.Time" = "Date/Time") %>% #rename date so that date and time could be seperated
  mutate(Date.Time = as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%S"))
  

#seperated day, date, month, hour, minute and second for futher analysis
df_merged <- df_merged %>%
  mutate(Day = factor(day(Date.Time)))%>%
  mutate(Month = factor(month(Date.Time)))%>%
  mutate(dayofweek = factor(wday(Date.Time)))%>%
  mutate(Hour = factor(hour(Date.Time))) %>%
  mutate(Minute = factor(minute(Date.Time))) %>%
  mutate(Second = factor(second(Date.Time)))


#1. Pivot table to show Trips by the hour
trip_hour <- df_merged%>%
  group_by(Hour)%>%
  dplyr::summarise(Total=n())

#3. Graph representation of Trips Every Hour
ggplot(trip_hour,aes(Hour,Total))+
  geom_bar(stat = "identity",fill="pink",color="gray")+
  ggtitle("Trips by the Hour")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels=comma)+xlab("Hour")+ylab("Total Trips")

#2.Trips by Hour and Month
trip_hour_month<- df_merged%>%
  group_by(Month, Hour)%>%
  dplyr::summarise(Total=n())

#Graph Representation 
ggplot(trip_hour_month,aes(Hour, Total, fill = Month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by Hour and Month")+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = comma)+xlab("Day of Week")+ylab("Total Trips")+
  scale_fill_manual(values = c("bisque4", "antiquewhite3", "cornsilk4", "darkolivegreen", "cadetblue4", "burlywood4"))


#5.Table of trips taken during every day of the month
trip_dayofweek_Month <- df_merged%>%
  group_by(Day)%>%
  dplyr::summarise(Total=n())

#4.Graph Representation
ggplot(trip_dayofweek_Month,aes(x = Day,y= Total))+
  geom_point(size = 2, color= "deeppink3")+
  labs(title = "Trips taken every day", x= "Day of week", y = "Total Trip")

#6.Trips by month
trip_month <- df_merged%>%
  group_by(Month)%>%
  dplyr::summarise(Total=n())

#Graph Represenataion
ggplot(trip_month,aes(Month, Total))+
  geom_bar(stat = "identity", fill= "cyan4", color="peachpuff3")+
  ggtitle("Trips by the Month")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = comma)+xlab("Month")+ylab("Total Trips")

#6.Trips by Day and Month (bar chart with each day of the week, x axis as the month)
trip_dayofweek_Month <- df_merged%>%
  group_by(dayofweek, Month)%>%
  dplyr::summarise(Total=n())

#Graph Representation
ggplot(trip_dayofweek_Month, aes(Month, Total, fill= dayofweek))+
  geom_bar(stat = "identity",aes(fill = dayofweek), position = "dodge")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = c("coral4", "seagreen", "goldenrod", "bisque2", "gray", "darkkhaki", "aquamarine4"))


#7.Trips by Bases and Month (Base is the X axis and Month is your label)
trip_base_month <- df_merged%>%
  group_by(Base, Month)%>%
  dplyr::summarise(Total=n())

#Graph Representation
ggplot(trip_base_month, aes(Base, Total, fill= Month))+
  geom_bar(stat = "identity",aes(fill = Month), position = "dodge")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = c("orchid2", "orange4", "plum3", "seashell4", "skyblue4", "aquamarine4"))

#8.Heat map that displays by hour and day
trip_day_hour <- df_merged %>%
  group_by(Day, Hour) %>%
  dplyr::summarize(Total = n())

#HeatMap
ggplot(trip_day_hour, aes(Day, Hour, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "pink", high = "red") +
  ggtitle("Heat Map by Hour and Day")

#9.Heat map by month and day
trip_month_day <- df_merged %>%
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

#11.Heat map Bases and Day of Week
trip_base_day <- df_merged %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

#Heatmap
ggplot(trip_base_day, aes(Base, dayofweek, fill= Total))+
  geom_tile(color="white")+
  scale_fill_gradient(low="deeppink", high="deeppink4")+
  ggtitle("Heat Map by Month and Day")

#------Convert the Pivot table to excel for Shinny manipulations------------

#1. Pivot table to show Trips by the hour
trip_hour <- df_merged%>%
  group_by(Hour)%>%
  dplyr::summarise(Total=n())
fwrite(trip_hour, file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip Per Hour.csv")

#2. Trips by Hour and Month
trip_hour_month<- df_merged%>%
  group_by(Month, Hour)%>%
  dplyr::summarise(Total=n())
fwrite(trip_hour_month,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip By Hour And Month.csv")

#3. Table of trips taken during every day of the month
trip_dayofweek_Month <- df_merged%>%
  group_by(dayofweek, Month)%>%
  dplyr::summarise(Total=n())
fwrite(trip_dayofweek_Month,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip by DayofWeek And Month.csv")

#4. Trips by month
trip_month <- df_merged%>%
  group_by(Month)%>%
  dplyr::summarise(Total=n())

fwrite(trip_month,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip by Month.csv")

#5. Trips by Bases and Month 
trip_base_month <- df_merged%>%
  group_by(Base, Month)%>%
  dplyr::summarise(Total=n())
fwrite(trip_base_month,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip By Base and Month.csv")

#6. Trips by day and hour
trip_day_hour <- df_merged %>%
  group_by(Day, Hour) %>%
  dplyr::summarize(Total = n())
fwrite(trip_day_hour,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip By Day and Hour.csv")

#7. Trips by month and day
trip_month_day <- df_merged %>%
  group_by(Month, Day) %>%
  dplyr::summarize(Total = n())
fwrite(trip_month_day,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip By Month and Day.csv")



#8. Trips by Bases and Day of Week
trip_base_day <- df_merged %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())
fwrite(trip_base_day,file = "~/Desktop/DATA/Data 332/UberProject/Files for Shinny/Trip By Base and Day.csv")



#---------------------------Prediction Model--------------------------------
#Prediction to know which month has the highest number of people use uber after or at 6 pm. 
ggplot(trip_hour_month, aes(Month, Total)) +
  geom_point(
    data = filter(trip_hour_month, rank(Hour) >= 18),
    size = 4, color = "red" )+
  geom_point(aes(colour = Hour))






