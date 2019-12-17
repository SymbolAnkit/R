setwd("C:\\Users\\Desktop\\New folder\\TimeSeries")
library(stringr)
library(dplyr)
train <- read.csv("Train_SU63ISt.csv")
head(train)
sapply(train,class)
train$Datetime1 <- strptime(train$Datetime, format = "%d-%m-%Y %H:%M")
train$Date <- as.Date(substr(train$Datetime1,1,10),"%Y-%m-%d")
train$Time <- substr(train$Datetime1,11,19)
train$Hours <- strftime(train$Datetime1,"%H")
train$Year <-  strftime(train$Datetime1,"%Y")
train$Month <- strftime(train$Datetime1,"%m")
train$Day <- strftime(train$Datetime1,"%d")
train$YearMonth <- paste(train$Year,train$Month,sep = "")
train$YearMonthDay <- paste(train$YearMonth,train$Day,sep = "")
train$YearMonthDayHour <- paste(train$YearMonthDay,train$Hours,sep = "")

sapply(train,class)

# Monthly Data
dataY1 <- subset(train,select = c(YearMonth,Count))
dataY2 <- dataY1 %>% group_by(YearMonth) %>%
  summarise(Count = sum(Count))
dataY2 <- as.data.frame(dataY2)
head(dataY2)
tail(dataY2)
ti_seriesY <- ts(data = dataY2$Count,start = c(2012,8),frequency = 12 )
plot(ti_seriesY)

# Daliy Data
dataD1 <- subset(train,select = c(YearMonthDay,Count))

dataD2 <- dataD1 %>% group_by(YearMonthDay) %>%
  summarise(Count = sum(Count))
dataD2 <- as.data.frame(dataD2)
head(dataD2)
tail(dataD2)

ti_seriesD <- ts(data = dataD2$Count,start = c(2012,8), frequency = 365 )
plot(ti_seriesD)

# Hourly Data

dataH1 <- subset(train,seelct = c(YearMonthDayHour,Count))
dataH2 <- dataH1 %>% group_by(YearMonthDayHour) %>%
  summarise(Count = sum(Count))
strt <- "2012-08-25 00:00:00"
en <- "2014-09-25 23:00:00"

ti_seriesH <- ts(data = dataH2$Count,start = c(2012,8,25,00,00) ,end = c(2014,9,25,23,00), frequency = 24 )
plot(ti_seriesH)

