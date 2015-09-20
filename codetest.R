getwd()
setwd("./GitHub/RepData_PeerAssessment1")

## Loading and preprocessing the data
#zipfile <- "activity.zip"
#unzip(zipfile)

csvfile <- "activity.csv"
df <- read.csv(csvfile, stringsAsFactors = FALSE)
df <- transform(df, date = as.Date(date))
summary(df$steps)

## What is mean total number of steps taken per day?
library(dplyr); library(ggplot2)

stepsPerDay <- df %>% group_by(date) %>% summarise( steps_per_day = sum(steps))
title = "Histogram of the total number of steps taken each day"
xlab = "Total number of steps taken each day"; ylab ="Frequency"
g <- ggplot(stepsPerDay, aes( steps_per_day ))
g + geom_histogram( binwidth = 5000 ) + labs(title=title, x=xlab, y=ylab)+theme_grey()

#hist(stepsPerDay$steps_per_day, main = "Histogram of the total number of steps taken each day", 
#     xlab = "The total number of steps taken each day")

mean_steps <- mean(stepsPerDay$steps_per_day, na.rm = TRUE)
median_steps <- median(stepsPerDay$steps_per_day, na.rm = TRUE)

## What is the average daily activity pattern?

meanSteps <- function(df){
        stepsIn5min <-  df%>% group_by( interval) %>% summarise( steps_per_interval= mean( steps, na.rm = TRUE ))
        #将internal编号格式化为四位整数
        stepsIn5min$time <- formatC(stepsIn5min$interval, width = 4, format = "d", flag = "0")
        #在小时和分钟之间插入：号
        stepsIn5min$time <- sub("([[:digit:]]{2,2})$", ":\\1", stepsIn5min$time)
        #转化为POSIXct时间格式，日期被赋值为当前日期
        stepsIn5min$time <- as.POSIXct( stepsIn5min$time, format="%H:%M")
        stepsIn5min
}

stepsIn5min <-  meanSteps( df )


title ="Time series of  the average number of steps taken\n in each 5-minute interval"
xlab = "Time intervals"; ylab = "Number of steps"
g <- ggplot(stepsIn5min, aes( interval, steps_per_interval ))
g + geom_line( size = 0.8 ) + labs(title=title, x=xlab, y=ylab)+theme_grey()

library(scales)
g <- ggplot(stepsIn5min, aes( time, steps_per_interval ))
mytimezone <- Sys.timezone()
g + geom_line( size = 0.8 ) + labs(title=title, x="time", y=ylab)+
        scale_x_datetime( labels= date_format("%H:%M", tz = mytimezone ))

maxsteps <- max(stepsIn5min$steps_per_interval)
intervalMaxSteps <- stepsIn5min$interval[which.max(stepsIn5min$steps_per_interval)]


## Imputing missing values

#the total number of missing values in the dataset
n_na <- sum(is.na(df$steps))

#filling in the missing values in the dataset with  
#the mean for corresponding 5-minute interval.

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdf <- df
stepsvec <- df$steps
datevec <- df$date
intervalvec <- df$interval
interval <- stepsIn5min$interval
for (i in interval) {
        newdf$steps[intervalvec == i & is.na(stepsvec)] <- 
                stepsIn5min$steps_per_interval[stepsIn5min$interval==i]
}

#Using auto recycling, we can impute the missing value.
filledsteps <- ifelse(is.na(stepsvec), stepsIn5min$steps_per_interval, stepsvec)
all(filledsteps==newdf$steps)

#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. 

stepsPerDay2 <- newdf %>% group_by(date) %>% summarise( steps_per_day=sum(steps))

title = "Histogram of the total number of steps taken each day" 
xlab = "Total number of steps taken each day"; ylab = "Frequency"
g <- ggplot(stepsPerDay2, aes( steps_per_day ))
g + geom_histogram( binwidth = 5000 ) + labs(title=title, x=xlab, y=ylab)+theme_grey()

mean_steps2 <- mean(stepsPerDay2$steps_per_day)
median_steps2 <- median(stepsPerDay2$steps_per_day)


#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
diff_mean_steps <- mean_steps2-mean_steps
diff_median_steps <- median_steps2-median_steps

## Are there differences in activity patterns between weekdays and weekends?

newdf2 <- newdf %>% mutate(weekend = as.POSIXlt(date)$wday %in% c(0,6))
workdaydf <- filter(newdf2, weekend==FALSE)
weekenddf<- filter(newdf2, weekend== TRUE)

workday <- transform(meanSteps(workdaydf), type = "workday" )
weekend <- transform(meanSteps(weekenddf), type = "weekend" )
uniondf <- rbind(workday, weekend)

title ="Time series of  the average number of steps taken\n in each 5-minute interval"
xlab = "Time intervals"; ylab = "Number of steps"
g <- ggplot(uniondf, aes(interval, steps_per_interval))
g + geom_line( size= 0.8 ) +facet_grid( type ~ .) +
        labs(title=title, x=xlab, y=ylab)
        
g <- ggplot(uniondf, aes( time, steps_per_interval))
g + geom_line( size= 0.8 ) +facet_grid( type ~ .) +
        labs(title=title, x="time", y=ylab)+
        scale_x_datetime( labels= date_format("%H:%M", tz = mytimezone ))
