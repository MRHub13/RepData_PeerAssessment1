---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document: null
  keep_md: yes
---
# **Assignment**

        
## **Loading and preprocessing the data**
Show any code that is needed to:

   1. **Load the data (i.e. read.csv())**
```{r, echo = TRUE}
## ... Load the data
filename <- "activity.zip"
# # ... Checking if archieve (*.zip) or file (*.csv) already exists.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL, filename, method="curl")
 }  
if (!file.exists("activity.csv")) { 
        unzip(filename) 
 }
if (!file.exists("figure")) { 
        dir.create("figure") 
 }
activity<-read.csv("activity.csv")
```

   2. **Process/transform the data (if necessary) into a format suitable for your analysis**
   
```{r, echo = TRUE}
library(dplyr)
library(ggplot2)
## ... Exploring the data - process/transform the data if necessary
dim(activity)
names(activity)
head(activity)
str(activity)
# # ... total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
# # ... transforming the date column (chr format) into date format using lubridate
library(lubridate)
activity$date<-ymd(activity$date)
length(unique(activity$date))
str(activity)
```   
       
        
## **What is mean total number of steps taken per day?**
For this part of the assignment, you can ignore the missing values in the dataset.

   1. **Make a histogram of the total number of steps taken each day**
```{r}
StepsEachDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsEachDay) <- c("Date", "Steps")
g <- ggplot(StepsEachDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkorange", fill="blue")+ggtitle("Histogram of steps taken each day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))
dev.copy(png, "figure/Figure2_1.png",width=480,height=480,units="px")
dev.off()
```

   2. **Calculate and report the mean and median total number of steps taken per day**      
```{r}
# ... Mean
mean(StepsEachDay$Steps, na.rm=TRUE)

# ... Median
median(StepsEachDay$Steps, na.rm=TRUE)


```    


## **What is the average daily activity pattern?**
   1. **Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**
```{r}   
# create table with steps per days
StepsPerDays <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerDays$time <- StepsPerDays$interval/100
# draw the line plot
h <- ggplot(StepsPerDays, aes(time, steps))
h+geom_line(col="orange")+ggtitle("Average steps per 5-minute interval, on average across all the days")+xlab("Days")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
dev.copy(png, "figure/Figure3_1.png")
dev.off()
```
   2. **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**
```{R}
# dplyr table 
SPD <- tbl_df(StepsPerDays)
# find the max. value
SPD %>% select(time, steps) %>% filter(steps==max(SPD$steps))
```

## **Imputing missing values**
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

   1. **Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**
```{R}
# dplyr table
AT <- tbl_df(activity)
# find the column
AT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

   2. **Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**
```{R}
# values without NA are imputed in a new column
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerDays$steps[match(activity$interval, StepsPerDays$interval)],0), activity$steps)
```
   3. **Create a new dataset that is equal to the original dataset but with the missing data filled in.**
```{R}
# new dataset - missing data filled in
MDFI <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
# see first 12 values of the new dataset
head(MDFI, n=12)
```

   4. **Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**
```{R}
# data preparation
SPDF <- aggregate(MDFI$steps, list(MDFI$date), FUN=sum)
colnames(SPDF) <- c("Date", "Steps")
# draw the histogram
g <- ggplot(SPDF, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkorange", fill="darkgreen")+ggtitle("Histogram of total number of steps taken each day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
dev.copy(png, "figure/Figure4_1.png")
dev.off()

# ... Mean
mean(SPDF$Steps)

# ... Median
median(SPDF$Steps)
```
The impact of imputing missing data on the estimates of the total daily number of steps is not significant - bearing in mind that different imputing / replacement methods may cause different results. In this case only in the interval from 10.000 to 12.500 steps the frequency has changed - grown from 18 to 26.


## **Are there differences in activity patterns between weekdays and weekends?**
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

   1. **Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**
```{R}
MDFI$weekday <- weekdays(MDFI$date)
# create a new variable indicating weekday or weekend
MDFI$DayType <- ifelse(MDFI$weekday=='Saturday' | MDFI$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(MDFI, n=10)

```

   2. **Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**
```{R}
# create table with steps per time across weekdaydays or weekend days
SPD_DT <- aggregate(steps~interval+DayType,data=MDFI,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
SPD_DT$time <- StepsPerDays$interval/100
# draw the line plot
j <- ggplot(SPD_DT, aes(time, steps))
j+geom_line(col="darkorange")+ggtitle("Average steps per days: weekdays vs. weekends")+xlab("Days")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
dev.copy(png, "figure/Figure5_1.png")
dev.off()
```




