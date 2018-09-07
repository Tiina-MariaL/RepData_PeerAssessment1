---
title: "Project1"
output:
  html_document: 
    keep_md: yes
  
---
```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

knitr::opts_chunk$set( fig.path = "~/figures")


```

Load, unzip and read the data

```{r}
setwd("C:/Users/tiina/Desktop")
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip")
}
activity<- read.csv(file="activity.csv", header=TRUE, as.is=TRUE)

```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.

```{r}
steps_per_day <- sum(activity$steps, na.rm = TRUE)
steps_per_day

```

2. Make a histogram of the total number of steps taken each day.
```{r}
library(ggplot2)
daily_steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
qplot(daily_steps, binwidth = 1000, xlab = "Total number of steps daily")
mean(daily_steps, na.rm = TRUE)

```
![](figures/"unnamed-chunk-1.png")
3. Calculate and report the mean and median of the total number of steps taken per day.
```{r}
mean(daily_steps, na.rm = TRUE)
median(daily_steps, na.rm = TRUE)

```
## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
mean_5minutes<-aggregate(activity$steps,by=list(activity$interval),FUN=mean, na.rm=TRUE)
names(mean_5minutes)<-c("interval","mean")
plot(mean_5minutes$interval, mean_5minutes$mean,type="l",col="red",lwd=2,xlab="Interval",ylab="Average number of steps", main="Time series of the average number of steps /5 minutes")

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
mean_5minutes[which.max(mean_5minutes$mean),]
```
##Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
```{r}
missing_values <- is.na(activity$steps)
table(missing_values)
```
2. Devise a strategy for filling in all of the missing values in the dataset. 
Replacing the missing values with the mean value.

```{r}
activity$steps[is.na(activity$steps)]<-mean(activity$steps, na.rm=TRUE)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
Filled_activity <- activity 
for (i in 1:nrow(Filled_activity)) {
    if (is.na(Filled_activity$steps[i])) {
        Filled_activity$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(Filled_activity)

```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
library(ggplot2)
daily_steps_filled <- tapply(Filled_activity$steps, Filled_activity$date, FUN = sum, na.rm = TRUE)
qplot(daily_steps_filled, binwidth = 1000, xlab = "Total number of steps daily")
mean(daily_steps_filled, na.rm = TRUE)
median(daily_steps_filled,na.rm=TRUE)
```

##Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
week_day <- function(given_date) {
    wd <- weekdays(as.Date(given_date, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
```
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r}
Filled_activity$date<-as.Date(Filled_activity$date)
Filled_activity$day<-sapply(Filled_activity$date,FUN=week_day)
averages <- aggregate(steps ~ interval + day, data = Filled_activity, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```
