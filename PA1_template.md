Course Project Activity Monitoring Data Analysis.
==========================================================

Load data to R and format it for better abalysis.


```r
setwd("C:/Users/mariat/Downloads/repdata%2Fdata%2Factivity")
activity<-read.csv('activity.csv')
```
Transform date column to Date format in R.

```r
activity$date<-as.Date(as.character(activity$date))
```

Calculate the total number of steps taken per day ignorring NA values.


```r
totalDay<-aggregate(steps~date,activity,sum)
```

Histogram of the total number of steps taken each day.


```r
library(ggplot2)
qplot(totalDay$steps,binwidth=1000,xlab='Steps',main='Total Number of Steps per Day with NA')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(totalDay$steps)
```

```
## [1] 10766.19
```

```r
median(totalDay$steps)
```

```
## [1] 10765
```

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
#Calculate average number of steps per interval for all days
aver<-aggregate(steps~interval,activity,mean)
#Create a plot
qplot(aver$interval,aver$steps,geom='line', col='red',xlab='Interval',ylab='Steps (mean)',main='Average Steps per Interval')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
#Find interval with maximum steps during the day
dd<-which.max(aver$steps)
aver[dd,]
```

```
##     interval    steps
## 104      835 206.1698
```

Impute missing values by finding the average value per interval and substituting NA with it. 


```r
library(reshape2)
#Perform data transformation to calculate mean steps per interval.
noNA<-activity[!is.na(activity$steps),c(1,3)]
noNAmelt<-melt(noNA,id.vars='interval')
noNAdcast<-dcast(noNAmelt,interval~variable,mean)
#create a new dataset that is equal to the existing one
activity1<-activity
#Assign mean steps values to the interval column.
activity1$noNA<-activity1$interval
#Create a new column in dataframe to for the mean values input.   
activity1$noNA<-noNAdcast$steps
#Create index to know which rows to replace.
indexNA<-which(is.na(activity1$steps))
#Perform replacement.
activity1$steps<-replace(activity1$steps,indexNA,activity1$noNA)
```

```
## Warning in x[list] <- values: number of items to replace is not a multiple
## of replacement length
```

```r
#Delete as the column is no longer needed
activity1$noNA<-NULL
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 



```r
totalDay1<-aggregate(steps~date,activity1,sum)
qplot(totalDay1$steps,binwidth=1000,xlab='Steps',main='Total Number of Steps per Day with NA removed')
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
mean(totalDay1$steps)
```

```
## [1] 10766.19
```

```r
median(totalDay1$steps)
```

```
## [1] 10766.19
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
#calculate average number of steps taken per day
averPerIntervalDate<-aggregate(steps~interval+date,activity1,mean)
#create a column with weekdays
averPerIntervalDate$wDays<-weekdays(averPerIntervalDate$date)
#create a column with date types
averPerIntervalDate$dType=ifelse(averPerIntervalDate$wDays=='Saturday'|averPerIntervalDate$wDays=='Sunday', 'Weekend','Weekday')
#make a plot
par(mar=c(4,4,2,4))
qplot(interval,steps,data=averPerIntervalDate,facets=dType~.,geom='line',col=steps,xlab='Interval',ylab='Average Steps',main='Steps on Weekdays vs Weekend')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

