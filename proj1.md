---
title: "Reproducible R Project 1"
author: "vibhor batra"
date: "Monday, February 09, 2015"
output: html_document
---

## Project Requirement

Use the given Activity Data and find answers to the following.

###What is mean total number of steps taken per day?

. For this part of the assignment, you can ignore the missing values in the dataset.

. Calculate the total number of steps taken per day

. Make a histogram of the total number of steps taken each day


###Calculate and report the mean and median of the total number of steps taken per day

. What is the average daily activity pattern?

. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

. Create a new dataset that is equal to the original dataset but with the missing data filled in.

. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in missing values for this part.

. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


##Solution





```r
# Including necessary R packages
library(dplyr)
library(lattice)
```

###Loading Data

First of all read the data into a data frame


```r
actv_data=read.csv(file="C:\\Users\\IBM_ADMIN\\Documents\\R\\data\\activity.csv", header=T)

head(actv_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

### Mean/Total number of steps taken per day

Calculate the total number of steps per day and load it into a data frame


```r
# Ignoring NAs

st_sum = summarize(group_by(actv_data,date), sum(steps, na.rm=T))

names(st_sum)=c("date","total_steps")
```

```r
#Total number of steps taken  

daily_sum = sum(st_sum$total_steps)

daily_sum
```

```
## [1] 570608
```

Plot a histogram with total daily steps.


```r
hist(st_sum$total_steps, breaks=10, xlab="total daily steps", main="Activity Trends" )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Calculate the mean and median.

Mean "round(mean(st_sum$total_steps),2)" :  9354.23

Median "median(st_sum$total_steps)" :  10395


### Average daily activity pattern

Lets get clean data (without NAs) one more time and calculate average steps by time intervals.

```r
cl_actv_data = na.omit(actv_data)

avg_steps = summarize(group_by(cl_actv_data, interval), mean(steps))

names(avg_steps)=c("interval", "avg_steps")
```
Plot a line graph to show average steps per time interval


```r
plot(avg_steps$interval, avg_steps$avg_steps, type="n", xlab="5 min interval", ylab="Avg Number of Steps", main= "Steps by Interval Trend")

lines(avg_steps$interval, avg_steps$avg_steps, lwd=3, col="red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

The time interval which has the maximum number of steps is 835

```r
subset(avg_steps, avg_steps==max(avg_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval avg_steps
## 1      835  206.1698
```
### Imputing Missing Values

Total number of missing values 'sum(is.na(actv_data))' : 2304

Seperate "NA" data, then merge it with mean steps and recombine it with the clean data


```r
#Seperate out na data
nodata= subset(actv_data, is.na(actv_data$steps))

# Merge with average steps data
mix_mean = merge(avg_steps, nodata[,c("interval","date")], by.avg_steps=interval, by.nodata=interval)

#Reorder and rename NA data set

reord_nodata = mix_mean[c("avg_steps", "date","interval")]

names(reord_nodata)=c("steps","date", "interval")

#imputed data by merging clean and NA data sets

imp_data=rbind(cl_actv_data,reord_nodata)
```

Making sure the number of rows in the original and imputed data sets is the same


```r
all.equal(nrow(actv_data),nrow(imp_data))
```

```
## [1] TRUE
```


Imputed daily steps


```r
imp_st_sum = summarize(group_by(imp_data,date), sum(steps))

names(imp_st_sum)=c("date","total_steps")
```

```r
#Total number of steps taken

imp_daily_sum= sum(imp_st_sum$total_steps)

imp_daily_sum
```

```
## [1] 656737.5
```

Plot a histogram with total daily steps.


```r
hist(imp_st_sum$total_steps, breaks=10, xlab="total daily steps", main="Activity Trends with Imputed Data" )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Calculate the mean and median.

Mean "round(mean(imp_st_sum$total_steps),2)" :  9354.23

Median "median(imp_st_sum$total_steps)" :  10395


###Conclusion:

Daily Total Steps taken increased between original and imputed data set by 86129.

Mean and median values remained unchanged.

### Patterns between weekdays and weekends


```r
#Add a factor weekend to the dataset

imp_data$wend <- as.factor(ifelse(weekdays( as.Date(imp_data$date)) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 

imp_avg_steps = summarize(group_by(imp_data, interval,wend), mean(steps))

names(imp_avg_steps)= c("interval","wend","avg_steps")

#Use lattice plotting system


xyplot(imp_avg_steps$avg_steps~imp_avg_steps$interval|imp_avg_steps$wend, type="l", ylab="Average # of Daily Steps", xlab="Time Intervals", main="Activity patterns by Weekend/Weekday", layout=c(1,2), col="red", lwd=2)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
