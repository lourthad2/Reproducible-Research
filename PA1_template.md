---
title: "Reproducible Research : Peer Assessment 1"
output: html_document
---
Load the data


```r
activity <- read.csv("activity.csv",colClasses =c("numeric","character","numeric"))
head(activity)
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

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
#library(lattice)
#activity$date <- as.Date(activity$date,"%Y-%m-%d")
```
####What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day

```r
total.steps <- tapply(activity$steps,activity$date,FUN=sum,na.rm=TRUE)
print(total.steps)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```
2.Histogram of the total number of steps taken each day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.2
```

```r
qplot(total.steps,binwidth=500,xlab="Total no.steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


3.Calculate and report the mean and median of the total number of steps taken per day

```r
print("Mean of the total number steps /day")
```

```
## [1] "Mean of the total number steps /day"
```

```r
mean.total <- mean(total.steps,na.rm = TRUE)
print(mean.total)
```

```
## [1] 9354.23
```

```r
print("Median of the total number steps /day")
```

```
## [1] "Median of the total number steps /day"
```

```r
median.total <- median(total.steps,na.rm = TRUE)
print(median.total)
```

```
## [1] 10395
```

####What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg <- aggregate(x=list(steps=activity$steps),by=list(interval=activity$interval),FUN=mean,na.rm=TRUE)
#ggplot(data=avg,aes(x=interval,y=steps)) #+geom_line(aes(group=1),colour="#000099") +xlab("Five minute Interval") + #ylab("Avg no. of steps taken")
ggplot(data=avg,aes(x=interval,y=steps))  +xlab("Five minute Interval") + ylab("Avg no. of steps taken") +geom_line(aes(group=1),colour="#000099") +
geom_point(size=1,colour="#CC0000")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
print("Max no. of steps")
```

```
## [1] "Max no. of steps"
```

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
####Imputing missing values
1.Calculate and report the total number of missing values in the dataset 

```r
missing <- sum(is.na(activity$steps))
print(missing)
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. 

```r
#Missing value will be replaced by mean in 5 min interval
fill.missvalue <- function(steps,interval){
  filled <- NA
  if(!is.na(steps))
    filled <-c(steps) 
  else 
    filled <- (avg[avg$interval == interval,"steps"])
  return(filled)
}
filled.activity <-activity
filled.activity$steps <- mapply(fill.missvalue,filled.activity$steps,filled.activity$interval)
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
total.steps <- tapply(filled.activity$steps,filled.activity$date,FUN=sum)
```


4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
qplot(total.steps,binwdth=500)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```
