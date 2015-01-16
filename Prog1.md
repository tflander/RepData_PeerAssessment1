Reproducible ResearchProject One -- Todd Flanders
========================================================

First we unzip the data.  The zip file activity.zip was part of the original project,
forked from https://github.com/rdpeng/RepData_PeerAssessment1


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

Convert date factor to date

```r
data$datePosix <- as.POSIXlt(data$date)
```

Add the interval in minutes to the timestamp to get samples in 5 minute intervals

```r
data$datePosix$min <- data$datePosix$min + data$interval 
```

Summarize total data by day

```r
library(plyr)
totalsByDay <- ddply(data[1:2], .(date), numcolwise(sum))
```
# What is mean total number of steps taken per day?

## Make a histogram of the total number of steps taken each day



```r
hist(
  totalsByDay$steps, 
  main="Total Number of Steps Taken Each Day",
  xlab = "steps per day"
)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

## Calculate and report the mean and median total number of steps taken per day


```r
meanStepsPerDay <- round(mean(totalsByDay$steps, na.rm = TRUE), digits = 0)
medianStepsPerDay <- median(totalsByDay$steps, na.rm = TRUE)
```


```r
print(meanStepsPerDay)
```

```
## [1] 10766
```

```r
print(medianStepsPerDay)
```

```
## [1] 10765
```

The mean total number of steps taken per day is 
10,766.

The median total number of steps taken per day is 
10,765.

# What is the average daily activity pattern?

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Note: Since we have 288 samples per day, we can deduce that samples are already in 
5-minute intervals.  This is confirmed by the data in the interval column.  Therefore, we can label the x-axis using sample days 0-60 using 288 samples per day.


```r
numDays <- as.integer(max(data$datePosix) - min(data$datePosix)) + 1
samplesPerDay <- length(data$date) / numDays
myts <- ts(data$steps, frequency=samplesPerDay) 
plot(myts, xlab="day", ylab="steps")
```

![plot of chunk timeseries](figure/timeseries-1.png) 


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

