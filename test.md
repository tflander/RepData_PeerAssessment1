Reproducible ResearchProject One -- Todd Flanders
========================================================

# 1 - Create data for analysis


```r
library(plyr)
```

Unzip the data.  The zip file activity.zip was part of the original project,
forked from https://github.com/rdpeng/RepData_PeerAssessment1


```r
unzip("activity.zip")
originaldata <- read.csv("activity.csv")
```

Convert date factor to date

```r
originaldata$datePosix <- as.POSIXlt(originaldata$date)
```

Add the interval in minutes to the timestamp to get samples in 5 minute intervals


```r
originaldata$datePosix$min <- originaldata$datePosix$min + originaldata$interval 
```

Add a column for day of week.


```r
originaldata$wday <- originaldata$datePosix$wday
```

Create meta variables


```r
numDays <- as.integer(max(originaldata$datePosix) - min(originaldata$datePosix)) 
samplesPerDay <- length(originaldata$date) / numDays
```

Analyze missing step data


```r
dataDirty <- originaldata[is.na(originaldata$steps),]
dirtyByDay <- ddply(dataDirty[1:2], .(date), numcolwise(length))
```

Here are the days with missing data out of 288 samples per day:


```r
print(dirtyByDay)
```

```
##         date steps
## 1 2012-10-01   288
## 2 2012-10-08   288
## 3 2012-11-01   288
## 4 2012-11-04   288
## 5 2012-11-09   288
## 6 2012-11-10   288
## 7 2012-11-14   288
## 8 2012-11-30   288
```
 
We will fill in missing values with the mean of the 5 minute interval for 
the day of week (Sun-Sat).


```r
dataClean <- originaldata[!is.na(data$steps),]
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
averageByIntervalAndWday <- ddply(dataClean[c(1,3,5)], .(interval, wday), numcolwise(mean))
```

```
## Error in empty(.data): object 'dataClean' not found
```

```r
averageByIntervalAndWday$steps <- round(averageByIntervalAndWday$steps, digits=0)
```

```
## Error in eval(expr, envir, enclos): object 'averageByIntervalAndWday' not found
```

```r
# TODO finish
```

Summarize total data by day


```r
# TODO: this needs to be on the data with imputed values
totalsByDay <- ddply(dataClean[1:2], .(date), numcolwise(sum))
```

```
## Error in empty(.data): object 'dataClean' not found
```

# 2 - What is mean total number of steps taken per day?

## Make a histogram of the total number of steps taken each day



```r
hist(
  totalsByDay$steps, 
  main="Total Number of Steps Taken Each Day",
  xlab = "steps per day"
)
```

```
## Error in hist(totalsByDay$steps, main = "Total Number of Steps Taken Each Day", : object 'totalsByDay' not found
```

## Calculate and report the mean and median total number of steps taken per day


```r
meanStepsPerDay <- round(mean(totalsByDay$steps, na.rm = TRUE), digits = 0)
```

```
## Error in mean(totalsByDay$steps, na.rm = TRUE): object 'totalsByDay' not found
```

```r
medianStepsPerDay <- median(totalsByDay$steps, na.rm = TRUE)
```

```
## Error in median(totalsByDay$steps, na.rm = TRUE): object 'totalsByDay' not found
```


```r
print(meanStepsPerDay)
```

```
## Error in print(meanStepsPerDay): object 'meanStepsPerDay' not found
```


```r
print(medianStepsPerDay)
```

```
## Error in print(medianStepsPerDay): object 'medianStepsPerDay' not found
```
