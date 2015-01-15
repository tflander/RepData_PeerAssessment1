Reproducible ResearchProject One -- Todd Flanders
========================================================

First we unzip the data.  The zip file activity.zip was part of the original project,
forked from https://github.com/rdpeng/RepData_PeerAssessment1


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
dataWithoutMissingValues <- na.omit(data)
```

# What is mean total number of steps taken per day?

## Make a histogram of the total number of steps taken each day



```r
hist(
  dataWithoutMissingValues$steps, 
  main="Total number of steps taken each day",
  xlab = "steps"
)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

## Calculate and report the mean and median total number of steps taken per day


```r
meanStepsPerDay <- mean(data$steps, na.rm = TRUE)
medianStepsPerDay <- median(data$steps, na.rm = TRUE)
```

The mean total number of steps taken per day is 37.3825996.

The median total number of steps taken per day is 0.
