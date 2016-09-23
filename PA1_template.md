# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
unzip(zipfile = "activity.zip", exdir = ".", unzip = "internal")
df <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
df$date <- as.Date(df$date, "%Y-%m-%d")
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(df, 3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

## What is mean total number of steps taken per day?


```r
df2 <- df[complete.cases(df),]
nr.steps <- aggregate(df2$steps, by=list(df2$date), sum)
ggplot(nr.steps, aes(x=nr.steps$x)) + geom_histogram(colour="black", fill="red", binwidth = 5000) + ggtitle("Frequency of Number of Steps per Day") + labs(x = "Number of steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meansteps <- as.integer(mean(nr.steps$x))
mediansteps <- median(nr.steps$x)
```

The mean of total number of steps taken per day is **10766** and the median is **10765**.

## What is the average daily activity pattern?


```r
activitypattern <- aggregate(df2$steps, by=list(df2$interval), mean)
ggplot(activitypattern, aes(x=activitypattern$Group.1)) + geom_line(aes(y=activitypattern$x)) + ggtitle("Daily Activity Pattern") + labs(x="5-minute interval", y="Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max.nr.steps <- activitypattern[which.max(activitypattern$x),]$Group.1
```

The 5-minute interval containing the maximum number of steps is **835**.

## Imputing missing values


```r
missing.values <- sum(rowSums(is.na(df)))
```

The total number of missing values is **2304**.



## Are there differences in activity patterns between weekdays and weekends?
