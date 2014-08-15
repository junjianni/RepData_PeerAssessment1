# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("C:/Users/jf08/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("activity.csv", header=T, sep=",", quote="\"")
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

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken each day
stepsumD <- aggregate(steps~date, data=activity, FUN="sum")
# Display total number of steps each day with histogram
hist(stepsumD$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
# Compute mean of total number of steps each day
mean(stepsumD$steps)
```

```
## [1] 10766
```

```r
# Compute median of total number of steps each day
median(stepsumD$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# Calculate the average number of steps taken versus 5-minute interval and display a few of them
stepAveInt <- aggregate(steps~interval, data=activity, FUN="mean")
head(stepAveInt)
```

```
##   interval   steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```

```r
# Display the average number of steps taken versus 5-minute interval with time series plot
plot(stepAveInt$interval, stepAveInt$steps, type='l', xlab="interval", ylab="Average steps", main="Changes in average steps of intervals during a day", col=1,axes=T)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Find the time series containing the maximum number of steps and report it
subset(stepAveInt, steps==max(stepAveInt$steps))
```

```
##     interval steps
## 104      835 206.2
```

```r
subset(stepAveInt, steps==max(stepAveInt$steps))$interval
```

```
## [1] 835
```

```r
# Calculate summary stats for the average number of steps taken versus 5-minute interval
summary(stepAveInt$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    2.49   34.10   37.40   52.80  206.00
```

## Imputing missing values


```r
# Check how many steps with missing value
length(activity[activity$steps=="NA",]$steps)
```

```
## [1] 2304
```

```r
# Imputing strategy: If missing value, using the average number of steps for that time interval to replace the missing value
# Merge the raw data with aggregating data (average number of steps for each interval)
activity1 <- merge(activity, stepAveInt, by="interval")
# Go through raw data set steps, if it's NA, then it will be replaced by aggreated average number of steps of that time interval
for(i in 1:nrow(activity1)){
  if(is.na(activity1$steps.x[i])){
    activity1$steps.x[i]<-activity1$steps.y[i]
  }
}
# Check a few records after imputing data
head(activity1, 10)
```

```
##    interval steps.x       date steps.y
## 1         0   1.717 2012-10-01   1.717
## 2         0   0.000 2012-11-23   1.717
## 3         0   0.000 2012-10-28   1.717
## 4         0   0.000 2012-11-06   1.717
## 5         0   0.000 2012-11-24   1.717
## 6         0   0.000 2012-11-15   1.717
## 7         0   0.000 2012-10-20   1.717
## 8         0   0.000 2012-11-16   1.717
## 9         0   0.000 2012-11-07   1.717
## 10        0   0.000 2012-11-25   1.717
```

```r
# Get and prepare new dataframe with imputated steps, date and interval, assign the column names
activity2<-data.frame(activity1$steps.x, activity1$date, activity1$interval)
colnames(activity2)<-c("steps","date","interval")
# Calculate the total number of steps taken each day again after missing data values were imputed, display that with histogram, calculate mean, median again.
stepsumD2 <- aggregate(steps~date, data=activity2, FUN="sum")
hist(stepsumD2$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(stepsumD2$steps)
```

```
## [1] 10766
```

```r
median(stepsumD2$steps)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# Add new variable wd, assign as weekend or weekday based on date situation, added that to previous dataframe, display a few records of the dataframe
wd<-weekdays(as.Date(activity2$date))
for(i in 1:length(wd)){
  if(wd[i]=="Saturday" | wd[i]=="Sunday"){
    wd[i]<-"weekend"
  }else{
    wd[i]<-"weekday"
  }
}
activity2$weekday<-wd
head(activity2)
```

```
##   steps       date interval weekday
## 1 1.717 2012-10-01        0 weekday
## 2 0.000 2012-11-23        0 weekday
## 3 0.000 2012-10-28        0 weekend
## 4 0.000 2012-11-06        0 weekday
## 5 0.000 2012-11-24        0 weekend
## 6 0.000 2012-11-15        0 weekday
```

```r
# Compute average nubmer of steps taken based on time series interval and weekdays, display a few records
stepAveInt1 <- aggregate(steps~interval+weekday, data=activity2, FUN="mean")
head(stepAveInt1)
```

```
##   interval weekday   steps
## 1        0 weekday 2.25115
## 2        5 weekday 0.44528
## 3       10 weekday 0.17317
## 4       15 weekday 0.19790
## 5       20 weekday 0.09895
## 6       25 weekday 1.59036
```

```r
# Display the average number of steps taken versus 5-minute interval and weekday with time series plot, using lattice package
library(lattice)
stepAveInt1<-transform(stepAveInt1, weekday=factor(weekday))
xyplot(steps~interval|weekday, data=stepAveInt1, layout=c(1,2), lty=1, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
