# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library("lattice") 
setwd("/Users/maartenelen/Documents/Modeling/Johns Hopkins Coursera/course 5")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day  


```r
steps_per_day <- activity %>% 
      group_by(date) %>% 
      summarise(dailysteps = sum(steps, na.rm = TRUE)) 
```

2. Make a histogram of the total number of steps taken each day  


```r
hist(steps_per_day$dailysteps, 
     main="Steps per day histogram", 
     xlab="steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day  


```r
summary(steps_per_day$dailysteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
steppattern <- activity %>% 
      group_by(interval) %>% 
      summarise(avg_steps = mean(steps, na.rm = TRUE)) 
plot(steppattern$interval, steppattern$avg_steps, 
     type="l",
     main="average daily step pattern", 
     xlab="5min interval", 
     ylab="average steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
steppattern %>% 
      filter(avg_steps == max(avg_steps)) %>%
      select(interval)
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
na1 <- is.na(activity[,1])
na2 <- is.na(activity[,2])
na3 <- is.na(activity[,3])
na_all <- na1 | na2 | na3
sum(na_all)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

* The below strategy replaces missing values by the average number of steps for that 5-minute interval *

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
activity_imputed <- activity
for (i in 1:length(activity_imputed$steps)) {
      if (is.na(activity_imputed$steps[i]) == TRUE) {
            x <- activity_imputed$interval[i]
            activity_imputed$steps[i] <- steppattern %>% 
                  filter(interval == x) %>%
                  select(avg_steps)
      }
}
activity_imputed$steps <- unlist(activity_imputed$steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
steps_per_day_v2 <- activity_imputed %>% 
      group_by(date) %>% 
      summarise(dailysteps = sum(steps, na.rm = TRUE)) 
hist(steps_per_day_v2$dailysteps, 
     main="Steps per day histogram", 
     xlab="steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
summary(steps_per_day_v2$dailysteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  


```r
activity_imputed <- activity_imputed %>% 
      mutate(weekdays = weekdays(as.Date(date))) %>%
      mutate(weekend = ifelse(
            weekdays %in% c("Saturday", "Sunday"), "weekend", "weekday")
            )
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  


```r
steppattern_v2 <- activity_imputed %>% 
      group_by(weekend, interval) %>% 
      summarise(avg_steps = mean(steps, na.rm = TRUE)) 
xyplot(avg_steps~interval|weekend, 
       data=steppattern_v2, 
       type="l", 
       layout=(c(1,2)), 
       ylab="Number of steps", 
       xlab="Interval"
       )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

