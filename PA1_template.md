---
title: "Project1"
output: 
  html_document: 
    highlight: tango
    theme: readable
---

### Loading and preprocessing the data

#### 1. Load the data 

```r
activity <- read.csv("activity.csv")
```

#### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date)
```


### What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

```r
tmp <- aggregate(steps ~ date, data = activity, sum, na.rm = T)
head(tmp, 15)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
```


#### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
ggplot(data = tmp, aes(x = steps)) + geom_histogram(binwidth = 5000) +
    ggtitle("The total number of steps taken each day") +
    theme(plot.title = element_text(size = 20, vjust = 1)) + 
    scale_x_continuous(breaks = seq(0, 30000, 5000))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


#### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(tmp$steps)
```

```
## [1] 10766.19
```

```r
median(tmp$steps)
```

```
## [1] 10765
```


### What is mean total number of steps taken per day?


#### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(dplyr)
activity_naomit <- na.omit(activity)
ts_tmp <- activity_naomit %>%
    group_by(interval) %>%
    summarise(m = mean(steps))

ggplot(data = ts_tmp, aes(x = interval, y = m)) + geom_line() +
    xlab("5-minute interval") + ylab("Average number of steps") +
    theme_light()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
ts_tmp$interval[which.max(ts_tmp$m)]
```

```
## [1] 835
```


### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```


#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mean_interval <- activity %>%
    group_by(interval) %>%
    summarise(m = mean(steps, na.rm = T))

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
tmp_steps <- vector()
for (i in 1:nrow(activity)) {
    if (is.na(activity[i, 1])) {
        tmp_steps <- append(tmp_steps, 
                            mean_interval$m[mean_interval$interval == activity$interval[i]])
    } else {
        tmp_steps <- append(tmp_steps, activity[i, 1])
    }
}
activityinput <- activity
activityinput$steps <- tmp_steps
```


#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
tmp2 <- aggregate(steps ~ date, data = activityinput, sum)

ggplot(data = tmp2, aes(x = steps)) + geom_histogram(binwidth = 5000) +
    ggtitle("The total number of steps taken each day") +
    theme(plot.title = element_text(size = 20, vjust = 1)) + 
    scale_x_continuous(breaks = seq(0, 30000, 5000))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


The mean and median are

```r
mean(tmp2$steps); median(tmp2$steps)
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```
After inputing NA values, the mean value stays the same but median increased very slightly.


### Are there differences in activity patterns between weekdays and weekends?

Note: For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityinput$weekend <- as.factor(ifelse(weekdays(activityinput$date) %in% 
                c("Saturday","Sunday"),"weekend", "weekday"))
```


#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
tmp_steps2 <- aggregate(steps ~ interval + weekend, data = activityinput, mean)

ggplot(data = tmp_steps2, aes(x = interval, y = steps)) + geom_line() + ylab("Number of steps") +
    facet_wrap(~weekend) +
    theme_light()
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

