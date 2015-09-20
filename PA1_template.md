# PA1_template.Rmd

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

### 1. Load the data (i.e. read.csv()) 


```r
# set working directoriy with setwd()
```


```r
# load data
data <- read.csv("activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# get structure 
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
# transform to date format
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
```


```r
# get structure
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
# display first parts of data 
head (data)
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

### 1. Calculate the total number of steps taken per day


```r
# Calculate the total number of steps taken per day (NA values removed)
steps_per_day <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)

# Rename columns
names(steps_per_day) <- c("date", "total_steps")

# Display the first parts
head(steps_per_day)
```

```
##         date total_steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
# make histogram
hist(steps_per_day$total_steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="darkgreen", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
# calculate mean
mean(steps_per_day$total_steps)
```

```
## [1] 9354.23
```


```r
# calculate median
median(steps_per_day$total_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Get mean of steps 
mean_steps <- aggregate(data$steps, 
                       by=list(data$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
```


```r
# Change column names
names(mean_steps) <- c("interval", "mean")
```


```r
# display first parts of data
head(mean_steps)
```

```
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```


```r
# create plot
# Compute the time series plot
plot(mean_steps$interval, 
     mean_steps$mean, 
     type="l", 
     col="darkgreen", 
     lwd=2, 
     xlab="Interval", 
     ylab="Average number of steps", 
     main="Time series of the average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# calculate position of maximum 
max_steps <- which(mean_steps$mean == max(mean_steps$mean))

# get position
max_steps
```

```
## [1] 104
```


```r
# find interval where max is located
max_int <- mean_steps[max_steps, 1]

# get interval
max_int
```

```
## [1] 835
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# calculate missing values
missing <- is.na(data$steps)

# get numbers
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

All of the missing values are filled in with mean of the steps attribute.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# find NA values
na_values <- which(is.na(data$steps))

# calculate a vector of means to replace the NA values
mean_val <- rep(mean(data$steps, na.rm=TRUE), times=length(na_values))

# replace NA values with means
data[na_values, "steps"] <- mean_val

# see first rows of new dataset
head(data)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Get total number of steps taken each day
total_steps <- aggregate(data$steps, by=list(data$date), FUN=sum)

# Change column names 
names(total_steps) <- c("date", "total")

# Make histogramm of the total number of steps taken each day
hist(total_steps$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="darkgreen", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png) 


```r
# calculate mean
mean(total_steps$total)
```

```
## [1] 10766.19
```


```r
# calculate median
median(total_steps$total)
```

```
## [1] 10766.19
```

The mean and median of the new dataset with filled missing values differ a lot from the previous mean and median. The reason is that the previous NA values were calculated as '0', which kind of flasified the result. The Na values in the new dataset were filled with the mean, which is greater than '0' and therefore leads to higher mean and median values.  


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# Make sure that weekdays are given in English
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
# Add weekdays to dataset
data <- data.frame(date=data$date, 
                  weekday=tolower(weekdays(data$date)), 
                  steps=data$steps, 
                  interval=data$interval)

# Identify weekdays and weekends
data <- cbind(data, 
              type=ifelse(data$weekday == "saturday" | data$weekday == "sunday", "weekend", 
              "weekday"))

# See first part of dataset
head(data)
```

```
##         date weekday   steps interval    type
## 1 2012-10-01  monday 37.3826        0 weekday
## 2 2012-10-01  monday 37.3826        5 weekday
## 3 2012-10-01  monday 37.3826       10 weekday
## 4 2012-10-01  monday 37.3826       15 weekday
## 5 2012-10-01  monday 37.3826       20 weekday
## 6 2012-10-01  monday 37.3826       25 weekday
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# calculate mean per type
mean_type <- aggregate(data$steps, 
                       by=list(data$weekday, 
                               data$type, data$interval), FUN=mean)

# change column names
names(mean_type) <- c("weekday", "type", "interval", "mean")

# see first part of generated dataset
head(mean_type)
```

```
##     weekday    type interval     mean
## 1    friday weekday        0 8.307244
## 2    monday weekday        0 9.418355
## 3  thursday weekday        0 9.375844
## 4   tuesday weekday        0 0.000000
## 5 wednesday weekday        0 7.931400
## 6  saturday weekend        0 4.672825
```



```r
# library lattice is needed
library(lattice)

# make plot
xyplot(mean ~ interval | type, mean_type, 
       type="l", 
       lwd=1, 
       col="darkgreen",
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png) 

