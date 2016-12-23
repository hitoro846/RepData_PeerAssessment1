# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data_raw<-read.csv("activity.csv")
data<-na.omit(data_raw)
```

## What is mean total number of steps taken per day?

```r
total_steps_pday<-length(data$steps)
mean_steps_pday<-mean(data$steps)
median_steps_pday<-median(data$steps)
total_steps_pday
```

```
## [1] 15264
```

```r
mean_steps_pday
```

```
## [1] 37.3826
```

```r
median_steps_pday
```

```
## [1] 0
```

```r
hist(data$steps,main="Histogram of steps per day",xlab="Steps in one day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?


```r
pattern<-data %>% group_by(interval) %>% summarize(mean(steps))
plot(pattern$interval,pattern$`mean(steps)`,type="l",xlab="Interval",ylab="Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
paste(pattern[pattern$`mean(steps)`==max(pattern$`mean(steps)`),"interval"])
```

```
## [1] "835"
```

## Imputing missing values

```r
data_imputed<-data_raw
length(data_imputed[,1])
```

```
## [1] 17568
```

```r
length(which(complete.cases(data_imputed)))
```

```
## [1] 15264
```

```r
for(i in 1:length(data_imputed[,1])) {
  if(is.na((data_imputed[i,1]))) {
    if(is.na(data_imputed[i,1]<-mean(data[data$interval==data[i,3],1])))
    {
      data_imputed[i,1]<-0
    }
    else 
    {
      data_imputed[i,1]<-mean(data[data$interval==data[i,3],1])
    }
  }
}
length(which(complete.cases(data_imputed)))
```

```
## [1] 17568
```

```r
total_steps_pday<-length(data_imputed$steps)
mean_steps_pday<-mean(data_imputed$steps)
median_steps_pday<-median(data_imputed$steps)
total_steps_pday
```

```
## [1] 17568
```

```r
mean_steps_pday
```

```
## [1] 36.76977
```

```r
median_steps_pday
```

```
## [1] 0
```

```r
hist(data$steps,main="Histogram of steps per day",xlab="Steps in one day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

```r
for(i in 1:length(data_imputed[,1])) {
  if(weekdays(as.Date(data_imputed[i,2]))=="Saturday" | weekdays(as.Date(data_imputed[i,2]))=="Sunday") {
    data_imputed$weekday[i]<-"weekend"
  }
  else {
    data_imputed$weekday[i]<-"weekday"
  }
}
weekends<-data_imputed[data_imputed$weekday=="weekend",]
weekdays<-data_imputed[data_imputed$weekday=="weekday",]
  
sum_weekend<-weekends %>% group_by(interval) %>% summarize(mean(steps))
sum_weekday<-weekdays %>% group_by(interval) %>% summarize(mean(steps))
plot(sum_weekend$interval,sum_weekend$`mean(steps)`,type="l",col="red",xlab="Interval",ylab="Mean Steps")
lines(sum_weekday$interval,sum_weekday$`mean(steps)`,col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
