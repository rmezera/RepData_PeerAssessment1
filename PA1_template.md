# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
I started by downloading the file from the website to my local
directory.  Next, I read in the data:


```r
activity <- read.csv("activity.csv")
```
Change the date variable to be of the data variety

```r
activity$date <- as.Date(activity$date,format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Ignoring the NAs, here is the histogram that shows how many steps were taken each day

```r
hist(aggregate(activity$steps,list(activity$date),sum)$x,xlab="Steps Taken Each Day",main="Histogram of Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

By finding the sum for each day and then taking the mean removing the NAs I get the following:

```r
mean(aggregate(activity$steps,list(factor(activity$date)),sum)$x,na.rm=TRUE)
```

```
## [1] 10766.19
```
When doing the same thing with the median I get this:

```r
median(aggregate(activity$steps,list(factor(activity$date)),sum)$x,na.rm=TRUE)
```

```
## [1] 10765
```
Looking at the histogram and the similarity between the mean and the median I would say that the steps taken each day looks to be similar to a normal distribution (at least when you ignore the NAs).

## What is the average daily activity pattern?
In order to see what the daily pattern is I have made a time series plot that shows the average number of steps across all days for each of the 5 minute intervals.


```r
meanbyinterval <- aggregate(activity$steps,list(factor(activity$interval)),function(x){mean(x,na.rm=TRUE)})
plot(as.numeric(as.character(meanbyinterval$Group.1)),meanbyinterval$x,type="l",xlab="Interval",ylab="Average Number of Steps", main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

The interval with the maximum average number of steps across all of the days is:

```r
as.numeric(as.character(meanbyinterval[meanbyinterval$x==max(meanbyinterval$x),1]))
```

```
## [1] 835
```

## Imputing missing values

The total number of rows that have missing data are:

```r
length(activity[is.na(activity$steps),1])
```

```
## [1] 2304
```

When replacing the 2304 missing values I chose to use the mean from the particular 5 minute interval across all available days.  I thought this was more fair than using the average for a particular day. I have the average values for each of the interval periods in the dataset meanbyinterval. So I just need to replace the NAs with the appropriate mean from the meanbyinterval dataset.

Start by creating second dataset

```r
activity2 <- activity

for (i in  1:17568) {
    if(is.na(activity2[i,1])) activity2[i,1]<-meanbyinterval[activity2$interval[i]==as.integer(as.character(meanbyinterval$Group.1)),2] 
}
```

Now, we will look at a Histogram of the data including the imputted data

```r
hist(aggregate(activity2$steps,list(activity2$date),sum)$x,xlab="Steps Taken Each Day",main="Histogram of Total Number of Steps Taken Each Day \n Including the Imputted Data")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

Now, we will compute the mean daily step totals using the imputted data and we will compare to the old mean

```r
mean(aggregate(activity2$steps,list(factor(activity2$date)),sum)$x,na.rm=TRUE)
```

```
## [1] 10766.19
```

The new Median is below using the imputted data.

```r
median(aggregate(activity2$steps,list(factor(activity2$date)),sum)$x,na.rm=TRUE)
```

```
## [1] 10766.19
```

The result of imputting the missing data is that the mean stayed exactly the same and the median shifted just slightly and now matches the mean exactly. Overall, the effect is very minor using the technique I used.  This would have likely been very different if I chose to use the daily average to replace each of the missing values instead of the interval average.


## Are there differences in activity patterns between weekdays and weekends?

In order to see if there are differences in activity patterns between weekdays and weekends, I needed to create a factor variable that breaks up the dataset into weekend and weekday days. Below is the code to create this variable. 

```r
activity$weekpart <- NA
activity$weekpart[is.element(weekdays(activity$date),c("Saturday","Sunday"))]<- "weekend"
activity$weekpart[!is.element(weekdays(activity$date),c("Saturday","Sunday"))]<- "weekday"
activity$weekpart <- as.factor(activity$weekpart)
```

Here is a plot that directly compares Weekend patterns vs. Weekday patterns

```r
meanbyinterval2 <- aggregate(activity$steps,list(factor(activity$interval),activity$weekpart),function(x){mean(x,na.rm=TRUE)})
meanbyinterval2$Group.1 <- as.integer(as.character(meanbyinterval2$Group.1))
library(lattice)
xyplot(x ~Group.1  |Group.2 , data = meanbyinterval2, type="l",layout = c(1, 2),xlab="Interval",ylab="Number of Steps",main="Activity Patterns - Weekend vs. Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)

You can see from the above plot that weekend activity is more spread out across the day while the weekday activity is concentrated in the morning and evening.  I also noticed that activity begins much earlier during the week. People must sleep in a little on the weekends.
