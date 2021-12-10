<style type="text/css">
  body{
  font-size: 10pt;
}
</style>

## Assignment requirements

1.  Code for reading in the dataset and/or processing the data
2.  Histogram of the total number of steps taken each day
3.  Mean and median number of steps taken each day
4.  Time series plot of the average number of steps taken
5.  The 5-minute interval that, on average, contains the maximum number
    of steps
6.  Code to describe and show a strategy for imputing missing data
7.  Histogram of the total number of steps taken each day after missing
    values are imputed
8.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends
9.  All of the R code needed to reproduce the results (numbers, plots,
    etc.) in the report

## Step 1: Reading in dataset and processing data

    # read csv file
    activity <- read.csv("activity.csv")

Explore data: get dimensions, field names, get a summary of the data

    # get summary and statistics of data
    dim(activity)

    ## [1] 17568     3

    names(activity)

    ## [1] "steps"    "date"     "interval"

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Upon exploration, ‘step’ variable has some NA values. Also need to
convert date field to be of date format.

    # install library
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    activity$date <- ymd(activity$date)

## Step 2: Histogram of the total number of steps taken each day

    library(ggplot2)

    # aggregate based on steps taken each day
    stepsPerDay <- aggregate(steps ~ date, activity, FUN = sum)

    # create histogram
    plot2 <- ggplot (stepsPerDay, aes (x = steps)) +
      geom_histogram(fill = "green", binwidth = 1000) +
        labs(title = "Histogram of Total Steps Taken Each Day ", x = "Steps", y = "Frequency")

    plot2 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    dev.copy(png,'Plot2.png')

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

## Step 3: Mean and median number of steps taken each day

    # compute mean of steps taken each day
    meanSteps <- mean(stepsPerDay$steps, na.rm=TRUE)
    meanSteps

    ## [1] 10766.19

    # compute median of steps taken each day
    medianSteps <- median(stepsPerDay$steps, na.rm=TRUE)
    medianSteps

    ## [1] 10765

## Step 4: Time series plot of the average number of steps taken

Make a time series plot (type = “l”) of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

    # aggregate steps based on 5-minute interval
    stepsPerInterval <- aggregate(steps ~ interval, activity, mean)

    # create time series
    timeSeries <- ggplot (stepsPerInterval, aes(x=interval, y=steps)) + geom_line() + labs(title = "Time Series Plot of Average Steps per Interval", x = "Interval", y = "Average Steps across All Days")

    timeSeries

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    dev.copy(png,'Plot4TimeSeries.png')

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

## Step 5: The 5-minute interval that, on average, contains the maximum number of steps

    # find 5-minute interval that, on average, contains the maximum number of steps
    intervalMaxSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]
    intervalMaxSteps

    ##     interval    steps
    ## 104      835 206.1698

## Step 6: Code to describe and show a strategy for imputing missing data

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

    # find missing values in dataset
    numberMissing <- nrow(activity[is.na(activity$steps),])
    numberMissing

    ## [1] 2304

Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

    # For simplicity, NA's are substituted based on average number of steps for that 5 minute interval and based on the specific weekday (out of 7 day week)

    activityCopy <- read.csv("activity.csv")
    activityCopy$Weekday <- weekdays(as.Date(activityCopy$date))

    # Compute average number of steps based on specific 5-minute interval and day of the week
    avgWeekday <- aggregate(steps ~ interval + Weekday, activityCopy, mean)
    naRows <- activityCopy[is.na(activityCopy$steps),]

    # Fill in NA's with averages computed above
    activityNew <- merge(naRows, avgWeekday, by=c("interval","Weekday"))

Create a new dataset that is equal to the original dataset but with the
missing data filled in.

    # filter on data without NA values
    validData <- activityCopy[!is.na(activityCopy$steps),]

    # Re-format filled in data to resemble original dataset
    activityNewUse <- activityNew[,c(5,4,1,2)]
    colnames(activityNewUse) <- c("steps","date","interval","Weekday")

    # Merge original & substituted data
    mergeData <- rbind(validData, activityNewUse)

## Step 7: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    # aggregate using filled data
    stepsPerDayFill <- aggregate(steps ~ date, mergeData, FUN = sum)

    plot6 <- ggplot (stepsPerDayFill, aes (x = steps)) +
      geom_histogram(fill = "blue", binwidth = 1000) +
        labs(title = "Histogram of Total Steps Taken Each Day Based on Fill Data", x = "Steps", y = "Frequency")

    plot6 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    dev.copy(png,'Plot6.png')

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

    meanStepsFill <- mean(stepsPerDayFill$steps, na.rm=TRUE)
    meanStepsFill

    ## [1] 10821.21

    medianStepsFill <- median(stepsPerDayFill$steps, na.rm=TRUE)
    medianStepsFill

    ## [1] 11015

    # Compute difference in mean
    meanStepsFill - meanSteps

    ## [1] 55.02092

    # Compute difference in median
    medianStepsFill - medianSteps

    ## [1] 250

## Step 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

Make a panel plot containing a time series plot (type = “l”) of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

    # assign Day of Week 
    mergeData$DayType <- 1
    mergeData$DayType[which(mergeData$Weekday %in% c("Saturday","Sunday"))] <- "Weekend"
    mergeData$DayType[which(mergeData$DayType=="1")] <- "Weekday"
                                 
    stepsPerIntervalFill <- aggregate(steps ~ interval + DayType, mergeData, mean)

    timeSeriesFill <- ggplot (stepsPerIntervalFill, aes(x=interval, y=steps)) + geom_line() + labs(title = "Time Series Plot of Average Steps per Interval for Weekday vs Weekend", x = "Interval", y = "Average Steps") + facet_grid(DayType~.)

    timeSeriesFill

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    dev.copy(png,'Plot8TimeSeries.png')

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2
