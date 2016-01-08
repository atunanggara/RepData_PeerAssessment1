# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
ActData<-read.csv("~/Documents/LearningR/ReproducibleResearch/Project1/activity.csv",header=TRUE,na.strings="NA",stringsAsFactors = FALSE)
dateData<-as.Date(ActData$date,"%Y-%m-%d")
ActData$date<-dateData
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

 Melt the data using reshape2

```r
library(reshape2)
meltedData<-melt(ActData,is=c("interval","date"),measure.vars="steps",na.rm=TRUE)
castedData<-dcast(meltedData,date~variable,sum)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(castedData$steps,main="Histogram of Total Steps per Day",xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/hist_1-1.png) 

 3. Calculate and report the mean and median of the total number of steps taken per day

```r
# we computed the mean and median of the total steps per day
mean(castedData$steps)
```

```
## [1] 10766.19
```

```r
median(castedData$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

 Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meancastedData<-dcast(meltedData,interval~variable,mean)
plot(meancastedData$interval,meancastedData$steps,type="l")
```

![](PA1_template_files/figure-html/cast_melt_1-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meancastedData[meancastedData$steps==max(meancastedData$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
# That would be interval 835 with the maximum step of 206.2
```

## Inputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NainSteps<-is.na(ActData$steps)

### To compute, we add the number of True statement up (since TRUE = 1 and FALSE = 0) 
sum(NainSteps)
```

```
## [1] 2304
```

```r
### It totaled into 2304 values that are missing in the dataset
```

2. Devise a strategy for filling in all of the missing values in the dataset.  The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy: will use the average from the 5-minute interval taken in the 3rd portion of this project. However, I will round all the average into integer


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Subset the data for all rows that are NAs

```r
NAdata<- ActData[NainSteps,]

ReplaceData<-merge(NAdata,meancastedData,by="interval")
ReplaceData$steps.x<-NULL
names(ReplaceData)<-c("interval","date","steps")
ReplaceData<-ReplaceData[c(3,2,1)]
ReplaceData$steps<-as.integer(ReplaceData$steps)

### Let's Remove all the NAs from the Original Data
WONAData<-ActData[!is.na(ActData$steps),]
FinalData<-rbind(ReplaceData,WONAData)
FinalData<-FinalData[order(FinalData$date,FinalData$interval),]
```

4. Make a histogram of the total number of steps taken each day and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of inputing missing data on the estimates of the total daily number of steps?


```r
FinalmeltedData<-melt(FinalData,is=c("interval","date"),measure.vars="steps",na.rm=TRUE)
FinalcastedData<-dcast(FinalmeltedData,date~variable,sum)
hist(FinalcastedData$steps,main="Histogram of Total Steps per Day",xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/hist_2-1.png) 

```r
mean(FinalcastedData$steps)
```

```
## [1] 10749.77
```

```r
### The mean is: 10749.77
median(FinalcastedData$steps)
```

```
## [1] 10641
```

```r
### The median is: 10641
### The mean and median value decreased from the 1st part
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
WeekValue<-weekdays(FinalData$date)
DayoftheWeek<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
WeekDayEnd<-c(rep("Weekday",5),rep("Weekend",2))
VariablesWeekDayEnd<-data.frame(DayoftheWeek,WeekDayEnd)
names(VariablesWeekDayEnd)<-c("WeekValue","WeekDayEnd")
WDayFinalData<-cbind(FinalData,WeekValue)
WdayAllData<-merge(WDayFinalData,VariablesWeekDayEnd,by="WeekValue")
str(WdayAllData)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ WeekValue : Factor w/ 7 levels "Friday","Monday",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ steps     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date      : Date, format: "2012-10-05" "2012-11-16" ...
##  $ interval  : int  2220 430 2235 435 2315 445 2255 2300 2305 2245 ...
##  $ WeekDayEnd: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
WdayAllData$WeekValue<-NULL
```

2. Make a panel plot containing a time series plot(i.e. type="l") of the  5-minute interval (x-axis) and the average number of steps taken averaged across all weekdays days or weekend days (y-axis).


```r
WdaymeltedData<-melt(WdayAllData,is=c("interval","date","WeekDayEnd"),measure.vars="steps")
WdaymeancastedData<-dcast(WdaymeltedData,interval+WeekDayEnd~variable,mean)
library(ggplot2)
ggplot(WdaymeancastedData,aes(x=interval,y=steps))+geom_line()+facet_grid(.~WeekDayEnd)
```

![](PA1_template_files/figure-html/PanelPlot-1.png) 
