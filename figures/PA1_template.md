---
title: "PeerAssignment1"
author: "MMR"
date: "December 20, 2015"
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---

## Loading and preprocessing the data

```{r, echo=TRUE}
setwd("~/Desktop/Data_Science_Course/Reproducible Research/PeerAssessment 1")
unzip("repdata-data-activity.zip", exdir =".", unzip = "internal", setTimes = TRUE)
myfile <- read.csv("activity.csv")
library(dplyr) 
library(ggplot2)
mytable <- myfile %>% group_by(date, interval) %>% summarise(steps= sum(steps))
p1 <- ggplot(data = mytable) + 
aes(x = date, y = steps)+
geom_histogram(stat = "identity", fill = "black")
 p1 + theme(axis.text.x = element_text(angle = 45))
 meanstepspd <- mytable %>% group_by(date) %>% summarise(steps= mean(steps))
 summary(meanstepspd)
 ```
 ## The mean steps per day is 10766 and median is 10765. 
 
 ## What is the average daily activity pattern?
 
 ## Average steps taken per day 
 mytable2 <- myfile %>% group_by(interval, date) %>% summarise(steps = mean(steps))
 plot(mytable2$interval, mytable2$steps, xlab = "interval", ylab = "mean steps per day",  type = "l")
 summary(mytable2)
 
 ##  as in the summary of mytable2, we get the maximum number of steps taken in an interval is 806. It is confirmed by 
 ```{r, echo = TRUE}
 mytable2 <- myfile %>% group_by(interval, date) %>% summarise(steps = mean(steps))
 max_steps <- max(mytable2$steps, na.rm = TRUE)
 max_steps
 ```
 
 ## The corresponding 5-minute interval is 615, as detected by :
 ```{r, echo= TRUE}
 mytable2$interval[match(806, mytable2$steps)]
 ```
 ## Imputing missing values
For simplicity, the missing values for the steps are replaced with the mean steps/interval as follows
 ```{r, echo = TRUE}
 newtable <- mytable ## makes a copy the original data
 replacement <- mean(mytable$steps, na.rm = TRUE) ## calculates the mean for steps to be used as replacement
 newtable$steps[is.na(newtable$steps)] <- replacement ## replaces the missing values with the replacement value
 miss_val <- sum(is.na(newtable$steps))  ## to confirm there is no missing value
 miss_val

 p2 <- ggplot(data = newtable) +             ## plotting the histogram
 aes(x = date, y = steps)+
 geom_histogram(stat = "identity", fill = "black")
 p2 + theme(axis.text.x = element_text(angle = 45))
```
 ## The mean and median steps are 
 ```{r, echo= TRUE}
 newmeansteps <- mean(newtable$steps)
 newmediansteps <- median(newtable$steps)
 print(c(newmeansteps,newmediansteps))
  ```
  
 ## to compare the mean, median and other features in the two tables
 ```{r, echo = TRUE}
 summary(mytable)
 summary(newtable) 
 ```
 ## The mean steps per day is 10766 and median is 10766. 
 ## There is no change in the mean. The median changes by 1 step only. 
 ## There is no significant impact for imputing the missing data on the estimates of the total daily number of steps.
 
 
##  Are there differences in activity patterns between weekdays and weekends?
 ```{r, echo = TRUE}
 datetable <- mutate(newtable, day= as.Date(date))
 stepsdate <- select(datetable, interval, steps, day)
 stepsdateday <- mutate(stepsdate, WEEKDAY = weekdays(day))
 stepsdateday$WEEKDAY <- factor(stepsdateday$WEEKDAY)
 days <- list(weekend = c("Saturday", "Sunday"), weekday= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
 levels(stepsdateday$WEEKDAY) <-days
 weekdayactivity <- stepsdateday %>% group_by(interval, WEEKDAY) %>% summarise(steps = mean(steps))
 meanweekdayactivity <- weekdayactivity %>% group_by(WEEKDAY) %>% summarise(steps = mean(steps))
 meanweekdayactivity
 ```
 ## Average number of steps taken in weekend is 42.366 and the same on weekday is 35.610. A slightly higher activity in the weekend. 
 
 ```{r, echo = TRUE}
 par(mfrow = c(2, 1))
 plot(mytable$interval, mytable$steps, xlab = "interval", ylab = "mean steps per day",  type = "l")
 plot(weekdayactivity$interval, weekdayactivity$steps, xlab = "interval", ylab = "mean steps per day",  type = "l")
 ```