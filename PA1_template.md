---
title: "Reproducible Research Assignment: Course Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data [52K]
 
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

### 1. Code for reading in the dataset and/or processing the data

- Loading the Libraries
```{r library, echo=TRUE}
## libraries
library(ggplot2)
library(lattice)
library(dplyr)
```

- Loading the Data
```{r data, echo=TRUE}
## get data
activity<- read.csv("activity.csv")  
head(activity)
dim(activity)  
summary(activity) 

# Convert date field from factor to date
activity$date <- as.Date(activity$date)
```

### 2. Histogram of the total number of steps taken each day

- Create and print number of steps per day
```{r histo1, echo=TRUE}
StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
head(StepsPerDay)
```

- Creating the historgram of total steps per day
```{r histo1a, echo=TRUE}
plot(StepsPerDay, type="h", main="Total steps per day", xlab="Date", ylab="Steps per Day", col="blue", lwd=5)
```

### 3. Mean and median number of steps taken each day

- Calculate and report the mean and median of the total number of steps taken per day
```{r histo1b, echo=TRUE}
MeanSteps<- mean(StepsPerDay$Steps, na.rm=TRUE)
MeanSteps

MedianSteps<- median(StepsPerDay$Steps, na.rm=TRUE)
MedianSteps
```

### 4. Time series plot of the average number of steps taken

- Calculating and plotting the average daily activity pattern by interval
```{r avrday, echo=TRUE}
MeanByInt <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
plot(row.names(MeanByInt),MeanByInt,type="l",xlab="Time intervals",ylab="Steps",
     main="Frequency of Steps Taken by Interval", col="blue", lwd=2)
```

### 5. The 5-minute interval that, on average, contains the maximum number of steps

- The mean and the median for the total number of steps per day 
```{r meanday, echo=TRUE}
MeanByInt[match(max(MeanByInt),MeanByInt)]
```

The maximum number of steps for a 5-minute interval was **206** steps and the interval which had the maximum number of steps was the **835** interval.

### 6. Code to describe and show a strategy for imputing missing data

- First I create a new dataset and count the missing value
```{r newdataset, echo=TRUE}
# new data frame
activity2 <- activity
# Calculate total number of missing values
sum(is.na(activity2$steps))
```

- I decide to replace the missing value by the **mean of the day**, each day without step will have a mean = 0
```{r strategy, echo=TRUE}
# Search all NA steps
nas<- is.na(activity2$steps)
# Calculate all mean per day
MeanPerDay<- tapply(activity2$steps, activity2$date, mean, na.rm=TRUE, simplify = TRUE)
# For day without steps the mean = NA, set then to 0  
MeanPerDay<- ifelse(is.na(MeanPerDay), 0, MeanPerDay)
# Add the steps to a new column fsteps
activity2$fsteps <- activity2$steps
# Replace the missing value by the mean of the day
activity2$fsteps[nas] <- MeanPerDay[as.character(activity2$date[nas])]

head(activity2)
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r StepsPerDay, echo=TRUE}
StepsPerDay2 <- aggregate(activity2$fsteps, list(activity2$date), FUN=sum)
colnames(StepsPerDay2) <- c("Date", "Steps")

plot(StepsPerDay2, type="h", main="Total steps per day (including NA)", xlab="Date", ylab="Steps", col="red", lwd=5)
```

- Show the Mean and the Median of the new Dataset
```{r StepsPerDay2, echo=TRUE}
## Calculate and report the mean and median of the total number of steps taken per day
MeanSteps2<- mean(StepsPerDay2$Steps, na.rm=TRUE)
MedianSteps2<- median(StepsPerDay2$Steps, na.rm=TRUE)

paste("New Mean of daily step : ",MeanSteps2," , with NA :",MeanSteps)
paste("New Median of daily step : ",MedianSteps2," , with NA :",MedianSteps)
```

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

- Add a new column for the Weekday value and the Day type value
```{r Steps8-1, echo=TRUE}
# Add Weekday in dataset table
activity2$WeekDay <- weekdays(activity2$date)

# Add Day Type in dataset table
activity2$DayType <- ifelse(weekdays(activity2$date)=="Saturday","Weekend", 
                             ifelse(weekdays(activity2$date)=="Sunday","Weekend", "Weekday"))
head(activity2)
```

- Weekdays and Weekends comparison plot
```{r Steps8-2, echo=TRUE}
ActDayType <- aggregate(fsteps ~ DayType+interval, data=activity2, FUN=mean)

xyplot(fsteps ~ interval | factor(DayType), layout = c(1, 2),
       xlab="Interval", ylab="Steps", type="l", lty=1, data=ActDayType)
```


