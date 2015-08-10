---
title: "PA1_template"
author: "Patricialrt"
date: "7 Aug 2015"
output: html_document
---

**Prerequisites**     
A) I downloaded the data (activity.csv) given in the assignment's description on coursera to my working directory

Dataset: Activity monitoring data [52K] (activity.csv)     
The variables included in this dataset are:      
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)     
- date: The date on which the measurement was taken in YYYY-MM-DD format      
- interval: Identifier for the 5-minute interval in which measurement was taken    

B) Load the following libraries    
library(knitr) 
library(ggplot2)   
library(lubridate) 
library(plyr)
library(dplyr)

# Loading and preprocessing the data    
Show any code that is needed to     
- Load the data (i.e. read.csv())         
- Process/transform the data (if necessary) into a format suitable for your analysis       

**reading data and familiarizing with it**    
```{r readdata, echo = TRUE}       
activity <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "character","integer")) #reading data   
head(activity) #return the first part of activity data    
str(activity) #display structure of activity data    
summary(activity) #produce result summary of activity data    
```

**subset complete values and define structure of date**     
Getting rid of NAs as requested in some of the tasks below
```{r subsetcomplete, echo = TRUE}     
komplett <- subset(activity, complete.cases(activity))  
komplett$date <- ymd(komplett$date)
```    


# What is mean total number of steps taken per day?    

For this part of the assignment, ignore the missing values in the dataset.    
- Make a histogram of the total number of steps taken each day  
- Calculate and report the mean and median total number of steps taken per day   

**eliminate NA from date**   
```{r stepspday, echo = TRUE}    
steps.day<-split(komplett,komplett$date, drop=TRUE)            
```

**daily sum of steps and removing NAs**     
```{r sumstepsdaily, echo = TRUE}    
sum.stepsdaily <-sapply(steps.day, function(x) sum(x$steps))    
```

**plotting the histogram**    
```{r histostep, echo = TRUE}      
hist(sum.stepsdaily, main="Total Number of Steps per Day", xlab= "Steps per day", col="grey")    
```

**calculating and return mean and median**      
```{r meanmeadian, echo = TRUE}    
summary(sum.stepsdaily)  
mean(sum.stepsdaily)
median(sum.stepsdaily)
```

# What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)        
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?       

**eliminate NA from interval**      
```{r completeintverval, echo = TRUE}    
intervall <- split(komplett,komplett$interval, drop=TRUE)    
```

**average steps per interval**
```{r averagestepsperinterval, echo = TRUE}       
avg.intervall <- sapply(intervall, function(x) mean(x$steps))    
```

**plotting**
```{r plot, echo = TRUE}  
plot(avg.intervall, type="l", main="5' Interval Time", 
ylab="Average Number of Steps", xlab="Interval", col="magenta")                 
```     
 
**finding the interval out of avg.interval with maximum number of steps**    
```{r maxsteps, echo = TRUE}
which.max(avg.intervall) # returning interval and index
round(max(avg.intervall)) # maximum average number of steps
```

# Imputing missing values

Note that there are a number of days/intervals where there are missing values     (coded as NA). The presence of missing days may introduce bias into some     calculations or summaries of the data.    

- Calculate and report the total number of missing values in the dataset    
(i.e. the total number of rows with NAs)       

- Devise a strategy for filling in all of the missing values in the dataset.     
The strategy does not need to be sophisticated. For example, you could use the      mean/median for that day, or the mean for that 5-minute interval, etc.   

- Create a new dataset that is equal to the original dataset but with the missing data filled in.    

- Make a histogram of the total number of steps taken each day and Calculate and    report the mean and median total number of steps taken per day. Do these values    differ from the estimates from the first part of the assignment? What is the    impact of imputing missing data on the estimates of the total daily number of steps?    

**how many NAs**
```{r howmanyNAs, echo TRUE}
sum(is.na(activity))
```






