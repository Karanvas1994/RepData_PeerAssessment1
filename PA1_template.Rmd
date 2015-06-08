#Reproducible Research : Peer Assessment 1

##Loading & Preprocessing the Data

```{r Load}
setwd("~/Rep_Research 1") #Just a name of the Directory in which I am working
unzip("repdata-data-activity.zip")
actdata <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
head(actdata)
```

##Total Number of Steps Taken Each Day

```{r Steps}
library(lattice)
stepsbyday <- aggregate(steps ~ date, data = actdata, sum, na.rm = TRUE)
head(stepsbyday)
hist(stepsbyday$steps, main = "Total steps by day", xlab = "date", col = "blue",breaks=10)
mean(stepsbyday$steps)
median(stepsbyday$steps)
```

##Average Daily Activity Pattern

```{r pattern}

timepattern <- tapply(actdata$steps, actdata$interval, mean, na.rm = TRUE)
head(timepattern)
#So,We have to plot between rownames of timepattern and timepattern itself.
plot(row.names(timepattern), timepattern, type = "l", xlab = "5-minutes interval", 
    ylab = "Avg. across all Days(after removal of NAs)", main = "Average number of steps taken", col = "orange")

#For Max. Interval
names(which.max(timepattern))
```

##Imputing Missing Values

- Total NA Values
```{r totNA}
sum(is.na(actdata))
```

- Now, We have to fill the missing values with stepwise-mean

```{r meanfill}
#Making a Duplicate of actdata
newactdata <- actdata

library(Hmisc) #For impute() Function

newactdata$steps <- impute(actdata$steps,fun=mean)
head(newactdata)
stepsbyday2 <- aggregate(steps ~ date, data = newactdata, sum)
hist(stepsbyday2$steps, main = "Total steps by day", xlab = "date", col = "blue",breaks=10)
mean(stepsbyday2$steps)
median(stepsbyday2$steps)
```
##### So,Mean is same in both cases but the median is slightly different from the previous data.

```{r subtract}
#Difference in Median:
median(stepsbyday2$steps)-median(stepsbyday$steps)
```
##Weekdays and Weekend Problem

```{r last}
day <- weekdays(actdata$date)
typeofday <- c(rep("a",17568))
for (i in 1:17568) {
     if (day[i] == "Saturday") {
         typeofday[i] <- "Weekend"
     } else if (day[i] == "Sunday") {
         typeofday[i] <- "Weekend"
     } else {
         typeofday[i] <- "Weekday"
     }
 }
actdata$typeofday <- typeofday
actdata$typeofday <- factor(actdata$typeofday)

final <- aggregate(steps ~ interval + typeofday, data = actdata, mean)

xyplot(steps ~ interval | typeofday, final, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```