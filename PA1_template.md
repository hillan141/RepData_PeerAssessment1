# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
require(lattice)
if (!file.exists("activity.zip"))
    unzip("acivity.zip")
steps <- read.csv("activity.csv")
smat <- tapply(steps$steps, list(steps$date, steps$interval), function(z)z)
```

## What is mean total number of steps taken per day?

```r
steps.perday <- sapply(split(steps$steps, steps$date),sum, na.rm=TRUE)
steps.med <- median(steps.perday, na.rm=TRUE)
steps.mean <- mean(steps.perday, na.rm=TRUE)
hist(steps.perday, main="Distribution of daily steps", 
     xlab="Steps per day", breaks=12)
```

![](PA1_template_files/figure-html/count-1.png) 
  
The median steps per day is: ***10395***  
The mean steps per day is: ***9354.2295082***  

## What is the average daily activity pattern?


```r
msi <- apply(smat,2,mean,na.rm=TRUE)
maxint <- names(msi)[which.max(msi)]
plot(names(msi), msi, xlab="5 minute interval within day", type="l", ylab="average # steps")
abline(v=names(msi)[which.max(msi)])
text(names(msi)[which.max(msi)], 150, sprintf("most active interval: %s", names(msi)[which.max(msi)]), pos=4)
```

![](PA1_template_files/figure-html/daily-1.png) 

The interval with the maximum average steps has identifier: ***835***  

## Imputing missing values

```r
myimpute <- function(z) {
  z[!is.finite(z)] <- median(z, na.rm=TRUE)
  z
}
smat.i <- apply(smat, 2, myimpute)
stopifnot(all(is.finite(smat.i)))
day.sum <- apply(smat.i, 1, sum)
day.mean <- mean(day.sum)
day.med <- median(day.sum)
hist(day.sum, main="Distribution of daily steps (imputed)", 
     xlab="Steps per day", breaks=12)
```

![](PA1_template_files/figure-html/impute-1.png) 

The median steps per day (after imputing) is: ***10395***  
The mean steps per day (after imputing) is: ***9503.8688525***  

These values are very similar to the estimates from the first part of the assignment.  So there is little impact of imputing missing data on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

The following code creates the panel plot comparing daily activity patterns on weekdays versus weekends.


```r
wd <- ifelse(weekdays(strptime(rownames(smat.i), "%Y-%m-%d")) %in% 
               c("Saturday", "Sunday"), "weekend", "weekday")
is <- split(1:nrow(smat.i), wd)
mdd <- lapply(is, function(ii) apply(smat.i[ii, ], 2, mean))
par(mfrow=c(2,1), mar=c(4,4,1,2), cex=0.8)
for (i in names(mdd)) {
  plot(names(mdd[[i]]), mdd[[i]], type="l", 
       xlab="Interval", ylab="Number of steps",
       ylim=c(0,200))
  text(x=1000,y=190, i,pos=4, cex=1.5)
}
```

![](PA1_template_files/figure-html/weekends-1.png) 

