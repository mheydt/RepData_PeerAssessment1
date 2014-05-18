library(scales)
library(ggplot2)
library(plyr)
library(lattice)

# read data
d = read.csv("activity.csv")

# convert from factors
d$interval <- as.numeric(as.character(d$interval))
d$steps <- as.numeric(as.character(d$steps))
d$date <- as.Date(d$date)

# it should be ordered already by day and interval, but I'll do it anyway
d <- d[with(d, order(date, interval)),]

# and add in a row id to help re-sort
#d$rowid <- 1:nrow(d)

# the interval in the data is wonky - they are not continuous
# 0 - 55, 100-155, 200-255, ...
# hundreds = hour of the day
# remainder of value / 100 is the minutes into the hour
# so I'm going to remap them to be continuous from 1 through 288
# this is important for mapping NAs to averages later in the analysis
#d$actualInterval <- trunc(d$interval / 100) * 12 + (d$interval %% 100)/5

stepsByDay <- aggregate(steps ~ date, d, sum)
ggplot(stepsByDay, aes(x=date, y=steps)) + geom_bar(stat="identity")

meanStepsPerDay <- mean(stepsByDay$steps)
medianStepsPerDay <- median(stepsByDay$steps)

# calculate the mean steps in each interval
#meanStepsByActualInterval <- aggregate(steps ~ interval, d, mean)
meanStepsByInterval <- aggregate(steps ~ interval, d, mean)

#ggplot(meanStepsByActualInterval, aes(interval)) + geom_line(aes(y=steps))
ggplot(meanStepsByInterval, aes(interval)) + geom_line(aes(y=steps))

# determine the actual interval with the maximum mean # of steps
#maxActualInterval <- which.max(meanStepsByActualInterval$steps) 
#maxInterval <- trunc(maxActualInterval / 12) * 100 + (maxActualInterval %% 12) * 5
maxInterval <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]$interval
#maxInterval <- trunc(maxActualInterval / 12) * 100 + (maxActualInterval %% 12) * 5
# meanStepsByDayAndIntervalOrdered <- avgStepsByDayAndInterval[with(avgStepsByDayAndInterval, order(date, interval)),]

# identify and count the number of NA's
nas <- is.na(d$steps)
whichNAs <- which(is.na(d$steps))
numNAs <- length(whichNAs)

#intervalForNAs <- d$interval[whichNAs]

#t <- merge(d, meanStepsByActualInterval, by="actualInterval")
t <- merge(d, meanStepsByInterval, by="interval")
t <- t[with(t, order(date, interval)),]

#t <- t[order(t$rowid),]
t$steps.x[is.na(t$steps.x)] <- t$steps.y[is.na(t$steps.x)]
names(t)[names(t)=="steps.x"] <- "steps"
t <- t[,-which(names(t) %in% c("steps.y"))]

imputedStepsByDay <- aggregate(steps ~ date, t, sum)
imputedMeanStepsPerDay <- mean(imputedStepsByDay$steps)
imputedMedianStepsPerDay <- median(imputedStepsByDay$steps)

ggplot(imputedStepsByDay, aes(x=date, y=steps)) + geom_bar(stat="identity")

weekendDays = c("Saturday", "Sunday")
#factormap <- data.frame(c(TRUE, FALSE), c("weekend", "weekday"))
#names(factormap)[1] <- "IsWeekend"
#names(factormap)[2] <- "Label"

isweekend <- weekdays(t$date) %in% weekendDays
#labels <- sapply(isweekend, function(x) if (x[1]) "weekend" else "weekday")
#t$f <- factor(labels)
t$isweekend <- isweekend

weekdayMeans <- aggregate(steps ~ interval, t[!t$isweekend,], mean)
weekendMeans <- aggregate(steps ~ interval, t[t$isweekend,], mean)
weekdayMeans$isweekend <- FALSE
weekendMeans$isweekend <- TRUE

comparison <- rbind(weekdayMeans, weekendMeans)
isweekend <- comparison$isweekend
isweekend <- factor(isweekend, labels=c("weekday", "weekend"))

x <- comparison$interval
y <- comparison$steps
xyplot(y ~ x | isweekend, layout=c(1,2), type='l')
