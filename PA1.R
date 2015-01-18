activity <-read.csv("activity.csv")

totalsteps <- tapply(activity$steps, activity$date, sum)

hist(totalsteps$steps, 
    col = "red", 
    main = "Total Steps Taken Each Day", 
    xlab="Total Steps (Day)", 
    breaks = 20)
    
meansteps <- mean(totalsteps$steps)
mediansteps <- median(totalsteps$steps)

averageDaily <-tapply(activity$steps, activity$interval, mean)
plot(averageDaily, 
    type = "l", 
    xlab = "5-minute interval", 
    ylab = "average number of steps taken")

maxStep <- which.max(averageDaily$steps)

missingValues <- sum(is.na(activity))

imputedActivity<-activity
imputedActivity[is.na(imputedActivity[, 1]), 1] <-averageDaily[is.na(imputedActivity[, 1]),2]

imputedtotalsteps <- tapply(imputedActivity$steps, imputedActivity$date, sum)
hist(imputedActivity$steps, xlab = "imputed total number of steps", main = "imputed" )
imputedmeansteps <- mean(imputedtotalsteps$steps)
imputedmediansteps <- median(imputedtotalsteps$steps)

library(lattice)
imputedActivity$day <-as.factor(ifelse(weekdays(imputedActivity$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
plot(steps ~ interval | day, aggregate(steps ~ interval + day, imputedActivity, FUN = mean), layout = c(1, 2), type = "l", group=day)
