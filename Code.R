library(ggplot2)
unzip("activity.zip")
baseData <- read.csv("activity.csv")
head(baseData)

#1
dim(baseData)
summary(baseData)
baseData$date <- as.Date(as.character(baseData$date))
baseDataNA <- is.na(baseData$steps)
cleanBase <- baseData[!baseDataNA,]

SummedDataByDay <- aggregate(baseData$steps, by=list(baseData$date), sum)
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
head(SummedDataByDay,15)

#2
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

#3
mean(SummedDataByDay$totalsteps,na.rm=TRUE)
median(SummedDataByDay$totalsteps,na.rm=TRUE)

nonNASubset <- baseData[!baseDataNA,]
MeanDataByInterval <- aggregate(nonNASubset$steps, by=list(nonNASubset$interval), mean)
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"
ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="red") 

maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval

missingVals <- sum(baseDataNA)
missingVals
baseData2 <- baseData
NABase2 <- baseData2[is.na(baseData2$steps),]
cleanBase2 <- baseData2[!is.na(baseData2$steps),]

MeanData2ByInterval <- aggregate(cleanBase2$steps, by=list(cleanBase2$interval), sum)
names(MeanData2ByInterval)[1] ="interval"
names(MeanData2ByInterval)[2] ="steps"

baseData2 <- baseData
missingData <- is.na(baseData2$steps)
meanVals <- tapply(cleanBase$steps, cleanBase$interval, mean, na.rm=TRUE, simplify=TRUE)
baseData2$steps[missingData] <- meanVals[as.character(baseData2$interval[missingData])]

sum(missingData)
sum(is.na(baseData2$steps))

##4

FullSummedDataByDay <- aggregate(baseData2$steps, by=list(baseData2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

mean(FullSummedDataByDay$totalsteps)
median(FullSummedDataByDay$totalsteps)

baseData2$weekday <- weekdays(baseData2$date)
baseData2$weekend <- ifelse (baseData2$weekday == "Saturday" | baseData2$weekday == "Sunday", "Weekend", "Weekday")
head(baseData2,5)
MeanDataWeekendWeekday <- aggregate(baseData2$steps, by=list(baseData2$weekend, baseData2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
