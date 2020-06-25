#Download the data and assign name
```{r}
pamd_activity <- read.csv("activity.csv")
```

#Summary of the data.
```{r}
head(pamd_activity)
```
#Processing the data

```{r}
pamd_activity$day <- weekdays(as.Date(pamd_activity$date))
pamd_activity$DateTime <- as.POSIXct(pamd_activity$date, format = "%Y-%M-%D") 
```

#Cleaning and Extracting the data
```{r}
clean_Data<- pamd_activity[!is.na(pamd_activity$steps),]
head(clean_Data)
```

#To calculate the total number of steps taken per day and summary.

```{r}
sumTable <- aggregate(pamd_activity$steps~pamd_activity$date, FUN = sum)
colnames(sumTable) <- c("Date", "Steps")
head(sumTable)
```

#Plot the histogram for the total number of steps per day

```{r}
hist(sumTable$Steps, breaks = 7,xlab = "Steps",main = "Aggregate Steps per Day")
```

#calculation of Mean and Median of the activity Dataset.

```{r}
as.integer(mean(sumTable$Steps))
```

```{r}
as.integer(median(sumTable$Steps))
```

#The Average Activity summary.
```{r}
clean <- pamd_activity[!is.na(pamd_activity$steps),]
```

#The needed R packages.
```{r}
library(plyr)
library(ggplot2)
```

#The Average number of steps per interval 
```{r}
intervalTable <- ddply(clean, .(interval), summarize, mean = mean(steps))
```

#Creating the line plot

```{r}
ye <- ggplot(intervalTable, aes(x=interval, y=mean), xlab = "interval", ylab = "Average Number of Steps")
ye + geom_line() + xlab("Average Number of Steps") + ggtitle("Average Number of Steps Per Interval")
```

```{r}
maxsteps<- max(intervalTable$mean)
# Interval that contains the maximum number of steps
intervalTable[intervalTable$mean == maxsteps,1]
```

#Check if there is any missing values.
```{r}
nrow(pamd_activity[is.na(pamd_activity$steps),])
```

#Number of steps per weekday and interval
```{r}
avgTable<- ddply(clean,.(interval,day),summarize, mean = mean(steps))
```


```{r}
pamd_data<- pamd_activity[is.na(pamd_activity$steps),]
```

```{r}
newdata<- merge(pamd_data,avgTable, by = c("interval","day"))

### Creating a dataset equal to the orignal dataset nut with the missing data filled in:
ndata3 <- newdata[,c(6,4,1,2,5)]
colnames(ndata3)<- c("steps","date","interval","day","DateTime")

## Merging the dataset 
merge <- rbind(clean, ndata3)

## Creating Sum of Steps per date to compare the data with step 1

sumTable2<- aggregate(merge$steps~merge$date, FUN = sum,)
colnames(sumTable2)<- c("Date","Steps")
```


#Calculate the mean and median steps taken per day.
```{r}
as.integer(mean(sumTable2$Steps))
as.integer(median(sumTable2$Steps))
```

#Histogram for the subgroup dataset.
```{r}
hist(sumTable2$Steps, breaks = 5,xlab = "Steps",main = "Total Steps",col = "Purple")
hist(sumTable$Steps, breaks = 5,xlab = "Steps",main = "Total Steps",col = "Brown",add = T)
legend("topright",c("Imputed Data","Non-NA Data"), fill = c("Purple","Brown"))
```

#Difference between Weekdays and Weekend for the Activity.
```{r}
merge$DayCategory <- ifelse(merge$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

## Time -series Plot of 5-min interval
library(lattice)
## Summarize data by interval and type of day
intervalTable2<- ddply(merge,.(interval,DayCategory),summarize, mean = mean(steps))
```

#Finally, here is the Panel Plot to show the difference between the activities performed.
```{r}
xyplot(mean~interval|DayCategory, data=intervalTable2, type="1",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

