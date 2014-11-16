# This file contains the working tests that I used to arrive at the final Rmd code.
## Loading and preprocessing the data
# use my path, or set you own in its place
myWD <- ("~/Documents/Coursera/05-ReproducableResearch/05_RepData_PeerAssessment1")
# Use whatever directory string is appropriate for you - the following will be referential
setwd(myWD)
# You can pull the data from the course web link or fork the GitHub repository
# https://github.com/rdpeng/RepData_PeerAssessment1.
if (!file.exists('activity.zip')) {  # this will be in place if you fork the repo
    file_URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(file_URL, "./activity.zip", method = "curl" )
    pulldate <- date()
} else {
    unzip("./activity.zip", overwrite = TRUE)
    unzipdate <- date()
}
data <- read.csv("./activity.csv")
# Validate the data structure 
str(data[, 1:3])  #  list out a few rows and all columns
# Prepare data for following analysis - restructure date and interval into Date and Time



## Requirement 1 --  What is mean total number of steps taken per day?
# 1) Make a histogram of the total number of steps taken each day
meanSPD <- round(mean(stepsPerDay$steps, na.rm = TRUE),0)
medianSPD <- median(stepsPerDay$steps, na.rm = TRUE)
plot(x = as.factor(stepsPerDay$date), y = stepsPerDay$steps, type = "h")
g <- ggplot(stepsPerDay, aes(date, steps))
g + geom_bar(stat = "identity") + 
    theme_bw() +
    geom_hline(yintercept = meanSPD, colour = "red") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    labs(title = "Steps Per Day") +
    labs(x = "Date") +
    labs(y = "Total Steps")
print(g)
# 2) Calculate and report the mean and median total number of steps taken per day


## Requirement 2 -- What is the average daily activity pattern?
# 1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
# the average number of steps taken, averaged across all days (y-axis)

# 2) Which 5-minute interval, on average across all the days in the dataset, contains
# the maximum number of steps?


## Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the
# total number of rows with NAs)
temp <- numeric()
NA_count <- for (d in data) {
    if (is.na(d) == TRUE) {temp <- temp + 1}
}

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Are there differences in activity patterns between weekdays and weekends?
