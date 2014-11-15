# Reproducible Research: Peer Assessment 1
Use whatever directory string is appropriate for you - after that, the following code will be referential for your environment.  
'''{r}
#Loading and preprocessing the data
mywd <-"~/Documents/Coursera/05-ReproducableResearch/05_RepData_PeerAssessment1"
'''


You can fork the GitHub repository https://github.com/rdpeng/RepData_PeerAssessment1     or pull the data from this web link.
'''{r}
if (!file.exists('activity.zip')) {  # this will be in place if you fork the repo
    file_URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(file_URL, "./activity.zip", method = "curl" )
}
    unzip("./data/data.zip", overwrite = TRUE)
    # unzip creates a file "activity.csv" in your working directory
'''    
    
###What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
