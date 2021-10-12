# 1. Merge the training and the test sets to create one data set.

mergeTrainingAndTestDatasets <- function() {
  X_test <- read.fwf("data/raw/test/X_test.txt", rep(c(-1,15), 561))
  y_test <- readLines("data/raw/test/y_test.txt")
  subject_test <- readLines("data/raw/test/subject_test.txt")
  
  X_train <- read.fwf("data/raw/train/X_train.txt", rep(c(-1,15), 561))
  y_train <- readLines("data/raw/train/y_train.txt")
  subject_train <- readLines("data/raw/train/subject_train.txt")
  
  X <- rbind(X_test, X_train)
  y <- c(y_test, y_train)
  subject <- c(subject_test, subject_train)
  
  data <- cbind(subject, y, X)
}

d <- mergeTrainingAndTestDatasets()

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 4. Appropriately labels the data set with descriptive variable names. 

assignMeaningfulNamesToVariables <- function(activityData) {
  features <- read.delim("data/raw/features.txt", sep=" ", header = FALSE)
  names(activityData) <- c("subject", "activity", features$V2)
  activityData
}

d <- assignMeaningfulNamesToVariables(d)

library(dplyr)
extractMeanAndStd <- function(activityData) {
  select(activityData, contains("subject") | contains("activity") | contains("mean()") | contains("std()"))
}

d <- extractMeanAndStd(d)

# 3. Uses descriptive activity names to name the activities in the data set
decodeActivities <- function(activityData) {
  activities <- read.delim("data/raw/activity_labels.txt", sep=" ", header = FALSE)
  activityData$activity <- factor(activityData$activity, levels=activities$V1, labels=activities$V2)
  activityData
}

d <- decodeActivities(d)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dm <- d %>% group_by (subject, activity) %>% summarize_at(vars(contains("std") | contains("mean")), mean)
write.table(dm, "data/tidy/activityMeans.txt", sep=" ", row.names=FALSE)