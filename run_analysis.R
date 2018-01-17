## 1.Merges the training and the test sets to create one data set.

## Download file and uzip in working directory
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "activity_data.zip")
unzip("activity_data.zip", exdir = "activity_data")
getwd()
list.files("activity_data")
list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

library(dplyr) 

##read activity lables
activity_labels <- read.table("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="", col.names = c("ActivityId", "ActivityName"))
activity_labels[,2] <- as.character(activity_labels[,2])

##read data features
features <- read.table("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/features.txt", quote="\"", comment.char="", col.names = c("FeatureId", "FeatureName"))
features[,2] <- as.character(features[,2])

# read and merge train data 

features_train <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/train/X_train.txt", sep="", col.names = features$FeatureName)

activity_train <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/train/y_train.txt", sep="", col.names = c("ActivityId"))

subject_train <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/train/subject_train.txt", sep="", col.names = c("SubjectId"))

data_train <- tbl_df(cbind(subject_train, activity_train, features_train))

##read and merge test data

features_test <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/test/X_test.txt", sep = "", col.names = features$FeatureName)

activity_test <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/test/y_test.txt", sep = "", col.names = c("ActivityId"))

subject_test <- read.csv("C:/Users/hnaty_000/Desktop/CourseraEDA/activity_data/UCI HAR Dataset/test/subject_test.txt", sep = "", col.names = c("SubjectId"))

data_test <- tbl_df(cbind(subject_test, activity_test, features_test))

##merging the training and the test sets to create one data set
dataset <- rbind(data_test, data_train) %>%
                inner_join(activity_labels, by = "ActivityId") 

## 2.Extracts only the measurements on the mean and standard deviation 
##for each measurement. 

extract_data <-select(dataset, 1:2, contains("mean.."), contains("std.."), -starts_with("angle"))

## 3.Uses descriptive activity names to name the activities in the data set
extract_data$ActivityId  <- factor(extract_data$ActivityId, levels = activity_labels[,1], labels = activity_labels[,2])
  
## 4.Appropriately labels the data set with descriptive variable names.
extract_data <- extract_data %>% 
              setNames(gsub("^t", "TimeDomain", names(.))) 

extract_data <- extract_data %>% 
            setNames(gsub("^f", "FrequencyDomain", names(.)))

extract_data <- extract_data %>% 
            setNames(gsub("Acc", "Accelerometer", names(.))) 
              
extract_data <- extract_data %>% 
            setNames(gsub("Gyro", "Gyroscope", names(.))) 
  
extract_data <- extract_data %>% 
            setNames(gsub("Mag", "Magnitude", names(.)))
  
extract_data <- extract_data %>%
            setNames(gsub("mean\\.\\.", "Mean", names(.))) 

extract_data <- extract_data %>%    
            setNames(gsub("std\\.\\.", "Std", names(.))) 

extract_data <- extract_data %>% 
            setNames(gsub("\\.", "", names(.))) 

## 5.From the data set in step 4, creates a second, independent tidy data 
## set with the average of each variable for each activity and each subject.

tidy_data <- extract_data %>% group_by(SubjectId, ActivityId)
tidy_data <- tidy_data %>% summarize_all(funs(mean))

write.table(tidy_data, "tidy_data.txt", row.names = FALSE, quote = FALSE)