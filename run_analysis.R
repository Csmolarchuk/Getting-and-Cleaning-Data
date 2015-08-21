##This code downloads and creates a tidy data set of the UCI HAR Dataset
#The final output is a summary mean for all the mean variables by activity type and subject

# Download and unzip the dataset

setwd("C:/Users/Chrissy/Documents/R/UCI HAR Dataset")


file <- "project_dataset.zip"

if (!file.exists(file)){
  Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(Url, file, method="libcurl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(file) 
}

#Load dplyr package for merging data
library(dplyr)

# Read in data

features <- read.table("./features.txt")
#This file contains the labesl for activities 1-6 in y file
activity_labels <- read.table("./activity_labels.txt")

#Test - contains 30% of the entries for the dataset (IDs 1-30)

#Subject ID
subject_test <- read.table("./test/subject_test.txt", header=FALSE)
#This file contains the data
x_test <- read.table("./test/x_test.txt", header=FALSE)
#This file contains the type of activity for each row in x_test (1-6)
y_test <- read.table("./test/y_test.txt", header=FALSE)

#Label Activity ID with Activity Type
y_test1<-merge(activity_labels, y_test, by.x="V1", by.y="V1", all=TRUE)
               
#add labels to dataset
colnames(subject_test)<- c("ID")
colnames(x_test)<- features[,2]
colnames(y_test1)<- c("Activity_ID", "Activity_Type")

#Merge test data set

test_data <- cbind(subject_test, y_test1, x_test)

#Train - contains 80% of the entries for the dataset (IDs 1-30)

subject_train <- read.table("./train/subject_train.txt", header=FALSE)
x_train <- read.table("./train/x_train.txt", header=FALSE)
y_train <- read.table("./train/y_train.txt", header=FALSE)

#Label Activity ID with Activity Type
y_train1<-merge(activity_labels, y_train, by.x="V1", by.y="Activity_ID", all=TRUE)

#Add lables to train dataset
colnames(subject_train)<- c("ID")
colnames(x_train)<- features[,2]
colnames(y_train1)<- c("Activity_ID", "Activity_Type")
#Merge Train dataset

train_data <- cbind(subject_train, y_train1, x_train)

#Merge test and train data

df <- rbind(test_data, train_data)

library(dplyr)

#arrange dataset by ID

df<-arrange(df, ID)


#Eliminate duplicate column names
data <- df[ , !duplicated(colnames(df))]

#Keep only data with mean and standard deviation for each measurement
extract_data <- select(data, ID, Activity_ID, Activity_Type, contains("mean()"), contains("std()"))

#Label Activity IDs with descriptive activity label
names(extract_data)<-gsub("^t", "time", names(extract_data))
names(extract_data)<-gsub("^f", "frequency", names(extract_data))
names(extract_data)<-gsub("Acc", "Accelerometer", names(extract_data))
names(extract_data)<-gsub("Gyro", "Gyroscope", names(extract_data))
names(extract_data)<-gsub("Mag", "Magnitude", names(extract_data))
names(extract_data)<-gsub("BodyBody", "Body", names(extract_data))


#Summarize by Activity_ID and Subject_ID
tidy_data <- select(extract_data, -Activity_Type)
aggdata <-aggregate(tidy_data, by=list(tidy_data$Activity_ID, tidy_data$ID), FUN=mean, na.rm=TRUE)

#Final Summarized Dataset
final_output <- select(aggdata, -Group.1, -Group.2)

#Create table for final data output
write.table(final_output, file="./dataset.txt", row.name=FALSE)


