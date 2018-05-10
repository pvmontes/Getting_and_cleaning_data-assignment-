#This file charga the files from "UCI HAR Dataset" folder.

#Question 1 
#Merges the training and the test sets to create one data set.

#Load the file.
y_train <- read.table("train/y_train.txt")

#Rename the variable.
names(y_train) <- c("Activity")

X_train <- read.table("train/X_train.txt")

#We load "features.txt" file to modify the name of variables and can look for in the next question.
features <- read.table("features.txt")

#We select the column with the name of feature
features_names <- features$V2

#And rename the variables with the original names.
names(X_train) <- features_names

subject_train <- read.table("train/subject_train.txt")

names(subject_train) <- c("Subject")

y_test <- read.table("test/y_test.txt")

names(y_test) <- c("Activity")

X_test <- read.table("test/X_test.txt")

names(X_test) <- features_names

subject_test <- read.table("test/subject_test.txt")

names(subject_test) <- c("Subject")


#We merge the data into two data sets.
datos_test <-  cbind(subject_test, y_test, X_test)

datos_train <- cbind(subject_train, y_train, X_train)

#And merge the two data sets in one.
datos_overall <- rbind(datos_test,datos_train)


#Second question
#Extracts only the measurements on the mean and standard deviation for each measurement.
#I am going to use dplyr library.

library(dplyr)

#The names of variables have invalid characters; first of all we write valid characters for them.
valid_column_names <- make.names(names=names(datos_overall), unique=TRUE, allow_ = TRUE)
names(datos_overall) <- valid_column_names


#Now we select the variables that we want.
#I select all the variables with "mean" and "std" in their names.
columns_selected <- datos_overall %>%
  select(Subject, Activity, contains("mean"), contains("std"))


#Third question.
#Uses descriptive activity names to name the activities in the data set.

#First of all we convert the variable "Activity" in a factor field.
columns_selected$Activity <- factor(columns_selected$Activity)

#After we change the levels of factor.
levels(columns_selected$Activity) <- gsub(pattern = "1",replacement = "WALKING", levels(columns_selected$Activity))
levels(columns_selected$Activity) <- gsub(pattern = "2",replacement = "WALKING_UPSTAIRS", levels(columns_selected$Activity))
levels(columns_selected$Activity) <- gsub(pattern = "3",replacement = "WALKING_DOWNSTAIRS", levels(columns_selected$Activity))
levels(columns_selected$Activity) <- gsub(pattern = "4",replacement = "SITTING", levels(columns_selected$Activity))
levels(columns_selected$Activity) <- gsub(pattern = "5",replacement = "STANDING", levels(columns_selected$Activity))
levels(columns_selected$Activity) <- gsub(pattern = "6",replacement = "LAYING", levels(columns_selected$Activity))


#Forth question
#Appropriately labels the data set with descriptive variable names.

#It is done when it was written the name from features.txt.

#Fith question
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#We use the dplyr library again.
tidy_data <- columns_selected %>%
  group_by(Subject,Activity) %>%
  summarise_all(mean)

#This file has the tidy data.
View(tidy_data)


