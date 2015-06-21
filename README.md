# dss-gcd-project project functions description

# Usage
#
# loadLibraries()
# dataset <- getDataSet()
----------------------------------------------------------------------------------------

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.

getDataSet <- function(){
  test <- getData("test")
  train <- getData("train", nrow(test))
  merge(test, train, all=TRUE)
}

 
# do the hard job
getData <- function(dir, start = 0){
  # load the list of features names to label data columns
  feat <- read.table("features.txt")
  
  # load data file and set features as its columns names
  data = read.table(paste(dir, '/X_', dir, '.txt', sep=''))
  names(data) <- feat[ , 2]
  
  # extracts only the measurements on the mean and standard deviation
  data <- onlyMeanStdMeasure(data)
  
  # create a new column named 'id' and insert it as the first column
  data <- data.frame(id = seq(start + 1, start + nrow(data)), data)
  
  subject <- getSubjectFrame(dir, start)
  label <- getLabelsFrame(dir, start)
  
  # merge data and its associated rows subject and labels 
  data <- merge(label, data, by = "id", all = TRUE)
  merge(subject, data, by = "id", all = TRUE)
}

# create the activity column
getLabelsFrame <- function(dir, start){
  # create the constant "descriptive activity names" vector to label rows
  labels <- getLabels()
  
  # load activities names associated to each data file row
  label = read.table(paste(dir, '/y_', dir, '.txt', sep=''))
  
  # create 'id' column to match label with data frame
  label <- data.frame(id = seq(start + 1, start + nrow(label)), label)
  
  # create a new empty column and set activity labels to data rows
  label[, "activity"] <- ''
  
  for(i in 1:nrow(label)){
    label[i, 3] <- labels[label[i, 2]]
  }
  
  # dismiss column V1, the numeric index of activities column
  label <- label[ , c(1, 3)]
}

# create the subject column
getSubjectFrame <- function(dir, start = 0){
  # load subject associated to each data file row
  subject = read.table(paste(dir, '/subject_', dir, '.txt', sep=''))
  
  # create 'id' column to match table with data frame
  subject <- data.frame(id = seq(start + 1, start + nrow(subject)), subject)
  
  # name the main column as subject
  names(subject) <- c("id","subject")
  subject
}

# extract the mean and std related columns from the dataset
onlyMeanStdMeasure <- function(dataset){
  data <- dataset[, grep('mean()', colnames(dataset), fixed=T)]
  data <- data.frame(data, dataset[, grep('std()', colnames(dataset), fixed=T)])
}


getLabels <- function(){
  c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
}


loadLibraries <- function(){
  library(tidyr)
  library(dplyr)
}
