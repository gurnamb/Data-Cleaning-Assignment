####################
# Course Assignement run_analysis
# 4/25/15
# Gurnam Basra
# 1. Dowload the files and store them in the data directory.
# 2. load the train and test files 
# 3. Merges the training and the test sets to create one data set.
# 4. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
# 5. Uses descriptive activity names to name the activities in the data set
# 6. Appropriately labels the data set with descriptive variable names. 
# 
# 7. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# 
# 
####################


# 1. Dowload the files and store them in the data directory.
  setwd("J:/DATA_DRIVE/coursera/Getting_and_Cleaning_Data/courseAssignment")
  getwd()

  ## create the data directory
  if(!file.exists("./data")){dir.create("./data")}

  ## if files don't exist then download them and unzip them
  if (!file.exists("data/UCI HAR Dataset")) {
    fileUrl1  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipfile="data/UCI_HAR_data.zip"
    download.file(fileUrl1, destfile=zipfile, method="auto")
    unzip(zipfile, exdir="./data")
  }

# 2. load the train and test files 
  # The dataset includes the following files:
  # 
  # 'README.txt'
  # 'features_info.txt': Shows information about the variables used on the 
  #  feature vector.
  # 'features.txt': List of all features.
  # 'activity_labels.txt': Links the class labels with their activity name.
  # 
  # The following files are available for the train and test data. Their 
  # descriptions are equivalent. 
  # 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from 
  #  the smartphone accelerometer X axis in standard gravity units 'g'. Every 
  #  row shows a 128 element vector. The same description applies for the 
  #  'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
  # 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal 
  #  obtained by subtracting the gravity from the total acceleration. 
  # 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector 
  # measured by the gyroscope for each window sample. The units are radians/second.

  ## path to the data files
  path <- paste(getwd(),"/data/UCI HAR Dataset/", sep = "")

  # 'train/X_train.txt': Training set.
  trainData <- read.csv(paste(path,"train/X_train.txt",sep=""), sep="", header=FALSE)
  # 'train/y_train.txt': Training labels.
  trainLabel <- read.csv(paste(path,"train/y_train.txt",sep=""), sep="", header=FALSE)
  # 'train/subject_train.txt': Each row identifies the subject who performed
  #  the activity for each window sample. Its range is from 1 to 30. 
  trainSubject <- read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)

  # 'test/X_test.txt': Test set.
  testData <- read.csv(paste(path,"test/x_test.txt",sep=""), sep="", header=FALSE)
  # 'test/y_test.txt': Test labels.
  testLabel <- read.csv(paste(path,"test/y_test.txt",sep=""), sep="", header=FALSE)
  testSubject <- read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)

# 3. Merges the training and the test sets to create one data set.
  ## add rows of testMergedData and trainMergedData
  trainTestMergedData <- rbind(trainData, testData)
  # combine train and test data labels 
  trainTestMergedLabel <- rbind(trainLabel, testLabel)
  # combine train and test Subject
  trainTestMergedSubject <- rbind(trainSubject, testSubject)
  names(trainTestMergedSubject)[1] <- "subject"


# 4. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
  features <- read.csv(paste(path,"features.txt",sep=""), sep="", header=FALSE)
  meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
  
  # length(meanStdIndices) 
  
  names(trainTestMergedData) <- features[, 2]

  # Extracts only the measurements on the mean and standard deviation for 
  # each measurement. 
  trainTestMergedData <- trainTestMergedData[, meanStdIndices]

  # remove "()"
  names(trainTestMergedData) <- gsub("\\(|\\)", "", names(trainTestMergedData))
  # substitutes "-mean" with ".mean"
  names(trainTestMergedData) <- gsub('-mean', '.mean', names(trainTestMergedData))
  # substitutes "-std" with ".std"
  names(trainTestMergedData) <- gsub('-std', '.std', names(trainTestMergedData))
  # substitutes "-" with "."
  names(trainTestMergedData) <- gsub('[-]', '.', names(trainTestMergedData))
  # change all names to lower case
  names(trainTestMergedData) <- tolower(names(trainTestMergedData))




# 5. Uses descriptive activity names to name the activities in the data set
  activityLables <- read.csv(paste(path,"activity_labels.txt",sep=""), sep="", header=FALSE)
  activityLables[, 2] = gsub("_", ".", tolower(as.character(activityLables[, 2])))

  ## name both tales with same ID so they can link to each other
  names(activityLables) <- list("activityID", "activity")
  names(trainTestMergedLabel) <- "activityID"

  # merge activity and label for descriptive labels
  mergeTrainTestLabelActivity <- merge(trainTestMergedLabel,activityLables)

# 6. Appropriately labels the data set with descriptive variable names. 
  # add the label columns to main data
  # append trainLabel and trainSubject to trainData as variable 562
  trainTestMergedData <- cbind(mergeTrainTestLabelActivity$activity, trainTestMergedData) 
  names(trainTestMergedData)[1] <- "activity"


  # Add trainSubject Column to data
  trainTestMergedData <- cbind(trainTestMergedSubject$subject, trainTestMergedData) 
  names(trainTestMergedData)[1] <- "subject"

  # put all cleaned data into data
  data <- trainTestMergedData

# 7. From the data set in step 6, creates a second, independent tidy data set 
  #    with the average of each variable for each activity and each subject. 


  # declare Activity and Subject as nominal data
  data$activity <- as.factor(data$activity)
  data$subject <- as.factor(data$subject)

  
  ## write all cleaned data to a file
  write.table(data, paste(path,"cleanedData.csv"), sep=",", row.names = FALSE)

  uniqueSubjects = unique(trainTestMergedSubject)[,1]
  numSubjects = length(unique(trainTestMergedSubject)[,1])
  numActivities = length(activityLables[,1])
  numCols = dim(data)[2]

#   result  <-  data[1:(numSubjects*numActivities), ]
  dataNameList <- list(names(data))
  result <- matrix(data=NA, nrow=numSubjects*numActivities, ncol=numCols) 
  result <- as.data.frame(result)
  names(result) <- names(data)
 
## For troubleshooting.  Was getting lots of NAN in result
#  data.walking <- data[data$activity == "walking", ]
#  data.walking.upstairs <- data[data$activity == "walking.upstairs", ]
#  data.walking.downstairs <- data[data$activity == "walking.downstairs", ]
#  data.sitting <- data[data$activity == "sitting", ]
#  data.standing <- data[data$activity == "standing", ]
#  data.laying <- data[data$activity == "laying", ]
  
row = 1
for (subject in 1:numSubjects) {
  for (activity in 1:numActivities) {
    result[row, 1] = uniqueSubjects[subject]
    result[row, 2] = activityLables[activity, 2]
    tmp <- data[data$subject==subject & data$activity==activityLables[activity, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}

  # remove all nan values
  result <- result[complete.cases(result), ]

  # order by subject
  library(plyr)
  result <- arrange(result, subject)

  ## write all aggregated data to a tidy file
  write.table(result, paste("data/tidyData.csv"), sep=",", row.names = FALSE)
