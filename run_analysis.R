


dostuff <- function()
{
  featuresFN <- "UCI HAR Dataset/features.txt"
  xTrainFN <- "UCI HAR Dataset/train/X_train.txt"
  xTestFN <- "UCI HAR Dataset/test/X_test.txt"
  
  subjectTrainFN <- "UCI HAR Dataset/train/subject_train.txt"
  subjectTestFN <- "UCI HAR Dataset/test/subject_test.txt"
  
  yTrainFN <- "UCI HAR Dataset/train/Y_train.txt"
  yTestFN <- "UCI HAR Dataset/test/Y_test.txt"
  
  activiyLabelsFN <- "UCI HAR Dataset/activity_labels.txt"
  
  ################################################################
  
  featuresDF <- read.csv(featuresFN, header=F, sep=" ")
  
  xTrainDF <- read.table(xTrainFN)
  xTestDF <- read.table(xTestFN)
  
  subjectTrainDF <- read.table(subjectTrainFN)
  subjectTestDF <- read.table(subjectTestFN)
  
  yTrainDF <- read.table(yTrainFN)
  yTestDF <- read.table(yTestFN)
  
  activityLabelsDF <- read.table(activiyLabelsFN)
  
  ################################################################
  # merge train and test sets
  mergedXDF <- rbind(xTrainDF, xTestDF)
  
  colnames(mergedXDF) <- featuresDF[,2]
  
  ################################################################
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  meanSdIDX <-grep("mean|std", colnames(mergedXDF))
  
  meanSDDF <- mergedXDF[,meanSdIDX]

  ################################################################
  # Uses descriptive activity names to name the activities in the data set
  mergedYDF <- rbind(yTrainDF, yTestDF)
  
  activityDF <- cbind(mergedYDF, meanSDDF)
  
  colnames(activityDF)[1] <- "Activity"

  ################################################################
  # Appropriately labels the data set with descriptive variable names. 
  activityLabelsDF[,2] <- as.character(activityLabelsDF[,2])

  for(i in 1:length(activityDF[,1]))
  {
    activityDF[i,1] <- activityLabelsDF[activityDF[i,1],2]
  }

  ################################################################  
  # creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  subjectsDF <- rbind(subjectTrainDF,subjectTestDF)
  
  dataDF <- cbind(subjectsDF, activityDF)
  colnames(dataDF)[1] <- "Subject"
  
  tidyDF <- aggregate(dataDF[,3] ~ Subject+Activity, data=dataDF, FUN="mean")
  
  for(i in 4:ncol(dataDF))
  {
    tidyDF[,i] <- aggregate( dataDF[,i] ~ Subject+Activity, data=dataDF, FUN="mean")[,3]
  }
  
  colnames(tidyDF)[3:ncol(tidyDF)] <- colnames(meanSDDF)
  
  write.table(tidyDF, file="ExtractedData.txt", row.name=F)
  
}
