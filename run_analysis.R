#download file


filesPath <- "C:/Users/UZagaar/Google Drive/Personal/School/DataScienceTookkitclass/cleaningdata/UCI HAR Dataset"
setwd(filesPath)


library(dplyr)
library(data.table)
library(tidyr)


# read above files
filesPath <- "C:/Users/UZagaar/Google Drive/Personal/School/DataScienceTookkitclass/cleaningdata/UCI HAR Dataset"
# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

###################Merges training and test sets
COMBINEdataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(COMBINEdataSubject, "V1", "subject")

COMBINEdataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(COMBINEdataActivity, "V1", "activityNum")


#combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)
 ####Stopped Here
# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#col names for activity labels
activitylbls<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activitylbls, names(activitylbls), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(COMBINEdataSubject, COMBINEdataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)




#2 Retrieve the measurements on the mean and SD

# Reading "features.txt" and pullin the mean and sD
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and SD and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 


#3 Goting to utilize descriptive actiivty names to provide a name to the activites data
#enter name of activity into dataTable
dataTable <- merge(activitylbls, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))


head(str(dataTable),2)
#4 Appropriately labels the data set with descriptive variable names.

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)


#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(dataTable, "TidyData.txt", row.name=FALSE)



