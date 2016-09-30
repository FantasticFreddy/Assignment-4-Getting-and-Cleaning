#You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.

# 'README.txt'
# 'features_info.txt': Shows information about the variables used on the feature vector.
# 'features.txt': List of all features.
# 'activity_labels.txt': Links the class labels with their activity name.                      
# 'train/X_train.txt': Training set.
# 'train/y_train.txt': Training labels.
# 'test/X_test.txt': Test set.
# 'test/y_test.txt': Test labels.

#Clean workspace
rm(list=ls())

# 1.  Merge training and test data to create one data set
# 1.1 Read in data 

setwd("C:/Users/Fredrik.Augustsson/Documents/Coursera data")
getwd()

# read in train data

features <- read.table('./features.txt')
xtrain <- read.table('./train/X_train.txt')
ytrain <- read.table('./train/y_train.txt')
subjecttrain <- read.table('./train/subject_train.txt')
activity_label <- read.table('./activity_labels.txt')

# Set column names for train data

colnames(subjecttrain) <- "subject"
colnames(ytrain) <- "activity_id"
colnames(xtrain) <- features$V2
colnames(activity_label) <- c("activity_id", "activity" )


# read in test data
subjecttest <- read.table('./test/subject_test.txt',header=FALSE); #imports subjecttrain.txt
xtest <- read.table('./test/X_test.txt',header=FALSE); #imports x_train.txt
ytest <- read.table('./test/y_test.txt',header=FALSE); #imports y_train.txt


# Update column names for test data

colnames(subjecttest) <- "subject"
colnames(ytest) <- "activity_id"
colnames(xtest) <- features$V2

# View test data
View(subjecttest)
View(xtest)
View(ytest)


# Merge data
# merge activity with activity id in ytest
ytest <- merge(x = ytest, y = activity_label, by = "activity_id", all.x = TRUE)
# merge columns from subjecttest, ytest and xtest
test <- cbind(subjecttest, ytest, xtest)


# merge activity with activity id for ytrain
ytrain <- merge(x = ytrain, y = activity_label, by = "activity_id", all.x = TRUE)
train <- cbind(subjecttrain, ytrain, xtrain)

# merges test and train to one data set
data <- rbind(test, train)
View(head(data))

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# logical vector which tells what columns to include

wanted_columns <- (grepl("id", names(data)) | grepl("activity", names(data)) | grepl("-mean", names(data)) & !grepl("mean..-",names(data)) & !grepl("-meanFreq", names(data)) | grepl("-std", names(data)) & !grepl("-std()..-",names(data)))

?grepl

# check number of columns to include
table(wanted_columns)

# removes unwanted columns from the merged data set
dt <- data[,wanted_columns]
names(dt)



# 4. Appropriately labels the data set with descriptive variable names.


grep("()", names(dt))

colnames(dt) <- gsub("-mean()", " Mean", names(dt))
colnames(dt) <- gsub("-std()", " St Dev", names(dt))
colnames(dt) <- gsub("_", " ", names(dt))
colnames(dt) <- gsub("^(t)", "Time ", names(dt))
colnames(dt) <- gsub("^(f)", "Frequency ", names(dt))
names(dt)


dt2 <- dt[,colnames(dt) != "activity type"]
View(dt2)

tidy <- aggregate(dt2[, names(dt2) != c("id", "activityid")], by = list(dt2$id, dt2$`activityid`), mean)







