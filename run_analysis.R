## ------ Step 0 ------

# Load some libraries
library(dplyr)
library(reshape2)

## ------ Step 1 ------

# Load the csv data
xTrain <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
yTrain <- read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
sTrain <- read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

xTest <- read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
yTest <- read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
sTest <- read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)

# This data has to be cleaned
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Append the additional data
xTrain[,562] <- yTrain
xTrain[,563] <- sTrain

xTest[,562] <- yTest
xTest[,563] <- sTest

# Combine it all in a single data frame
xAll = rbind(xTrain, xTest)

## ------ Step 2 ------

# Drop anything but means and stds
filterCols <- grep(".*Mean.*|.*Std.*", features[,2])
features <- features[filterCols,]
# We'll need two more cols, standing for subject and activity
filterCols <- c(filterCols, 562, 563)
# And get rid of all the undesired data
xAll <- xAll[,filterCols]

## ------ Step 3 and 4 ------

# Append human readable names
colnames(xAll) <- c(features$V2, "Activity", "Subject")

## ------ Step 5 ------

# First of all, tidy up the Activity and Subject columns
counter = 1
for (label in activityLabels$V2) {
  xAll$Activity <- gsub(counter, label, xAll$Activity)
  counter <- counter + 1
}

xAll$Activity <- as.factor(xAll$Activity)
xAll$Subject <- as.factor(xAll$Subject)

tidy = aggregate(xAll, by=list(Activity = xAll$Activity, Subject=xAll$Subject), mean)

# For some reason, there are two apparently duplicate columns. Let's remove it
tidy <- tidy[,-(89:90)]
# And save
write.table(tidy, "tidy.txt", row.name=FALSE, sep="\t")
