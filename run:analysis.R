# run_analysis.R
# try a tidy data set
# I couldn't fnish this project on time because it's a little difficult, for my currente knowledge
# So I tried to find some exemple from previous Getting and Cleaning Data courses
# And I found several course projects on GitHub
# I choosed the work done by Ross Flieger-Allison (https://github.com/rfoxfa/Getting_and_Cleaning_Data), among several other
# Then my work was to understand the flow procedures and instructions used by "rfoxfa"
# So, if you want you can give me 0 (zero) in completing this course project, because I did a lot of cheating...
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.
# 1.1 I set a diferent path, because my files were insed the UCI_HAR_Dataset folder, on the R root 
setUpTempTrainDataTable <- read.table("UCI_HAR_Dataset/train/X_train.txt")
setUpTempTestDataTable <- read.table("UCI_HAR_Dataset/test/X_test.txt")
# Merge the two data table sets with rbind (row bind) R function
X <- rbind(setUpTempTrainDataTable, setUpTempTestDataTable)
setUpTempTrainSubjectRowsDataTable <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
setUpTempTestSubjectRowsDataTable <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
# Merge the two data table sets with rbind (row bind) R function
S <- rbind(setUpTempTrainSubjectRowsDataTable, setUpTempTestSubjectRowsDataTable)
setUpTempActivityTrainDataTable <- read.table("UCI_HAR_Dataset/train/y_train.txt")
setUpTempActivityTestDataTable <- read.table("UCI_HAR_Dataset/test/y_test.txt")
# Merge the two data table sets with rbind (row bind) R function
Y <- rbind(setUpTempActivityTrainDataTable, setUpTempActivityTestDataTable)
# 2. Extracts only the measurements on the mean and standard deviation for each variable measurement.
variables <- read.table("UCI_HAR_Dataset/features.txt")
# applies some kind of regular expression to extract the variables with the mean and standard deviation (std) words
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", variables[, 2])
# sub sets the mean and std variables
X <- X[, indices_of_good_features]
names(X) <- variables[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
# applies the tolower R function to lower the letters in the words names
names(X) <- tolower(names(X))
# 3. Uses descriptive activity names to name the activities in the data set.
activities <- read.table("UCI_HAR_Dataset/activity_labels.txt")
# sub sets again
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"
# 4. Appropriately labels the data set with descriptive activity names.
names(S) <- "subject"
# Merge the data columns with cbind (column bind) R function
cleaned <- cbind(S, Y, X)
# writes the table data to a new txt file
write.table(cleaned, "middle_merged_clean_data.txt")
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(S)[,1]
# finds several lengths
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
# sub sets again
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
# loop 
for (s in 1:numSubjects) {
    # a nested loop
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
# writes the final table data toa text file
write.table(result, "final_averages_data_set.txt")
