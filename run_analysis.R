# a scritp that is used to process data linked to from the course website 
# and create a tidy data set 

library(dplyr)

#import three data sets (X_test.txt, y_test.txt, subject_test.txt) from "/UCI HAR Dataset/test/"
#and bind them to create testSet.

X_test <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")
y_test <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
testSet <- cbind(subject_test, y_test, X_test)


#import three data sets (X_train.txt, y_train.txt, subject_train.txt) from "/UCI HAR Dataset/train/"
#and bind them to create trainSet.
X_train <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
subject_train <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
trainSet <- cbind(subject_train, y_train, X_train)

#merge testSet and trainSet to create whole(testSet and trainSet do not have same subjects so "rbind" function was selected rather than "merge").
wholeSet <- rbind(testSet, trainSet)

#import features.txt that has variable names of all columns except first two
#and names first two columns "subject" and "activity".
#names all columns of wholeSet.
features <- read.table(file = "./UCI HAR Dataset/features.txt")
varNames <- c("subject", "activity", features$V2)
names(wholeSet) <- varNames


#extracts only the measurements on the mean and standard deviation for each measurement to create new.data.
new.data <- wholeSet[ grepl("(subject)|(activity)|(\\-mean\\(\\))|(\\-std\\(\\))", names(wholeSet) )]

#Uses descriptive activity names to name the activities in the new.data.
new.data$activity <- cut(x = new.data$activity, breaks = c(0,1,2,3,4,5,6) )
activityNames <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")
levels(new.data$activity) <- activityNames$V2
new.data <- arrange(new.data, subject, activity)

#split new.data by different subject 
#then split every sub-dataframe from new.data by different activity.
s_data <- split(new.data, new.data$subject) %>% lapply( function(x){ split(x, x$activity)} )


#create a new empty my_data which variable names is as same as new.data
#use a three-layer cycle to fill my_data with the average of each variable for each activity and each subject.
new.var <- names(new.data)
varNum <- length(new.var)
my_data <- data.frame(matrix(ncol = varNum, nrow = 180))
names(my_data) <- new.var
row_num <- 1
for(i in 1:30){
        for(j in activityNames$V2){
                my_data[["subject"]][row_num] <- i
                my_data[["activity"]][row_num] <- j
                for( e in new.var[3:varNum]){
                        my_data[[e]][row_num] <- colMeans(s_data[[i]][[j]][e],na.rm = TRUE)
                }
                row_num <- row_num+1
        }
}

#export my_data.
write.table(my_data, file = "./tidySet.txt",row.names = FALSE)
