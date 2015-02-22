#First we read the training and test data, including the subjects and labels,
#into R.

trainno_labs <- read.table("X_train.txt") 
trainsubject <- read.table("subject_train.txt", col.names = c("subjects"))
trainlabs <- read.table("y_train.txt", col.names = c("labels"))

testsubject <- read.table("subject_test.txt", col.names = c("subjects"))
testno_labs <- read.table("X_test.txt")
testlabs <- read.table("y_test.txt", col.names = c("labels"))

#Next we merge the data together into one data set with the test data first.

test_first <- rbind(testno_labs, trainno_labs)

#At this point we must subset test_first based on which variables are
#calculuations of standard deviations or means for given measurements and put
#the subset into mean_std_data

#We then name the columns of mean_std_data based on the variable names we have
#from the features text.
features <- read.table("features.txt")
mean_std_cols <- subset(features, grepl("mean", features$V2) | 
                          grepl("std", features$V2))

special_cols <- mean_std_cols$V1
mean_std_data <- test_first[, special_cols]
names(mean_std_data) <- mean_std_cols$V2

#Lastly we attach the subjects and labels to mean_std_data (again wih the test
#set going first)
subjects <- rbind(testsubject, trainsubject)
labs <- rbind(testlabs, trainlabs)

mean_std_data <- cbind(mean_std_data, subjects)
mean_std_data <- cbind(mean_std_data, labs)

#Each of the labels in mean_std_data corresponds to one of six activities
#(described in the CodeBook), so we add a column to mean_std_data which lists
#the activity for a given observation and get rid of the former integer labels.

activity_labs <- read.table("activity_labels.txt")

mean_std_data$activity_labels <- activity_labs[match(mean_std_data$labels, 
                                                     activity_labs[,1]), 2]
mean_std_data$labels <- NULL

#Lastly we take the means of every standard deviation and mean calculation
#for each activity and each subject.

tidy_data <- mean_std_data 
                %>% group_by(activity_labels,subjects) 
                  %>% summarise_each(funs(mean))

#R cannot deal with variable names which look like calls to R functions, so
#we change the names of our now-tidy data with the make.names() function.
names(tidy_data) <- make.names(names(tidy_data))
names(tidy_data)[1] <- "Activity"
names(tidy_data)[2] <- "Subject"

#Lastly we write tidy_data to a text file.
write.table(tidy_data, "tidy_data.txt")