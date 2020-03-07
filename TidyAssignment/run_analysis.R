## The approach to generating this script to tidy the Human Activity Recognition Using Smartphones Data Set
## referenced at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## is as follows:
## The main measurement data has been split 70/30 from the same data source into training and test data 
## The subjects and activities associated with this data has also been spliced from the main data sets
## into separate file subject and y activity data. The features file contains a description
## of the columns on the main training and test data files and in effect a "transposed" column index.
##
## This will be used to drive the selection of the required mean and std columns data and to also
## label those column subsets using the features "transposed" observation number as a column index 
## in the main training and test datasets
## Finally as the activity, subject and main measurement data have been spliced off an original single
## source they can be spliced back in a unadulterated mannner using cbind as the number of rows are the same
## before using rbind to append both the reduced training and test data back together and finally
## using merge by activityCode to assign a new Activity label description column to complete the data set
## using.
## 
## dplyr and data.table libraries should be loaded


library(dplyr)
library(data.table)

############ TEST FILES SECTION ############

## Read the X_test file
## Print the number of rows of each DF as you go to ensure correct row alignment
## Read the y_test activity file
## Read the test subjects file

X_test<-read.table(file="X_test.txt", sep = "",header = F,colClasses = c("numeric"), na.strings ="NA",stringsAsFactors= F)
print(paste("X_test rows:",dim(X_test)[1]))

y_test<-read.table(file="y_test.txt", sep = "",header = F, col.names=c("activityCode"),na.strings ="NA", stringsAsFactors= F)
print(paste("y_test rows:",dim(y_test)[1]))


subj_test<-read.table(file="subject_test.txt", sep = "",header = F, col.names=c("subject"), na.strings ="NA", stringsAsFactors= F)
print(paste("subj_test rows:",dim(subj_test)[1]))

############## TRAIN FILES SECTION ############

## Read the X_train file
## Print the number of rows of each DF as you go to ensure correct alignment
## Read the y_train activity file
## Read the train subjects file

X_train<-read.table(file="X_train.txt", sep = "",header = F,colClasses = c("numeric"), na.strings ="NA",stringsAsFactors= F)
print(paste("X_train rows:",dim(X_train)[1]))

y_train<-read.table(file="y_train.txt", sep = "",header = F, col.names=c("activityCode"),na.strings ="NA", stringsAsFactors= F)
print(paste("y_train rows:",dim(y_train)[1]))

subj_train<-read.table(file="subject_train.txt", sep = "",header = F, col.names=c("subject"), na.strings ="NA",stringsAsFactors= F)
print(paste("subj_train rows:",dim(subj_train)[1]))

############ ACTIVITY AND FEATURES SECTION ##################

## Read the activity_labels file to get the acivity description for later merging
## Read the features file which ultimately reflects the list of X_test and X_train column names
## as these files are from the same source split in two
## Extract only those feature names that have the string mean or std in it
## the features obs number can be used to construct a vector of column indexes
## to subset the X_test & X_train data frame for the desired columns required (Assignment task 2)
## the features column name can also be used to change the default var column names
## to the more descriptive feature column names (Assignment task 4)
## Also, prefix each of the column names at this point with the prefix "mean-" to inidcate that final variables both those mean and std 
## will be an average of the mean and std variables by subject and activity. This is the easiest point to do this operation
## although the data has not technically been averaged as yet

activity_desc<-read.table(file="activity_labels.txt", sep = "",header = F,col.names=c("activityCode","activity"), na.strings ="NA", stringsAsFactors= F)
print(paste("activity_desc rows:",dim(activity_desc)[1]))

features<-read.table(file="features.txt", sep = "",header = F,col.names=c("colIndex","colName"), na.strings ="NA", stringsAsFactors= F)
print(paste("features rows:",dim(features)[1]))

meanstd_cols<-features[grep("mean|std", features$colName),]
print(paste("meanstd_cols column indexes:",dim(meanstd_cols)[1]))

meanstd_cols$colName <- paste0("mean-",meanstd_cols$colName)

################ X_test SECTION ################

## Assign the the subsetted column names in the X_test DF based on mean/std descriptions and
## equivalent column index in features DF
## This is step2 of assignment - 2.Extracts only the measurements on the mean and standard deviation for each measurement.
## Change the default var column names to more descriptive names
## This is step4 of assignment. 4. Appropriately labels the data set with descriptive variable names

X_test_red<-X_test[,meanstd_cols[,1]]
print(paste("X_test_red rows:",dim(X_test_red)[1]))

names(X_test_red)<-meanstd_cols[,2]

## append subject and acivity codes DF together by column as same length
subj_activity_test<-cbind(subj_test,y_test)
print(paste("subj_activity_test rows:",dim(subj_activity_test)[1]))

## append the two column DF to the reduced X_test ready to combine with X train
## This is first part of assignment task 3 & 5 to assign the activity code and the subject in order to prepare summary data set
combined_test<-cbind(subj_activity_test,X_test_red)
print(paste("combined_test rows:",dim(combined_test)[1]))

################## X_train SECTION ####################

## Assign the the subsetted column names in the X_train DF based on mean/std descriptions and
## equivalent column index in features DF
## Use the same subsetting vector meanstd_cols derived from the features table to subset train data
## This is step2 of assignment 2.Extracts only the measurements on the mean and standard deviation for each measurement.
## Change the default var column names to more descriptive names
## This is step4 of assignment. 4. Appropriately labels the data set with descriptive variable names

X_train_red<-X_train[,meanstd_cols[,1]]
print(paste("X_train_red rows:",dim(X_train_red)[1]))

names(X_train_red)<-meanstd_cols[,2]

## append subject and acivity codes DF together by column as same length
subj_activity_train<-cbind(subj_train,y_train)
print(paste("subj_activity_train rows:",dim(subj_activity_train)[1]))

## append the two column DF to the reduced X_train ready to combine with X train
## This is first part of assignment task 3 & 5 to assign the activity code and the subject in preparation for later summary data set
combined_train<-cbind(subj_activity_train,X_train_red)
print(paste("combined_train rows:",dim(combined_train)[1]))

################ FINAL TIDY SECTION ######################

## Finally append both test and train data sets back together
## This is assignment task 1.Merges the training and the test sets to create one data set.
## And last,merge the activities_lable DF and the combined data set to get the "Activity" description
## This is assignment task 3.Uses descriptive activity names to name the activities in the data set

full_set<-rbind(combined_train,combined_test)
print(paste("full_set rows:",dim(full_set)[1]))

full_set<-merge(full_set,activity_desc, by="activityCode")
print(paste("full_set rows:",dim(full_set)[1]))

################ SECOND TIDY SET ######################

## use dplyr and data.table fwrite to create a separate tidy data set of the mean of all the variables by subject and activity
## write this to a file grouped.txt

grouped<-full_set %>% 
        group_by(subject,activity) %>% 
        summarise_all(mean)

write.table(grouped, file="grouped.txt", sep=" ", row.names = FALSE)
