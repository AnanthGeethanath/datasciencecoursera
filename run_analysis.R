library(dplyr)
library(tidyr)

run_analysis <- function ()
{
  ## Set Working directory
  setwd("C:/Data")
  
  ## 1.Merges the training and the test sets to create one data set.
  ## Read the data set Labels
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  features_labels <- read.table("./UCI HAR Dataset/features.txt")
  
  
  ## Read test Data
  activity_y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  features_x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  ## Read train Data
  activity_y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  features_x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

  ## Name the Columns
  names(subject_test) <- c("Subject")
  names(activity_y_test) <- c("Activity")
  names(features_x_test) <- features_labels$V2

  names(subject_train) <- c("Subject")
  names(activity_y_train) <- c("Activity")
  names(features_x_train) <- features_labels$V2

  subject_test$ID = 1:nrow(subject_test)
  activity_y_test$ID = 1:nrow(activity_y_test)
  features_x_test$ID = 1:nrow(features_x_test)
  
  subject_train$ID = 1:nrow(subject_train)
  activity_y_train$ID = 1:nrow(activity_y_train) 
  features_x_train$ID = 1:nrow(features_x_train)
  
  
  ##  Create the Test Data Set
  interim_data <- inner_join(subject_test,activity_y_test, by="ID" )
  test_data <- inner_join(interim_data,features_x_test, by = "ID" )
  test_data <-tbl_df(test_data)
  test_data$ID <- c("Test_Data")
  write.table(test_data, "./UCI_HAR_DATA.csv", col.names=TRUE, row.names=FALSE, sep =',') 
  
  interim_data <- inner_join(subject_train,activity_y_train, by="ID" )
  train_data <- inner_join(interim_data,features_x_train, by = "ID" )
  train_data <-tbl_df(train_data)
  train_data$ID <- c("Train_Data")
  write.table(train_data, "./UCI_HAR_DATA.csv", col.names=FALSE, row.names=FALSE, sep =',', append =TRUE) 
  
  uhc_har_data <- tbl_df(read.csv("./UCI_HAR_DATA.csv", head=T, sep=","))
  
  ## Select the Mean and Std Dev variables 
  ## First get the column names
  mean_sd_cols <- features_labels$V2
  colIndex <- grep("-mean()|-std()",mean_sd_cols)
  
  
  uhc_har_mean_sd_data <- cbind( uhc_har_data$Subject,
                                         uhc_har_data$Activity,
                                         uhc_har_data[mean_sd_cols[colIndex]])
  
  names(uhc_har_mean_sd_data)[1] <- "Subject"
  names(uhc_har_mean_sd_data)[2] <- "Activity"
    
  write.table(uhc_har_mean_sd_data,"tidy_data_uhc_har.csv", col.names=TRUE, row.names=FALSE, sep=',')
  
  uhc_har_mean_sd_data <- read.csv("tidy_data_uhc_har.csv", head=T, sep=',')
  
  for (i in 1:nrow(uhc_har_mean_sd_data))
  {
    if (as.character(uhc_har_mean_sd_data$Activity[i]) == "1")
    {
      uhc_har_mean_sd_data$Activity[i] <-"WALKING"
    }else if (as.character(uhc_har_mean_sd_data$Activity[i]) == "2")
    {
      uhc_har_mean_sd_data$Activity[i] <-"WALKING_UPSTAIRS"
    }else if (as.character(uhc_har_mean_sd_data$Activity[i]) == "3")
    {
      uhc_har_mean_sd_data$Activity[i] <-"WALKING_DOWNSTAIRS"
    }else if (as.character(uhc_har_mean_sd_data$Activity[i]) == "4")
    {
      uhc_har_mean_sd_data$Activity[i] <-"SITTING"
    }else if (as.character(uhc_har_mean_sd_data$Activity[i]) == "5")
    {
      uhc_har_mean_sd_data$Activity[i] <-"STANDING"
    }else if (as.character(uhc_har_mean_sd_data$Activity[i]) == "6")
    {
      uhc_har_mean_sd_data$Activity[i] <-"LAYING"
    }else
    {
      uhc_har_mean_sd_data$Activity[i] <-"NA"
    }
    


    
  }
  
  write.table(uhc_har_mean_sd_data,"tidy_data_uhc_har.txt", col.names=TRUE, row.names=FALSE, sep=',')
  
  
}


