library(dataMaid)
library(dplyr)

run_analysis <- function() {
  ## Read all the data
  testx <- read.table("test//X_test.txt")
  trainx <- read.table("train//X_train.txt")
  testy <- read.table("test//Y_test.txt")
  trainy <- read.table("train//Y_train.txt")
  testst <- read.table("test//subject_test.txt")
  trainst <- read.table("train//subject_train.txt")
  labels <- read.table("features.txt")
  ##Bind the the measurements, activities and subjects for train and test
  compx <- rbind(trainx,testx)
  colnames(compx)<- labels$V2
  compx <- select(complete,contains("std()") | contains("mean()"))
  compy <- rbind(trainy,testy)
  colnames(compy)<- c("activity")
  compst <- rbind(trainst,testst)
  colnames(compst)<- c("subject")
  ##Bind the 3 datasets to produce a dataset with all the measurements with the id of the subject and the activity performed
  complete <- cbind(compst,compy,compx)
  ##Apply the mean function with the dataset grouped by subject and activity to produce the tidy dataset
  tidy <- complete %>% 
    group_by(subject, activity) %>%
    summarise_each(funs(mean))
  ##Labels subjects and activities
  attributes(finaltest$subject)$label <- "ID of the subjects"
  attributes(finaltest$activity)$label <- "Activity performed in the session"
  ##Write tidy.txt
  write.table(tidy, "tidy.txt", row.names = FALSE, quote = FALSE)
  return(tidy)
}