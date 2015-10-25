library(readr)
library(dplyr)
library(reshape2)
setwd("~/Dropbox/Rprogramming")

#Step 1
featureNames <- read_delim("./features.txt", delim = " ", col_names = FALSE)
subjectTest <- read_table("./test/subject_test.txt", col_names = FALSE)
XTest <- read_table("./test/X_test.txt", col_names = FALSE)
YTest <- read_table("./test/Y_test.txt", col_names = FALSE)
colnames(subjectTest) <- "SubjectID" 
colnames(YTest) <- "ActivityID"
colnames(XTest) <- featureNames[["X2"]] #Step 4
test <- cbind(subjectTest, YTest, XTest)

subjectTrain <- read_table("./train/subject_train.txt", col_names = FALSE)
XTrain <- read_table("./train/X_train.txt", col_names = FALSE)
YTrain <- read_table("./train/Y_train.txt", col_names = FALSE)
colnames(subjectTrain) <- "SubjectID"
colnames(YTrain) <- "ActivityID"
colnames(XTrain) <- featureNames[["X2"]] #Step 4
train <- cbind(subjectTrain, YTrain, XTrain)
Step1.dataSet <- rbind(test, train)

#Step 2
colNumber <- filter(featureNames, grepl('mean()|std()', X2))
colNumbers <- colNumber[["X1"]] +2
Step2.dataSet <- subset(Step1.dataSet, select = c(1,2,colNumbers))

#Step 3
activityLabels <- read_delim("./activity_labels.txt", delim = " ", col_names = FALSE)
colnames(activityLabels) <- c("ActivityID","ActivityName")
Step3.dataSet <- left_join(Step2.dataSet, activityLabels)
Step3.dataSet <- subset(Step3.dataSet, select = c("SubjectID","ActivityName",colNumber[["X2"]]))
#Step 4 was performed within Step 1

#Step 5
reshape.dataSet <- melt(Step3.dataSet, id.vars = c("SubjectID","ActivityName"), 
                        measure.vars = colNumber[["X2"]],
                        variable.name = "measurementName",
                        value.name = "measurement")
Step5.dataSet <- reshape.dataSet %>%
                        group_by(SubjectID, ActivityName, measurementName) %>%
                        summarize(Average = mean(measurement))
write.csv(Step5.dataSet, file ="Step 5 Results.txt", row.names= FALSE)
