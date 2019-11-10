#### file: run_analyis.R
#### intention: load wearables data and munge it to a tidy dataset
#### date: 10.11.2019
#### name: fabian wagner

### 1. load and merge the training and test data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "original_data.zip", method = "curl")
unzip(zipfile = "original_data.zip")

#load data from extracted zip folder
train_X_path <- "UCI Har Dataset/train/X_train.txt" #data
train_y_path <- "UCI Har Dataset/train/y_train.txt" #labels
train_subject_path <- "UCI Har Dataset/train/subject_train.txt"

test_X_path <- "UCI Har Dataset/test/X_test.txt" #data
test_y_path <- "UCI Har Dataset/test/y_test.txt" #labels
test_subject_path <- "UCI Har Dataset/test/subject_test.txt"

train_data <- read.table(train_X_path)
train_labels <- read.table(train_y_path)
train_subjects <- read.table(train_subject_path)

test_data <- read.table(test_X_path)
test_labels <- read.table(test_y_path)
test_subjects <- read.table(test_subject_path)



#merge data by cols and rows
train <- cbind(train_subjects, train_labels, train_data)
test <- cbind(test_subjects, test_labels, test_data)

all <- rbind(train, test)

#numbers seem to fit:
nrow(all) == nrow(train) + nrow(test)


### 2. variable names <- loading for extraction of only mean and standard deviation

features_path <- "UCI Har Dataset/features.txt"
features <- read.table(features_path)
features <- as.character(features$V2)

#ncol of all data - label
length(features) == ncol(all)-2

#rename data frame for extraction (could have been done only with vector too)
names(all) <- c("subject", "label", features)

# searches all columns with mean or standard deviaton
indices <- grep("mean()|std()", names(all))
# meanFreq was included and I was to lazy to find the regexp to exclude from grep
# so i just searched it in the vektor and excluded it afterwards
neamfreq <- grep("Freq",names(all)[indices])
col_indeces <- indices[-neamfreq]
extraction <- c("subject","label", names(all)[col_indeces])


#just take mean and std variables
data <- all[,extraction]


### 3. Descriptive activity names
### activity labels in the txt files are ordered
### 1 walking, 2 walking upstairs, 3 walking downstairs, 4 sitting, 5 standing, 6 laying
data$label <- factor(data$label, levels = c(1:6), 
                     labels = c("walking","walking upstairs","walking downstairs",
                                "sitting", "standing", "laying"))


#4 make variable names more descriptive

# has already been done in the step above, where i used the feature names for variabel
# selection


#5  From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)

tidydata <- data%>%
  rename(activity = label) %>%
  group_by(activity, subject) %>%
  summarise_each(list(mean = mean))

write.table(tidydata, file = "tidydata.txt", row.names = FALSE)
