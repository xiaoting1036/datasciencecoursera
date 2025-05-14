# Step 0: Load necessary packages
library(dplyr)
install.packages("tidyr")
library(tidyr)
# Read features and activity labels
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("id", "feature"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

# Read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

# Read train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# Merge datasets
x_merged <- rbind(x_train, x_test)
y_merged <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)
merged_data <- cbind(subject_merged, y_merged, x_merged)

# Step 2: Extract only the measurements on the mean and standard deviation
tidy_data <- merged_data %>% 
    select(subject, code, contains("mean"), contains("std"), -contains("angle"), -contains("Mean"))
# Step 3: Use descriptive activity names
tidy_data$code <- activity_labels[tidy_data$code, 2]

# Step 4: Appropriately label the data set with descriptive variable names
names(tidy_data)[2] <- "activity"
names(tidy_data) <- gsub("^t", "Time", names(tidy_data))
names(tidy_data) <- gsub("^f", "Frequency", names(tidy_data))
names(tidy_data) <- gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data) <- gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data) <- gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data) <- gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data) <- gsub("\\.mean", "Mean", names(tidy_data))
names(tidy_data) <- gsub("\\.std", "Std", names(tidy_data))
names(tidy_data) <- gsub("\\.\\.", "", names(tidy_data))

# Step 5: Create a second, independent tidy data set with averages
final_data <- tidy_data %>%
    group_by(subject, activity) %>%
    summarise_all(mean)

# Write the final tidy data to a file
write.table(final_data, "final_tidy_data.txt", row.names = FALSE)

