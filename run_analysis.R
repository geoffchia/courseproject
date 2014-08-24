# function that returns TRUE if either substr1 or substr2 is found in string
# returns FALSE otherwise
# can choose whether to ignore case
myregexpr <- function(substr1, substr2, string, ignore.case) {
    if (regexpr(substr1, string, ignore.case=ignore.case)[1] > 0) {
        return(TRUE)
    } else {
        if (regexpr(substr2, string, ignore.case=ignore.case)[1] > 0) {
            return(TRUE)
        } else {
            return(FALSE)
        }            
    }
}

# reads activity labels and feature full names
activities <- read.table("./activity_labels.txt")
features <- read.table("./features.txt")

# reads in 3 test data files
subject_test <- read.table("./test/subject_test.txt")
y_test <- read.table("./test/y_test.txt")
x_test <- read.table("./test/X_test.txt")

# change column names of x_test to full feature names
colnames(x_test) <- features$V2

# use function written above to determine whether "mean" or "std" is a substring of the
# full feature names
found <- sapply(features$V2, function(x) myregexpr("mean", "std", x, ignore.case=T))

# subset x_test by choosing only columns with feature name having either "mean" or "std"
tidy_x_test <- x_test[, features[found, 1]]

# combine subject_test, y_test and tidy_x_test column-wise in a new data.frame
df_test <- data.frame(subject=subject_test$V1, activity=y_test$V1, x=tidy_x_test)

# merge df_test and activity so that activity names appear in data.frame
df_test = merge(df_test, activities, by.x="activity", by.y="V1")

# drop column "activity" now that we have the name of the activity
df_test <- df_test[, !(names(df_test) %in% c("activity"))]

# rename activity name column (now called "V2") to "activity"
colnames(df_test)[which(names(df_test) == "V2")] <- "activity"

# reads in 3 training data files
subject_train <- read.table("./train/subject_train.txt")
y_train <- read.table("./train/y_train.txt")
x_train <- read.table("./train/X_train.txt")

# change column names of x_train to full feature names
colnames(x_train) <- features$V2

# subset x_train by choosing only columns with feature name having either "mean" or "std"
tidy_x_train <- x_train[, features[found, 1]]

# combine subject_train, y_train and tidy_x_train column-wise in a new data.frame
df_train <- data.frame(subject=subject_train$V1, activity=y_train$V1, x=tidy_x_train)

# merge df_train and activity so that activity names appear in data.frame
df_train = merge(df_train, activities, by.x="activity", by.y="V1")

# drop column "activity" now that we have the name of the activity
df_train <- df_train[, !(names(df_train) %in% c("activity"))]

# rename activity name column (now called "V2") to "activity"
colnames(df_train)[which(names(df_train) == "V2")] <- "activity"

# combine test and training data row-wise
df_comb <- rbind(df_test, df_train)

# calculate average of each variable for each activity and each subject
ss <- split(df_comb, list(df_test$subject, df_test$activity))
df_tidy <- sapply(ss, function(x) colMeans(x[, !(names(ss[[1]]) %in% c("subject", "activity"))]))

# write to file
write.table(df_tidy, "output.txt", sep=",")
