# course project
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "projectdata.zip",method="curl")
unzip("projectdata.zip")
# Anyway to unzip to a directory of choice?? - exdir
# reading attribute data
activity_labels <- scan(file="UCI HAR Dataset/activity_labels.txt",what=list(""))
x<-read.table(file="UCI HAR Dataset/activity_labels.txt", sep = "", col.names = c("id","activity"))
# $ activity: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1
featureinfo <- scan(file="UCI HAR Dataset/features_info.txt",what=list(""),skip=23,n=17)
# reading training dataset
subject_train <- scan(file="UCI HAR Dataset/train/subject_train.txt",what = integer(),sep="\n")
X_train <- scan(file="UCI HAR Dataset/train/X_train.txt",what = numeric(),sep="")
X_train_1 <- scan(file="UCI HAR Dataset/train/X_train.txt",what = list(""),sep = "\n")
# how to get the first 5 lines from the list??
length(X_train_1[[1]])
# length of subject & X_train doesn't seem to match!!! however matches for X_train_1???
Y_train <- scan(file="UCI HAR Dataset/train/Y_train.txt",what = numeric(),sep="")
Y_train_df <- read.table(file="UCI HAR Dataset/train/Y_train.txt",sep = "")
X_train_df <- read.table(file="UCI HAR Dataset/train/X_train.txt",sep="")
X_train_df_1 <- read.table(file="UCI HAR Dataset/train/X_train.txt",sep="")
col <- c(1:561)
means <- sapply(X[,col],mean)