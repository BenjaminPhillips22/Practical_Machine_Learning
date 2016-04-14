
setwd("/home/ben/R/Hopkins_Data_Science_Specialization/Practical_Machine_Learning")

# Clean up the Data

library(ggplot2)
library(dplyr)
library(caret)

df1 <- read.csv("pml-training.csv")
table(df1$classe)

dim(df1)
func1 <- function(col){ sum(is.na(col))/length(col) }
nas <- sapply(X = df1, FUN = func1)
hist(nas)

# a lot of colums are mostly NAs. Let's get rid of them

df1 <- df1[,nas<0.5]
dim(df1)

# we're down to 93 colums 

# let's only use complete cases.

sum(complete.cases(df1))==dim(df1)[1]
#TRUE
#no need to remove non complete cases.

View(df1)

# we still have a few columns that are just empty

str(df1)

func2 <- function(col){ sum(col=="")/length(col) }
nothings <- sapply(X = df1, FUN = func2)
hist(nothings)

#again we have about 30 columns that are mostly nothings

df1 <- df1[,nothings<0.5]
dim(df1)
# now we're down to 60 colums

View(df1)

classes <- sapply(df1, class)
table(classes)

# the classe factor is what we will try to predict
# the timestamp and new_window data don't have anything
#to do with the activity. It's just when the data was downloaded I think.

df1 <- df1[,-grep("window",names(df1))]

df1 <- df1[,-grep("timestamp",names(df1))]

View(df1)
dim(df1)
# down to 55 columns

classes <- sapply(df1, class)
table(classes)
# make the integer variables numeric

df1[,which(classes==c("integer"))] <- lapply(df1[,which(classes==c("integer"))], FUN = as.numeric )
classes <- sapply(df1, class)
table(classes)

# df1 looks good. Let's ship it

set.seed(9993)
inTrain <- createDataPartition(df1$classe, p=0.7, list = FALSE)
train <- df1[inTrain,]
test <- df1[-inTrain,]

write.csv(train, "clean_training_data.csv")
write.csv(test, "clean_test_data.csv")











