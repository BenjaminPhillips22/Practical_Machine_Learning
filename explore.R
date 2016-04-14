
# Explore the data

library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(rpart.plot)
library(rattle)

setwd("/home/ben/R/Hopkins_Data_Science_Specialization/Practical_Machine_Learning")

df <- read.csv("clean_training_data.csv")

#split it into training and validation

set.seed(1580003)
inTrain <- createDataPartition(df$classe, p=0.9, list = FALSE)
training <- df[inTrain,]
validation <- df[-inTrain,]
rm(df)
# OK, finally! We can explore our training data

View(training)

dim(training)
# 56 variables
names(training)

# variable classes
classes <- sapply(training, class)
table(classes)
which(classes==c("factor"))

# correlations? (remove factor variables)
corrplot(cor(training[, -which(classes==c("factor"))]), method="color")
# There's quite a bit of correlation. 

# let's see our users

table(training$user_name)
# 6 users, around 2000 data points each. 

str(training)
#There are still integers. must load csv ints as ints

table(training$classe)
# 5 classe types A B C D E

# Is one type of classe more common?
df1 <- training %>% group_by(classe) %>% summarise(count = n())
ggplot(data = df1, aes(x=classe,y=count)) + geom_bar(stat = "identity")
# A is the most common


# do some people do more of one type of exercise?
df2 <- training %>% group_by(classe, user_name) %>% summarise(count = n())
ggplot(data = df2, aes(x=classe,y=count,fill=user_name)) + 
    geom_bar(stat = "identity", position = "dodge", colour = "black") + 
    scale_color_brewer()




# classe is a factor (category) so we should think about 
# categorical classifiers 


# Let's try a tree model

md1 <- train(classe ~ . , method = "rf", data = training, ntree = 10)
md1$finalModel
(md1$finalModel)

# can't veiw it....

#let's run it on the validation data
predict1 <- predict(md1, validation[,-which(names(validation)==c("classe"))])
confusionMatrix(validation$classe, predict1)

# OK, this is weird. My model (random tress) preforms perfectly 
# on the validation data.


# Let's run it on the test data

test <- read.csv("clean_test_data.csv")
predictTest <- predict(md1, test[,-which(names(test)==c("classe"))])
confusionMatrix(test$classe, predictTest)




























