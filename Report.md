

## Practical Machine Learning Report
#### Benjamin Phillips
[GitHub Repository](https://github.com/BenjaminPhillips22/Practical_Machine_Learning)


### Clean the data

```{r}
df1 <- read.csv("pml-training.csv")

dim(df1)
```

a lot of colums are mostly NAs. Let's get rid of them
```{r}
func1 <- function(col){ sum(is.na(col))/length(col) }
nas <- sapply(X = df1, FUN = func1)
hist(nas)

df1 <- df1[,nas<0.5]
dim(df1)
```

We're down to 93 colums 

let's only use complete cases.
```{r}
sum(complete.cases(df1))==dim(df1)[1]
```

No need to remove non complete cases.

We still have a few columns that are just empty
```{r}
func2 <- function(col){ sum(col=="")/length(col) }
nothings <- sapply(X = df1, FUN = func2)
hist(nothings)

df1 <- df1[,nothings<0.5]
dim(df1)
```

Now we're down to 60 colums


```{r}
classes <- sapply(df1, class)
table(classes)
```

The classe factor is what we will try to predict.

The timestamp and new_window data don't have anything to do with the activity. It's just when the data was downloaded to a computer.

```{r}
df1 <- df1[,-grep("window",names(df1))]

df1 <- df1[,-grep("timestamp",names(df1))]

dim(df1)
```

Down to 55 columns

df1 looks good. Let's ship it

```{}
set.seed(9993)
inTrain <- createDataPartition(df1$classe, p=0.7, list = FALSE)
train <- df1[inTrain,]
test <- df1[-inTrain,]
```

Save it for later exploration and use.

```{eval = FALSE}
write.csv(train, "clean_training_data.csv")
write.csv(test, "clean_test_data.csv")
```



### Explore the data

```{r, message=FALSE}
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(rpart.plot)
library(rattle)
```


Split it into training and validation

```{r}
df <- read.csv("clean_training_data.csv")

set.seed(1580003)
inTrain <- createDataPartition(df$classe, p=0.9, list = FALSE)
training <- df[inTrain,]
validation <- df[-inTrain,]
rm(df)
```

OK, finally! We can explore our training data

```{r}
dim(training)

names(training)

classes <- sapply(training, class)
table(classes)
which(classes==c("factor"))
```

Correlations? (remove factor variables)
```{r}
corrplot(cor(training[, -which(classes==c("factor"))]), method="color")
```

There's quite a bit of correlation. 

let's see our users
```{r}
table(training$user_name)
```

6 users, around 2000 data points each. 

```{r}
table(training$classe)
```

5 classe types

Is one type of classe more common?
```{r}
df1 <- training %>% group_by(classe) %>% summarise(count = n())
ggplot(data = df1, aes(x=classe,y=count)) + geom_bar(stat = "identity")
```

A is the most common

Do some people do more of one type of exercise?

```{r}

df2 <- training %>% group_by(classe, user_name) %>% summarise(count = n())
ggplot(data = df2, aes(x=classe,y=count,fill=user_name)) + 
    geom_bar(stat = "identity", position = "dodge", colour = "black") + 
    scale_color_brewer()
```

<br>
<br>

### Predicting

Classe is a factor (category) so we should think about categorical classifiers.


Let's try a tree model

```{r, cache=TRUE, message=FALSE}
md1 <- train(classe ~ . , method = "rf", data = training, ntree = 10)
```

Let's run it on the validation data.

```{r}
predict1 <- predict(md1,validation[,-which(names(validation)==c("classe"))])
confusionMatrix(validation$classe, predict1)
```

The model performs very well on the validation data. Normally I would try a few different models and select the one that performed the best on the validation data, but there doesn't seem to be a need.

Let's run it on the test data.

```{r}
test <- read.csv("clean_test_data.csv")
predictTest <- predict(md1, test[,-which(names(test)==c("classe"))])
confusionMatrix(test$classe, predictTest)
```


### Conclusion

This model using random forest is an extremely good fit with and accuracy of 99.97%. Only two observation out of 5885 of the test data was mis-clasified.

<br>
<br>
<br>
<br>
<br>


























