Practical Machine Learning Assignment
========================================================

The data consists of a set of readings from sensors attached to subjects performing a variety of exercises. The task is to build a model which will correctly classify activities, given unlabelled sets of readings.

Load the [Caret](http://caret.r-forge.r-project.org/) library and set the random number generator's seed, to ensure reproducibility:


```r
library(caret)
set.seed(1234)
```

## Reading and Cleaning the Data

```r
rawData <- read.csv("pml-training.csv", na.strings=c("NA",""), strip.white=T)
dim(rawData)
```

```
## [1] 19622   160
```

```r
na <- apply(rawData, 2, function(x) { sum(is.na(x)) })
cleandata <- subset(rawData[, which(na == 0)], select=-c(X, user_name, new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))
dim(cleandata)
```

```
## [1] 19622    53
```

### Partition
Partitioning the data into training and test 


```r
inTrain <- createDataPartition(cleandata$classe, p=0.7, list=F)
training <- cleandata[inTrain,]
testing <- cleandata[-inTrain,]
```

## Training a Random Forest Model

Now use the train model on the training set.


```r
ctrl <- trainControl(allowParallel=T, method="cv", number=4)
rawdata2 <- read.csv("pml-testing.csv", na.strings=c("NA",""), strip.white=T)
testdata <- subset(rawdata2[, which(na == 0)], 
                        select=-c(X, user_name, new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))

validdata <- subset(cleandata, select=c(roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_y, pitch_belt, magnet_dumbbell_z, roll_forearm, accel_dumbbell_y, roll_dumbbell, magnet_dumbbell_x,classe))
model <- train(classe ~ ., data=validdata[inTrain,], model="rf", trControl=ctrl)
```

## Predictions

```r
predict(model, newdata=testdata)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pred <- predict(model, newdata=testing)
sum(pred == testing$classe) / length(pred)
```

```
## [1] 0.9842
```

```r
confusionMatrix(testing$classe, pred)$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1662    2    9    1    0
##          B   13 1100   11   13    2
##          C    1   10 1008    7    0
##          D    0    4    2  956    2
##          E    0    7    5    4 1066
```


I use this method because it is fast and its accuracy on the test set is 98.5%.

