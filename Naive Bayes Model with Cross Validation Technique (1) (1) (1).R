
# load the libraries
library(caret)
library(klaR)#REQUIRED FOR NAIVE BAYES
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]

View(data_test)
# train a naive bayes model
?NaiveBayes()
model <- NaiveBayes(Species~., data=data_train)
model
# make predictions
x_test <- data_test[,1:4]
x_test
y_test <- data_test[,5]
y_test
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)


#Data Split using bootsrapping method using 100 resamples

# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)


#K fold cross validation
train_control <- trainControl(method="repeatedcv", number=10)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)

#repeated K-fold cross validation

# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)


#Leave out cross validation

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)



