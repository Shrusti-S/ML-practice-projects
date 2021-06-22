#Use Case:
#Predict the price of new apartments

#Package Used
library(MASS)
data("Boston")

#To View the Boston Package
View(Boston)

#Help File
?Boston

#Structure
str(Boston)

#Summary
summary(Boston)

#To Split the Data into Train and Test
nrow(Boston)

#Logic to Split the data - Version1
library("caTools")
split <- sample.split(Boston$medv,SplitRatio = 0.7)

training_data <- subset(Boston,split == "TRUE")
testing_data <- subset(Boston,split == "FALSE")


#Logic to split the data - Version2
trainData <- Boston[1:300,]
testData <- Boston[301:505,]

#Finding the Corelation in the Data Set
cr <- cor(Boston)
library(corrplot)
corrplot(cr,type = "lower")
corrplot(cr,method = "number")
#Tax rad are highly corealted

library(car)
library(carData)
#Building The Model
model <- lm(medv ~ .,data = trainData)
vif(model)
summary(model)

#Building The Model
model_1 <- lm(medv ~ . ,data = training_data)

#How To find Multicollinearity
vif(model_1)

#Building The Model(Different way)
model_1 <- lm(medv ~ crim + zn + indus + chas + nox + rm
              + age + dis + rad + tax + ptratio + black + lstat,data = training_data)

summary(model_1)

#Building The Model(Different way)
model_2 <- lm(medv ~ crim + zn  + chas + nox + rm
              + dis  + rad + ptratio + black + lstat,data = training_data)

summary(model_2)


#To predict on the Test Data Set
predict_linear_Model <- predict(model_1,testing_data)
predict_linear_Model
