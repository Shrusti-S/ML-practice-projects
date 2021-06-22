rm(list = ls())

## Load Titanic library to get the dataset
library(titanic)

data("titanic_train") 
data("titanic_test")

View(titanic_train)

for(i in c(1:ncol(titanic_train)))
{
  #print(i)
  titanic_train[i][titanic_train[i] == ""] <- NA
}

View(titanic_train)

str(titanic_train)

titanic_train <- subset(titanic_train,select = -c(1,4,9,11))

colSums(is.na(titanic_train))

library(missForest)
str(titanic_train)
imputed_values = missForest(titanic_train,maxiter = 2)

#Convert to factor
titanic_train <- as.data.frame(unclass(titanic_train))
str(titanic_train)

library(missForest)
str(titanic_train)
imputed_values = missForest(titanic_train,maxiter = 2)

titanic_train <- imputed_values$ximp

colSums(is.na(titanic_train))

## Splitting training and test data
library(caTools)
split <- sample.split(titanic_train$Survived,SplitRatio = 0.7)
trainData <- subset(titanic_train,split == "TRUE")
validateData <- subset(titanic_train,split == "FALSE")

dim(trainData)
dim(validateData)

model <- glm(Survived ~.,family=binomial(link='logit'),data=trainData)

summary(model)
step(model)

# save the model to disk
saveRDS(model , "./train_model.rds")

#Model Prediction

predict <- predict(model,validateData,type = "response")
predict

result <- ifelse(predict > 0.5,1,0)
result

library(caret)
metrics <- confusionMatrix(data=result, reference=validateData$Survived)
metrics$byClass

library("ROCR")
#ROC Curve and Area under the Curve(AUC)
dev.off()
ROCRpredict <- predict(model,validateData,type = "response")
ROCRPredict1 <- prediction(ROCRpredict,validateData$Survived)
ROCRgraph <- performance(ROCRPredict1,measure = "tpr",x.measure = "fpr")
plot(ROCRgraph, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

################################

#test Data
getwd()

# load the model
saved_model <- readRDS("./train_model.rds")

#prediction for test data
testData <- titanic_test
predict_test <- predict(model,testData,type = "response")
predict

result <- ifelse(predict > 0.5,1,0)
result
