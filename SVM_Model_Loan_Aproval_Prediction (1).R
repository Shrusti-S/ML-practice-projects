rm(list = ls())

bankDataSet <- read.csv("C:\\Users\\Nanitha M\\Desktop\\lor\\course\\creditloan.csv")
str(bankDataSet)

bankDataSet$default[bankDataSet$default==2] <- "Yes"
bankDataSet$default[bankDataSet$default==1] <- "No"
bankDataSet$default <- as.factor(bankDataSet$default)
str(bankDataSet)

#Data Split(Another Approach)
#1. Split data into training and test datasets [70:30]
set.seed(125)
dst = sort(sample(nrow(bankDataSet), nrow(bankDataSet)*0.7))
trainData <- bankDataSet[dst,]
validateData <- bankDataSet[-dst,]

#To check the Data Distribution
prop.table(table(trainData$default))                          # ratio of yes/no in train set
prop.table(table(validateData$default))                       # ratio of yes/no in validate Set

##################### Linear Kernel #######################

#Build SVM Model using linear Kernel
library("e1071")
library("caret")

#Cross Validation Techniques
ctrl <- trainControl(method="repeatedcv",number = 10,repeats = 3)

#Selecting the Optimal Regularization Parameter(C value/Penalization)
grid <- expand.grid(C= c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,0.025,0, 1, 1.25, 1.5, 1.75, 2,5))

#0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5
str(trainData)

?train
#Model Bulding
svmLinear_model <- train(default ~. ,trainData,method = "svmLinear",tuneGrid = grid,trainControl = ctrl)
svmLinear_model

#Predicton
svmLinear_prediction <- predict(svmLinear_model,validateData)
svmLinear_prediction

#Confusion Matrix Result
metrics <- confusionMatrix(svmLinear_prediction,validateData$default)
metrics$byClass


 ################################### Radial Kernel ######################

#The tuning parameter grid should have columns sigma, C
?train()
#sigma for converting from 2D to 3D
grid_radial <- expand.grid(sigma = c(0.01,0,015,0.2),C= c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

#Radial Kernel Model Building
svmRadialKernel_Model <- train(default ~.,trainData,method ="svmRadial",tuneGrid = grid_radial,trainControl= ctrl)
svmRadialKernel_Model

#Radial Kernel Model Prediction
svmRadialKernel_Prediction <- predict(svmRadialKernel_Model,validateData)
svmRadialKernel_Prediction


metrics <- confusionMatrix(svmRadialKernel_Prediction,validateData$default)
metrics$byClass


#########Comparision between Linear and Radial Kernel#######
result <- resamples(list(linear = svmLinear_model,Radial = svmRadialKernel_Model))
result


summary(result)
bwplot(result)
dotplot(result)

#####################################################################








