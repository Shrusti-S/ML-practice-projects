rm(list = ls())

#Extract Data
temp1<-tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp1)
unzip(temp1,"student-mat.csv")

studentData1<-read.csv("student-mat.csv",sep=";",header=T)
#studentData<-read.csv2("student-mat.csv")
View(studentData)
sum(is.na(studentData1))

unzip(temp1,"student-por.csv")
studentData2<-read.csv2("student-por.csv")
View(studentData2)

studentDataset<-rbind(studentData1,studentData2)
dim(studentDataset)

sum(is.na(studentDataset))
??prodNA()
library(missForest)
studentDataset<-prodNA(studentDataset,noNA=0.1)
sum(is.na(studentDataset))

######################################
??avg
studentDataset$prevScore<-(studentDataset$G1+studentDataset$G2)/2
str(studentDataset)

#Feature Engineering 
#?subset
studentDataset<-subset(studentDataset,select=-c(address,famsize,nursery,Walc,G1,G2))

str(studentDataset)
colnames(studentDataset)[colnames(studentDataset)=="G3"] <- "finalScore"
str(studentDataset)

#Plotting graphs
library(ggplot2)


studentDataset$FinalGrade<-factor(ifelse(studentDataset$finalScore>=median(studentDataset$finalScore),"PASS","FAIL"))
str(studentDataset)

print(ggplot(studentDataset, aes(x=FinalGrade))+geom_bar()+facet_grid(.~sex)+ggtitle("Result of student by Gender of Applicant"))
print(ggplot(studentDataset,aes(x=FinalGrade)) + geom_bar()+facet_grid(.~goout)+ggtitle("result of student regarding the impact of going out with friends(1-5 people)"))

print(ggplot(studentDataset,aes(x=FinalGrade)) + geom_bar()+facet_grid(.~higher)+ggtitle("result of student prediction based on the higher education plans"))

###############
#Imputing Null Values

library(DT)
datatable(head(studentDataset))

sum(is.na(studentDataset$prevScore))
studentDataset$prevScore<-ifelse(is.na(studentDataset$prevScore),0,
                                 studentDataset$prevScore)


studentDataset_imputed<-missForest(studentDataset,maxiter = 2)
studentDataset<-studentDataset_imputed$ximp
View(studentDataset)
colSums(is.na(studentDataset))

str(studentDataset)

studentDataset$FinalGrade<-factor(ifelse(studentDataset$finalScore>=median(studentDataset$finalScore),"PASS","FAIL"))

########
## Splitting training and test data
library(caTools)
split <- sample.split(studentDataset$FinalGrade,SplitRatio = 0.7)
trainData <- subset(studentDataset,split == "TRUE")
validateData <- subset(studentDataset,split == "FALSE")


#######################Logistic Regression###################
str(trainData)
LogisticModel_1 <- glm(FinalGrade ~ . -FinalGrade,trainData[-c(28,27)],family = "binomial")
summary(LogisticModel_1)

#By Doing backward Elimination we came to know that the attributes such as address,Walc,Helth can be removed
step(LogisticModel_1)


LogisticPredict_1 <- predict(LogisticModel_1,validateData[-c(28,27)],type = "response")
summary(LogisticPredict_1)

#Here we have considered threshold value of 0.5
result = ifelse(LogisticPredict_1 > 0.5,"PASS","FAIL")
str(validateData$FinalGrade)

metrics <- confusionMatrix(result,validateData$FinalGrade)
metrics$byClass

#How to find the Threshold -> ROCR

#For this we will take train data to know the threshold for that and apply for test data
predict_train <- predict(LogisticModel_1,trainData,type= "response")
library(ROCR)
ROCRPrediction <- prediction(predict_train,trainData$FinalGrade)
ROCRPerformance <- performance(ROCRPrediction,"tpr","fpr")
plot(ROCRPerformance,print.cutoffs.at=seq(0.1,by=0.1))

#Here we have considered threshold value of 0.4 by looking into the ROCR graph
table(Actualvalue = validateData$FinalGrade,PredictedValue=LogisticPredict_1 >0.4)
table(Actualvalue = validateData$FinalGrade,PredictedValue=LogisticPredict_1 >0.6)
table(Actualvalue = validateData$FinalGrade,PredictedValue=LogisticPredict_1 >0.1)
table(Actualvalue = validateData$FinalGrade,PredictedValue=LogisticPredict_1 >0.9)
table(Actualvalue = validateData$FinalGrade,PredictedValue=LogisticPredict_1 >0.5)
