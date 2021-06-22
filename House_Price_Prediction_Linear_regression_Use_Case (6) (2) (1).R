#Building Linear Regression model for House Sale Prediction

#1)Check he data Set
#check fo NA
#Check for blank Characters
#Check for the count of NA with the overall data set
#Check for data structures by knowing what types of data are present
#Have to write the csv file which has only ID,sine it will not allow the missforest to be succesfull
#Replace the Blank with NA
#Identify which columns has NA values which hels to know which feature to consider while
#building the model

#2)Imputation Techniques:(Cleansing the Data)
#MICE
#MISSFOREST
#Amelia(not used)
#KNN Technique(not used)

#Note: if there are many NA values then lm function will give error !!!!!!
#Predict the significant value by summary of lm with the whole data set

#3)MODEL Devolpment using linear regression and logisic Regression
#Linear Regression- when to predict saleprice,numerical data for Eg
#Logisitic Regression- when to predict the binomial Data..like Yes or No.etc...i.e predicting whether to give the loan or not in the loan approval Data Set


#Checking the Data

getwd()
rm(list = ls())
trainData <- read.csv("C:\\Users\\A\\Downloads\\train.csv")
testData <- read.csv("C:\\Users\\A\\Downloads\\test.csv")

trainData_new <- read.csv("C:\\Users\\A\\Downloads\\train.csv")
SalePrice <- trainData_new$SalePrice


rm(SalePrice)

trainData <- trainData[-81]

combinedDataSet <- rbind(trainData,testData)



write.csv(combinedDataSet, "combined_id.csv")
combinedDataSetDuplicate <-  read.csv("C:\\Users\\A\\Documents\\combined_id.csv")
combinedDataSetId  <- subset(combinedDataSetDuplicate, select = c(Id))
View(combinedDataSetId)

View(combinedDataSet)

#Checking which columns has NA Values
summary(combinedDataSet)
summary(testData)

#How many NA Values are present
sum(is.na(combinedDataSet))
sum(is.na(testData))


#Checking the structure of Train Data

str(combinedDataSet)
str(testData)

summary(combinedDataSet)

#To Replace the Blank Cell with NA which is usefull for our calculation
for(i in c(1:ncol(combinedDataSet)))
{
  combinedDataSet[i][combinedDataSet[i] == ""] <- NA
}

#To know the NA values in columns and to make seperate data set for those
for(i in c(1:ncol(combinedDataSet)))
{
  
  a <- sum(is.na(combinedDataSet[i])) 
  b <-  names(combinedDataSet[i])
  print(paste(b,"->",a,"NA Values"))
} 

##Alley,poolQC,Fence,MiscFeature,FirePLACEqU,aALEY

Features_which_has_NA_Values <- subset(combinedDataSet,select = c(Alley,PoolQC,Fence,MiscFeature,FireplaceQu))
summary(Features_which_has_NA_Values)
str(Features_which_has_NA_Values)

a <- as.character(Features_which_has_NA_Values$Alley)


is.na(Features_which_has_NA_Values$Alley)
Features_which_has_NA_Values$Alley <- (ifelse(is.na(a)
                                              ,"MISSING",a))
str(Features_which_has_NA_Values$Alley)
Features_which_has_NA_Values$Alley <- as.factor(Features_which_has_NA_Values$Alley)
str(Features_which_has_NA_Values$Alley)
summary(Features_which_has_NA_Values)
View(Features_which_has_NA_Values)

###########################

b <- as.character(Features_which_has_NA_Values$PoolQC)

Features_which_has_NA_Values$PoolQC <- (ifelse(is.na(b)
                                               ,"MISSING",b))
str(Features_which_has_NA_Values$PoolQC)
Features_which_has_NA_Values$PoolQC <- as.factor(Features_which_has_NA_Values$PoolQC)
str(Features_which_has_NA_Values$PoolQC)
summary(Features_which_has_NA_Values)
View(Features_which_has_NA_Values)

###########################

c <- as.character(Features_which_has_NA_Values$Fence)

Features_which_has_NA_Values$Fence <- (ifelse(is.na(c)
                                              ,"MISSING",c))
str(Features_which_has_NA_Values$Fence)
Features_which_has_NA_Values$Fence <- as.factor(Features_which_has_NA_Values$Fence)
str(Features_which_has_NA_Values$Fence)
summary(Features_which_has_NA_Values)
View(Features_which_has_NA_Values)

#############################

d <- as.character(Features_which_has_NA_Values$MiscFeature)

Features_which_has_NA_Values$MiscFeature <- (ifelse(is.na(d)
                                                    ,"MISSING",d))
str(Features_which_has_NA_Values$MiscFeature)
Features_which_has_NA_Values$MiscFeature <- as.factor(Features_which_has_NA_Values$MiscFeature)
str(Features_which_has_NA_Values$MiscFeature)
summary(Features_which_has_NA_Values)
View(Features_which_has_NA_Values)

##############################

e <- as.character(Features_which_has_NA_Values$FireplaceQu)

Features_which_has_NA_Values$FireplaceQu <- (ifelse(is.na(e)
                                                    ,"MISSING",e))
str(Features_which_has_NA_Values$FireplaceQu)
Features_which_has_NA_Values$FireplaceQu <- as.factor(Features_which_has_NA_Values$FireplaceQu)
str(Features_which_has_NA_Values$FireplaceQu)
summary(Features_which_has_NA_Values)
View(Features_which_has_NA_Values)

?subset
###############################
combinedDataSet <- subset(combinedDataSet,select = -c(Alley,PoolQC,Fence,MiscFeature,FireplaceQu))
ncol(combinedDataSet)

combinedDataSet <- cbind(combinedDataSet,Features_which_has_NA_Values)
ncol(combinedDataSet)

################################

sum(is.na(combinedDataSet))

#Converting all character into Factors
Features_which_has_NA_Values=Features_which_has_NA_Values %>% mutate_if(is.character, as.factor)

#####
#Imputation Techniques

#APPLYING missforest imputation techniques for which has less NA values
summary(combinedDataSet)

library(missForest)
imputed_values <- missForest(combinedDataSet)
?missForest()


combinedDataSet <- imputed_values$ximp
summary(combinedDataSet)
sum(is.na(combinedDataSet))

####The Data is clean now so now converting them back to train and test set

#how to seperate the train ans test by row
nrow(combinedDataSet)
View(combinedDataSet)
trainData <- combinedDataSet[1:1460, ]
testData <- combinedDataSet[1461:2919, ]


sum(is.na(trainData))
sum(is.na(testData))

warnings()
#Model Initial Development to find which features to keep
#trainData <- cbind(trainData,trainData_new$SalePrice)
rm(trainData)
str(trainData)
trainData <- cbind(trainData,subset(trainData_new, select = c(81)))
#trainData$SalePrice <- as.factor(trainData$SalePrice)

str(trainData)
View(trainData)


nlevels(trainData$SalePrice)

str(trainData)
model1<-lm(formula = trainData$SalePrice ~ MSSubClass + MSZoning +LotFrontage + 
             LotArea + Street + LotShape + LandContour + LotConfig + LandSlope + 
             Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + 
             OverallCond + YearBuilt + YearRemodAdd + RoofMatl + Exterior1st +
             MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtExposure + 
             BsmtFinSF1 + BsmtFinSF2 + BsmtFinType2 + BsmtUnfSF + CentralAir + X1stFlrSF + X2ndFlrSF + 
             BsmtFullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
             TotRmsAbvGrd + Functional  + GarageCars + GarageArea + 
             GarageQual + GarageCond + PoolArea + PoolQC + MiscFeature + Fence + FireplaceQu,data = trainData)
summary(model1)

vif(model1)

rm(model1)
str(testData)
View(testData)
#testData <- subset(testData,select =-c(Exterior1st))

testData$Saleprice <- predict(model1, testData)

test1 <- data.frame(Id = testData$Id, SalePrice=testData$Saleprice)
head(test1)
write.csv(test1, file = "sample_submission_new.csv", row.names = FALSE)




