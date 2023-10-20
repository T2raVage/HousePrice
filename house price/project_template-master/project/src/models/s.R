#this is to clear the data
rm(list=ls())

#we load the respective libraries for predicting
library(data.table)
library(Metrics)
library(dplyr)



#Reading data for both test as well as training datasets respectively
test <- fread("/Users/Sada/Desktop/Stat 380/house price/project_template-master/project/volume/data/raw/Stat_380_test.csv")
test


train <- fread("/Users/Sada/Desktop/Stat 380/house price/project_template-master/project/volume/data/raw/Stat_380_train.csv")
train


#focus only on certain columns and so we create a sub column
sub_train <-train[!is.na(train$SalePrice)][,.(LotFrontage,LotArea,FullBath,Heating,TotRmsAbvGrd,YearBuilt,CentralAir,OverallCond,TotalBsmtSF,BedroomAbvGr,SalePrice,GrLivArea)]

#omitting the empty rows if there are any and the filter function this can be performed with dplyr
sub_train %>% filter(CentralAir=='Y') %>%na.omit()

#create a linear model using linear regression and use all known variables that can predict the outcome as shown in the video lecture example
##we can see that certain variables help in showing efficiency of the code like the addition of OverallCond showed in linearmodel


linearmodel <- glm(SalePrice ~ LotArea +GrLivArea + TotalBsmtSF +Heating+ YearBuilt +TotRmsAbvGrd+ FullBath +  BedroomAbvGr + CentralAir + OverallCond , data = sub_train)
print(linearmodel)


#assign the value to the test data SalePrice and we use the predict function to get the predictions as shown in the video lecture example
test$SalePrice <- predict.glm(linearmodel, test,CentralAir="Y")
print(test)
summary(test)



#finally get the answer and add data into fa or final answer variable as shown in the video lecture example 
fa <- test[,.(Id,SalePrice)]
print(fa)
summary(fa)



#compute rmse to attempt to get a close enough final kaggle score
rmse(train$SalePrice,fa$SalePrice)




#write the answer using fwrite into the submission file called s

fwrite(fa,"/Users/Sada/Desktop/Stat 380/house price/project_template-master/project/volume/data/processed/s.csv")
