#install and import required packages
install.packages("tidyverse")
library("tidyverse")
install.packages("glmnet", repos = "https://cran.us.r-project.org")
library("glmnet")
#stop installing/importing required packages

#load the csv file
housing <- read.csv("housing.csv",header=TRUE)
#eda (color not intended) the data frame
housing <- housing |>
  select(c(-1,-2,-3)) |>
  mutate(isRenovated=yr_renovated!=0,
    hasBasement=sqft_basement!=0,
    isWaterfront=waterfront!=0) |>
  select(c(-15,-16,-17,-7,-12,-14))
#impute the data frame
housing$bedrooms[is.na(housing$bedrooms)]<-mean(housing$bedrooms[!is.na(housing$bedrooms)])
housing$bathrooms[is.na(housing$bathrooms)]<-mean(housing$bathrooms[!is.na(housing$bathrooms)])
#train-test split the data frame
split=floor(nrow(housing)*.8)
housingTrain<-housing|>slice(1:split)
housingTest<-housing|>slice(split+1:nrow(housing))
#fit the model to the train and make predictions
housingTrainPrice<-as.matrix(housingTrain$price,nrow=1)
housingTrainPredictors<-as.matrix(select(housingTrain,sqft_living,view,grade,yr_built,isWaterfront,bedrooms,bathrooms,sqft_lot15,floors))
#grade,yr_built,isWaterfront,bedrooms,bathrooms,sqft_lot15,floors
housingTestPrice<-as.matrix(housingTest$price,nrow=1)
housingTestPredictors<-as.matrix(select(housingTest,sqft_living,view,grade,yr_built,isWaterfront,bedrooms,bathrooms,sqft_lot15,floors))
predictionsTrain<-predict(glmnet(housingTrainPredictors,housingTrainPrice,alpha=0),newx=housingTrainPredictors)[,"s99"]
residualsTrain<-(housingTrain$price)-predictionsTrain
predictionsTest<-predict(glmnet(housingTrainPredictors,housingTrainPrice,alpha=0),newx=housingTestPredictors)[,"s99"]
residualsTest<-(housingTest$price)-predictionsTest
SSresTrain<-sum(residualsTrain^2)
SSresTest<-sum(residualsTest^2)
SStotTrain<-sum((housingTrain$price-mean(housingTrain$price))^2)
nTrain<-nrow(housingTrain)
nTest<-nrow(housingTest)
K<-ncol(housingTrainPredictors)
adjustedRSquared<-1-(SSresTrain/(nTrain-K))/(SStotTrain/(n-1))
rmse<-sqrt(sum(residualsTest^2)/nTest)
cat("Adjusted R-Squared on Train Data: ", adjustedRSquared, "\n")
cat("RMSE on Test Data: ", rmse, "\n")