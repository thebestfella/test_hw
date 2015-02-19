#load library
library(caret)
library(randomForest)
library(kernlab)

#download and load file
data_train<-read.csv("pml-training.csv",na.strings= c("NA",""," "),header=TRUE)

#clean up the 'NA's in original 'training' file
#get a boolean list that indicates if there's no 'NA' exists in the column
data_train_na_list<-apply(data_train,2,function(x){!any(is.na(x))})

#clean up the data that keeps columns that don't have 'NA'
data_train_clean<-data_train[,data_train_na_list]

#remove the first few that're irrelevent
data_train_clean<-data_train_clean[,8:ncol(data_train_clean)]

#seperate training set into training set and validataion
set.seed(12345)
train_row<-createDataPartition(y=data_train_clean$classe,p=0.7,list=FALSE)
final_data_training<-data_train_clean[train_row,]
final_data_validation<-data_train_clean[-train_row,]
    
#fit a random forest model 
#use all the variables except 'class' as predictor
train_model<-randomForest(classe ~.,data=final_data_training)

#apply the second set from training set to validate
validate_model<-predict(train_model,final_data_validation)

#plot validataion
result<-confusionMatrix(final_data_validation$classe, validate_model)

#clean up the 'NA's in original 'testing' file
data_test<-read.csv("pml-testing.csv",na.strings= c("NA",""," "),header=TRUE)
data_test_na_list<-apply(data_test,2,function(x){!any(is.na(x))})
data_test_clean<-data_test[,data_test_na_list]
data_test_clean<-data_test_clean[,8:ncol(data_test_clean)]

#test on testing set
test_model<-predict(train_model,data_test_clean)
