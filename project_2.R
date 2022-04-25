library(adabag)
library(rpart.plot)
library(pROC)
library(xgboost)
library(caret)
library(ROSE)

setwd("C:/Users/ViJay/Desktop/POM-681/project_2")
mydata <- read.csv("train.csv", header=TRUE)
mydata$Buy <- as.factor(mydata$Buy)
# spliting of train dataset
set.seed(086)
var <- sample(2,nrow(mydata),replace= T,prob=c(0.7,0.3))
train_data <-mydata[var==1,]
test_data <- mydata[var==2,]

test <- read.csv("test.csv", header=TRUE)

#logistic regression model
lgc <- glm(Buy ~ . , data = train_data, family = 'binomial')
summary(lgc)
#confusion matrix for train
p1<- predict(lgc,train_data, type = 'response')
pred1 <- ifelse (p1 > 0.5,1,0)
cf_matrix <-table(predicted = pred1, Actual = train_data$Buy)
cf_matrix
#train accuracy
train_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
train_accuracy

# confusion matrix for testing
p2<- predict(lgc,test_data, type = 'response')
pred2 <- ifelse (p2 > 0.5,1,0)
cf_matrix2 <- table(predicted = pred2,Actual=test_data$Buy)
cf_matrix2
# accuracy
test_accuracy = sum(diag(cf_matrix2))/sum(cf_matrix2)
test_accuracy

#################################################################################
# taking significant variables
final_lgc <- glm(Buy ~ V48+V56+V60+V77+V83, data=train_data,family= 'binomial')
summary(final_lgc)

#confusion matrix for train
p3<- predict(final_lgc,train_data, type = 'response')
pred3 <- ifelse (p3 > 0.5,1,0)
cf_matrix <-table(predicted = pred3, Actual = train_data$Buy)
cf_matrix
#train accuracy
train_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
train_accuracy

# confusion matrix for testing
p4<- predict(final_lgc,test_data, type = 'response')
p4
pred4 <- ifelse (p4 > 0.5,1,0)
cf_matrix <- table(predicted = pred4, Actual = test_data$Buy)
cf_matrix
# test accuracy
test_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
test_accuracy


# lets create submission file
p5<- predict(final_lgc,newdata=test, type ='response')
p5
submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p5
write.csv(submission,'submission.csv')

########### Over Sampling for logistic Regression #################################


data <- read.csv("C:/Users/ViJay/Desktop/POM-681/project_2/train.csv", header=TRUE)
data$Buy <- as.factor(data$Buy)

# over sampling
library(ROSE)
over<- ovun.sample(Buy~.,data=train_data, method = "over",N=5454)$data
table(over$Buy)
final_lgc <- glm(Buy ~ V48+V56+V60+V77+V83, data=over,family= 'binomial')
summary(final_lgc)

#confusion matrix for train
p6<- predict(final_lgc,train_data, type = 'response')
pred6 <- ifelse (p6 > 0.5,1,0)
cf_matrix <-table(predicted = pred6, Actual = train_data$Buy)
cf_matrix
#train accuracy
train_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
train_accuracy

# confusion matrix for testing
p7<- predict(final_lgc,test_data, type = 'response')
pred7 <- ifelse (p7 > 0.5,1,0)
cf_matrix <- table(predicted = pred7, Actual = test_data$Buy)
cf_matrix
# test accuracy
test_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
test_accuracy


# lets create submission file
p8<- predict(final_lgc,newdata=test, type ='response')
p8

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p8
write.csv(submission,'submission.csv')

########## Under Sampling for logistic regression ######################

# by using under sampling method
data <- read.csv("C:/Users/ViJay/Desktop/POM-681/project_2/train.csv", header=TRUE)
# under sampling

under<- ovun.sample(Buy~.,data=train_data, method = "under",N=354)$data
table(under$Buy)
final_lgc <- glm(Buy ~ V48+V56+V60+V77+V83, data=under,family= 'binomial')
summary(final_lgc)

#confusion matrix for train
p9<- predict(final_lgc,train_data, type = 'response')
pred9 <- ifelse (p9 > 0.5,1,0)
cf_matrix <-table(predicted = pred9, Actual = train_data$Buy)
cf_matrix
#train accuracy
train_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
train_accuracy

# confusion matrix for testing
p10<- predict(final_lgc,test_data, type = 'response')
pred10 <- ifelse (p10 > 0.5,1,0)
cf_matrix <- table(predicted = pred10, Actual = test_data$Buy)
cf_matrix
# test accuracy
test_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
test_accuracy


# lets create submission file
p11<- predict(final_lgc,newdata=test, type ='response')
p11

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p11
write.csv(submission,'submission.csv')

############### Both Sampling for Logistic Regression #########################
# by using Both sampling method
data <- read.csv("C:/Users/ViJay/Desktop/POM-681/project_2/train.csv", header=TRUE)
# both sampling
library(ROSE)
both<- ovun.sample(Buy~.,data=train_data, method = "both",p=0.5,seed=086,N=2904)$data
table(both$Buy)
final_lgc <- glm(Buy ~ V48+V56+V60+V77+V83, data=both,family= 'binomial')
summary(final_lgc)

#confusion matrix for train
p12<- predict(final_lgc,train_data, type = 'response')
pred12 <- ifelse (p12 > 0.5,1,0)
cf_matrix <-table(predicted = pred12, Actual = train_data$Buy)
cf_matrix
#train accuracy
train_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
train_accuracy

# confusion matrix for testing
p13<- predict(final_lgc,test_data, type = 'response')
pred13 <- ifelse (p13 > 0.5,1,0)
cf_matrix <- table(predicted = pred13, Actual = test_data$Buy)
cf_matrix
# test accuracy
test_accuracy = sum(diag(cf_matrix))/sum(cf_matrix)
test_accuracy


# lets create submission file
p14<- predict(final_lgc,newdata=test, type ='response')
p14

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p14
write.csv(submission,'submission.csv')

################## Random Forest ######################

library(randomForest)
set.seed(086)
rforest <- randomForest(Buy ~.,data=train_data)
print(rforest) 

# prediction and confusion matrix for training data
library(caret)
p15 <- predict(rforest,train_data)
confusionMatrix(p15,train_data$Buy,positive = '1')

# prediction and confusion matrix- test data
p16 <- predict(rforest,test_data)
confusionMatrix(p16,test_data$Buy, positive='1')

# Error rate of random forest
plot(rforest) 
# tune mtry
set.seed(086)
t <- tuneRF(train_data[,-87],
            train_data[, 87], 
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry =400,
            trace=TRUE, 
            improve=0.01)
# improve the model as
set.seed(086)
final_forest <- randomForest(Buy ~.,data=train_data,
                             ntree=400,
                             mtry=2,
                             importance=TRUE,
                             proximity=TRUE)
print(final_forest)
# prediction and confusion matrix for training_data
p17<- predict(final_forest,train_data)
confusionMatrix(p17,train_data$Buy,positive = '1')

# prediction and confusion matrix for testing_data
p18 <- predict(final_forest,test_data)
confusionMatrix(p18,test_data$Buy,positive = '1')
####
# lets create submission file
p19<- predict(final_forest,newdata=test,type = 'prob')
p19

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p19[,2]
write.csv(submission,'submission.csv')

############# Over sampling for Random Forest ############################

# over sampling
library(ROSE)
over<- ovun.sample(Buy~.,data=train_data, method = "over",N=5454)$data
table(over$Buy)
library(randomForest)
# improve the model as
set.seed(086)
final_forest <- randomForest(Buy ~.,data=over,
                             ntree=400,
                             mtry=2,
                             importance=TRUE,
                             proximity=TRUE)
print(final_forest)
# prediction and confusion matrix for training_data
p20<- predict(final_forest,train_data)
confusionMatrix(p20,train_data$Buy,positive = '1')

# prediction and confusion matrix for testing_data
p21 <- predict(final_forest,test_data)
confusionMatrix(p21,test_data$Buy,positive = '1')
####
#ROC
p22<- predict(final_forest, newdata= test_data,type= 'prob')
p22
roc1 <- roc(test_data$Buy,p22[,2])
auc(roc1)
plot(roc1,col= c(2))

# lets create submission file
p23<- predict(final_forest,newdata=test, type ='prob')
p23

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p23[,2]
write.csv(submission,'submission.csv')

########### Under sampling for Random Forest ###########################

library(ROSE)
under<- ovun.sample(Buy~.,data=train_data, method = "under",N=354)$data
table(under$Buy)
library(randomForest)
set.seed(086)
# improve the model as
set.seed(086)
final_forest <- randomForest(Buy ~.,data=under,
                             ntree=400,
                             mtry=2,
                             importance=TRUE,
                             proximity=TRUE)
print(final_forest)
# prediction and confusion matrix for training_data
p24<- predict(final_forest,train_data)
confusionMatrix(p24,train_data$Buy,positive = '1')

# prediction and confusion matrix for testing_data
p25 <- predict(final_forest,test_data)
confusionMatrix(p25,test_data$Buy, positive = '1')
####
#ROC
p26<- predict(final_forest, newdata= test_data,type= 'prob')
roc2 <- roc(test_data$Buy,p26[,2])
auc(roc2)
plot(roc2,col= c(3))

# lets create submission file
p27<- predict(final_forest,newdata=test, type = 'prob')
p27 

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p27[,2]
write.csv(submission,'submission.csv')

############## Both sampling for Random Forest ##########################
both<- ovun.sample(Buy~.,data=train_data,method = "both",p= 0.5, seed= 086,N=2904)$data
table(both$Buy)
library(randomForest)

set.seed(086)
final_forest <- randomForest(Buy ~.,data=both,
                             ntree=400,
                             mtry=2,
                             importance=TRUE,
                             proximity=TRUE)
print(final_forest)
# prediction and confusion matrix for training_data
p28<- predict(final_forest,train_data)
confusionMatrix(p28,train_data$Buy,positive = '1')

# prediction and confusion matrix for testing_data
p29 <- predict(final_forest,test_data)
confusionMatrix(p29,test_data$Buy,positive = '1')
####
#ROC
p30<- predict(final_forest, newdata= test_data,type= 'prob')
roc3 <- roc(test_data$Buy,p30[,2])
auc(roc3)
plot(roc3,col= c(4))

# lets create submission file
p31<- predict(final_forest,newdata=test, type = 'prob')
p31

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p31[,2]
write.csv(submission,'submission.csv')
#comparision of roc curve
plot(roc1, col=(2),main = "ROC Curve for Random Forest")
plot(roc2, add = T, col=(3))
plot(roc3,add = T, col = (4))
legend(x = "bottomright", legend = c("Over_sample"," under_sample","Both_sample"), fill = c(2,3,4))

################ Extreme Gradient Boosting ######################################

library(adabag)
library(rpart.plot)
library(pROC)
library(xgboost)
library(dplyr)
library(caret)

set.seed(086)
cv <- trainControl (method = 'repeatedcv', number =10,repeats=5,allowParalle=TRUE)
set.seed(086)
boosting <- train(Buy~.,data= train_data,
                  method= 'xgbTree',
                  trControl= cv,
                  tuneGrid= expand.grid(nrounds=200,
                                        max_depth=1,
                                        eta=0.1,
                                        gamma=0.01,
                                        colsample_bytree=1,
                                        min_child_weight=1,
                                        subsample=1))
boosting
# confusion matrix for training data
p32<- predict(boosting,newdata=train_data,type='raw')
confusionMatrix(p32,train_data$Buy,positive = '1')

# confusion matrix for testing data
p33<- predict(boosting,newdata=test_data,type='raw')
confusionMatrix(p33,test_data$Buy,positive = '1')

#ROC
p34<- predict(boosting, newdata= test_data,type= 'prob')
roc <- roc(test_data$Buy,p34[,2])
auc(roc)
plot(roc,col= c(4))

# lets create submission file
p35<- predict(boosting,newdata=test,type='prob')
p35

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p35[,2]
write.csv(submission,'submission.csv')

############# Over Sampling for Boosting ######################

setwd("C:/Users/ViJay/Desktop/POM-681/project_2")
data <- read.csv("train.csv", header=TRUE)
data$Buy <- as.factor(data$Buy)
table(data$Buy)
test <- read.csv("test.csv", header=TRUE)
summary(test)
# spliting of  dataset
set.seed(086)
var <- sample(2,nrow(data),replace= TRUE,prob=c(0.7,0.3))
train_data <- data[var==1,]
table(train_data$Buy)
test_data <- data[var==2,]
table(test_data$Buy)
library(ROSE)
over<- ovun.sample(Buy~.,data=train_data,method = "over",N=5454)$data
table(over$Buy)


set.seed(086)
cv <- trainControl (method = 'repeatedcv', number =10,repeats=5,allowParalle=TRUE)
set.seed(086)
boosting <- train(Buy~.,data= over,
                  method= 'xgbTree',
                  trControl= cv,
                  tuneGrid= expand.grid(nrounds=200,
                                        max_depth=1,
                                        eta=0.1,
                                        gamma=0.01,
                                        colsample_bytree=1,
                                        min_child_weight=1,
                                        subsample=1))
boosting
# confusion matrix for training data
p36<- predict(boosting,newdata=train_data,type='raw')
confusionMatrix(p36,train_data$Buy,positive='1')

# confusion matrix for testing data
p37<- predict(boosting,newdata=test_data,type='raw')
confusionMatrix(p37,test_data$Buy,positive='1')

#ROC
p38<- predict(boosting, newdata= test_data,type= 'prob')
roc_1 <- roc(test_data$Buy,p38[,2])
auc(roc_1)
plot(roc_1,col= c(2))

# lets create submission file
p39<- predict(boosting,newdata=test,type='prob')
p39 


submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p39[,2]
write.csv(submission,'submission.csv')

########### Under Sampling for Boosting ######################################

under<- ovun.sample(Buy~.,data=train_data,method = "under",N=354)$data
table(under$Buy)
summary(under)

set.seed(086)
cv <- trainControl (method = 'repeatedcv', number =10,repeats=5,allowParalle=TRUE)
set.seed(086)
boosting <- train(Buy~.,data= under,
                  method= 'xgbTree',
                  trControl= cv,
                  tuneGrid= expand.grid(nrounds=200,
                                        max_depth=1,
                                        eta=0.1,
                                        gamma=0.01,
                                        colsample_bytree=1,
                                        min_child_weight=1,
                                        subsample=1))
boosting
# confusion matrix for training data
p40<- predict(boosting,newdata=train_data,type='raw')
confusionMatrix(p40,train_data$Buy,positive = '1')

# confusion matrix for testing data
p41<- predict(boosting,newdata=test_data,type='raw')
confusionMatrix(p41,test_data$Buy,positive = '1')

#ROC
p42<- predict(boosting, newdata= test_data,type= 'prob')
roc_2 <- roc(test_data$Buy,p42[,2])
auc(roc_2)
plot(roc_2,col= c(3))

# lets create submission file
p43<- predict(boosting,newdata=test,type='prob')
p43

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p43[,2]
write.csv(submission,'submission.csv')

################# Both sampling for Boosting ##########################

both<- ovun.sample(Buy~.,data=train_data,method = "both",p= 0.5, seed= 086,N=2904)$data
table(both$Buy)
set.seed(086)
cv <- trainControl (method = 'repeatedcv', number =10,repeats=5,allowParalle=TRUE)
set.seed(086)
boosting <- train(Buy~.,data= both,
                  method= 'xgbTree',
                  trControl= cv,
                  tuneGrid= expand.grid(nrounds=200,
                                        max_depth=1,
                                        eta=0.1,
                                        gamma=0.01,
                                        colsample_bytree=1,
                                        min_child_weight=1,
                                        subsample=1))
boosting
# confusion matrix for training data
p44<- predict(boosting,newdata=train_data,type='raw')
confusionMatrix(p44,train_data$Buy,positive = '1')

# confusion matrix for testing data
p45<- predict(boosting,newdata=test_data,type='raw')
confusionMatrix(p45,test_data$Buy,positive = '1')

#ROC
p46<- predict(boosting, newdata= test_data,type= 'prob')
roc_3 <- roc(test_data$Buy,p46[,2])
auc(roc_3)
plot(roc_3,col= c(4))


###################################################
#comparision of roc curve
plot(roc_1, col=(2),main = "ROC Curve for Extreme Gradient Boosting")
plot(roc_2, add = T, col=(3))
plot(roc_3,add = T, col = (4))
legend(x = "bottomright", legend = c("Over_sample"," under_sample","Both_sample"), fill = c(2,3,4))
########################################
# lets create submission file
p47<- predict(boosting,newdata=test,type='prob')
p47

submission <- test$Id
submission <- data.frame(submission)
submission$Predicted <- p47[,2]
write.csv(submission,'submission.csv')



