#data preperation
getwd()
setwd("C:/Users/nirvi/Documents/sem3/ABA")

#data import
data <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#explore data
View(data)
nrow(data)
summary(data)

#Dropping the following variables from the data: "cand_id", "last_name", "first_name", "twitterbirth", "facebookdate", "facebookjan", "youtubebirth".
data$cand_id <- NULL
data$last_name <- NULL
data$first_name <- NULL
data$twitterbirth <- NULL
data$facebookdate <- NULL
data$facebookjan <- NULL
data$youtubebirth <- NULL

#Converting the following variables into factor variables using function as.factor(): "twitter", "facebook", "youtube", "cand_ici", and "gen_election".
data$twitter <- as.factor(data$twitter)
data$facebook <- as.factor(data$facebook)
data$youtube <- as.factor(data$youtube)
data$cand_ici <- as.factor(data$cand_ici)
data$gen_election <- as.factor(data$gen_election)

summary(data)

#removing missing values
data <- data[complete.cases(data),]
n = nrow(data)
n

#Randomly assigning 70% of the observations to train_data and the remaining observations to test_data
trainIndex = sample(1:n,
size = round(0.7*n),
replace=FALSE)
train_data = data[trainIndex,]
test_data = data[-trainIndex,]
summary(train_data)
summary(test_data)

install.packages("randomForest")
library(randomForest)

##random forest classifier with 10 trees
rf <-randomForest(gen_election~., data=train_data, ntree=10, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 20 trees
rf <-randomForest(gen_election~., data=train_data, ntree=20, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 30 trees
rf <-randomForest(gen_election~., data=train_data, ntree=30, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 40 trees
rf <-randomForest(gen_election~., data=train_data, ntree=40, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 50 trees
rf <-randomForest(gen_election~., data=train_data, ntree=50, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 60 trees
rf <-randomForest(gen_election~., data=train_data, ntree=60, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 70 trees
rf <-randomForest(gen_election~., data=train_data, ntree=70, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 80 trees
rf <-randomForest(gen_election~., data=train_data, ntree=80, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 90 trees
rf <-randomForest(gen_election~., data=train_data, ntree=90, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#random forest classifier with 100 trees
rf <-randomForest(gen_election~., data=train_data, ntree=100, na.action=na.exclude, importance=T,proximity=T)
print(rf)

#Using tuneRF() function to find the best value for mtry
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=90,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, , na.action=na.exclude)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)
print(mtry)
rf <-randomForest(gen_election~., data=train_data, mtry=best.m, importance=TRUE, ntree=90)
print(rf)
importance(rf)

#Using varImpPlot() to create the plot for variable importance
varImpPlot(rf)

#Using library(caret) for creating the confusion matrix for test_data
library(caret)
predicted_values <- predict(rf, test_data,type= "prob")
head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, "W", "L") )
levels(test_data$gen_election)[2]
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

#calculating AUC and create the ROC curve
library(ROCR)
library(ggplot2)
predicted_values <- predict(rf, test_data,type= "prob")[,2] 
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


#Using library(nnet) for building a neural network classifier
install.packages("nnet")
library(nnet)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000)
summary(ann)
print(ann)

#creating the confusion matrix for test_data
predicted_values <- predict(ann, test_data,type= "raw")
head(predicted_values)
threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,1] > threshold, "W", "L") )
head(pred)
levels(test_data$gen_election)[2]
confusionMatrix(pred, test_data$gen_election, positive = levels(test_data$gen_election)[2])

#calculating AUC and create the ROC curve
predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#increasing the number of inner nodes
#10 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=10, maxit=1000)
summary(ann)
print(ann)

#20 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=20, maxit=1000)
summary(ann)
print(ann)

#21 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=21, maxit=1000)
summary(ann)
print(ann)

#22 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=22, maxit=1000)
summary(ann)
print(ann)

#23 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=23, maxit=1000)
summary(ann)
print(ann)

#24 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)
summary(ann)
print(ann)

#25 nodes
ann <- nnet(gen_election ~ ., data=train_data, size=25, maxit=1000)
summary(ann)
print(ann)

predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

ftable(xtabs(~twitter+gen_election,data=data))
ftable(xtabs(~facebook+gen_election,data=data))
ftable(xtabs(~youtube+gen_election,data=data))
