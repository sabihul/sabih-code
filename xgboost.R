library(caret)
library(xgboost)
library(dplyr)
library(ggplot2)
train = back_office[ back_office$year>2015 &back_office$month<11,]
test<-back_office[ back_office$year>2016 &back_office$month==11,]
str(train)
names(train)[4]<-"Datetime"
names(test)[4]<-"Datetime"
train$Datetime<-as.Date(train$Datetime,"%Y-%m-%d")
test$Datetime<-as.Date(test$Datetime,"%Y-%m-%d")


train$year<-as.numeric(format(train$Datetime, "%Y"))
test$year<-as.numeric(format(test$Datetime, "%Y"))
train$month<-as.numeric(format(train$Datetime, "%m"))
test$month<-as.numeric(format(test$Datetime, "%m"))
train$day<-as.numeric(format(train$Datetime, "%d"))
test$day<-as.numeric(format(test$Datetime, "%d"))
train$weekday<-as.factor(weekdays(train$Datetime))
test$weekday<-as.factor(weekdays(test$Datetime))






                                         ))
predictors<-c("day","month","year","week","holiday","Wednesday flag","Thursday flag","Friday flag"
              ,"Sunday flag","Saturday flag","Monday flag","Tuesday flag")

library(xgboost)
dtrain <- xgb.DMatrix(data=as.matrix(train[,predictors]), label=train$y)
dtest <- xgb.DMatrix(data=as.matrix(test[,predictors]))



evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  precision <- sum(preds & labels) / sum(preds)
  recall <- sum(preds & labels) / sum(labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  return(list(metric = "F-Score", value = fmeasure))
}


param <- list(
  objective = "reg:linear",
  eval_metric = "rmse",  
  eta = 0.3,
  #  lambda = 2
  max_depth = 2
)



cv <- xgb.cv(
  params = param, 
  data = dtrain, 
  nrounds = 10000, 
  #  watchlist = watchlist,
  nfold = 10,  
  maximize = F,
  early_stopping_rounds = 20,
  print_every_n = 50
)

xgb_price <- xgb.train(
  params = param, 
  data = dtrain, 
  nrounds = 1500
  #  watchlist = watchlist,
  #  maximize = F,
  #  early_stopping_rounds = 20
)

param2 <- list(
  objective = "reg:linear",
  eval_metric = "rmse",  
  eta = 0.3,
  #  lambda = 2
  max_depth = 2
)



library(Ckmeans.1d.dp)
gg <- xgb.ggplot.importance(xgb.importance(model = xgb_price,feature_names =  predictors), measure = "Gain", rel_to_first = TRUE)
gg + ggplot2::ylab("Frequency")

y_pred_price<-exp(predict(object = xgb_price,newdata = dtest))-1
y<-as.data.frame(y_pred_price)
write.csv(y,"y.csv")
