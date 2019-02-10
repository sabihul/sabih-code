sme<-read.csv('sme.csv',header = T)
holiday<-read.csv('holiday_list.csv',header = T)










channel



data_sme<-merge(sme,holiday,by=c('day','month','year'),all = T)

data_sme$holiday<-ifelse(is.na(data_sme$holiday)==TRUE,0,1)

summary(data_sme)

dummy<-function(data,weekname){
  week_name<-unique(data[,weekname])
  channel<-unique(data$channel)
for (i in week_name){
  var<-paste(i,"flag")
  data[,var]<-ifelse(data[,weekname]==i,1,0)
 
  
}
  return(data)
}

data_sme<-dummy(data_sme,weekname = "week_name")
back_office<-data_sme[data_sme$channel=="back_office",]

back_office<-back_office[back_office$year>2014,]

######Arimax#####






train = back_office[ back_office$year>2015 &back_office$month<11,]
for (i in names){
  train[,i]<-as.factor(train[,i])
}



test<-back_office[ back_office$year>2016 &back_office$month==11,]
for (i in names){
  test[,i]<-as.factor(test[,i])
}

train$ds<-NULL
arimax_cols = colnames(back_office)[!(colnames(back_office) %in% c("year","ds","day","month","week","week_name",
                                                                   "channel","count","date","month_index"))]

train.series = ts(train$count,frequency = 7)
train_xreg = train[ ,arimax_cols]
train_xreg$`Sunday flag`<-NULL
test_xreg = test[ ,arimax_cols]
test_xreg$`Sunday flag`<-NULL
library(forecast)
arima_model = auto.arima(train.series,seasonal = T,max.q = 0,xreg = train_xreg,trace = T) 
summary(train)
summary(arima_model)
sum(is.na(train))
plot(arima_model$fitted)
lines(train.series,col="green")

ff = forecast(arima_model,h = 30,xreg = test_xreg)
ff<-as.data.frame(ff$mean)
mape = mean(abs(ff$mean-test$count)/test$count)
temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
temp_results$mape<-









