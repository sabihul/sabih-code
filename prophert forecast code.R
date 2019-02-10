#####prophet###
library(lubridate)
library(prophet)
library(reader)
cards<-read.csv('cards_voice.csv',header = T)
cards$count<-as.numeric(as.character(cards$count))
str(cards)
holiday<-read.csv('holiday_list.csv',header = T)
cards = aggregate(data = cards,count~date+year+month+day+week+weekname+channel,FUN=sum,na.rm=T)



channel<-as.character(unique(cards$channel))
#channel<-channel[c(1,3:4)]
#result<-NULL
result_future<-NULL
prophet_fn<-function(data){
  for ( i in channel){
    #library(prophet)
    #library(readr)
    print(i)
    temp<-data[data$channel==i,]
    temp<-temp[temp$year>2015,]
    
    #temp<-temp[temp$year>2014,]
    temp_x<-temp[,c(1,ncol(temp))]
    holiday<-holiday[holiday$year>2015,]
    library(dplyr)
    names(temp_x)[2]<-"y"
    names(temp_x)[1]<-"ds"
    temp_x$ds<-as.Date(temp_x$ds,"%m/%d/%Y")
    holid<-holiday[,c(1,5)]
    holid$ds<-as.Date(holid$ds,"%m/%d/%Y")
    temp_x<-temp_x[order(temp_x$ds),]
    
    m<-prophet(temp_x,holidays = holid,daily.seasonality = T,weekly.seasonality = T,yearly.seasonality = T,
               holidays.prior.scale = 20)
    
    #cross_validate<-cross_validation(m, 60, "days")
    
    #cross_validate<-as.data.frame(cross_validate)
    
    #cross_validate$day<-day(cross_validate$ds)
    #cross_validate$month<-month(cross_validate$ds)
    #cross_validate$year<-year(cross_validate$ds)
    #cross_validate$weekname<-weekdays(cross_validate$ds)
    #cross_validate$channel<-i
    #print(cross_validate)
    #result<-rbind(result,cross_validate)
    
    future <-make_future_dataframe(m, periods = 75,freq = "day",include_history = F)
    forecast <- predict(m, future)
    forecast$channel<-i
    print(forecast)
    forecast<-as.data.frame(forecast)
    result_future<-rbind(result_future,forecast)
    
    
  }
  return(result_future)
  #return(list(result,result_future))
}


data_cards<-prophet_fn(cards)
#cross_valid<-as.data.frame(data_cards[1])
future_forecast<-as.data.frame(data_cards)
#write.csv(cross_valid,'cards_cross_valid_yearly_new.csv',row.names = F)
write.csv(future_forecast,'cards_voice_future_forecast_april.csv',row.names = F)
