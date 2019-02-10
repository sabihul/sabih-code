model_building_testing = function(data,start,end,h,channel)
{
  
  results=NULL
  
  for(i in start:(end-1))
  {
    print(i)
    train = data[ data$week_index<=i,]
    test= data[ data$week_index >i & data$week_index <= (i+13),]
    train = train[ order(train$week_index),] 
    test = test[ order(test$week_index),]
    horizon = h
    if( nrow(test) <h)
    {
      horizon=nrow(test)
    }
    
    train.series = ts(train$count,start = c(2014,01),frequency = 52)
    train.series.log = log(train.series)
    
    
    
    arima_model = auto.arima(train.series,seasonal = T) 
    print(summary(arima_model))
    ff = forecast(arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
  
   

    log_arima_model = auto.arima(train.series.log,seasonal = T) 
    ff = forecast(log_arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    #print(ff)
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "log_arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
    
    ets_model = ets(train.series) 
    ff = forecast(ets_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
  
    ###### stlf
    stlm_model = stlm(x = train.series,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "stlm"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm log
    
    stlm_model = stlm(x = train.series.log,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "stlm_log"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm ets
    
    stlm_model = stlm(x = train.series,method = "ets") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = (as.numeric(ff$mean)))
    temp_results$model = "stlm_ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
  
  
  
  
  
  }
  results$diff = results$actuals - results$forecast
  results$ape = abs(results$diff)/results$actuals
  results$bias = results$diff/results$actuals
  results$accuracy = results$forecast/results$actuals
  correction=modify(actual_results = results,h = 13)
  
#   ### build a new model
#   
#   
#   train = data[ order(data$week_index),]
#   train.series = ts(data = train$count,start = c(2014,01),frequency = 52)
#   arima_model = auto.arima(train.series,seasonal = T)
#   prediction = as.numeric(forecast(arima_model,h = h)$mean)
#   
#   final_results = data.frame(forecast=prediction)
#   final_results$week_index = end
#   final_results$week_index = end + (1:nrow(final_results))
#   print(correction)
#   bias_correc = mean(correction$bias_correc[ correction$trained_till >= (max(correction$trained_till)-13)])
#   
#   final_results$mod_forecast = final_results$forecast+bias_correc
  return(results)
  
}

model_building_testing_2 = function(data,start,end,h,lob,channel)
{
  results=NULL
  data = data[ order(data$week_index),]
  for(i in start:(end-1))
  {
    print(i)
    train = data[ data$week_index<=i,]
    test= data[ data$week_index >i & data$week_index <= (i+h),]
    train = train[ order(train$week_index),] 
    test = test[ order(test$week_index),]
    horizon = h
    
    if( nrow(test) <h)
    {
      horizon=nrow(test)
    }
    
    train.series = ts(train$count,frequency = 4)
    train.series.log = log(train.series)
    
    
    
    arima_model = auto.arima(train.series,seasonal = T,) 
    ff = forecast(arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
    
    
    
    log_arima_model = auto.arima(train.series.log,seasonal = T,) 
    ff = forecast(log_arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    #print(ff)
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "log_arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
    
    ets_model = ets(train.series) 
    ff = forecast(ets_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    ###### stlf
    stlm_model = stlm(x = train.series,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "stlm"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm log
    
    stlm_model = stlm(x = train.series.log,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "stlm_log"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm ets
    
    stlm_model = stlm(x = train.series,method = "ets") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = (as.numeric(ff$mean)))
    temp_results$model = "stlm_ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    
    
    
    
  }
  results$diff = results$actuals - results$forecast
  results$ape = abs(results$diff)/results$actuals
  results$bias = results$diff/results$actuals
  results$accuracy = results$forecast/results$actuals
  #correction=modify(actual_results = results,h = 13)
  
  #   ### build a new model
  #   
  #   
  #   train = data[ order(data$week_index),]
  #   train.series = ts(data = train$count,start = c(2014,01),frequency = 52)
  #   arima_model = auto.arima(train.series,seasonal = T)
  #   prediction = as.numeric(forecast(arima_model,h = h)$mean)
  #   
  #   final_results = data.frame(forecast=prediction)
  #   final_results$week_index = end
  #   final_results$week_index = end + (1:nrow(final_results))
  #   print(correction)
  #   bias_correc = mean(correction$bias_correc[ correction$trained_till >= (max(correction$trained_till)-13)])
  #   
  #   final_results$mod_forecast = final_results$forecast+bias_correc
  return(results)
}

model_building_testing_monthly = function(data,start,end,h,lob,channel)
{
  results=NULL
  data = data[ order(data$month_index),]
  
  arimax_cols = colnames(data)[!(colnames(data) %in% c("year","month","channel","count","month_index"))]
  
  for(i in start:(end-1))
  {
    print(i)
    train = data[ data$month_index<=i,]
    test= data[ data$month_index >i & data$month_index <= (i+h),]
    train = train[ order(train$month_index),] 
    test = test[ order(test$month_index),]
    horizon = h
    if( nrow(test) <h)
    {
      horizon=nrow(test)
    }
    
    train.series = ts(train$count,frequency = 12)
    train.series.log = log(train.series)
    
    
    
    arima_model = auto.arima(train.series,seasonal = T) 
    ff = forecast(arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
    
    
    
    log_arima_model = auto.arima(train.series.log,seasonal = T) 
    ff = forecast(log_arima_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    #print(ff)
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "log_arima"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i    
    results = rbind(temp_results,results)
    
    ets_model = ets(train.series) 
    ff = forecast(ets_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    ?arima
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    ###### stlf
    stlm_model = stlm(x = train.series,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
    temp_results$model = "stlm"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm log
    
    stlm_model = stlm(x = train.series.log,method = "arima") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
    temp_results$model = "stlm_log"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    
    ####### stlm ets
    
    stlm_model = stlm(x = train.series,method = "ets") 
    ff = forecast(stlm_model,h = horizon)
    mape = mean(abs(ff$mean-test$count)/test$count)
    
    ## store in a temp data.frame
    temp_results = data.frame(actuals = test$count,forecast = (as.numeric(ff$mean)))
    temp_results$model = "stlm_ets"
    temp_results$step_ahead = 1:nrow(temp_results)
    temp_results$trained_till = i
    results = rbind(temp_results,results)
    
    ########## ARIMAX
    ##loop over 
    for( arxcol in arimax_cols)
    {
      train_xreg = train[ ,arxcol]
      test_xreg = test[ ,arxcol]
      arima_model = auto.arima(train.series,seasonal = T,xreg = train_xreg) 
      ff = forecast(arima_model,h = horizon,xreg = test_xreg)
      mape = mean(abs(ff$mean-test$count)/test$count)
      
      ## store in a temp data.frame
      temp_results = data.frame(actuals = test$count,forecast = as.numeric(ff$mean))
      temp_results$model = paste("arimax",arxcol,sep = "_")
      temp_results$step_ahead = 1:nrow(temp_results)
      temp_results$trained_till = i    
      results = rbind(temp_results,results)
      
    }
    
    for( arxcol in arimax_cols)
    {
      train_xreg = train[ ,arxcol]
      test_xreg = test[ ,arxcol]
      arima_model = auto.arima(train.series.log,seasonal = T,xreg = train_xreg) 
      ff = forecast(arima_model,h = horizon,xreg = test_xreg)
      mape = mean(abs(ff$mean-test$count)/test$count)
      
      ## store in a temp data.frame
      temp_results = data.frame(actuals = test$count,forecast = exp(as.numeric(ff$mean)))
      temp_results$model = paste("log_arimax",arxcol,sep = "_")
      temp_results$step_ahead = 1:nrow(temp_results)
      temp_results$trained_till = i    
      results = rbind(temp_results,results)
      
    }
    
    
  }
  results$diff = results$actuals - results$forecast
  results$ape = abs(results$diff)/results$actuals
  results$bias = results$diff/results$actuals
  results$accuracy = results$forecast/results$actuals
  #correction=modify(actual_results = results,h = 13)
  
  #   ### build a new model
  #   
  #   
  #   train = data[ order(data$week_index),]
  #   train.series = ts(data = train$count,start = c(2014,01),frequency = 52)
  #   arima_model = auto.arima(train.series,seasonal = T)
  #   prediction = as.numeric(forecast(arima_model,h = h)$mean)
  #   
  #   final_results = data.frame(forecast=prediction)
  #   final_results$week_index = end
  #   final_results$week_index = end + (1:nrow(final_results))
  #   print(correction)
  #   bias_correc = mean(correction$bias_correc[ correction$trained_till >= (max(correction$trained_till)-13)])
  #   
  #   final_results$mod_forecast = final_results$forecast+bias_correc
  return(results)
}







modify = function(actual_results,h)
{
  
  actual_results$looking_at = actual_results$trained_till+actual_results$step_ahead
  temp=NULL
  models_start_from = sort(unique(actual_results$trained_till))
  
  check_start = min(models_start_from) + 13
  for( model in unique(actual_results$model))
  {
    for(i in check_start: max(models_start_from))
    {
      #print(i)
      temp_res = actual_results[ actual_results$model==model,]
      avg_err = mean(temp_res$diff[ temp_res$looking_at<=i & temp_res$looking_at>=(i-13)])
      df_err =  data.frame(model=model,trained_till=i,bias_correc = avg_err)
      temp = rbind(temp,df_err)
    }
  }  
  return(temp)
}

# 
# fix_up_missing_days=function(data)
# {
#   for(yr in 2014:max(year))
#   {
#     ## check if the year has a 0 week
#     week_0 = sum(data$week[ data$year==yr]==0)
#     if(week_0 & length(week_0)!=0)
#     {
#       data$count[ data$week==52 & data$year==(yr-1)]=data$count[ data$week==52 & data$year==(yr-1)] +data$count[ data$week==0 & data$year==yr] 
#     }
#     
#   }
#   return(data) 
# }


fix_up_missing_days=function(data)
{
  for(yr in 2012:max(year))
  {
    ## check if the year has a 0 week
    week_53 = sum(data$week[ data$year==yr]==53)
    if(week_53 & length(week_53)!=0)
    {
      week_to_fix=data$week==1 & data$year==(yr+1)
      
      data$count[ data$week==1 & data$year==(yr+1)]=data$count[ data$week==53 & data$year==(yr)] +data$count[ data$week==1 & data$year==(yr+1)]
    }
    
  }
  return(data) 
}

#results=bo_month_res
testing_eval =function(results,should_i_print=TRUE) 
{
  
  
  rolling_agg_mape=aggregate(data = results,ape~model+trained_till,FUN=mean)
  rolling_mape_summ = aggregate(data = rolling_agg_mape,ape~model,FUN=summary)
  
  rolling_agg_acc=aggregate(data = results,accuracy~model+trained_till,FUN=mean)
  rolling_acc_summ = aggregate(data = rolling_agg_acc,accuracy~model,FUN=summary)
  
  ### lookaheads
  step_ahead_summ = aggregate(data = results,cbind(ape,accuracy)~model+step_ahead,FUN=mean)
  step_ahead_summ_cast=cast(data = step_ahead_summ,formula = model~step_ahead,FUN=mean,value="ape")
  
  step_ahead_summ_acc_cast=cast(data = step_ahead_summ,formula = model~step_ahead,FUN=mean,value="accuracy")
  
  
  
  ### print here just in case  
  if(should_i_print)
  {
    print("distribution of MAPE")
    print(rolling_mape_summ)
    
    print("Printing M+n errors mape")
    print(step_ahead_summ_cast)
  
    print("distribution of Accuracy")
    print(rolling_acc_summ)
    
    print("Printing M+n Accuracy mape")
    print(step_ahead_summ_acc_cast)
  
  
  }
  
  y = list(rolling_agg_mape = rolling_agg_mape,step_ahead_summ=step_ahead_summ)
  return(y) 
}

