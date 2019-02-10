library(dplyr)

train <- read.csv("train.csv", header=T)
y<-train$Loan_Status

x=colnames(train[2:(ncol(train)-1)])



GBM_train1=function(data,target,tunning,rootpath)
{
  library(h2o)
  library(caret)
  library(ggplot2)
  
  h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
           max_mem_size = "20G")
  data=as.h2o(data)
  splits <- h2o.splitFrame(data, ratios=0.70, seed=1234)
  train1  <- h2o.assign(splits[[1]], "train1.hex") # 70%
  valid  <- h2o.assign(splits[[2]], "valid.hex") # 30%
  
  
  if(tunning=="True")
  {
    gbm_params1 <- list(learn_rate = c(0.01,0.02, 0.1),
                        max_depth = c(12,16,20),
                        sample_rate = c(0.6,0.8, 1.0),
                        col_sample_rate = c(0.2, 0.5, 1.0))
    
    
    
    
    
    
    gbm_grid1 <- h2o.grid("gbm", x = x, y = target,
                          training_frame = train1,
                          validation_frame = valid,
                          ntrees = 200,
                          seed = 1,
                          hyper_params = gbm_params1)
    
    
    
    
    
    
    
    if(is.factor(train1[,target]))
    {
      
      gbm_sorted_grid <- h2o.getGrid(grid_id = gbm_grid1@grid_id, 
                                     sort_by = "accuracy", 
                                     decreasing = TRUE)
      print(gbm_sorted_grid)
      
      best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
      
      
      pred <- h2o.predict( best_model,valid)
      
      predictions=as.data.frame(pred$predict)
      
      valid1=as.data.frame(valid)
      #Output which confusion matrix generates has to be taken and displayed
      flag=0
      tryCatch({
        y=confusionMatrix(table(valid1[,target],predictions$predict))
        l=list("model"=best_model,"table"=y,"flag"=flag)
      }, error = function(e) {
        y=table(valid1[,target],predictions$predict)
        Accuracy=sum(diag(y))/sum(y)
        flag=1
        l=list("model"=best_model,"table"=y,"flag"=flag,"Accuracy"=Accuracy)
      }, finally = {
      })
      if(flag==1)
      {
        y=table(valid1[,target],predictions$predict)
        Accuracy=sum(diag(y))/sum(y)
        l=list("model"=best_model,"table"=y,"flag"=flag,"Accuracy"=Accuracy)
      }else
      {
        y=confusionMatrix(table(valid1[,target],predictions$predict))
        l=list("model"=best_model,"table"=y,"flag"=flag) 
      }
      
      
      
      visualizeConfusionMatrix(valid1,predictions,target,rootpath)
    }else if(is.numeric(train1[,target]))
    { 
      gbm_sorted_grid <- h2o.getGrid(grid_id = gbm_grid1@grid_id, 
                                     sort_by = "mse", 
                                     decreasing = FALSE)
      print(gbm_sorted_grid)
      
      best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
      
      
      pred <- h2o.predict( best_model,valid)
      
      predictions=as.data.frame(pred$predict)
      
      valid1=as.data.frame(valid)
      rmse=sqrt( mean( (valid1[,target]-predictions$predict)^2 , na.rm = TRUE ) )
      l=list("model"=best_model,"rmse"=rmse,"valid"=valid1,"pred"=predictions)
      
      
    }
    
    
  }else {
    
    
    gbm1 <- h2o.gbm(
      training_frame = train1,        ## the H2O frame for train1ing
      validation_frame = valid,      ## the H2O frame for validation (not required)
      x=x,                        ## the predictor columns, by column index
      y=target,                          ## the target index (what we are predicting)
      ntrees = 20,                ## add a few trees (from 20, though default is 50)
      learn_rate = 0.3,           ## increase the learning rate even further
      max_depth =9,             ## 
      sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
      col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
      stopping_rounds = 1,        ## 
      stopping_tolerance = 0.01,  ##
      score_each_iteration = T,   ##
      model_id = "gbm_covType3",  ##
      seed = 2000000)           ##)                ## Set the random seed for reproducability
    
    pred <- h2o.predict(gbm1,valid)
    
    predictions=as.data.frame(pred$predict)
    
    valid1=as.data.frame(valid)
    
    
    
    
    
    
    if(is.factor(train1[,target]))
    {
      #Output which confusion matrix generates has to be taken and displayed
      flag=0
      tryCatch({
        y=confusionMatrix(table(valid1[,target],predictions$predict))
        library(pROC)
        my_roc <- roc(valid1[,target], as.data.frame(pred)$p1)
        coords(my_roc, "best", ret = "threshold")
        print(coords(my_roc, "best", ret = "threshold"))
        
      }, error = function(e) {
        y=table(valid1[,target],predictions$predict)
        Accuracy=sum(diag(y))/sum(y)
        flag=1
        
      }, finally = {
      })
      if(flag==1)
      {
        y=table(valid1[,target],predictions$predict)
        Accuracy=sum(diag(y))/sum(y)
        l=list("model"=gbm1,"table"=y,"flag"=flag,"Accuracy"=Accuracy)
      }else
      {
        y=confusionMatrix(table(valid1[,target],predictions$predict))
        l=list("model"=gbm1,"table"=y,"flag"=flag) 
      }
      visualizeConfusionMatrix(valid1,predictions,target,rootpath)
    }else if(is.numeric(train1[,target]))
    {
      rmse=sqrt( mean( (valid1[,target]-predictions$predict)^2 , na.rm = TRUE ) )
      l=list("model"=gbm1,"rmse"=rmse,"valid"=valid1,"pred"=predictions)
      
      
    }
    
    
  }
  return(l)
  
}

visualizeConfusionMatrix<-function(valid1,predictions,target,rootpath)
{
  
  #compute frequency of actual categories
  actual = as.data.frame(table(valid1[,target]))
  names(actual) = c("Actual","ActualFreq")
  
  #build confusion matrix
  confusion = as.data.frame(table(valid1[,target], predictions$predict))
  names(confusion) = c("Actual","Predicted","Freq")
  
  #calculate percentage of test cases based on actual frequency
  confusion = merge(confusion, actual, by="Actual")
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  
  #render plot
  # we use three different layers
  # first we draw tiles and fill color based on percentage of test cases
  q=ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
    labs(x="Actual",y="Predicted") + 
    geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
    scale_fill_gradient(low="white",high="sky blue") + 
    geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 
  
  q+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  ggsave(rootpath, device = "png",scale=2)
  
  
}


ret=GBM_train1(train,"Loan_Status","True","Image.png")




GBM_Predict=function(test,imp,rootpath)
{
  
  test=as.h2o(test)
  pred <- h2o.predict( ret$model,test)
  
  test=as.data.frame(test)
  
  predictions=as.data.frame(pred)
  
  pred1= predictions$predict
  
  
  
  write.csv(submit,"new_logic_20.csv",row.names=F)
  return(pred)
}




print(ret)

pred=GBM_Predict(test,ret,"new_logic_20.csv")
pred<-as.data.frame(pred)
write.csv(pred,"new_logic_20.csv",row.names=F)  

}

main()
