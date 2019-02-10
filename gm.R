
data<-read.csv("C:\\Users\\sabihul.haque\\Desktop\\chubb\\data.csv",header = T,na.strings = c(""," "))
setwd("C:\\Users\\sabihul.haque\\Desktop\\chubb")



gbm_data2<-data
gbm_data<-gbm_data2
rm(gbm_data2)
gc()



gbm_data_all<-gbm_data

colnames(gbm_data)

#gbm_score<-out_time

set.seed(1234)
select.obs <- runif(nrow(gbm_data))
gbm.data.train <- gbm_data[select.obs<0.7,]
gbm.data.test <- gbm_data[select.obs>=0.7,]


gbm.data.train$set_flag <- "Training"
gbm.data.test$set_flag <- "Test"

gbm_data_split <- rbind(gbm.data.train,gbm.data.test)
gbm_data_split$pred_default <- NULL


library(gbm)
var_names<-names(gbm.data.train)[c(1:4)]
var<-"Target"
form <- paste(var_names, collapse = '+')
form_in <- formula(paste(var, '~', form))
create_gbm_100 <- function(train_fr,n_trees,int_dep,min_obs) {
  set.seed(44445555)
  gbm_model_100 <- gbm(form_in
                       
                       , data=gbm_data_split[gbm_data_split$set_flag=='Training',]
                       , train.fraction=train_fr
                       , n.trees = n_trees
                       , n.minobsinnode=min_obs
                       , bag.fraction = 0.5
                       , interaction.depth=int_dep
                       , shrinkage = 0.01
                       , distribution = "bernoulli"
                       , verbose=TRUE
  )
  return (gbm_model_100)
}



# This code is used to carry out model iterations (for different input parameters) and to
# obtain train, test and validation ROC  for different models
# 
# This code creates a preliminary model asssuming initial number of trees to be 2000 and train_fr as 0.8
# along with input parameters for each iteration to predict the optimum number of tree (using gbm.perf).
#
# Using optimum number of trees and the defined parameters, GBM model is created and their ROC values are stored 
# for a particular iteration
# 
# Model is selected based on ROC values

#Declaring lists and arrays required to store roc values ---------------------------------
training_roc=list()
test_roc=list()
val_roc=list()
best_trees=list()

# Creating a list of interationdepth and minosninnode for which GBM iterations are to be run ----
# Optimum hyperparametrs are identified from this input set

dep=c(9) 
obs=c(500)


ncolumns <- ncol(gbm_data_split)

# Loops to run the model for various values of gbm parameters:  ----------------------------
# interaction.depth and n.minobsinnode
gbm_buld<-gbm_data
inner <- 1

for (int_dep in dep) #defines values of interaction.depth
{ 
  for (min_obs in obs) #defines values of n.minobsinnode
  {
    k <- inner
    n <- paste(int_dep,min_obs, sep="__")
    
    print(paste("interatino_depth=", int_dep))
    print(paste("min_obs=", min_obs))
    
    # To fix the best tree use 80% of data (0.8) while running with best tree use complete training data train_fr=1'
    gbm_tg100 = create_gbm_100(0.8, 3000, int_dep, min_obs)
    
    #     # Invoking create_gbm function to decide best number of trees
    bestTrees <- gbm.perf(gbm_tg100, method="test")
    best_trees[k]=bestTrees
    
    gbm_tg1002 = create_gbm_100(1, bestTrees, int_dep, min_obs)
    # summary(gbm_tg, n.trees=bestTrees)
    
    # Running on entire data (training + test data)'
    gbm_data_split$pred_gbm<- predict(gbm_tg1002, n.trees=bestTrees, newdata=gbm_data_split, type='response')
    
    train=subset(gbm_data_split,set_flag=="Training")
    # val=subset(gbm_data_split,set_flag=="Validate")
    test=subset(gbm_data_split,set_flag=="Test")
    
    colnames(gbm_data_split)[ncolumns+k] = paste("pred_gbm", k, sep="_")        
    
    training_roc[k]=gbm.roc.area(gbm_data_split[gbm_data_split$set_flag=='Training' ,var],
                                 gbm_data_split[gbm_data_split$set_flag=='Training',paste("pred_gbm", k, sep="_")])
    
    test_roc[k] = gbm.roc.area(gbm_data_split[gbm_data_split$set_flag=='Test',var],
                               gbm_data_split[gbm_data_split$set_flag=='Test',paste("pred_gbm", k, sep="_")])
    
    
    
    inner <- inner+1  
    
    write.csv(summary(gbm_tg1002), file=paste0("Default_",n,".csv"))
    
    ## Scoring train
    train$pred <- predict(gbm_tg1002, n.trees=bestTrees, newdata=train, type='response')
    train$COUNT <- 1
    train$pred_rank <- ave(train$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(train$pred_rank,probs= seq(0,1,by=0.01))
    train<-train[order(train$pred_rank),]
    train$Percentile<-cut(train$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    
    write.csv(train,paste0("Train_ALLDATA_",n,".csv"))    
    
    scoring_buld <- aggregate(cbind(
      COUNT,
      Target
    ) ~ Percentile, train, sum)
    
    
    write.csv(scoring_buld,paste0("Train_",n,".csv"))    
    
    ## Scoring test
    test$pred <- predict(gbm_tg1002, n.trees=bestTrees, newdata=test, type='response')
    test$COUNT <- 1
    test$pred_rank <- ave(test$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(test$pred_rank,probs= seq(0,1,by=0.01))
    test<-test[order(test$pred_rank),]
    test$Percentile<-cut(test$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    
    write.csv(test,paste0("test_ALLDATA_",n,".csv"))    
    
    scoring_test <- aggregate(cbind(
      COUNT,
      Target
    ) ~ Percentile, test, sum)
    
    
    write.csv(scoring_test,paste0("test_",n,".csv"))    
    
    
    
    
    ## Scoring holdout
    gbm_score$pred <- predict(gbm_tg1002, n.trees=bestTrees, newdata=gbm_score, type='response')
    gbm_score$COUNT <- 1
    #unique(gbm_score$CREATED_MONTH)
    
    
    gbm_score$pred_rank <- ave(gbm_score$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Percentile <- quantile(gbm_score$pred_rank,probs= seq(0,1,by=0.01))
    gbm_score<-gbm_score[order(gbm_score$pred_rank),]
    gbm_score$Percentile<-cut(gbm_score$pred_rank,breaks = Percentile,labels = FALSE,include.lowest = TRUE)
    write.csv(gbm_score,paste0("HOLDOUT_ALLDATA_",n,".csv"))    
    
    scoring_results <- aggregate(cbind(
      COUNT,
      LESS_THAN_60K
    ) ~ Percentile, gbm_score, sum)
    
    #   val_roc[k] = gbm.roc.area(gbm_score[,"LESS_THAN_60K"],
    #                            gbm_score[,paste("pred", k, sep="_")])
    write.csv(scoring_results,paste0("Holdout_",n,".csv"))    
    
    
    
    saveRDS(gbm_tg1002,file=paste0("Model_",n,".RDS"))
    save.image(paste0("Workspace_",n,".RData"))
  }
}
#####3##333333
# # # To Get ROC areas for training and test datasets in a dataframe --------------
train_roc_a = array(training_roc,c(k,1))
test_roc_a = array(test_roc,c(k,1))
val_roc_a = array(val_roc,c(k,1))
# #
params = list()
depth_vals = list()
# #
for (i in 1:length(dep))
{
  params = list(rep(dep[i],length(obs)))
  depth_vals = unlist(c(depth_vals,params))
  # #
}
# #
best_trees = unlist(best_trees)
obs_vals = unlist(list(rep(obs,length(dep))))
roc_df = data.frame(train_roc_a, test_roc_a)
roc_df_bind = cbind(depth_vals, obs_vals, best_trees, roc_df)
roc_df_bind_n <- data.frame(lapply(roc_df_bind, as.character), stringsAsFactors=FALSE)
write.csv(roc_df_bind_n, file=paste0('Summary','.csv'))


