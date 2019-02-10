library(gbm)
library(gtools)

#setwd("//10.156.16.9/Bridgei2idata/Wealth/MF/Satyakii/GB_ITER1")
#gbm_data<-read.csv("MF_MODEL_DATA.csv")
#val1<-read.csv("MF_MODEL_DATA_VAL.csv")

#index <- sample(1:nrow(gbm_data), size = 0.7 * nrow(gbm_data), replace = FALSE)
#gbm_buld <- train_new
  #gbm_data[index,]
#gbm_score <- gbm_data[-index,] 
#write.csv(gbm_buld,paste0("gbm_buld",".csv")) 
#write.csv(gbm_score,paste0("gbm_score",".csv")) gbm_buld$set_flag <- "Training"
#gbm_score$set_flag <- "Test"
#gbm_buld <- read.csv("//10.156.16.9/Bridgei2idata/PLCS_Propensity/ThinModel/Data/gbm_buld.csv")
#gbm_score <- read.csv("//10.156.16.9/Bridgei2idata/PLCS_Propensity/ThinModel/Data/gbm_score.csv")
#names()

#names(gbm_buld) <- toupper(names(gbm_buld))
#names(gbm_score) <- toupper(names(gbm_score))
#names(val1) <- toupper(names(val1))
#names(val2) <- toupper(names(val2))
#names(val3) <- toupper(names(val2))

gbm_data_split <- train_new

# Creating set_flag which indicates to which split it belongs to (Training, Test, Validation)
var_names<-names(gbm_data_split)[c(2:9,11:ncol(gbm_data_split))]
names(gbm_data_split)[10]<-"Y_VAR"
var<-"Y_VAR"
form <- paste(var_names, collapse = '+')
form_in <- formula(paste(var, '~', form))


create_gbm_100 <- function(train_fr,n_trees,int_dep,min_obs) {
  set.seed(44445555)
  gbm_model_100 <- gbm(  form_in
                       , data=gbm_data_split
                       , train.fraction=train_fr
                       , n.trees = n_trees
                       , n.minobsinnode=min_obs
                       # , bag.fraction = 1
                       , interaction.depth=int_dep
                       , shrinkage = 0.1
                       , distribution = "bernoulli"
                       , verbose=TRUE
  )
  return (gbm_model_100)
}


####################################################################################################################################


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
validation_roc=list()
test_roc=list()
best_trees=list()

# Creating a list of interationdepth and minosninnode for which GBM iterations are to be run ----
# Optimum hyperparametrs are identified from this input set

dep=c(5) 
obs=c(200)
ncolumns <- ncol(gbm_data_split)

# Loops to run the model for various values of gbm parameters:  ----------------------------
# interaction.depth and n.minobsinnode

inner <- 1
int_dep <- dep
min_obs <-obs

for (int_dep in dep) #defines values of interaction.depth
{ 
  for (min_obs in obs) #defines values of n.minobsinnode
  {
    k <- inner
    n <- paste(int_dep,min_obs, sep="__")
    
    # To fix the best tree use 80% of data (0.8) while running with best tree use complete training data train_fr=1'
    bestTrees<-100
    gbm_tg100_x <- create_gbm_100(1, bestTrees, int_dep, min_obs)
    gbm_data_split$pred_gbm<- predict(gbm_tg100_x, n.trees=bestTrees, newdata=gbm_data_split, type='response')
    
    inner <- inner+1  
    
    write.csv(summary(gbm_tg100_x), file=paste0("Var_Imp_with_owned",n,".csv"))
    
    ## Scoring build
    gbm_data_split$pred <- predict(gbm_tg100_x, n.trees=bestTrees, newdata=gbm_data_split, type='response')
    
    gbm_data_split$COUNT <- 1
    gbm_data_split$pred_rank <- ave(gbm_data_split$pred, FUN = function(x) rank(-x, ties.method = "first"))
    Decile <- quantile(gbm_data_split$pred_rank,probs= seq(0,1,by=0.1))
    gbm_data_split<-gbm_data_split[order(gbm_data_split$pred_rank),]
    gbm_data_split$Decile<-cut(gbm_data_split$pred_rank,breaks = Decile,labels = FALSE,include.lowest = TRUE)

    scoring_buld <- aggregate(cbind(
      COUNT,
      Y_VAR
    ) ~ Decile, gbm_data_split, sum)

    scoring_buld$Non_Event <-scoring_buld$COUNT - scoring_buld$Y_VAR
    scoring_buld$PERC_CUM_EVENT <- cumsum(scoring_buld$Y_VAR) / sum(scoring_buld$Y_VAR)
    scoring_buld$PERC_CUM_NON_EVENT <- cumsum(scoring_buld$Non_Event) / sum(scoring_buld$Non_Event)
    scoring_buld$KS <- scoring_buld$PERC_CUM_EVENT - scoring_buld$PERC_CUM_NON_EVENT
    View(scoring_buld)
    
    write.csv(scoring_buld,paste0("Dev_FD_with_owned_Dev",".csv")) 
    saveRDS(gbm_tg100_x,"model_100_trees_owned.RDS")
  }
}

#scoring out of time Dec16

#gbm_tg100_x<-readRDS(file = "model_100_trees.rds")
gbm_oot1<- test_new
names(gbm_oot1)[10]<-"Y_VAR"
gbm_oot1$pred <- predict(gbm_tg100_x, n.trees=100, newdata=gbm_oot1, type='response')

gbm_oot1$COUNT <- 1
gbm_oot1$pred_rank <- ave(gbm_oot1$pred, FUN = function(x) rank(-x, ties.method = "first"))
Decile <- quantile(gbm_oot1$pred_rank,probs= seq(0,1,by=0.1))
gbm_oot1<-gbm_oot1[order(gbm_oot1$pred_rank),]
gbm_oot1$Decile<-cut(gbm_oot1$pred_rank,breaks = Decile,labels = FALSE,include.lowest = TRUE)
scoring_results <- aggregate(cbind(COUNT, Y_VAR) ~ Decile, gbm_oot1, sum)

scoring_results_oot1 <- aggregate(cbind(
  COUNT,
  Y_VAR
) ~ Decile ,
      #+ AFFLUENCE + city_cluster + ocupation_category +  Tier,
      gbm_oot1, sum)


scoring_results_oot1$Non_Event <-scoring_results_oot1$COUNT - scoring_results_oot1$Y_VAR
scoring_results_oot1$PERC_CUM_EVENT <- cumsum(scoring_results_oot1$Y_VAR) / sum(scoring_results_oot1$Y_VAR)
scoring_results_oot1$PERC_CUM_NON_EVENT <- cumsum(scoring_results_oot1$Non_Event) / sum(scoring_results_oot1$Non_Event)
scoring_results_oot1$KS <- scoring_results_oot1$PERC_CUM_EVENT - scoring_results_oot1$PERC_CUM_NON_EVENT
View(scoring_results_oot1)

write.csv(scoring_results_oot1,paste0("OOT_new_27_4_18_owned",".csv")) 

#scoring out of time May17

