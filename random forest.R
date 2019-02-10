
y<-"Loan_Status"

x=colnames(train[2:(ncol(train)-1)])

random_forest<-function(data,x,y){
  
  library(h2o)
  ## Create an H2O cloud 
  h2o.init(
    nthreads=-1,            ## -1: use all available threads
    max_mem_size = "2G") 
data<-as.h2o(data)  


splits <- h2o.splitFrame(
  data,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  validation_frame = valid,     ##
  x=x,                       ##
  y=y,                         ##
  model_id = "rf_covType2",     ## 
  ntrees = 200,                 ##
  max_depth = 30,               ## 
  stopping_rounds = 2,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,     ##
  seed=3000000) 

summary(rf2)

finalRf_predictions<-h2o.predict(
  object = rf2
  ,newdata = test)

finalRf_predictions

}

random_forest(train,x,y)
