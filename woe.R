library(woeBinning)
library(dplyr)
library(discretization)


woe_based_binning <- function(data,target,event,predvar){
  library(dplyr)
  d = predvar
  
  y = woeBinning::woe.binning(df=data,target.var = target,event.class = event,min.perc.total = 0.05,
                              stop.limit = 0.05,pred.var = d)
  z = woeBinning::woe.binning.deploy(data,binning=y,min.iv.total=0.1)
  coln = names(z %>% select(contains("binned")))
  return(z[,c(target,coln)])
}

Chi_sq_based_bin <- function(data){
  y = discretization::chiM(data)
  return(y$Disc.data)
}

entropy_based_bin <- function(df){
  z = discretization::mdlp(df)
  return(z$Disc.data)
}