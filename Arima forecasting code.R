nsarima <- function(x,fr=12,pr.ahead=4){
  
  z <- ts(x,frequency = fr)
  
  nsdiff = forecast::ndiffs(z)
  
  Sdiffs <- forecast::nsdiffs(z)
  
  ARS <- NULL
  
  MAS <- NULL
  
  Ar <- 0:3
  
  Ma <- 0:3
  
  inc <- 0:1
  
  len <- length(z)
  
  train <- 1:(len-pr.ahead)
  
  valid <- (length(train)+1):(length(z))
  
  mpe <- function(x,y){
    
    d <- try(mean(abs(x-y)/x))
    
    return(d)
    
  }
  
  Mape <- NULL
  
  Penalisedmape <- NULL
  
  Diifs <- NULL
  
  SAR <- NULL
  
  SMA <- NULL
  
  SDIFF <- NULL
  
  cnst <- NULL
  
  h =0
  
  for(i in Ar){
    
    for(j in Ma){
      
      for(k in inc){
        
        for(s in 0:2){
          
          for(sm in 0:2){
            
            tryCatch(arm <- forecast::Arima(ts(z[train],frequency=fr),order=c(i,nsdiff,j),seasonal = c(s,Sdiffs,sm), include.constant = k,method ='CSS',lambda = 0),warning=function(w)
            {arm <- "Model ignored becuase of unstability issue"},error=function(e){
              arm <- "model not run because of some error"
            })
            
            if(class(arm)!= "character"){
              
              fo <- forecast::forecast(arm,h=pr.ahead)$mean
              
              valmape <- mpe(z[valid],fo)
              
              fit <- arm$fitted
              
              trainmape <- mpe(z[train],fit)
              
              difmape <- abs(trainmape-valmape)
              
              ARmetric <- valmape/length(train) + difmape
              #ARmetric <- trainmape/length(train)+difmape+log1p(i+j+k+s+sm)/(length(train)-1)
              #ARmetric <- (valmape*length(train) + trainmape*length(valid))/(length(train)+length(valid))
              ARS <- rbind(ARS,i)
              
              MAS <- rbind(MAS,j)
              
              SAR <- rbind(SAR,s)
              
              SMA <- rbind(SMA,sm)
              
              cnst <- rbind(cnst,k)
              
              SDIFF <- rbind(SDIFF,Sdiffs)
              
              Mape <- rbind(Mape,valmape)
              
              Penalisedmape <- rbind(Penalisedmape,ARmetric)
              
              Diifs <- c(Diifs,nsdiff)
              
              h <- h+1
              
              print(h)
            }else{ return(arm)}
            #aut <- forecast::auto.arima(ts(z[train],frequency=fr))
            
            
          }
          
        }
        
      }
      
      
      
    }
    
    
    
  }
  
  
  
  ArimaMetrics <- as.data.frame(cbind(ARS,Diifs,MAS,SAR,SDIFF,SMA,cnst,Mape,Penalisedmape))
  
  colnames(ArimaMetrics) <- c('AR','Diffs','MA','SAR','SDIFF','SMA','const','Mape','Penalisedmape')
  
  rownames(ArimaMetrics) <- NULL
  
  
  
  ind <- match(min(ArimaMetrics$Penalisedmape),ArimaMetrics$Penalisedmape)
  
  Selected_Metrics <- ArimaMetrics[ind,]
  
  
  
  tryCatch(Fin_Model <- forecast::Arima(z,order=c(as.numeric(Selected_Metrics$AR),as.numeric(Selected_Metrics$Diffs),as.numeric(Selected_Metrics$MA)),
                               
                               seasonal = c(as.numeric(Selected_Metrics$SAR),as.numeric(Selected_Metrics$SDIFF),as.numeric(Selected_Metrics$SMA)),
                               
                               include.constant =as.numeric(Selected_Metrics$const),method= 'CSS',lambda = 0),warning=function(w){
                                 Fin_Model <- "Model ignored because of invertbitlty issue"
                                 
                               }, error = function(e){ Fin_Model <- "Model ignored becasue of some error"})
  if(class(Fin_Model) != "character"){
    Fin_Forecas <- try(forecast::forecast(Fin_Model,h=pr.ahead))
    
    Final_Fore <- Fin_Forecas$mean
    
    Final_Fit <- Fin_Model$fitted
    
    
    
    return(list(parameters =Selected_Metrics, Fore= Final_Fore,Fit=Final_Fit))
  }else{return(Fin_Model)}
  
  
  
  #return(Selected_Metrics)
  
}

new_wr<-tsclean(sme_wr$count[1:36])
nsarima(new_wr)

d <- auto.arima(new_wr)
f <- forecast(d,h=4)$mean
