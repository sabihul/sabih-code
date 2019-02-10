####### Creation of Variable List ########


#bk <- dataTrain
generateVarList<-function(bk,path){
  # removing extra space and special characters from col names and replacing it with underscore
  names(bk)<- gsub("[[:punct:]]","_",names(bk))
  names(bk)<-gsub(" ", "_", names(bk), fixed = TRUE)
  data<<-bk
  
  
  print("If any special character or space is present in the column headers then it is replaced with underscore")
  print("Caution:All nominal,ordinal and numeric variables should have N,O and C respectively present in the var_scale corresponding to it.Make sure that your entires on the excel sheet are all in uppercase")
  # preparing a matrix to get the variable names and other colmuns populated
  x<-matrix(0,ncol(bk),2)
  
  for(i in 1:ncol(bk)){
    
    x[i,1]<-colnames(bk[i])
    
    x[i,2]<-class(bk[,i])
    
  }
  
  x<-data.frame(x)
  
  for(i in 1:nrow(x))
    
  {
    
    x$levels[i]<-length(unique(bk[!(is.na(bk[,i])),i]))
    x$no_of_NAs[i]<-sum(is.na(bk[,i]))
    
  }
  
  x<-data.frame(x)
  names(x)<-c("varName","varType","levels","no_of_NAs")
  
  x$varScale="N"
  x$varScale[x$varType=="integer"]<-"O"
  x$varScale[x$varType=="numeric"]<-"C"
  x$selection="Y"
  path <<-path
  write.csv(x,path,row.names=FALSE)
  return(x)
  
}

#path <- "C:/Users/sabihul.haque/Desktop/Data/collection_project/varList1.csv"
varlist_check<- function(path){
  varList<<-read.csv(path)
  
  # checking NAs
  if(sum(varList$no_of_NAs)>0)
    {
    stop("Error: Data has NAs present please impute before building the model")
  }
  # Checking for the target variable assignment
  if(!(nrow(varList[varList$selection=="T",]))==1)
    {
    stop("Error: Please select a target variable")
  }
  # checking the number of classes for the target variable
if(!(varList[varList$selection%in%"T","levels"])==2)
  {
  stop("The target variable should be binary in nature")
}  
  # checking the list of ordinal variables
  ord_var <- as.character(varList[varList$selection%in%"Y" & varList$varScale%in%"O"
                                  ,"varName"])
  if(length(ord_var)>0){
  print(paste0(paste0("Caution: All the levels for "),paste(ord_var,collapse = " , ")," must be encoded and should be of class integer/numeric.If you have done that then please ignore this message"))
  }
  # checking the nominal variables
  nominal_var <- as.character(varList[varList$selection%in%"Y" & varList$varScale%in%"N","varName"])
  for(i in nominal_var){
    if(any(grepl(",", levels(data[,i])))==T){
      levels(data[,i])<- gsub(",","_",levels(data[,i]))
      print(paste0(paste0("Comma present in levels of ",i)," is converted into underscore"))
    }
      
    }
}


#d<-tree_structure
visual=function(d){
  data = d
  left = NULL
  left1 = NULL
  right1 = NULL 
  left2 = NULL
  right2 = NULL
  left3 = NULL
  right3 = NULL
  left4 = NULL
  right4 = NULL
  i = 1
  x = unique(d$Node)
  for (i in 1:nrow(d)){
    if (d[i,1] == 1) {
      data=Node$new(paste(d[i,1],d[i,2]))
      left = data$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right =  data$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
    }
    else if(d[i,1]== 2){ 
      main = left$AddChild(paste(d[i,1],d[i,2]))
      left1 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right1= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      left = left1
    }
    
    else if(d[i,1]%%4 == 0){ 
      main = left$AddChild(paste(d[i,1],d[i,2]))
      left1 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right1= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      left = left1
    }
    else if((d[i,1]-1)%%4 == 0){ 
      main = right1$AddChild(paste(d[i,1],d[i,2]))
      left2 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right2= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      right1 = right2
    }
    
    
    else if (d[i,1]==3) {
      main = right$AddChild(paste(d[i,1],d[i,2]))
      left3 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right3= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      right = right3
    }
    
    else if ((d[i,1]-3)%%4==0) {
      main = right$AddChild(paste(d[i,1],d[i,2]))
      left3 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right3= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      right = right3
    }
    
    else if((d[i,1]-2)%%4 == 0){ 
      main = left3$AddChild(paste(d[i,1],d[i,2]))
      left4 = main$AddChild(paste(d[i,4],'{',d[i,10],',',d[i,7],'}'), obs = d[i,7])
      right4= main$AddChild(paste(d[i,5],'{',d[i,8],',',d[i,6],'}'), obs = d[i,6])
      left3 = left4
    }
  }
  
  plot(data)
}




### Computation of Gini index for categorical & numeric variable
#x<- dataTrain$education.num
#Data
#Data<-dataTrain
#varList<- varList
#Node=1
#dataTest <- dataTrain
gini_index<-function (Data,varList,Node,dataTest,
                      numeric_round_off,round_off_digits,range_for_decimal) 
{
  
  if(nrow(varList[varList$selection=="T",])==1)
  {  # selecting names of all categorical variables
    cat_var_list<-varList[varList$varScale%in%c("N","O") &
                            varList$selection%in%"Y"  ,"varName"]
    # selecting names of all continous variables
    cont_var_list<-varList[varList$varScale%in%c("C") &
                            varList$selection%in%"Y"  ,"varName"]
    
    # rounding-off continous variables to two decimals
    if(length(cont_var_list)>0){
      print("All numeric variables would be rounded to two decimal points for optimizing the computation")
    }
    
    dataTrain <<- Data
    
    # creating a vector of names for all categorical & continous variables     
    cat_var_list<- as.character(cat_var_list)
    cont_var_list<- as.character(cont_var_list)
    
    # selecting the target variable     
    target <- as.character(varList[varList$selection%in%"T"  ,"varName"])
    if(!class(Data[,target])%in%c("numeric","integer")){
      
      stop("Target variable should be converted into numeric or interger class")
    }
    # initiating the node flag creation
    if(Node==1){
      Data[,paste0("Node_",Node)] <- 1
    }
    dataTrain <<- Data  
    
    # predicting the target based on the proportion of 1's in the root node
    # for test data
    if(Node==1 & nrow(dataTest)>0){
      if((sum(Data[,target])/nrow(Data))>=0.5)
        {
        dataTest$y_pred<-1 
      }
      else
      {
        dataTest$y_pred<-0
          }
      
      dataTest[,paste0("Node_",Node)] <- 1
      
      }
    dataValid<<-dataTest
    
    # subseting the data
    
    cat_data <-Data[Data[,paste0("Node_",Node)]%in%"1",c(target,cat_var_list)]
    
    cont_data <-Data[Data[,paste0("Node_",Node)]%in%"1",c(target,cont_var_list)]
    
    # for cat
    Variable_importance <-NULL
    if(ncol(as.data.frame(cat_data))>1){
    for(i in 2:ncol(cat_data)){
      # aggregating the data for the computation of gini values
      agg1<-aggregate(cat_data[,1]~cat_data[,i],FUN = function(x){No_of_one=sum(x)})
      names(agg1)<- c("levels","Sum_of_1")
      agg2<-aggregate(cat_data[,1]~cat_data[,i],FUN = function(x){Total_obs=length(x)})
      names(agg2)<- c("levels","Total_obs")
      agg<-merge(agg1,agg2,by = "levels")
      agg$wt <- agg$Total_obs/sum(agg$Total_obs)
      agg$gini <- 1 - ((agg$Sum_of_1/agg$Total_obs)^2+
                         ((agg$Total_obs-agg$Sum_of_1)/agg$Total_obs)^2) 
      varName <- names(cat_data)[i]
      gini_index <- sum(agg$gini*agg$wt)
      temp <- cbind.data.frame(varName,gini_index)
      Variable_importance <-rbind.data.frame(Variable_importance,temp)
    }
    }
    # for cont
    if(ncol(as.data.frame(cont_data))>1){
    for(i in 2:ncol(cont_data)){
      # aggregating the data for the computation of gini values
    
      if((max(cont_data[,i])-min(cont_data[,i]))<=range_for_decimal
         &
         numeric_round_off%in%c("T","True","TRUE","true"))
        {
        agg1<-aggregate(cont_data[,1]~round(cont_data[,i],digits = round_off_digits),FUN = function(x){No_of_one=sum(x)})
        names(agg1)<- c("levels","Sum_of_1")
        agg2<-aggregate(cont_data[,1]~round(cont_data[,i],digits = round_off_digits),FUN = function(x){Total_obs=length(x)})
        names(agg2)<- c("levels","Total_obs")
      }
      else
        {
        agg1<-aggregate(cont_data[,1]~cont_data[,i],FUN = function(x){No_of_one=sum(x)})
        names(agg1)<- c("levels","Sum_of_1")
        agg2<-aggregate(cont_data[,1]~cont_data[,i],FUN = function(x){Total_obs=length(x)})
        names(agg2)<- c("levels","Total_obs")
      }
      
      agg<-merge(agg1,agg2,by = "levels")
      agg$wt <- agg$Total_obs/sum(agg$Total_obs)
      agg$gini <- 1 - ((agg$Sum_of_1/agg$Total_obs)^2+
                         ((agg$Total_obs-agg$Sum_of_1)/agg$Total_obs)^2) 
      varName <- names(cont_data)[i]
      gini_index <- sum(agg$gini*agg$wt)
      temp <- cbind.data.frame(varName,gini_index)
      Variable_importance <-rbind.data.frame(Variable_importance,temp)
    }
      }
    # adding required variables from varlist to the variable importance table
    Variable_importance <- merge(Variable_importance,varList[,c("varName","varScale")],
                                 by="varName" )
    Variable_importance <-  Variable_importance[order(Variable_importance$gini_index),]
    Variable_importance$var_rank <- rank(Variable_importance$gini_index)
    # Variable_importance$var_selection <- c("Y",rep("N",nrow(Variable_importance)-1))
    
    print(paste("The Gini index for all the variables at Node",Node,sep = " "))
   
    # write.csv(Variable_importance,path,row.names=FALSE)
    
    return(Variable_importance)}
  else  {
    
    print("Error:: Please mark selection of your dependant variable as 'T'. ")  }
}

#var_name <-"capital_loss"
#Data=dataTrain
#var_name<-"relationship"
#Node=1
#dataTest <-dataValid
# mini_split=50
# mini_node=100
# cut_off=0.5

## Single function to compute gini split
gini_split<- function(Data,var_name,Node,dataTest,mini_split=50,mini_node=100
                      ,cut_off=0.5,numeric_round_off=T,round_off_digits=2,range_for_decimal){
  if(mini_node<=mini_split){
    stop("mini_node should be strictly greater than mini_split")
  }
  # subsetting data for the node selected
  Data_sub <- Data[Data[,paste0("Node_",Node)]%in%"1",]
  
  # Getting details of the variable form the varlist for the selected variable
  var_selected <- varList[varList$varName%in%var_name,]
  # sanity checks
  if(var_selected$selection=="N"){
    stop("This variable is not in your selection list")
  }
  # checking which split algo works for the selected variables
  if(var_selected$varScale%in%c("C","O"))
    {
    gini_split_numeric(Data = Data_sub ,cont_var = var_name, mini_split,
                       numeric_round_off, round_off_digits, range_for_decimal)
    }
  else
  {
    gini_split_nominal(Data = Data_sub ,cat_var = var_name, mini_split)
  }
  if(var_selected$varScale%in%c("C","O")){
    # Gathering the information required at each node to build the branches for the nominal variable
    limit<-as.numeric(sub('.*\\=', '', gini_split_df$LHS_name))
    left_child_node <- 2*Node
    right_child_node <- (2*Node)+1
    # train
    Data[,paste0("Node_",left_child_node)] <- ifelse(Data[,paste0("Node_",Node)]%in%"1" & Data[,var_name]<=limit,1,0)
    Data[,paste0("Node_",right_child_node)] <- ifelse(Data[,paste0("Node_",Node)]%in%"1" & Data[,var_name]>limit,1,0)
    
    #test
    dataTest[,paste0("Node_",left_child_node)] <- ifelse(dataTest[,paste0("Node_",Node)]%in%"1" & dataTest[,var_name]<=limit,1,0)
    dataTest[,paste0("Node_",right_child_node)] <- ifelse(dataTest[,paste0("Node_",Node)]%in%"1" & dataTest[,var_name]>limit,1,0)
  }
  else
    {
      # Gathering the information required at each node to build the tree for numeric variable
      LH_limit<-sub('.*\\.', '', gini_split_df$LHS_name)
      RH_limit<-sub('.*\\.', '', gini_split_df$RHS_name)
      LH_limit<-trimws(unlist(strsplit(LH_limit, ",")))
      RH_limit<-trimws(unlist(strsplit(RH_limit, ",")))
      
      left_child_node <- 2*Node
      right_child_node <- (2*Node)+1
    # Train
      Data[,paste0("Node_",left_child_node)] <- ifelse(Data[,paste0("Node_",Node)]%in%"1" & trimws(Data[,var_name])%in%LH_limit,1,0)
      Data[,paste0("Node_",right_child_node)] <- ifelse(Data[,paste0("Node_",Node)]%in%"1" & trimws(Data[,var_name])%in%RH_limit,1,0)
    #  test
      dataTest[,paste0("Node_",left_child_node)] <- ifelse(dataTest[,paste0("Node_",Node)]%in%"1" & trimws(dataTest[,var_name])%in%LH_limit,1,0)
      dataTest[,paste0("Node_",right_child_node)] <- ifelse(dataTest[,paste0("Node_",Node)]%in%"1" & trimws(dataTest[,var_name])%in%RH_limit,1,0)
      }
  
  if(sum(Data[,paste0("Node_",left_child_node)])<mini_node |
     sum(Data[,paste0("Node_",right_child_node)])<mini_node ){
    print(paste(paste(paste(paste0("Warning: Split based on ",var_name),"is resulting in less then"), mini_node), "observations at one of the node"))
  }
  # train
  dataTrain <<-  Data
  
  # generating tree_structure
  target <- as.character(varList[varList$selection%in%"T","varName"])
  if(var_selected$varScale%in%c("C","O")){
    limit<-as.numeric(sub('.*\\=', '', gini_split_df$LHS_name))
    RH_limit<-paste0(">",limit)
    LH_limit<-paste0("<=",limit)
    RH_obs<- sum(Data[,paste0("Node_",right_child_node)])
    LH_obs<-sum(Data[,paste0("Node_",left_child_node)])
    
    Total_1_train <- sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])+
                     sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])
    
    RH_1_percent<- paste0(round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs)*100),"%") 
    RH_0_percent<- paste0(round((1-(sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs))*100),"%")
    LH_1_percent<- paste0(round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs)*100),"%")
    LH_0_percent<- paste0(round((1-(sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs))*100),"%")
  }
  else
    {
    LH_limit<-sub('.*\\.', '', gini_split_df$LHS_name)
    RH_limit<-sub('.*\\.', '', gini_split_df$RHS_name)
    
    RH_obs<- sum(Data[,paste0("Node_",right_child_node)])
    LH_obs<-sum(Data[,paste0("Node_",left_child_node)])
    
    Total_1_train <- sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])+
                     sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])
    
    RH_1_percent<- paste0(round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs)*100),"%") 
    RH_0_percent<- paste0(round((1-(sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs))*100),"%")
    LH_1_percent<- paste0(round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs)*100),"%")
    LH_0_percent<- paste0(round((1-(sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs))*100),"%")
  }
  if(round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs))>=cut_off){
    dataTest$y_pred[dataTest[,paste0("Node_",right_child_node)]=="1"] <-1
  }else{
    dataTest$y_pred[dataTest[,paste0("Node_",right_child_node)]=="1"]<-0
  }
  if(round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs))>=cut_off){
    dataTest$y_pred[dataTest[,paste0("Node_",left_child_node)]=="1"] <-1
  }else{
    dataTest$y_pred[dataTest[,paste0("Node_",left_child_node)]=="1"]<-0
  }
  
  #test
  dataValid <<-  dataTest
  # generating data for tree_structure from the test data
  RH_obs_test<- sum(dataTest[,paste0("Node_",right_child_node)])
  LH_obs_test<-sum(dataTest[,paste0("Node_",left_child_node)])
  
  Total_1_test <- sum(dataTest[dataTest[,paste0("Node_",right_child_node)]=="1",target])+
                  sum(dataTest[dataTest[,paste0("Node_",left_child_node)]=="1",target])
  
  RH_1_percent_test<- paste0(round((sum(dataTest[dataTest[,paste0("Node_",right_child_node)]=="1",target])/RH_obs_test)*100),"%") 
  RH_0_percent_test<- paste0(round((1-(sum(dataTest[dataTest[,paste0("Node_",right_child_node)]=="1",target])/RH_obs_test))*100),"%")
  LH_1_percent_test<- paste0(round((sum(dataTest[dataTest[,paste0("Node_",left_child_node)]=="1",target])/LH_obs_test)*100),"%")
  LH_0_percent_test<- paste0(round((1-(sum(dataTest[dataTest[,paste0("Node_",left_child_node)]=="1",target])/LH_obs_test))*100),"%")
  
  
  if(round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs)*100)=="0" |
     round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs)*100)=="100"){
    print(paste(round((sum(Data[Data[,paste0("Node_",right_child_node)]=="1",target])/RH_obs)*100),right_child_node,sep="% observation have 1 present at node "))
  }
  if(round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs)*100)=="0" |
     round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs)*100)=="100"){
    print(paste(round((sum(Data[Data[,paste0("Node_",left_child_node)]=="1",target])/LH_obs)*100),left_child_node,sep="% observation have 1 present at node "))
  }
  
      
  if(Node==1){
    var_scale <- as.character(var_selected$varScale)
    m<-cbind.data.frame(Node,var_name,var_scale,LH_limit,RH_limit,LH_obs,RH_obs,LH_1_percent,RH_1_percent,LH_0_percent,RH_0_percent,
                        RH_obs_test,LH_obs_test,RH_1_percent_test,RH_0_percent_test,LH_1_percent_test,LH_0_percent_test)
    Total_obs_train <- LH_obs+RH_obs
    Total_obs_test <- RH_obs_test+LH_obs_test
    prop_1_train <- paste0(round(Total_1_train/Total_obs_train)*100,"%")
    prop_1_test <- paste0(round(Total_1_test/Total_obs_test)*100,"%")

    n<-cbind.data.frame(Node,var_name,var_scale,LH_limit,RH_limit,Total_obs_train,prop_1_train,
                        Total_obs_test,prop_1_test)
    valid_tree <- n 
    tree_structure <<-m
  }else{
    var_scale <- as.character(var_selected$varScale)
    m<-cbind.data.frame(Node,var_name,var_scale,LH_limit,RH_limit,LH_obs,
                        RH_obs,LH_1_percent,RH_1_percent,LH_0_percent,RH_0_percent,
                        RH_obs_test,LH_obs_test,RH_1_percent_test,RH_0_percent_test,
                        LH_1_percent_test,LH_0_percent_test)
    
    Total_obs_train <- LH_obs+RH_obs
    Total_obs_test <- RH_obs_test+LH_obs_test
    prop_1_train <- paste0(round((Total_1_train/Total_obs_train)*100),"%")
    prop_1_test <- paste0(round((Total_1_test/Total_obs_test)*100),"%")
    
    n<-cbind.data.frame(Node,var_name,var_scale,LH_limit,RH_limit,Total_obs_train,prop_1_train,
                        Total_obs_test,prop_1_test)
    
    # removing the the node no. attached from the previous iteration for the same node
    if(any(tree_structure$Node%in%m$Node)){
      tree_structure<<-tree_structure[!(tree_structure$Node)%in%m$Node,]
      valid_tree<<-valid_tree[!(valid_tree$Node)%in%m$Node,]
    }
    tree_structure <<-rbind(tree_structure[,names(m)],m)
    tree_structure <<- tree_structure[order(tree_structure$Node),]
    valid_tree <<-rbind(valid_tree[,names(n)],n)
    valid_tree <<- valid_tree[order(valid_tree$Node),]
  }
  
  
  # Adding terminal split flag
  valid_tree$Terminal_split_node <-"None of the split is a terminal node"
  for(i in unique(valid_tree$Node)){
    if(!((i*2)%in%unique(valid_tree$Node))){
      valid_tree[valid_tree$Node%in%i,"Terminal_split_node"]<-"RHS is the Terminal Node"}
    if(!(((i*2)+1)%in%unique(valid_tree$Node))){
      valid_tree[valid_tree$Node%in%i,"Terminal_split_node"]<-"LHS is the Terminal Node"}
    if(!((i*2)%in%unique(valid_tree$Node)) & !(((i*2)+1)%in%unique(valid_tree$Node))){
      valid_tree[valid_tree$Node%in%i,"Terminal_split_node"]<-"Both RHS & LHS are the Terminal Nodes"}
  }

  tree_structure <<-tree_structure
  valid_tree <<- valid_tree
  
  visual(tree_structure) 
}


# cat_var <- var_name
#Data <- Data_sub
# for categorical variables
gini_split_nominal  <- function(Data,cat_var,mini_split) {
  
    # creating a vector of names of all the categorical variables     
    cat_var<- as.character(cat_var)
    
    # selcting the target variable     
    target <- as.character(varList[varList$selection%in%"T"  ,"varName"])
    if(!class(Data[,target])%in%c("numeric","integer")){
      
      stop("Target variable should be converted into numeric or interger class")
    }
    # subseting the data
    cat_data <- Data[,c(target,cat_var)] 
    agg1<-aggregate(cat_data[,1]~cat_data[,2],FUN = function(x){No_of_one=sum(x)})
    names(agg1)<- c("levels","Sum_of_1")
    agg2<-aggregate(cat_data[,1]~cat_data[,2],FUN = function(x){Total_obs=length(x)})
    names(agg2)<- c("levels","Total_obs")
    agg<-merge(agg1,agg2,by = "levels")
    agg$wt <- agg$Total_obs/sum(agg$Total_obs)
    agg$gini <- 1 - ((agg$Sum_of_1/agg$Total_obs)^2+
                       ((agg$Total_obs-agg$Sum_of_1)/agg$Total_obs)^2) 
    agg <- agg[order(agg$gini),]
    
    gini_split_df <- NULL
    for(i in 1:(nrow(agg)-1)){
      LHS <- agg[1:i,]
      RHS <- na.omit(agg[(i+1):nrow(agg),])
      condition_LHS <- paste(LHS$levels,collapse = " , ")
      LHS_name <- paste(cat_var,condition_LHS,sep = "_split.")
      condition_RHS <- paste(RHS$levels,collapse = " , ")
      RHS_name <- paste(cat_var,condition_RHS
                        ,sep = "_split.")
      RHS_obs<-sum(RHS$Total_obs)
      LHS_obs<-sum(LHS$Total_obs)
      gini_t <- 1 -(sum(Data[,target])/length(Data[,target]))^2-
        ((length(Data[,target])-sum(Data[,target]))/length(Data[,target]))^2
      
      
      gini_l <- 1-((sum(LHS$Sum_of_1)/sum(LHS$Total_obs))^2+
                     ((sum(LHS$Total_obs)-sum(LHS$Sum_of_1))/sum(LHS$Total_obs))^2) 
      
      gini_r <- 1-((sum(RHS$Sum_of_1)/sum(RHS$Total_obs))^2+
                     ((sum(RHS$Total_obs)-sum(RHS$Sum_of_1))/sum(RHS$Total_obs))^2)
      
      gini_split <- gini_t -(sum(LHS$Total_obs)/(sum(LHS$Total_obs)+sum(RHS$Total_obs)))*gini_l-
        (sum(RHS$Total_obs)/(sum(LHS$Total_obs)+sum(RHS$Total_obs)))*gini_r
      temp <- cbind.data.frame(LHS_name,RHS_name,gini_l,gini_r,gini_split,RHS_obs,LHS_obs)
      gini_split_df <- rbind(gini_split_df,temp)  
    }
    if(gini_split_df[which.max(gini_split_df$gini_split),"RHS_obs"]>mini_split &
       gini_split_df[which.max(gini_split_df$gini_split),"LHS_obs"]>mini_split)
    {
      gini_split_df<<-gini_split_df[which.max(gini_split_df$gini_split),]
    }else{
      gini_split_df<-gini_split_df[gini_split_df$RHS_obs>mini_split &
                                     gini_split_df$LHS_obs>mini_split,]
      if(nrow(gini_split_df)==0){
        stop("None of the splits satisfy the condition of mini_split")
      }
      gini_split_df<<-gini_split_df[which.max(gini_split_df$gini_split),]
    }
}



# for numeric variables
#cont_var<-"capital_loss"
gini_split_numeric  <- function(Data,cont_var, mini_split, numeric_round_off,
                                round_off_digits, range_for_decimal) {
  
    # creating a vector of names of the numeric variables     
    cont_var<- as.character(cont_var)
    
    # selecting the target variable     
    target <- as.character(varList[varList$selection%in%"T"  ,"varName"])
    if(!class(Data[,target])%in%c("numeric","integer")){
      
      stop("Target variable should be converted into numeric or interger class")
    }
    # subseting the data
    cont_data <- Data[,c(target,cont_var)] 

    if((max(cont_data[,cont_var])-min(cont_data[,cont_var]))<=range_for_decimal
       &
       numeric_round_off%in%c("T","True","TRUE","true"))
      {
    agg1<-aggregate(cont_data[,1]~round(cont_data[,2],digits = round_off_digits),FUN = function(x){No_of_one=sum(x)})
    names(agg1)<- c("levels","Sum_of_1")
    agg2<-aggregate(cont_data[,1]~round(cont_data[,2],digits = round_off_digits),FUN = function(x){Total_obs=length(x)})
    names(agg2)<- c("levels","Total_obs")
    }
    else
      {
      agg1<-aggregate(cont_data[,1]~cont_data[,2],FUN = function(x){No_of_one=sum(x)})
      names(agg1)<- c("levels","Sum_of_1")
      agg2<-aggregate(cont_data[,1]~cont_data[,2],FUN = function(x){Total_obs=length(x)})
      names(agg2)<- c("levels","Total_obs")
    }
    
    agg<-merge(agg1,agg2,by = "levels")
    agg <- agg[order(agg$levels),]

      gini_split_df <- NULL
    for(i in 1:(nrow(agg)-1)){
      LHS <- agg[1:i,]
      RHS <- na.omit(agg[(i+1):nrow(agg),])
      condition_LHS <- paste0("<=",max(LHS$levels))
      LHS_name <- paste(cont_var,condition_LHS,sep = "_split.")
      condition_RHS <- paste0(">",max(LHS$levels))
      RHS_name <- paste(cont_var,condition_RHS
                        ,sep = "_split.")
      RHS_obs<-sum(RHS$Total_obs)
      LHS_obs<-sum(LHS$Total_obs)
      gini_t <- 1 -(sum(Data[,target])/length(Data[,target]))^2-
        ((length(Data[,target])-sum(Data[,target]))/length(Data[,target]))^2
      
      
      gini_l <- 1-((sum(LHS$Sum_of_1)/sum(LHS$Total_obs))^2+
                     ((sum(LHS$Total_obs)-sum(LHS$Sum_of_1))/sum(LHS$Total_obs))^2) 
      
      gini_r <- 1-((sum(RHS$Sum_of_1)/sum(RHS$Total_obs))^2+
                     ((sum(RHS$Total_obs)-sum(RHS$Sum_of_1))/sum(RHS$Total_obs))^2)
      
      gini_split <- gini_t -(sum(LHS$Total_obs)/(sum(LHS$Total_obs)+sum(RHS$Total_obs)))*gini_l-
        (sum(RHS$Total_obs)/(sum(LHS$Total_obs)+sum(RHS$Total_obs)))*gini_r
      temp <- cbind.data.frame(LHS_name,RHS_name,gini_l,gini_r,gini_split,RHS_obs,LHS_obs)
      gini_split_df <- rbind(gini_split_df,temp)
    }
    if(gini_split_df[which.max(gini_split_df$gini_split),"RHS_obs"]>mini_split &
       gini_split_df[which.max(gini_split_df$gini_split),"LHS_obs"]>mini_split)
    {
      gini_split_df<<-gini_split_df[which.max(gini_split_df$gini_split),]
    }else{
      gini_split_df<-gini_split_df[gini_split_df$RHS_obs>mini_split &
                                     gini_split_df$LHS_obs>mini_split,]
      if(nrow(gini_split_df)==0){
        stop("None of the splits satisfy the condition of mini_split")
        }
      gini_split_df<<-gini_split_df[which.max(gini_split_df$gini_split),]
    }
}

