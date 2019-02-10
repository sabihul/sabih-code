# library required
library(data.tree)

##### Manual Data upload and target variable creation #####
# both test and train should be appended into a single dataset so that the format for both datasets is consistent

data<-read.csv("C:/Users/jasksing/Desktop/IN-orbit/adult-training.csv/adult-training.csv")

#### Create Variable List for Discredtization ####
generateVarList(data,"C:/Users/Jaskaran Singh/Desktop/Inorbit/varList1.csv")
### varScale
# O = Ordinal
# N = Nominal
# C = Continous

### This step will generate a csv output containing all variables in the dataset. 
### Please enter Y in selection for the target

# Run the function to load the edited varilist back to R 
varlist_check(path)

# divide data into test and train
#dataValid=dataTrain
dataTrain<-data
dataValid<-data[sample(1:nrow(data),size = 10000,replace = F),]


#### computing the gini index and variable imporatnce for the first split
gini_index(Data = dataTrain,dataTest = dataValid,varList = varList,Node = 1
           ,numeric_round_off=T,round_off_digits=2,range_for_decimal=10)


# running gini for split
# If your split node is 5 then the child nodes would be 2x i.e 10 and 2x+1 i.e 11
gini_split(Data = dataTrain,dataTest = dataValid,var_name = "relationship",Node = 1,
           mini_split=50,mini_node=100,cut_off=0.5,numeric_round_off=T,round_off_digits=2,
           range_for_decimal=10)


# validation reults
y <- as.character(varList[varList$selection%in%"T","varName"])
p<-table(dataValid[,y],dataValid[,"y_pred"])
p

# Computing precision, recall and accuracy 
precision<-p[2,2]/(p[2,1]+p[2,2])
recall<-p[2,2]/(p[1,2]+p[2,2])
accuracy<-(p[1,1]+p[2,2])/(p[1,1]+p[1,2]+p[2,1]+p[2,2])
output <- as.data.frame(cbind(precision,recall,accuracy))
output
