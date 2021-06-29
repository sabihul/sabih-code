# %%
### Importing the Modules:
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 
from scipy.stats import linregress

import time
from multiprocessing import Pool

import warnings
warnings.filterwarnings(action="ignore")


# ML Libraries:
from sklearn import ensemble
from sklearn import metrics
from sklearn.model_selection import train_test_split, StratifiedKFold
from imblearn.over_sampling import RandomOverSampler, SMOTE
from imblearn.under_sampling import RandomUnderSampler
from bayes_opt import BayesianOptimization
from sklearn.feature_selection import RFECV
from sklearn.model_selection import cross_val_score
import joblib

import xgboost as xgb 
import pickle

Input_Path = r"F:\OFZ\OneDrive - Anheuser-Busch InBev\_MUKIL_\00_WORK\00_PROJECTS\14_CHURN_PREDICTION_ONTRADE_UK\03.Output\Model_Input\\"
Output_Path = r"F:\OFZ\OneDrive - Anheuser-Busch InBev\_MUKIL_\00_WORK\00_PROJECTS\14_CHURN_PREDICTION_ONTRADE_UK\03.Output\Model_Results\Model Results_LatestPOCImage\\"
pd.set_option("max_rows", 1000)
pd.set_option("max_columns", 1000)

# %%
### Data Transformation Functions:
def POC_Vol_Grpby_Df(Vol_ONTrade, grouping_list:list):
    # Grouping the Brand Volume:
    Vol_ONTrade_grpby = Vol_ONTrade.groupby(grouping_list, as_index=False).agg({'2018 1':sum, '2018 2':sum, '2018 3':sum, '2018 4':sum, '2018 5':sum, '2018 6':sum, '2018 7':sum, 
                                                            '2018 8':sum,'2018 9':sum, '2018 10':sum, '2018 11':sum, '2018 12':sum, '2019 1':sum, '2019 2':sum, 
                                                            '2019 3':sum,'2019 4':sum, '2019 5':sum, '2019 6':sum, '2019 7':sum, '2019 8':sum, '2019 9':sum,
                                                            '2019 10':sum, '2019 11':sum, '2019 12':sum, '2020 1':sum, '2020 2':sum, '2020 3':sum, '2020 4':sum, '2020 5':sum, '2020 6':sum, 
                                                            '2020 7':sum, '2020 8':sum})
    
    # DataFrame for Volume Bucket Calculation:
    Vol_ONTrade_Vol_Buck_df = Vol_ONTrade_grpby.groupby(["Outlet Id"], as_index=False).agg({'2018 1':sum, '2018 2':sum, '2018 3':sum, '2018 4':sum, '2018 5':sum, '2018 6':sum, '2018 7':sum, 
                                                            '2018 8':sum,'2018 9':sum, '2018 10':sum, '2018 11':sum, '2018 12':sum, '2019 1':sum, '2019 2':sum, 
                                                            '2019 3':sum,'2019 4':sum, '2019 5':sum, '2019 6':sum, '2019 7':sum, '2019 8':sum, '2019 9':sum,
                                                            '2019 10':sum, '2019 11':sum, '2019 12':sum, '2020 1':sum, '2020 2':sum, '2020 3':sum, '2020 4':sum, '2020 5':sum, '2020 6':sum, 
                                                            '2020 7':sum, '2020 8':sum})


    # Getting the volume and Number of active months:
    Vol_ONTrade_Vol_Buck_df["Total_Vol"] = Vol_ONTrade_Vol_Buck_df.loc[:,Vol_ONTrade_Vol_Buck_df.columns[Vol_ONTrade_Vol_Buck_df.columns.str.contains("2018|2019")].tolist()].sum(axis=1)
    Vol_ONTrade_Vol_Buck_df["Active_Months"] = Vol_ONTrade_Vol_Buck_df.loc[:,Vol_ONTrade_Vol_Buck_df.columns[Vol_ONTrade_Vol_Buck_df.columns.str.contains("2018|2019")].tolist()].astype(bool).sum(axis=1)
    Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"] = Vol_ONTrade_Vol_Buck_df["Total_Vol"]/Vol_ONTrade_Vol_Buck_df["Active_Months"]

    # Capping it with 99% quantile:
    print("Upper Cap for Volume: ", Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"].quantile(0.99))
    Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"] = np.where(Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"]>Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"].quantile(0.99), 
                                                          Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"].quantile(0.99)   , 
                                                          Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"])

    Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol_Buck"] = pd.qcut(Vol_ONTrade_Vol_Buck_df["Avg_Monthly_Vol"],5)
    
    # Getting the Volume Bucket information on the main dataframe:
    Vol_ONTrade_grpby = pd.merge(Vol_ONTrade_grpby,Vol_ONTrade_Vol_Buck_df[["Outlet Id", "Active_Months","Total_Vol","Avg_Monthly_Vol","Avg_Monthly_Vol_Buck"]].drop_duplicates(), 
                                                        how="left", on="Outlet Id" )

    print("POC Volume Groupby Completed !!")
    return Vol_ONTrade_grpby

### Unpivot the Data:
def unpivot_df(Vol_ONTrade_grpby, grouping_list:list):
    Vol_ONTrade_grpby_unpivot = pd.melt(Vol_ONTrade_grpby, id_vars=grouping_list, var_name="Year_Month", value_name='Volume')
    Vol_ONTrade_grpby_unpivot[["Year", "Month"]] = Vol_ONTrade_grpby_unpivot["Year_Month"].str.split(" ", expand =True)
    Vol_ONTrade_grpby_unpivot["Year"] = Vol_ONTrade_grpby_unpivot["Year"].astype("int64")
    Vol_ONTrade_grpby_unpivot["Month"] = Vol_ONTrade_grpby_unpivot["Month"].astype("int64")
    Vol_ONTrade_grpby_unpivot.sort_values(by=["Outlet Id", "Year", "Month"], ascending=[True,True, True], inplace=True)
    Vol_ONTrade_grpby_unpivot.reset_index(drop=True, inplace=True)
    Vol_ONTrade_grpby_unpivot.dropna(subset=['Avg_Monthly_Vol'], inplace=True)
    
    # Creating the Rolling Month:
    Yr_Mth_Df = Vol_ONTrade_grpby_unpivot[["Year", "Month"]].drop_duplicates().sort_values(["Year", "Month"])
    Yr_Mth_Df["Rolling_Month"] = [x for x in range(1, len(Yr_Mth_Df)+1)]
    Vol_ONTrade_grpby_unpivot = pd.merge(Vol_ONTrade_grpby_unpivot, Yr_Mth_Df, how="left", on=["Year", "Month"])

    return Vol_ONTrade_grpby_unpivot

# Initial Data Preparation:
def Data_Prep(Data):
    Data["Quarter_Num"] = pd.to_datetime(Data["Year_Month"]).dt.quarter
    Data["Month"] = pd.Categorical(Data["Month"])
    Data["Quarter_Num"] = pd.Categorical(Data["Quarter_Num"])

    # Removing the Inactive Months before the first Transaction:
    Data["Vol_Cumsum_Flag"] = Data.groupby(["Outlet Id"])["Volume"].cumsum()
    Data = Data[Data["Vol_Cumsum_Flag"]!=0].reset_index(drop=True)

    # Sorting the Data for Churn Flag Creation:
    Data = Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, False, False]).reset_index(drop=True)
    Data["Transaction_Flag"] = np.where(Data["Volume"]!=0,1,0)
    # Creating the Churn DataFrame:
    Churn_Flag_Df = pd.DataFrame(Data.groupby(["Outlet Id"], as_index=False)["Transaction_Flag"].rolling(window=3).apply(lambda x: 1 if sum(x)==0 else 0)).reset_index(drop=True)
    Churn_Flag_Df.columns=["Churn_Flag"]

    Data = pd.concat([Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, False, False]).reset_index(drop=True), Churn_Flag_Df], axis=1)
    Data = Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, True, True]).reset_index(drop=True)
    
    

    return Data
# Last Transaction:
def find_last_transaction(y):
    y=pd.Series(y)
    try:
        last_transaction=(len(y)-1)-max(y[y==1].index)
    except:
        last_transaction=len(y)
    return(last_transaction)

# Normalized Slope:
def find_normalized_slope(y):
    y=pd.Series(y)
    y=(y/max(abs(y))).fillna(0)
    x=pd.Series(range(0,len(y)))/(len(y)-1)
    slope=linregress(x, y).slope
    return(slope)

# Churn Status:
def find_churn(y):
    y=pd.Series(y)
    if(y[:3].sum()==0):
        return(1)
    else:
        return(0)  

### Feature Creation Function:
def Feature_Create(Data_Sub, Raw_Data):
    # Creating the Transaction Flag:
    Data_Sub["Transaction_Flag"] = np.where(Data_Sub["Volume"]>0, 1 ,0)

    # Getting the last transactions:
    Data_Sub["Last_Transaction"] = Data_Sub.groupby(["Outlet Id"])["Transaction_Flag"].rolling(window=12).apply(lambda x:find_last_transaction(x)).reset_index(drop=True)

    # Quarter Number:
    Data_Sub["Quarter_Num"] = pd.to_datetime(Data_Sub["Year_Month"]).dt.quarter

    # Getting the Segment and Sub Segment Information:
    Data_Sub = pd.merge(Data_Sub, Raw_Data[["Outlet Id",'Segment','Sub Segment', 'Avg_Monthly_Vol_Buck']].drop_duplicates(), on=["Outlet Id"], how="left")

    # Number  of Transactions Historically from the Given Year Month:
    Data_Sub["Transaction_rolling_sum_3"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=3).sum().reset_index(drop=True)
    Data_Sub["Transaction_rolling_sum_6"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=6).sum().reset_index(drop=True)
    Data_Sub["Transaction_rolling_sum_9"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=9).sum().reset_index(drop=True)
    Data_Sub["Transaction_rolling_sum_12"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=12).sum().reset_index(drop=True)

    # Average Num of Transaction Historically from the Given Year Month:
    Data_Sub["Transaction_rolling_mean_3"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=3).mean().reset_index(drop=True)
    Data_Sub["Transaction_rolling_mean_6"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=6).mean().reset_index(drop=True)
    Data_Sub["Transaction_rolling_mean_9"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=9).mean().reset_index(drop=True)
    Data_Sub["Transaction_rolling_mean_12"] = Data_Sub.groupby('Outlet Id')[['Transaction_Flag']].shift(1).rolling(window=12).mean().reset_index(drop=True)
    print("Rolling Transaction Sum & Mean Completed !!")

    # Volume:
    Data_Sub["Volume_rolling_sum_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).sum().reset_index(drop=True)

    Data_Sub["Volume_rolling_mean_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).mean().reset_index(drop=True)
    print("Volume Rolling Sum & Mean Completed !!")
    
    # Getting the Number of Brands Active by Month:
    Brand_Activ_Yr_Mth = Raw_Data[Raw_Data["Volume"]>0].groupby(["Outlet Id", "Year_Month"], as_index=False).agg({"Brand Name":pd.Series.nunique})
    Brand_Activ_Yr_Mth.rename(columns={"Brand Name":"NumBrands_Mth"}, inplace=True)
    Data_Sub = pd.merge(Data_Sub,Brand_Activ_Yr_Mth, how="left", on=["Outlet Id", "Year_Month"] )
    print(Data_Sub.columns)
    Data_Sub["NumBrand_Mth_rolling_sum_3"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=3).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_6"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=6).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_9"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=9).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_12"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=12).sum().reset_index(drop=True)

    Data_Sun = Data_Sub.reset_index(drop=True)
    
    return Data_Sub

def Normalized_Slope(Data_Sub):

    print("Outlet Id:", Data_Sub["Outlet Id"].unique())
    # Slope of Volume:
    Data_Sub["Volume_normalized_slope_rolling_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    print("Rolling Volume Slope Completed !!")

    return Data_Sub

def Predictions_(Model, Predict_Df, Imp_Features_:list):
    # Getting the Class Predictions and Class Probabilities:
    Predict_Churn_Status = Model.predict(Predict_Df[Imp_Features_])
    Predict_Churn_Status = pd.DataFrame(Predict_Churn_Status)
    Predict_Churn_Status.columns = ["Prediction"]
    Predict_Churn_Prob = Model.predict_proba(Predict_Df[Imp_Features_])
    Predict_Churn_Prob = pd.DataFrame(Predict_Churn_Prob)
    Predict_Churn_Prob.columns = ["Prediction_Probs_0", "Prediction_Probs_1" ]
    # Concatenating the Results:
    Final_Prediction_Df = pd.concat([Predict_Df,Predict_Churn_Status ], axis=1)
    Final_Prediction_Df = pd.concat([Final_Prediction_Df,Predict_Churn_Prob], axis=1)

    return Final_Prediction_Df
# %%
# if __name__ == '__main__':
# Providing the list at which the data needs to be grouped: (Check with GOA team if in case changes req.)
Poc_Vol_Grpby_Gouping_List = ['Outlet Id', 'Outlet Postcode', 'Postal District', 'Segment','Sub Segment',
                                'BDR Territory Level 1',  'BDR Territory Level 2', 'Supplier Name','Dispense Name','Brand Name']


# Unpvotting the data:
Unpivot_Df_Gouping_List = ['Outlet Id', 'Outlet Postcode', 'Postal District', 'Segment','Sub Segment',
                                'BDR Territory Level 1',  'BDR Territory Level 2', 'Supplier Name','Dispense Name','Brand Name',
                                'Total_Vol', 'Active_Months', 'Avg_Monthly_Vol', 'Avg_Monthly_Vol_Buck' ]

# %%
# Importing the Raw Data:
RawData = pd.read_excel(Input_Path + "Volume By Product_Holly_Sep_2020.xlsx")
# RawData = RawData[RawData["Outlet Id"].isin(RawData["Outlet Id"].unique().tolist()[0:10])]
print("Number of Duplicates: ", RawData.duplicated().sum())
RawData = RawData.drop_duplicates()

# Importing the Poc Attributes:
Outlet_Attributes = pd.read_excel(Input_Path + "Outlet_Attributes.xlsx", index=False)
Outlet_Attributes = Outlet_Attributes.drop_duplicates()

# Getting the POC Attributes:
RawData = pd.merge(RawData, Outlet_Attributes, on="Outlet Id", how="left")
RawData = RawData.drop_duplicates().reset_index(drop=True)

# Imputing the Missing values:
Years_Cons = "2018|2019|2020"
# Initial Data Treatment:
Vol_ONTrade_part_1 = RawData[RawData.columns[~(RawData.columns.str.contains(Years_Cons))]]
Vol_ONTrade_part_2 = RawData[RawData.columns[RawData.columns.str.contains(Years_Cons)]].fillna(0)

# Treating the Negative Values of Volume to 0:
for i in range(0, len(Vol_ONTrade_part_2.columns)):
    Vol_ONTrade_part_2.iloc[:,i] = Vol_ONTrade_part_2.iloc[:,i].apply(lambda x: x if (x>0) else 0)
    
if  Vol_ONTrade_part_1.shape[0] ==  Vol_ONTrade_part_2.shape[0]:
    RawData = pd.concat([Vol_ONTrade_part_1, Vol_ONTrade_part_2], axis=1)
else:
    print("Error in Initial Data Transformation !!")
    
del Vol_ONTrade_part_1, Vol_ONTrade_part_2

# Getting the Unpivotted Data:
Init_DF_1 = POC_Vol_Grpby_Df(RawData, grouping_list = Poc_Vol_Grpby_Gouping_List)
Final_Pivot_Data = unpivot_df(Init_DF_1, grouping_list=Unpivot_Df_Gouping_List)
# %%
# Removing April, May and June Month from the Data to avoid Bias:
Ignore_Months = ['2020 4', '2020 5', '2020 6']
Final_Pivot_Data = Final_Pivot_Data[~Final_Pivot_Data["Year_Month"].isin(Ignore_Months)].reset_index(drop=True)
Final_Pivot_Data["Year_Month"].unique()
# %%
# Initial Data Preparation:
Init_Data_Prep = Data_Prep(Final_Pivot_Data)
Poc_Level_Vol = Init_Data_Prep.groupby(["Outlet Id", "Year", "Month", "Year_Month"], as_index=False).agg({"Volume":sum})

# %%
# Getting the Rolling Sum and Mean:
Trans_Data = Feature_Create(Data_Sub = Poc_Level_Vol, Raw_Data =Init_Data_Prep)

#%%
# Array for Multiprocessing:
Outlet_Array = [Trans_Data[Trans_Data["Outlet Id"]==Outlet] for Outlet in Trans_Data["Outlet Id"].unique().tolist()]
print("Length of the Churn Status Master Array: ", len(Outlet_Array))

# %%
# Churn Duration Computation:
print("Normalized Slope Calculation Starts !!")
start = time.time()
n_cores = 8
pool = Pool(n_cores)
FinalData = pd.concat(pool.map(Normalized_Slope, Outlet_Array))
FinalData.to_csv(Output_Path + "FinalData_Fr_Prediction.csv", index=False)
pool.close()
pool.join()
end = time.time()
print((end-start)/60)
print("Normalized Slope Calculation Completed !!")

# %%
# Importing the Final Data for Prediction:
FinalData = pd.read_csv(Input_Path + "FinalData_Fr_Prediction.csv")
FinalData["Month"] = pd.Categorical(FinalData["Month"])
FinalData["Quarter_Num"] = pd.Categorical(FinalData["Quarter_Num"])
FinalData = FinalData.sort_values(by=["Outlet Id", "Year", "Month"])
FinalData = FinalData.reset_index(drop=True)

#### Importing the Latest Oxford Mapping for POC Image Mapping:
Oxford_POC_Image = pd.read_excel(r"F:\OFZ\OneDrive - Anheuser-Busch InBev\_MUKIL_\00_WORK\00_PROJECTS\14_CHURN_PREDICTION_ONTRADE_UK\01.Data\Oxford_Latest_Mapping_Extract_Frm_Marcus_v1.xlsx")
Oxford_POC_Image = Oxford_POC_Image[['bbg_ou_id', 'quality_key']].drop_duplicates().reset_index(drop=True)
Oxford_POC_Image['bbg_ou_id'] = Oxford_POC_Image['bbg_ou_id'].astype("float64").fillna(0).astype("int64")
Oxford_POC_Image = Oxford_POC_Image[Oxford_POC_Image['bbg_ou_id'].isin(FinalData["Outlet Id"].unique())].reset_index(drop=True)
Oxford_POC_Image = Oxford_POC_Image[~Oxford_POC_Image["quality_key"].isna()].reset_index(drop=True)
Oxford_POC_Image["bbg_ou_id"].value_counts().sort_values(ascending=False).head(15)
Oxford_POC_Image[Oxford_POC_Image["bbg_ou_id"].isin([93973,3973,39209, 38866,4894, 17287, 122995, 214107, 23137, 17348, 212968, 63439])]
Oxford_POC_Image["quality_key"] = np.where(Oxford_POC_Image["bbg_ou_id"] ==38866, "SP", Oxford_POC_Image["quality_key"])
Oxford_POC_Image["quality_key"] = np.where(Oxford_POC_Image["bbg_ou_id"].isin([93973,3973,39209,4894, 17287, 122995, 214107, 23137, 17348, 212968, 63439]), "P", Oxford_POC_Image["quality_key"])
Oxford_POC_Image = Oxford_POC_Image.drop_duplicates().reset_index(drop=True)
Oxford_POC_Image["bbg_ou_id"].value_counts().sort_values(ascending=False).head(15)

Oxford_POC_Image.columns = ["Outlet Id","POC Image"]
Oxford_POC_Image.head()

# Adding POC Image Attributes:
FinalData = pd.merge(FinalData, Oxford_POC_Image, how="left", on="Outlet Id")

# Creating the Dummy Variables:
Dummy_Var_Df = pd.get_dummies(FinalData[["Sub Segment", "Month", "Quarter_Num", "POC Image"]])
FinalData = pd.concat([FinalData, Dummy_Var_Df], axis=1)
del Dummy_Var_Df
# %%

# Selecting the Variables in the Model for prediction:
## Features from Imp_Features_
# Imp_Features_ = ['Transaction_rolling_sum_3', 'Transaction_rolling_sum_6', 'Transaction_rolling_sum_9', 'Transaction_rolling_sum_12',
# 'Volume_rolling_sum_3', 'Volume_rolling_sum_6', 'Volume_rolling_sum_9', 'Volume_rolling_sum_12',
# 'Volume_rolling_mean_6', 'Volume_normalized_slope_rolling_3', 'Volume_normalized_slope_rolling_6', 'Volume_normalized_slope_rolling_9',
# 'Volume_normalized_slope_rolling_12', 'NumBrand_Mth_rolling_sum_3', 'NumBrand_Mth_rolling_sum_6', 'NumBrand_Mth_rolling_sum_12',
# 'Sub Segment_3 Star and below', 'Sub Segment_Craft Beer Bar', 'Sub Segment_Restaurant',
# 'Sub Segment_Sports Venue', 'Sub Segment_Venue bars', 'Month_1', 'Month_3', 'Month_4', 'Month_6',
# 'Month_9', 'Month_11', 'Quarter_Num_1', 'Quarter_Num_2', 'Quarter_Num_3', 'Quarter_Num_4']

Imp_Features_ = pickle.load(open(Output_Path + "Imp_Features_Final.pkl", "rb"))

### Loading the Model:
Model = pickle.load(open(Output_Path + "FinalModel_XGBoost_09112020.sav", 'rb'))

# %%
# Selecting the data for Prediction:
Pred_Months = ['2018 1','2018 2', '2018 3','2018 4','2018 5', '2018 6','2018 7','2018 8', '2018 9','2018 10','2018 11','2018 12',
                '2019 1','2019 2', '2019 3','2019 4','2019 5', '2019 6','2019 7','2019 8', '2019 9','2019 10','2019 11','2019 12',
                '2020 1','2020 2', '2020 3', '2020 7', '2020 8']
Predict_Df= FinalData[FinalData["Year_Month"].isin(Pred_Months)].reset_index(drop=True)
# %%
# Getting the Predictions:
Final_Prediction_Df = Predictions_(Model=Model, Predict_Df=Predict_Df, Imp_Features_=Imp_Features_)
Final_Prediction_Df = Final_Prediction_Df.sort_values(by=["Outlet Id", "Year", "Month"]).reset_index(drop=True)
print(Final_Prediction_Df.head(50))

# Exporting the Predictions:
Final_Prediction_Df.to_csv(Output_Path + "Final_Prediction_Df_full_2018_2020_v2.csv", index=False)


# %%
