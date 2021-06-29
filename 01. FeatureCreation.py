#%%
### Importing the Modules:
import os
import pandas as pd 
import numpy as np
import matplotlib.pyplot as pyt 
from scipy.stats import linregress

pd.set_option("max_rows", 999)
pd.set_option("max_columns", 999)

# Setting up the Input Path:
Input_Path = r"F:/OFZ/OneDrive - Anheuser-Busch InBev/_MUKIL_/00_WORK/00_PROJECTS/14_CHURN_PREDICTION_ONTRADE_UK/03.Output/Model_Input/"
Output_Path = r"F:/OFZ/OneDrive - Anheuser-Busch InBev/_MUKIL_/00_WORK/00_PROJECTS/14_CHURN_PREDICTION_ONTRADE_UK/03.Output/Model_Results/"
# %%
### Importing the data:
Data = pd.read_csv(Input_Path + 'POC_Churn_Status_20082020.csv').drop(columns='key')
Data.sort_values(by=['Outlet Id', 'Year', 'Month'], inplace=True)
Data_Trans = Data.copy(deep=True)
Data_Trans["Year_Month"] = pd.Categorical(Data_Trans["Year_Month"])

# %%
Raw_Data = pd.read_csv(Input_Path + 'Unpivoted_Data_Latest_25082020.csv')
Raw_Data.head()

# %%
### EDA:
print('Total Number of POCs by Year: ', Data[Data["Start_Restart"]!='Inactive']["Outlet Id"].nunique())
print("Volume by Year: ")
Data.groupby(["Year"], as_index=False).agg({"Volume":sum})
# %%
### Functions:
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

# %%
# ### Feature Creation:
# Data_Sub = Data_Trans[Data_Trans["Outlet Id"].isin(Data_Trans["Outlet Id"].unique().tolist()[:20])]

# Raw_Data_Sample = Raw_Data.iloc[:500,:]    
# %%
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


    # Volume:
    Data_Sub["Volume_rolling_sum_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).sum().reset_index(drop=True)
    Data_Sub["Volume_rolling_sum_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).sum().reset_index(drop=True)

    Data_Sub["Volume_rolling_mean_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).mean().reset_index(drop=True)
    Data_Sub["Volume_rolling_mean_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).mean().reset_index(drop=True)

    # Slope of Volume:
    Data_Sub["Volume_normalized_slope_rolling_3"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=3).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_6"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=6).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_9"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=9).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)
    Data_Sub["Volume_normalized_slope_rolling_12"] = Data_Sub.groupby('Outlet Id')[['Volume']].shift(1).rolling(window=12).apply(lambda x:find_normalized_slope(x)).reset_index(drop=True)

    # Getting the Number of Brands Active by Month:
    Brand_Activ_Yr_Mth = Raw_Data[Raw_Data["Volume"]>0].groupby(["Outlet Id", "Year_Month"], as_index=False).agg({"Brand Name":pd.Series.nunique})
    Brand_Activ_Yr_Mth.rename(columns={"Brand Name":"NumBrands_Mth"}, inplace=True)
    Data_Sub = pd.merge(Data_Sub,Brand_Activ_Yr_Mth, how="left", on=["Outlet Id", "Year_Month"] )
    print(Data_Sub.columns)
    Data_Sub["NumBrand_Mth_rolling_sum_3"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=3).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_6"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=6).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_9"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=9).sum().reset_index(drop=True)
    Data_Sub["NumBrand_Mth_rolling_sum_12"] = Data_Sub.groupby('Outlet Id')[['NumBrands_Mth']].shift(1).rolling(window=12).sum().reset_index(drop=True)

    # Average Volume By Brand:
    Data_Sub["Avg_NumBrand"] = Data_Sub["Volume"]/Data_Sub["NumBrands_Mth"]

    # POC Vol/Supplier Name Proportion:
    # Proportion of the POC Volume out of Total Supplier for a given Month Year:
    Supplier_Yr_Mth_Vol = Raw_Data.groupby(["Outlet Id","Supplier Name", "Year"], as_index=False).agg({"Volume":sum}).rename(columns={"Volume":"Supplier_Vol"})
    POC_Yr_Mth_Vol = Raw_Data.groupby(["Outlet Id", "Supplier Name", "Year", "Year_Month"], as_index=False).agg({"Volume":sum}).rename(columns={"Volume":"Outlet_Vol"})
    POC_Yr_Mth_Vol = pd.merge(POC_Yr_Mth_Vol,Supplier_Yr_Mth_Vol, how="left", on=["Outlet Id", "Supplier Name", "Year"] )
    POC_Yr_Mth_Vol["Outlet_Supplier_Prop"] = POC_Yr_Mth_Vol["Outlet_Vol"]/POC_Yr_Mth_Vol["Supplier_Vol"]
    Data_Sub = pd.merge(Data_Sub,POC_Yr_Mth_Vol[["Outlet Id", "Year_Month","Outlet_Supplier_Prop" ]].drop_duplicates(), on=["Outlet Id", "Year_Month"], how="left")

    # Getting the Brand Proportion of Top Brands which contribute to 95% Volume:
    Brand_Vol_Year = Raw_Data.groupby(["Brand Name", "Year"], as_index=False).agg({"Volume":sum})
    Brand_Vol_Year["Perc_Cont"] = Brand_Vol_Year.groupby("Year")["Volume"].apply(lambda x :round(x/x.sum(),2))
    Brand_Vol_Year = Brand_Vol_Year.sort_values(by=["Year", "Volume"], ascending=[True, False])
    Brand_Vol_Year["Cumm_Perc"] = Brand_Vol_Year.groupby("Year")["Perc_Cont"].cumsum() 
    Brand_Vol_Year["Brand_Final"] = np.where(Brand_Vol_Year["Cumm_Perc"]<0.95,Brand_Vol_Year["Brand Name"], "Others" )
    List_Brands = Brand_Vol_Year["Brand_Final"].unique().tolist()
    List_Brands.remove("Others")
    Raw_Data["Brands_Fin"] = np.where(Raw_Data["Brand Name"].isin(List_Brands), Raw_Data["Brand Name"],"Others" )
    Raw_Data["Brand_Vol_Prop"] = Raw_Data.groupby(["Outlet Id", "Year_Month"])["Volume"].apply(lambda x:x/x.sum())
    Brand_Vol_Prop = pd.pivot_table(Raw_Data, index=["Outlet Id", "Year_Month"], columns=["Brands_Fin"], values="Brand_Vol_Prop")
    Data_Sub = pd.merge(Data_Sub, Brand_Vol_Prop, how="left", on=["Outlet Id", "Year_Month"])
    Data_Sub = Data_Sub.reset_index(drop=True)
    
    return Data_Sub
#%%

Data_Processed = Feature_Create(Data_Sub=Data_Trans, Raw_Data=Raw_Data)

# %%
Data_Processed.to_csv(Output_Path + "Data_Processed_04092020_v2.csv", index=False )

# %%
