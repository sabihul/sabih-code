# %%
### Importing the Modules:
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 

from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

# ML Libraries:
from sklearn import ensemble
from sklearn import metrics
from sklearn.model_selection import train_test_split, StratifiedKFold
from imblearn.over_sampling import RandomOverSampler, SMOTE
from imblearn.under_sampling import RandomUnderSampler
from bayes_opt import BayesianOptimization
from sklearn.feature_selection import RFECV
from sklearn.model_selection import cross_val_score

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import RandomizedSearchCV
import xgboost as xgb 
import lightgbm as ltb
import catboost as ctb
import pickle

Input_Path = r"F:/OFZ/OneDrive - Anheuser-Busch InBev/_MUKIL_/00_WORK/00_PROJECTS/14_CHURN_PREDICTION_ONTRADE_UK/05. Project Final Folder/02. Churn Model/00. Data/"
Output_Path = r"F:/OFZ/OneDrive - Anheuser-Busch InBev/_MUKIL_/00_WORK/00_PROJECTS/14_CHURN_PREDICTION_ONTRADE_UK/03.Output/Model_Results/Model Results_LatestPOCImage/"

pd.set_option("max_rows", 1000)
# %%
### Importing the data:
Data = pd.read_csv(Input_Path + "Data_Processed_04092020_v2.csv")

# Creating the Key:
Data["Key"] = Data["Outlet Id"].astype("str") + "_" + str(Data["Year_Month"])
Sample_Data = Data.iloc[:100]

Data["Month"] = pd.Categorical(Data["Month"])
Data["Quarter_Num"] = pd.Categorical(Data["Quarter_Num"])

# Removing the Inactive Months before the first Transaction:
Data["Vol_Cumsum_Flag"] = Data.groupby(["Outlet Id"])["Volume"].cumsum()
Data = Data[Data["Vol_Cumsum_Flag"]!=0].reset_index(drop=True)

# Removing the Unwanted Columns:
Data = Data.drop(columns=['NumBrands_Mth', 'Avg_NumBrand', 'Outlet_Supplier_Prop', 'Vol_Cumsum_Flag']).drop_duplicates().reset_index(drop=True)
Data.groupby(["Outlet Id"], as_index=False).agg({"Year_Month":pd.Series.nunique})["Year_Month"].unique()

# Sorting the Data for Churn Flag Creation:
Data = Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, False, False]).reset_index(drop=True)
Data["Transaction_Flag"] = np.where(Data["Volume"]!=0,1,0)
# Creating the Churn DataFrame:
Churn_Flag_Df = pd.DataFrame(Data.groupby(["Outlet Id"], as_index=False)["Transaction_Flag"].rolling(window=3).apply(lambda x: 1 if sum(x)==0 else 0)).reset_index(drop=True)
Churn_Flag_Df.columns=["Churn_Flag"]

Data = pd.concat([Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, False, False]).reset_index(drop=True), Churn_Flag_Df], axis=1)
Data = Data.sort_values(by=["Outlet Id", "Year","Month"], ascending=[True, True, True]).reset_index(drop=True)
#%%
Data[["Outlet Id", "Year", "Month","Volume", "Churn_Flag"]].head(100)
#%%
#### Importing the Latest Oxford Mapping for POC Image Mapping:
Oxford_POC_Image = pd.read_excel(r"F:\OFZ\OneDrive - Anheuser-Busch InBev\_MUKIL_\00_WORK\00_PROJECTS\14_CHURN_PREDICTION_ONTRADE_UK\01.Data\Oxford_Latest_Mapping_Extract_Frm_Marcus_v1.xlsx")
Oxford_POC_Image = Oxford_POC_Image[['bbg_ou_id', 'quality_key']].drop_duplicates().reset_index(drop=True)
Oxford_POC_Image['bbg_ou_id'] = Oxford_POC_Image['bbg_ou_id'].astype("float64").fillna(0).astype("int64")
Oxford_POC_Image = Oxford_POC_Image[Oxford_POC_Image['bbg_ou_id'].isin(Data["Outlet Id"].unique())].reset_index(drop=True)
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
Data = pd.merge(Data, Oxford_POC_Image, how="left", on="Outlet Id")

# Creating Dummy Variables:
Dummy_Var_Df = pd.get_dummies(Data[["Sub Segment", "Month", "Quarter_Num", "POC Image"]])
Data = pd.concat([Data, Dummy_Var_Df], axis=1)
Data.head()
# %%
# Filtering out the required data:
# Splitting the data into train and test:
Val_Data= pd.concat([Data[(Data["Year"]==2019) &(Data["Month"].isin([11,12]))],Data[(Data["Year"]==2020)&(Data["Month"].isin([1]))]]).reset_index(drop=True)
Train_Data = pd.concat([Data[(Data["Year"]==2018) &(Data["Month"].isin([1,2,3,4,5,6,7,8,9,10,11,12]))],Data[(Data["Year"]==2019)&(Data["Month"].isin([1,2,3,4,5,6,7,8,9,10]))]]).reset_index(drop=True)

# shuffling the Data:
Train_Data = Train_Data.sample(frac=1).reset_index(drop=True)
Train_Data.columns = Train_Data.columns.str.replace("/","_").str.replace("(","").str.replace(",","_").str.replace("]","")
Val_Data = Val_Data.sample(frac=1).reset_index(drop=True)
Val_Data.columns = Val_Data.columns.str.replace("/","_").str.replace("(","").str.replace(")","").str.replace(",","_").str.replace("]","")


# %%
# Checking the Distribution of Target:
print(Train_Data["Churn_Flag"].value_counts(normalize=True))
print(Val_Data["Churn_Flag"].value_counts(normalize=True))

# %%
### Functions for Modeling:
# Model Metrics:
def Classification_Metrics_Report(y_actuals, y_predicted):
    Model_Metrics = dict()
    # Accuracy:
    accuracy = round(metrics.accuracy_score(y_actuals, y_predicted),2)
    print("Accuracy of the Model:", accuracy)
    # Precision:
    precision = round(metrics.precision_score(y_actuals, y_predicted),2)
    print("Precision of the Model:",precision)
    # Recall:
    recall = round(metrics.recall_score(y_actuals, y_predicted),2)
    print("Recall of the Model:", recall)
    # F1 Score:
    F1_Score = round(metrics.f1_score(y_actuals, y_predicted),2)
    print("F1-Score of the Model:", F1_Score)
    print()
    # Confusion Matrix:
    print("Confusion Matrix: ")
    matrix = metrics.confusion_matrix(y_actuals, y_predicted)
    print(matrix)
    print()
    # Classification Report:
    print("Classification Report: ")
    print(metrics.classification_report(y_actuals, y_predicted))
    print()
    # Area Under Curve:
    Auc_ = metrics.roc_auc_score(y_actuals, y_predicted)
    print("Area Under Curve: ", Auc_)
    print()

    # Collecting the Model Metrics:
    Model_Metrics["accuracy"] = accuracy
    Model_Metrics["precision"] = precision
    Model_Metrics["recall"] = recall
    Model_Metrics["F1_Score"] = F1_Score
    Model_Metrics["Auc_"] = Auc_

    return Model_Metrics

# Cross Validation:
def Cross_Validation(model_name,X, y):
    model = Models_(model_name=model_name)
    Accuracy_CV = []
    Precision_CV = []
    Recall_CV = []
    F1_SCore_CV = []
    KFold = StratifiedKFold(n_splits=5,shuffle=True, random_state=123)
    for train_idx, test_idx in KFold.split(X,y):
        X_train, X_test = X.iloc[train_idx,:], X.iloc[test_idx,:]
        y_train, y_test = y[train_idx], y[test_idx]

        # Fitting the Model:
        model.fit(X_train, y_train)
        y_predicted = model.predict(X_test)
        accuracy, precision, recall, F1_Score = Classification_Metrics_Report(y_actuals=y_test, y_predicted=y_predicted)
        Accuracy_CV.append(accuracy)
        Precision_CV.append(precision)
        Recall_CV.append(recall)
        F1_SCore_CV.append(F1_Score)
    print("CV Model Metrics: ")
    print("Mean Accuracy: " ,np.mean(Accuracy_CV))
    print("Mean Precision: ", np.mean(Precision_CV))
    print("Mean Recall: ", np.mean(Recall_CV))
    print("Mean F1_Score: ", np.mean(F1_SCore_CV))

# Sampling of the data:
def UnderSampling(X,y):
    RUS_ = RandomUnderSampler(random_state=123)
    X_UnderSamp, y_UnderSamp = RUS_.fit_sample(X,y)
    return X_UnderSamp, y_UnderSamp

def OverSampling(X,y):
    ROS_ = RandomOverSampler(random_state=123)
    X_OverSamp, y_OverSamp = ROS_.fit_sample(X,y)
    return X_OverSamp, y_OverSamp

def SMOTE_OverSampling(X,y):
    sm = SMOTE(random_state=123)
    X_OverSamp, y_OverSamp = sm.fit_sample(X,y)
    return X_OverSamp, y_OverSamp

# All Base Models:
def Models_(model_name,X,y, X_test, y_actuals ):
    
    if model_name == "RFClassifier":
        print("--"*80)
        print("Model Used: Random Forest Classifier !!")
        print("--"*80)
        model = ensemble.RandomForestClassifier(random_state=123,verbose=0)
    elif model_name=="LogisticRegression":
        print("--"*80)
        print("Model Used: LogisticRegression !!")
        model = LogisticRegression(random_state=123,verbose=0)
        print("--"*80)
    elif model_name == "XGBoost":
        print("--"*80)
        print("Model Used: XGBoost Classifier !!")
        print("--"*80)
        model = xgb.XGBClassifier(random_state=123,verbosity=0)
    elif model_name == "LightGBM":
        print("--"*80)
        print("Model Used: LightGBM Classifier !!")
        print("--"*80)
        model = ltb.LGBMClassifier(random_state=123, verbose=0)
    elif model_name == "CatBoost":
        print("--"*80)
        print("Model Used: CatBoost Classifier !!")
        print("--"*80)
        model = ctb.CatBoostClassifier(random_state=123, verbose=0)
    else:
        pass 
    
    # Fitting the Model:
    model.fit(X, y)

    # make predictions
    y_predicted = model.predict(X_test)
    y_predicted_prob = model.predict_proba(X_test)
    Prob_Prediction_Df = pd.DataFrame([y_actuals, y_predicted_prob]).transpose()
    Prob_Prediction_Df.columns = ["Actuals", "Predicted_Probs"]
    Model_Metrics = Classification_Metrics_Report(y_actuals, y_predicted)
    Prediction_Df = pd.DataFrame([y_actuals, y_predicted]).transpose()
    Prediction_Df.columns = ["Actuals", "Predicted"]


    return model,Prediction_Df, Prob_Prediction_Df, Model_Metrics


### Feature Selection using RFE:
def rfecv(X,y):
    # Create the RFE object and compute a cross-validated score.
    model = xgb.XGBClassifier(random_state=123, n_estimators=100, n_jobs=-1)
    rfecv = RFECV(estimator=model, step=1, min_features_to_select=10, scoring='roc_auc', verbose=1)
    rfecv.fit(X, y)
    print("Optimal number of features : %d" % rfecv.n_features_, '\n')

    # Plot number of features VS. cross-validation scores
    plt.figure()
    plt.xlabel("Number of features selected")
    plt.ylabel("Cross validation score (roc-auc)")
    plt.plot(range(1, len(rfecv.grid_scores_) + 1), rfecv.grid_scores_)
    plt.show()

    #features = [f for f,s in zip(X.columns, rfecv.support_) if s]    
    features = pd.DataFrame(rfecv.support_,index = X.columns, columns=['keep'])

    return(features)

### Model Parameter Optimization using Randomized CV:
def Final_Tuned_Model(model_name, model, X, y, X_test, y_actuals):
    Random_Search = RandomizedSearchCV(estimator = model, param_distributions = params, 
                                scoring= 'f1', n_iter = 50, 
                                cv = StratifiedKFold(n_splits=5),
                                verbose=2, random_state=123, n_jobs = -1)
    # Model for Stratified Cross Validation:
    Random_Search.fit(X, y)
    # RandomSearchCV results:
    print(Random_Search.cv_results_)
    print(Random_Search.best_estimator_)
    print(Random_Search.best_params_)
    
    # Optimized Parameters:
    Optim_Model_Parameters = Random_Search.best_params_

    # Building the Model using Tuned Parameters:
    Final_Model = model(**Optim_Model_Parameters) ###
    Final_Model.fit(X, y)
    y_predicted = Final_Model.predict(X_test)
    Model_Metrics = Classification_Metrics_Report(y_actuals, y_predicted)
    Prediction_Df = pd.DataFrame([y_actuals, y_predicted])
    Prediction_Df.columns = ["Actuals", "Predicted"]

    # Save it to a pickle file:
    pickle.dump(Optim_Model_Parameters, open(Output_Path + model_name+"_Optimized_Parameters.pickle","wb"))
    # Save it to a pickle file:
    pickle.dump(Model_Metrics, open(Output_Path + model_name+"_Final_Model_Metrics.pickle","wb"))
    # Exporting the Predictions:
    Prediction_Df.to_excel(Output_Path + model_name + "_Final_Predictions.xlsx", index=False)

# %%
# Removing unwanted Columns:
Unwanted_Cols = ['Year', 'Year_Month', 'Rolling_Month', 'Last_Transaction','Avg_Monthly_Vol_Buck','Segment', 'Sub Segment', "Start_Restart","Month", "Quarter_Num",\
                    'Sub Segment_Sports venues', 'Sub Segment_Venue Bar' , 'Sub Segment_Pub & Kitchen Gastro Pub',\
                         'Becks', 'Becks Blue', 'Becks Vier', 'Boddingtons Draughtflow', 'Brahma', 'Bud Light', 'Budweiser', 'Camden Hells', 'Camden Pale Ale',\
                     'Corona Extra', 'Goose Island IPA', 'Goose Island Midway', 'Lowenbrau', 'Magners Dark Fruit', 'Magners Irish Cider',\
       'Magners Irish Cider Long Neck', 'Orchard Pig', 'Others',  'Stella Artois', 'Stella Artois 4%', 'Stella Cidre', 'Unmatched', 'Draught Bass Cask)', "POC Image"]

### Getting the data for Modeling:
X = Train_Data.drop(columns=["Outlet Id", "Key", "Churn_Event", 'Churn_Flag_1', 'Transaction_Flag', "Volume" ,"Churn_Flag"] + Unwanted_Cols).fillna(0)
cols_ = X.columns
y = Train_Data["Churn_Flag"].values
# Random Forest:
X_test = Val_Data[X.columns].fillna(0)
y_actuals = Val_Data["Churn_Flag"].values

# Sampled Data:
# X, y= SMOTE_OverSampling(X=X, y=y)
# X = pd.DataFrame(X, columns=cols_)
# del Data, Train_Data, Dummy_Var_Df
#%%
# Running the CrossValidation:
# Cross_Validation(model_name="RFClassifier",X=X, y=y)
# %%
### Running all the Base Models:
Predictions_Df_Dict = dict()
for model_ in ["RFClassifier", "LogisticRegression", "XGBoost","LightGBM",  "CatBoost" ]:
    Predictions_Df_Dict[model_] = dict()
    model, Prediction_Df, Model_Metrics = Models_(model_name = model_,X = X,y = y, X_test = X_test, y_actuals = y_actuals )
    Predictions_Df_Dict[model_]["Model"] = model
    Predictions_Df_Dict[model_]["Predicted_df"] = Prediction_Df
    Predictions_Df_Dict[model_]["Model_Metrics"] = Model_Metrics
    Predictions_Df_Dict
    print("--"*80)

# Checking the Model Metrics:
for keys in Predictions_Df_Dict.keys():
    print(keys, Predictions_Df_Dict[keys]["Model_Metrics"])


# %%
### Getting the Important Features:
Imp_Features_RFECV = rfecv(X,y) # Comment when Not Required
# Getting the Important Features:
Imp_Features_RFECV = Imp_Features_RFECV.reset_index()
Imp_Features_RFECV.columns = ["Variables", "Import_Flag"]
Imp_Features_ = Imp_Features_RFECV[Imp_Features_RFECV["Import_Flag"]==True]["Variables"].tolist()

### Saving the Important Features as Pickle Files:
pickle.dump(Imp_Features_, open(Output_Path + "Imp_Features_Final.pkl", 'wb'))

### Loading the Features from Imp_Features_
Imp_Features_ = pickle.load(open(Output_Path + "Imp_Features_Final.pkl", "rb"))
Imp_Features_
# %%
### XGBoost using the Important Vars given by RFE:
Base_XGBoost_model,Base_XGBoost_Prediction_Df, Base_XGBoost_Prob_Prediction_Df, Base_XGBoost_Model_Metrics = Models_(model_name = "XGBoost", X=X[Imp_Features_],y=y, X_test=X_test[Imp_Features_], y_actuals=y_actuals )

# save the model to disk
filename = Output_Path +  'FinalModel_XGBoost_0911092020.sav'
pickle.dump(Base_XGBoost_model, open(filename, 'wb'))
# %%
Var_Import_Df = pd.DataFrame([X.columns , Base_XGBoost_model.feature_importances_]).transpose()
Var_Import_Df.columns = ["Variables", "Importance_"]
Var_Import_Df = Var_Import_Df.sort_values(by="Importance_",ascending=False)
Var_Import_Df.to_excel(Output_Path + "Important_Var_XgBoost_v7.xlsx", index=False)

Predicted_Df_Full = pd.concat([Val_Data,Base_XGBoost_Prediction_Df, Base_XGBoost_Prob_Prediction_Df], axis=1)
Predicted_Df_Full = Predicted_Df_Full.sort_values(by=["Outlet Id", "Year", "Month"])
# Predicted_Df_Full.to_excel(Output_Path + "Predicted_Df_Full_v7.xlsx", index=False)


############################################################################################################################################################
#%%
# # Model for Optimization:
# xgb_model = xgb.XGBClassifier(random_state=123,verbosity=0)
# # Hyperparameters for XGBoost:
# params={
#  "learning_rate"    : [0.05, 0.10, 0.15, 0.20, 0.25, 0.30 ] ,
#  "max_depth"        : [ 3, 4, 5, 6, 8, 10, 12, 15],
#  "min_child_weight" : [ 1, 3, 5, 7 ],
#  "gamma"            : [ 0.0, 0.1, 0.2 , 0.3, 0.4 ],
#  "colsample_bytree" : [ 0.3, 0.4, 0.5 , 0.7 ]  
# }

# # Calling RandomSearchCV for XGBoost:
# Final_Tuned_Model(model_name="XGBoost", model=xgb_model, X=X[Imp_Features_], y=y)

# #%%
# # Getting the Important variable for XGBoost:
# ImportVar = Var_Import_Df[Var_Import_Df["Importance_"]>0.01]["Variables"].tolist()

# # Final XGBoost Model:
# Final_XGBoost_model,Final_XGBoost_Prediction_Df, Final_XGBoost_Model_Metrics = Models_(model_name = "XGBoost", X=X[ImportVar],y=y, X_test=X_test[ImportVar], y_actuals=y_actuals )
# Final_XGBoost_Prediction_Df_Trans = Final_XGBoost_Prediction_Df.transpose()
# Final_XGBoost_Prediction_Df_Trans.columns = ["Actuals", "Predicted"]
# Final_XGBoost_Prediction_Df_Trans.head()
# Predicted_Df_Full = pd.concat([Val_Data,Final_XGBoost_Prediction_Df], axis=1)

# Predicted_Df_Full.to_excel(Output_Path + "Predicted_Df_Full_Final_v4.xlsx", index=False)
# #%%
# ### LightGBM:
# # Base Model:
# Light_GBM_BaseModel = Models_(model_name = "LightGBM", X=X,y=y, X_test=X_test, y_actuals=y_actuals )[0]
# Var_Import_Df = pd.DataFrame([X.columns , Light_GBM_BaseModel.feature_importances_]).transpose()
# Var_Import_Df.columns = ["Variables", "Importance_"]
# Var_Import_Df = Var_Import_Df.sort_values(by="Importance_",ascending=False)

# # Getting the Important variable for LightGBM:
# ImportVar = Var_Import_Df[Var_Import_Df["Importance_"]>0.005]["Variables"].tolist()

# # Creating the Dataset for the LightGBM Model:
# Train_LGBM = ltb.Dataset(data=X[Var_Import_Df], label=y)

# def hyp_lgbm(num_leaves, feature_fraction, bagging_fraction, max_depth, min_split_gain, min_child_weight):
      
#     params = {'application':'regression','num_iterations': 5000,
#                 'learning_rate':0.05, 'early_stopping_round':50,
#                 'metric':'l1'} # Default parameters
#     params["num_leaves"] = int(round(num_leaves))
#     params['feature_fraction'] = max(min(feature_fraction, 1), 0)
#     params['bagging_fraction'] = max(min(bagging_fraction, 1), 0)
#     params['max_depth'] = int(round(max_depth))
#     params['min_split_gain'] = min_split_gain
#     params['min_child_weight'] = min_child_weight
#     cv_result = ltb.cv(params, Train_LGBM, nfold=5, seed=123, stratified=True, verbose_eval =None, metrics=['auc'])
#     print(cv_result)
    
#     return np.max(cv_result['auc'])

# # Define the Parameter Space:
# pds = {'num_leaves': (45, 60),
#           'feature_fraction': (0.1, 0.9),
#           'bagging_fraction': (0.8, 1),
#           'max_depth': (9, 13 ),
#           'min_split_gain': (0.001, 0.1),
#           'min_child_weight': (30, 50)
#           }

# # Surrogate model
# optimizer = BayesianOptimization(hyp_lgbm,pds,random_state=7)
# optimizer.maximize(init_points=5, n_iter=15)

# # Getting the Best Parameters:
# Light_GBM_OptimizedParameters = optimizer.max['params']

# # Final LightGBM Model:
# Light_GBM_FinalModel = ltb.LGBMClassifier(random_state=123, **Light_GBM_OptimizedParameters)
# Light_GBM_FinalModel.fit(X[Var_Import_Df],y)
# y_predicted = Light_GBM_FinalModel.predict(X_test[Var_Import_Df])

# Model_Metrics = Classification_Metrics_Report(y_actuals, y_predicted)
# Prediction_Df = pd.DataFrame([y_actuals, y_predicted])
# Prediction_Df.columns = ["Actuals", "Predicted"]

# model_name = "LightGBM"
# # Save it to a pickle file:
# pickle.dump(Light_GBM_OptimizedParameters, open(Output_Path + model_name+"_Optimized_Parameters.pickle","wb"))
# # Save it to a pickle file:
# pickle.dump(Model_Metrics, open(Output_Path + model_name+"_Final_Model_Metrics.pickle","wb"))
# # Exporting the Predictions:
# Prediction_Df.to_excel(Output_Path + model_name + "_Final_Predictions.xlsx", index=False)

# # %%
