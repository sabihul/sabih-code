# -*- coding: utf-8 -*-
"""
Created on Thu Feb 22 10:40:25 2018

@author: ishwarya.sriraman
"""
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import train_test_split
import numpy as np
import pandas as pd
from sklearn.metrics import confusion_matrix  
from sklearn.metrics import precision_recall_fscore_support
from sklearn import metrics
from sklearn.metrics import roc_auc_score
from sklearn.metrics import matthews_corrcoef
#import statsmodels.stats.gof as ssg
from scipy import stats

class machine_learning:
    
    def __init__(self,df,target):
        self.df=df
        self.target=target
        
    def precision_recall_score(self,Y_test,pred):
        Y=self.target
        confmat=confusion_matrix(Y_test, pred)
        l=sorted(set(self.df[Y].values))
        p=pd.DataFrame(data=confmat,index=l,columns=l)
        p.to_excel("Confusion_matrix.xlsx")
        score=precision_recall_fscore_support(Y_test, pred, average='macro') 
        print("Precision, Recall and f-Score is")
        print(score)
        
    def roc_area(self,Y_test,pred):
        fpr, tpr, thresholds = metrics.roc_curve(Y_test, pred,pos_label=0)
        roc_score=roc_auc_score(Y_test, pred)
        print("Area under ROC curve is")
        print(roc_score)
        
    def goodness_of_fit(self,Y_test,pred):
        print("Goodness of fit is")
        sum1=pred.sum()
        pred1=pred/sum1
        print(stats.chisquare(pred1, Y_test,ddof=2))
        
    def logistic_regression(self,cat,X):
        self.df=self.df.fillna(0)
        self.df=self.df.replace([np.inf, -np.inf], 0)
        Y=self.target
        X_train,X_test,Y_train,Y_test = train_test_split(self.df[X],self.df[Y],test_size=0.33,random_state=3)
        clf=LogisticRegression(penalty='l2', dual=False, tol=0.0001, C=1.0, fit_intercept=True, intercept_scaling=1, class_weight='balanced', random_state=None, solver='liblinear', max_iter=100, multi_class='ovr', verbose=0, warm_start=False, n_jobs=1)
        clf.fit(X_train,Y_train)
        print ('Mean accuracy Scikit learn: ')
        print(clf.score(X_test,Y_test))
        pred=clf.predict(X_test)
        bins=10
        y_prob = clf.predict_proba(X_test)
        cols = ['ACTUAL','PROB_POSITIVE','PREDICTED']
        data = [Y_test,y_prob[:,1],pred]
        df = pd.DataFrame(dict(zip(cols,data)))
        
        #Observations where y=1
        total_positive_n = df['ACTUAL'].sum()
        #Total Observations
        total_n = df.index.size
        natural_positive_prob = total_positive_n/float(total_n)
    
    
        #Create Bins where First Bin has Observations with the
        #Highest Predicted Probability that y = 1
        df['BIN_POSITIVE'] = pd.qcut(df['PROB_POSITIVE'],bins,labels=False)
        
        pos_group_df = df.groupby('BIN_POSITIVE')
        #Percentage of Observations in each Bin where y = 1 
        lift_positive = pos_group_df['ACTUAL'].sum()/pos_group_df['ACTUAL'].count()
        lift_index_positive = (lift_positive/natural_positive_prob)*100
        
        
        #Consolidate Results into Output Dataframe
        lift_df = pd.DataFrame({'LIFT_POSITIVE':lift_positive,
                                   'LIFT_POSITIVE_INDEX':lift_index_positive,
                                   'BASELINE_POSITIVE':natural_positive_prob})
        
        print(lift_df)
        print("Matthews corrcoef")
        print(matthews_corrcoef(Y_test, pred))
        
        return (Y_test,pred)