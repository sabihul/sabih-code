# -*- coding: utf-8 -*-
"""
Created on Mon Feb 26 12:37:46 2018

@author: ishwarya.sriraman

"""
from sklearn.feature_selection import RFE
from sklearn.svm import SVR
import numpy as np

class feature_selection:
     def __init__(self,df,target):
        self.df=df
        self.target=target
    
     def recursive_feature_elimination(self,X):
        self.df=self.df.fillna(0)
        self.df=self.df.replace([np.inf, -np.inf], 0)
        y=self.target
        estimator = SVR(kernel="linear")
        selector = RFE(estimator, 20, step=1)
        selector = selector.fit(self.df[X], self.df[y])
        print("Recursive feature elimination")
        print(selector)
        print("Feature ranking")
        print(selector.ranking_)