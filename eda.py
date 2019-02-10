import numpy as np
import pandas as pd
from scipy.stats import kurtosis
from scipy.stats import skew
import missingno as msno
from rpy2.robjects.packages import STAP


class eda:
    def __init__(self,Train,Target):
        self.Train = Train
        self.Target = Train[Target]
    def eventRatio(self,event):
        y = self.Target.value_counts()
        self.ratio = y[event]/self.Target.count()
        return self.ratio
    def impstat(self):
        self.impStat = self.Train.describe()
        return self.impStat
    def range(self):
        x = [x for x in self.Train.columns if self.Train[x].dtypes != 'object']
        z = self.Train[x]
        def Range(y):
            mi = np.min(y)
            ma = np.max(y)
            rng = ma-mi
            return rng
        self.Rnge = z.apply(Range)
        return pd.Series(self.Rnge.values)
    def iqr(self):
        x = [x for x in self.Train.columns if self.Train[x].dtypes != 'object']
        z = self.Train[x]
        def IQR(y):
            x25 = np.percentile(y,25)
            x75 = np.percentile(y,75)
            Iqr = x75 - x25
            return Iqr
            
        self.iQr = z.apply(IQR)
        return  pd.Series(self.iQr)
    
    def corr(self):
        x = [x for x in self.Train.columns if self.Train[x].dtypes != 'object']
        z = self.Train[x]
        self.corr = z.corr()
        return self.corr
    def skew(self):
        x = [x for x in self.Train.columns if self.Train[x].dtypes != 'object']
        z = self.Train[x]
        def Skew(y):
            d = skew(y)
            return d
        self.skewness = z.apply(Skew)
        return pd.Series(self.skewness)
    def kurt(self):
        x = [x for x in self.Train.columns if self.Train[x].dtypes != 'object']
        z = self.Train[x]
        def Kurt(y):
            d = kurtosis(y)
            return d
        self.Kurtosis = z.apply(Kurt)
        return pd.Series(self.Kurtosis)
    def missinginfo(self):
        def getPctMissing(series):
            num = series.isnull().sum()
            den = series.count()
            return 100*(num/den)
        self.missing = self.Train.apply(getPctMissing)
        self.missing.sort_values(ascending=False,inplace=True)
        self.totalmiss = self.Train.isnull().sum().sum()
        return [self.totalmiss,self.missing]
    def missingplot(self):
       msno.matrix(self.Train)
       #show(plt)
    def missingcorr(self):
        msno.heatmap(self.Train)
    def missingpattern(self):
        msno.dendrogram(self.Train)
    def bin(self,method,target,event,*args):
        import rpy2.robjects as robjects
        from rpy2.robjects.vectors import StrVector
        import rpy2.robjects as ro
        from rpy2.robjects.packages import importr
        import rpy2.robjects.packages as rpackages
        #from rpy2.robjects.lib.dplyr import dplyr
        from rpy2.robjects import pandas2ri
        from rpy2.robjects import default_converter
        from rpy2.robjects.conversion import localconverter
        base = importr('base')
        utils = importr('utils')
        try:
            woeBinning = importr('woeBinning')
        except:
             print("woeBinning package is being installed")
             utils.install_packages('woeBinning')
        try:
            discretization = importr('discretization')
        except:
            print("discretization pacakge is being installed")
            utils.install_packages('discretization')
            
        try:
            lazyeval = importr('lazyeval')
        except:
            print("lazyeval pacakge is being installed")
            utils.install_packages('lazyeval')
        
        
                   
        column_Name = []
        cl =[]
        if method == 'woe':
            print('woe')
            for arg in args:
                print(arg)
                if self.Train[arg].dtypes != 'object':
                    print(self.Train[arg].dtypes)
                    column_Name.append(arg)
                    cl.append(arg)
                    print(column_Name)
    
                else:
                    print("Column Name "+arg+" is Object so no binning is done for it")
            column_Name.append(target)
            df = self.Train[column_Name]
            try:
                with open('woe.r', 'r') as f:
                    string = f.read()
                woe = STAP(string, "woe")
            except FileNotFoundError:
                path = input("set directory to path: ")
                from os import chdir
                chdir(path)
                with open('woe.r', 'r') as f:
                    string = f.read()
                woe = STAP(string, "woe")
            pandas2ri.activate()
                
            self.woe_based_bin = pandas2ri.ri2py(woe.woe_based_binning(df,target,event,
                                                      cl))
            pandas2ri.deactivate()
            return self.woe_based_bin
        elif method=='Chisq':
            print('Chisq')
            cl =[]
            s =[]
            for arg in args:
                s.append(arg)
                
            #column_name =[]
            if s[0] == 'ALL':
                print("ALL")
                for j in self.Train.columns:
                    if self.Train[j].dtypes != 'object':
                        cl.append(j)
                        #column_name.append(arg)
                cl.append(target)
                df = self.Train[cl]
                try:
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe')
                        
                        
                except FileNotFoundError:
                    path = input("set directory to path: ")
                    from os import chdir
                    chdir(path)
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe')
                
            
                pandas2ri.activate()
                print('pandas2ri is activated')
                self.chisq_based_bin = pandas2ri.ri2py(woe.Chi_sq_based_bin(df))
                pandas2ri.deactivate()    
                return self.chisq_based_bin
            else:
                cl =[]
                for arg in args:
                    if self.Train[arg].dtypes != 'object':
                        cl.append(arg)
                    else:
                        print(arg + " is not continuous")
                if len(cl)==0:
                    message = ' Binning can not be done as all variable passed is not continuos'
                    return message
                cl.append(target)
                df = self.Train[cl].copy()
                try:
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe module loaded')
                        
                        
                except FileNotFoundError:
                    path = input("set directory to path: ")
                    from os import chdir
                    chdir(path)
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe module loaded')
                
            
                
                pandas2ri.activate()
                print('pandas2ri is activated')
                self.chisq_based_bin = pandas2ri.ri2py(woe.Chi_sq_based_bin(df))
                pandas2ri.deactivate()  
                print('pandas2ri is deactivated')
                return self.chisq_based_bin
        elif method == 'Entropy':
            print('Entropy')
            cl =[]
            s =[]
            for arg in args:
                s.append(arg)
            if s[0] == 'ALL':
                print("ALL")
                for j in self.Train.columns:
                    if self.Train[j].dtypes != 'object':
                        cl.append(j)
                        #column_name.append(arg)
                cl.append(target)
                df = self.Train[cl]
                try:
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe')
                        
                        
                except FileNotFoundError:
                    path = input("set directory to path: ")
                    from os import chdir
                    chdir(path)
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe')
                
            
                pandas2ri.activate()
                print('pandas2ri is activated')
                self.entropy_based_bin = pandas2ri.ri2py(woe.entropy_based_bin(df))
                pandas2ri.deactivate()    
                return self.entropy_based_bin
            else:
                cl =[]
                for arg in args:
                    if self.Train[arg].dtypes != 'object':
                        cl.append(arg)
                    else:
                        print(arg + " is not continuous")
                if len(cl)==0:
                    message = ' Binning can not be done as all variable passed is not continuos'
                    return message
                cl.append(target)
                df = self.Train[cl].copy()
                try:
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe module loaded')
                        
                        
                except FileNotFoundError:
                    path = input("set directory to path: ")
                    from os import chdir
                    chdir(path)
                    with open('woe.r', 'r') as f:
                        string = f.read()
                        woe = STAP(string, "woe")
                        print('woe module loaded')
                
            
                
                pandas2ri.activate()
                print('pandas2ri is activated')
                self.entropy_based_bin = pandas2ri.ri2py(woe.entropy_based_bin(df))
                pandas2ri.deactivate()  
                print('pandas2ri is deactivated')
                return self.entropy_based_bin
            def crt():
                pass
    
                        
                        
                
             
            
            
                   
            
        
            
            
            
        
            
            
        
        
               
                
                
                
                
      
            
        
        
        
            
            
                
                
        
            
            
            
            
        
 
       
        

#class Logistic:
#    """ Objective of this class is to build Logistic Regression based on automated process
#        process of selecting variable. For unbalanced distribution it will use precision
#        and recall as metrics"""
#        
#    def __init__(self,Train, Cv = 5):
#        self.Train = Train.copy()
#        self.Cv = Cv.copy()
#       
#       
#    def buildLogistic(self):
#        pass
#    
#    def metrics(self):
#        pass
#    
#
#class Xgbm:
#    def __init__(self,Train, Cv = 5):
#        self.Train = Train.copy()
#        self.Cv = Cv.copy()
#       
#       
#    def buildXGbm(self,tuneParameter=False):
#        pass
#    
#    def metrics(self):
#        pass
#    
#class SVM:
#    def __init__(self,Train, Cv = 5):
#        self.Train = Train.copy()
#        self.Cv = Cv.copy()
#       
#       
#    def buildSVM(self,tuneParameter=False):
#        pass
#    
#    def metrics(self):
#        pass
#    
#class RandomForest:
#    def __init__(self,Train, Cv = 5):
#        self.Train = Train.copy()
#        self.Cv = Cv.copy()
#       
#       
#    def buildRandomForest(self,tuneParameter=False):
#        pass
#    
#    def metrics(self):
#        pass
#
#class NN:
#    
#    def __init__(self,Train, Cv = 5):
#        self.Train = Train.copy()
#        self.Cv = Cv.copy()
#       
#       
#    def buildNN(self,tuneParameter=False):
#        pass
#    
#    def metrics(self):
#        pass
#    
