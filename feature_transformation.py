import pandas as pd
import numpy as np
from scipy import stats
from sklearn.kernel_approximation import RBFSampler,Nystroem
from sklearn.preprocessing import PolynomialFeatures
from sklearn.preprocessing import OneHotEncoder
from sklearn.linear_model import SGDClassifier
from sklearn.metrics import f1_score

class feature_transformation:
    def __init__(self,rolled_df,target):
        self.rolled_df=rolled_df
        self.target=target
    
    def poly_features(self):
        datecol=[x for x in self.rolled_df.columns if self.rolled_df[x].dtypes=='datetime64[ns]'].copy()
        cont = [x for x in self.rolled_df.columns if self.rolled_df[x].dtypes != 'object' and x not in datecol].copy()
        X=self.rolled_df[cont].copy()
        poly = PolynomialFeatures(interaction_only=True,include_bias=False)
        f=poly.fit(X)
        l=f.get_feature_names()
        p= poly.transform(X)
        l=[]
        l=poly.get_feature_names()
        poly_df=pd.DataFrame(data = p, columns=l)
        return poly_df
    
    def label_encoding(self,final_df):
        datecol=[x for x in final_df.columns if final_df[x].dtypes=='datetime64[ns]'].copy()
        cat=[x for x in final_df.columns if final_df[x].dtypes == 'object' and x not in datecol].copy()
        for i in cat:
            final_df[i]=final_df[i].astype('category')
            final_df[i]=final_df[i].cat.codes
        return(final_df)
        
    def one_hot_encoding(self,label_encod,cat1):    
        enc = OneHotEncoder(categorical_features='all',handle_unknown='error', n_values='auto', sparse=True)
        cat=[x for x in cat1 if x!=self.target]
        enc.fit(label_encod[cat])  
        n=enc.transform(label_encod[cat]).toarray()
        cols=[]
        for i in cat:
            unique_values=label_encod[i].unique()
            for j in unique_values:
                cols.append(i + '_'+ str(j))
        one_hot_df=pd.DataFrame(data = n, columns=cols)
        return one_hot_df
         
    def kernel_transformation_using_nystroem_rbf(self,df,cat):
        df=df.fillna(0)
        df=df.replace([np.inf, -np.inf], 0)
        datecol=[x for x in df.columns if df[x].dtypes=='datetime64[ns]']
        X1=[x for x in df.columns if df[x].dtypes != 'object' and x not in datecol and x not in self.target]     
        X=[x for x in X1 if x not in cat]
        y=self.target
        j = np.linspace((10**-2),(10**2),50)
        g=0
        max1=0
        df=df.fillna(0)
        df=df.replace(np.inf, 0)
        df=df.replace(-np.inf, 0)
        for i in j:
            rbf_feature = Nystroem(kernel = 'rbf', gamma=i, random_state=2,n_components=10)
            rbf_feature.fit(df[X])
            X_features = rbf_feature.transform(df[X])
            X_features=np.nan_to_num(X_features)
#            SGDClassifier(loss='hinge', penalty='l2', alpha=0.0001, l1_ratio=0.15, fit_intercept=True, shuffle=True, verbose=0, epsilon=0.1, n_jobs=1, random_state=None, learning_rate='optimal', eta0=0.0, power_t=0.5, class_weight=None, warm_start=False, average=False, n_iter=None)
            clf = SGDClassifier()   
            clf.fit(X_features, df[y])
            y_pred = clf.predict(X_features)
            score=f1_score(df[y], y_pred, average='micro') 
            if(score>max1):
                max1=score
                g=i
        rbf_feature = RBFSampler(gamma=g, random_state=2,n_components=10)
        rbf_feature.fit(df[X])
        X_features = rbf_feature.transform(df[X])
        l=[]
        for r in range(10):
            l.append('k_'+str(r))
        X_features=pd.DataFrame(data=X_features,columns=l)
#        SGDClassifier(loss='hinge', penalty='l2', alpha=0.0001, l1_ratio=0.15, fit_intercept=True,shuffle=True, verbose=0, epsilon=0.1, n_jobs=1, random_state=None, learning_rate='optimal', eta0=0.0, power_t=0.5, class_weight=None, warm_start=False, average=False, n_iter=None)
        clf = SGDClassifier()   
        clf.fit(X_features, df[y])
        score=f1_score(df[y], y_pred, average='micro') 
        print("Score is")
        print(score)
        print(g)
        return X_features
            
#        clf.predict([[2., 2.]])
        
    def transformation(self):
        """This function transforms the continous variables"""
        datecol=[x for x in self.rolled_df.columns if self.rolled_df[x].dtypes=='datetime64[ns]'].copy()
        cont = [x for x in self.rolled_df.columns if self.rolled_df[x].dtypes != 'object' and x not in datecol].copy()
        for i in cont:
            self.rolled_df[i +'_square']=self.rolled_df[i]**2
            self.rolled_df[i +'_square root']=self.rolled_df[i].apply(np.sqrt)
            self.rolled_df[i + '_inverse']=1/self.rolled_df[i]
            self.rolled_df[i + '_log']=np.log10(self.rolled_df[i])
            try:
                self.rolled_df[i + '_box_cox']=stats.boxcox(self.rolled_df[i])[0]
                self.rolled_df[i + '_box_cox']=self.rolled_df[i + '_box_cox'].astype(float)
            except:
                self.rolled_df[i + '_box_cox']='NEGATIVE VALUE'
        
        return self.rolled_df