import pandas as pd
class deriving_feature:
    """this class is supposed to help end user derive feature on the go as well create 
    some feature automatically"""
    
    """Please use Python 3"""
    
    def __init__(self):
        pass
        
    def load_data(self,Path):
        """Function to load data"""
        f=True
        while(f):
            self.Path=Path
            try:
                self.df=pd.read_csv(self.Path).copy()
                f=False
                #return self.df
                for col in self.df.columns:
                    if self.df[col].isnull().sum()==len(self.df):
                        del self.df[col]
                return self.df
            except FileNotFoundError:
                f=True
                print("File not Found in the specified path. Give path again")
                Path=input("Enter the path again: ")
                
#    def treat_missing_values(self):
#        """Treating the missing values in the data"""
#        str1=input("Enter what to replace instead of mising values")
#        self.df.fillna(str1)
#        return self.df
        
           
    def change_to_char(self,clmn_nm_list):
        """To change datatype of given columns to object datatype"""
        f=True
        while(f):
            for i in clmn_nm_list:
                if i in self.df.columns:
                    f=False
                else:
                    f=True
                    p1=True
                    while(p1):
                        try:
                            b=input(i + " column is not found enter 1 if you want to continue else 0 ")
                            b=int(b)
                            if (b==1):
                                str1=input("Enter the column names.Please seperate the column names with a comma")
                                clmn_nm_list=str1.split(",")
                                break
                            else:
                                return self.df
                            p1=False
                        except:
                            p1=True
                            print("You have not entered a correct value. Please enter again")
                 
        if(f==False):
            for i in clmn_nm_list:
                p=str(self.df[i])
                self.df[i]=p
                del p
        
        return self.df
    
    def change_to_num(self,clmn_nm_list1):
        """To change datatype of given columns to float datatype"""
        f=True
        while(f):
            for i in clmn_nm_list1:
                if i in self.df.columns:
                    f=False
                else:
                    f=True
                    p1=True
                    while(p1):
                        try:
                            p1=False
                            b=input(i + " column is not found please enter 1 to continue else 0 to exit")
                            if(int(b)==1):
                                str1=input("Enter the column names.Please seperate the column names with a comma ")
                                clmn_nm_list1=str1.split(",")
                                break
                            else:
                                return self.df
                        except:
                            p1=True
                            print("You have entered wrong value enter again.")
         
        if(f==False):
           for i in clmn_nm_list1: 
                try:
                    self.df[i]=self.df[i].astype(float)
                except:
                    p1=True
                    while(p1):
                        try:
                            p1=False
                            b=input(i + "Column cannot be converted to float enter 1 to enter columns again else enter 0.")
                            b=int(b)
                            if(b==1):
                                str1=input("Enter the column names.Please seperate the column names with a comma ")
                                clmn_nm_list1=str1.split(",")
                                break
                            else:
                                return self.df
                        except:
                            p1=True
                            print("You have entered wrong value. Please enter again.")
                
        return self.df
    
    def change_to_date(self,clmn_nm_list1):
        """To change datatype of given columns to date datatype"""
        f=True
        while(f):
            for i in clmn_nm_list1:
                if i in self.df.columns:
                    f=False
                else:
                    f=True
                    p1=True
                    while(p1):
                        try:
                            p1=False
                            b=input(i + " column is not found please enter 1 to continue else 0 to exit")
                            if(int(b)==1):
                                str1=input("Enter the column names.Please seperate the column names with a comma ")
                                clmn_nm_list1=str1.split(",")
                                break
                            else:
                                return self.df
                        except:
                            p1=True
                            print("You have entered the wrong value. Please try again.")
                 
        if(f==False):
           for i in clmn_nm_list1: 
                try:
                    self.df[i]=pd.to_datetime(self.df[i])
                except:
                    p1=True
                    while(p1):
                        try:
                            p1=False
                            b=input(i + " column cannot be changed to date datatype.Please enter 1 to continue else 0 to exit")
                            if(int(b)==1):
                                str1=input("Enter the column names.Please seperate the column names with a comma ")
                                clmn_nm_list1=str1.split(",")
                                break
                            else:
                                return self.df
                        except:
                            p1=True
                            print("You have entered the wrong value.Please try again.")
        return self.df
            
            
    def create_derived_columns(self):
        """To create derived columns using arithmetic operations given by the user"""
        str1=" "
        f=1
        datecol=[x for x in self.df.colummns if self.df[x].dtypes=='datetime64[ns]'].copy()
        cont = [x for x in self.df.columns if self.df[x].dtypes != 'object' and x not in datecol].copy()
        cat=[x for x in self.df.columns if self.df[x].dtypes == 'object' and x not in datecol].copy()
    
#        cat = [x for x in self.df.columns  if x not in cont ]    
        while(f==1):
            str1=input("Enter + to sum columns - to subtract * to multiply and / to divide or enter 0 to exit")
            if str1==str(0):
                return self.df
            else:
                print("Columns:")
                print(cont)
                f1=True
                while(f1):
                    str2=input("Enter the column names.Please seperate the column names with a comma ")
                    li=str2.split(",")
                    for i in li:
                        if i in self.df.columns:
                            f1=False
                        else:
                            f1=True
                            print(i + " column is not found.Please enter again.")
                            break
                f1=True
                while(f1):
                        for i in li:
                            if i in cat:
                                p1=True
                                while(p1):
                                    try:
                                        p1=False
                                        b=input("It is a categorical variable.Enter 1 to change to continous variable")
                                        b=int(b)
                                        if(b==1):
                                            self.df[i].astype(float)
                                            b1=input("Enter 1 to continue to enter column names else enter 0")
                                            if(int(b1)==1):
                                                str2=input("Enter column names.Please sepearate using comma ")
                                                li=str2.split(",")
                                                f1=False
                                            else:
                                                return self.df
                                    except:
                                        p1=True
                                        print("You have entered wrong value. Please enter again.")
                                        
                            else:
                                p1=True
                                while(p1):
                                    try:
                                        p1=False
                                        b1=input("Enter 1 to continue to enter column names else enter 0")
                                        if(int(b1)==1):
                                            str2=input("Enter column names.Please sepearate using comma ")
                                            li=str2.split(",")
                                            f1=False
                                        else:
                                            return self.df  
                                    except:
                                        p1=True
                                        print("You have entered wrong value. Please enter again.")
                                                
                if(str1=='+'):
                    self.df[str2 + '_sum']=self.df[li].sum(axis=1)
                elif(str1=='-'):
                    self.df[str2 + '_subtract']=self.df[li].subtract(axis=1)
                elif(str1=='*'):
                    self.df[str2 + '_multiply']=self.df[li].prod(axis=1)
                else:
                    self.df[str2 + '_divide']=self.df[li[1]]/self.df[li[0]]
                p1=True
                while(p1):
                    try:
                        f=input("Enter 1 if you want to form another derived column ")
                        f=int(f)
                        p1=False
                    except:
                        p1=True
                        print("You have entered wrong value.Please enter again.")
                
            return self.df
        
        
    def create_features(self,groupby_col):
        """Rolling up of data by an user defined column"""
        datecol=[x for x in self.df.columns if self.df[x].dtypes=='datetime64[ns]'].copy()
        cont = [x for x in self.df.columns if self.df[x].dtypes != 'object' and x not in datecol].copy()
        cat=[x for x in self.df.columns if self.df[x].dtypes == 'object' and x not in datecol].copy()
#        cont = self.df._get_numeric_data().columns
#        cat = [x for x in self.df.columns if x not in cont ]
        f=True
        while(f):
            for i in groupby_col:
                if i in self.df.columns or i in cat:
                    f=False
                else:
                    f=True
                    print(i + " column is not found.")
                    g=0
                    g=input("Enter 0 if you want to exit the function else enter 1:")
                    if(int(g)==1):
                        str1=input("Enter the column names.Please seperate the column names with a comma ")
                        groupby_col=str1.split(",")
                        break
                    else:
                        return self.df
#                    break
                
        f=True
        while(f):
            for i in groupby_col:
                if i in cont:
                    f=True
                    print(i + "column is considered as continous variable.")
                    p1=True
                    while(p1):
                        try:
                            p1=False
                            f1=input("Enter 1 to convert to categorical else enter 0")
                            f1=int(f1)
                        except:
                            p1=True
                            print("You have entered wrong value. Please try again.")
                    if(f1==1):
                        try:
                            p=str(self.df[i])
                            self.df[i]=p
                            del p
                            cat=[x for x in self.df.columns if self.df[x].dtypes == 'object'].copy()
                            b=input("The column is converted to categorical.Please enter 1 to enter the column names else 0 to exit from the function")
                            if(int(b)==1):
                                str1=input(" Please enter the column names to group by.Please seperate the column names with a comma ")
                                groupby_col=str1.split(",")
                            else:
                                return self.df
                        except:
                            print("This column cannot be converted to categorical variable")
                            f=True
                        break
                    elif f1==0:
                        f=False
                else:
                    f=False
        if(f==False):
            print ('group by 1 ' , cont)
            self.gmean = self.df.groupby(by=groupby_col)[cont].mean().copy()
            self.gmean.reset_index(inplace=True)
            cl = groupby_col.copy()
            
            for x in cont:
                cl.append(x+'_Mean')   
            self.gmean.columns = cl.copy()  
            
            self.gmedian = self.df.groupby(by=groupby_col)[cont].median().copy()
            self.gmedian.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Median')   
            self.gmedian.columns = cl.copy()
            
            self.gsum = self.df.groupby(by=groupby_col)[cont].sum().copy()
            self.gsum.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Sum')   
            self.gsum.columns = cl.copy()
    
            self.gkurtosis = self.df.groupby(by=groupby_col)[cont].apply(pd.DataFrame.kurt).copy()
            self.gkurtosis.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Kurtosis')   
            self.gkurtosis.columns = cl.copy() 
            
            self.gskew = self.df.groupby(by=groupby_col)[cont].apply(pd.DataFrame.skew).copy()
            self.gskew.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Skew')   
            self.gskew.columns = cl.copy()
            
            self.gmin = self.df.groupby(by=groupby_col)[cont].min().copy()
            self.gmin.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Min')   
            self.gmin.columns = cl.copy()
            
            
            self.gquantile = self.df.groupby(by=groupby_col)[cont].quantile(.25).copy()
            self.gquantile.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Quantile(.25)')   
            self.gquantile.columns = cl.copy()
            
            self.gquantile1 = self.df.groupby(by=groupby_col)[cont].quantile(.75).copy()
            self.gquantile1.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Quantile(.75)')   
            self.gquantile1.columns = cl.copy()
            
            self.gmax = self.df.groupby(by=groupby_col)[cont].quantile(1).copy()
            self.gmax.reset_index(inplace=True)
            
            cl = groupby_col.copy()
            for x in cont:
                cl.append(x+'_Max')   
            self.gmax.columns = cl.copy()
            
            self.rolled_df=pd.concat([self.gmean,self.gmedian.iloc[:,2:],self.gsum.iloc[:,2:],self.gkurtosis.iloc[:,2:],self.gskew.iloc[:,2:],self.gquantile.iloc[:,2:],self.gquantile1.iloc[:,2:],self.gmax.iloc[:,2:],self.gmin.iloc[:,2:]],axis=1).copy()
            
            del self.gmean
            del self.gmedian
            del self.gsum
            del self.gkurtosis
            del self.gskew
            del self.gquantile
            del self.gquantile1
            del self.gmax
            del self.gmin
            
        return(self.rolled_df)


def create_dataframe_with_features():   
    """Wrapper function"""

    d=deriving_feature()
    Path=input("Enter path of file:")
    
    rolled_df=d.load_data(Path)
    print(rolled_df.info())
    
    """To handle if columns has commas"""
    rolled_df.columns = rolled_df.columns.str.replace("[,]", "_")

    datecol=[x for x in rolled_df.columns if rolled_df[x].dtypes=='datetime64[ns]'].copy()
    cont = [x for x in rolled_df.columns if rolled_df[x].dtypes != 'object' and x not in datecol].copy()
    cat=[x for x in rolled_df.columns if rolled_df[x].dtypes == 'object' and x not in datecol].copy()
    
    print("Continous variables are")
    print(cont)
    print("Categorical variables are")
    print(cat)
    
    p=True
    while(p):
        f=input("Enter 1 if you want to change columns into date datatype ")
        try:
            f=int(f)
            if(f==1):
                str1=input("Enter the columns you want to change to date datatype.Please seperate it with a comma ")
                list1=str1.split(",")
                rolled_df=d.change_to_date(list1)
            p=False
        except:
            print("You have not entered correct value please try again")
            p=True
    
#    f=0
#    f=input("Enter 1 if you want to treat missing values else enter 0 ")
#    if(int(f)==1):
#        rolled_df=d.treat_missing_values()
    f=0
    p=True
    while(p):
        f=input("Enter 1 if you want to change any column to object datatype else enter 0 ")
        try:
            f=int(f)
            if(f==1):
                str1=input("Enter the column names you want to change.Please seperate the column names with a comma ")
                list1=str1.split(",")
                rolled_df=d.change_to_char(list1)
            p=False
        except:
            print("You have not entered correct value please try again")
            p=True
        
#    cont = [x for x in rolled_df.columns if rolled_df[x].dtypes != 'object']
##    cont = rolled_df._get_numeric_data().columns
#    cat=[x for x in rolled_df.columns if rolled_df[x].dtypes == 'object']
#        
#    print("Continous variables are")
#    print(cont)
#    print("Categorical variables are")
#    print(cat)
    
    p=True
    while(p):
        f=input("Enter 1 if you want to change any column to float datatype else enter 0 ")
        try:
            f=int(f)
            p=False
            if(int(f)==1):
                str1=input("Enter the column names you want to change.Please seperate the column names with a comma ")
                list1=str1.split(",")
                rolled_df=d.change_to_num(list1)
        except:
            print("You have not entered correct value please try again")
            p=True
            
    datecol=[x for x in rolled_df.columns if rolled_df[x].dtypes=='datetime64[ns]'].copy()
    cont = [x for x in rolled_df.columns if rolled_df[x].dtypes != 'object' and x not in datecol].copy()
    cat=[x for x in rolled_df.columns if rolled_df[x].dtypes == 'object' and x not in datecol].copy()
    print("Date columns:")
    print(datecol)
    print("Continous variables:")
    print(cont)
    print("Categorical variable:")
    print(cat)
    
    p=True
    while(p):
        f=input("Enter 1 if you want to create any derived columns else enter 0 ")
        try:
            f=int(f)
            p=False
            if(f==1):
                rolled_df=d.create_derived_columns()
        except:
            print("You have not entered correct value please try again")
            p=True
    
    p=True
    while(p):
        f=input("Enter 1 if you want to group data by any column and roll up data else enter 0 ")
        try:
            f=int(f)
            p=False
            if(int(f)==1):
                print ("Columns:")
                print(cat)
                str1=" "
                str1=input("Enter the column names you want to group by. Please seperate the column names with a comma")
                list2=str1.split(",")
                rolled_df=d.create_features(list2)
        except:
            print("You have not entered correct value please try again")
            p=True
            
                
    del f
#    del str1
#    del list2
    del cont
    del cat
    del Path
    del datecol
    
    return (rolled_df)