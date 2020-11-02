#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  5 11:23:14 2020

@author: gohawks
"""


import numpy as np
import pandas as pd
import statistics as st
import matplotlib.pyplot as plt
import statsmodels.api as sm
import math as m
import ast 
from sklearn.preprocessing import LabelEncoder, OneHotEncoder, Imputer, PolynomialFeatures
from sklearn.model_selection import KFold, train_test_split
from sklearn.linear_model import LinearRegression

from scipy import stats as ss

#first, import the csv doc with/ all the parms


parms = pd.read_csv('~/Desktop/diagRegPandas/parms_50_startups.txt', delimiter=';')
 #0 is row
yName = parms.iloc[1,0]
outLog = open(parms.iloc[7,0],"w+") 


# Importing the dataset
df = pd.read_csv(parms.iloc[0,0]) #must be in /Desktop already?
nRow = df.shape[0]
nCol = df.shape[1] 


#check for null/na values & fill in with mean
if df.isnull().values.any() == True:
    imputer = Imputer(missing_values=np.nan, strategy = 'mean', axis = 1)
    df = imputer.fit_transform(df)


yColNum = df.columns.get_loc(yName)
xData = df.drop(yName,axis=1)
yData = df.iloc[:, yColNum]

#drops any predictors you automatically don't want to consider
if parms.iloc[2,0] == '0':
    if "," in parms.iloc[3,0]:
        xList = parms.iloc[3,0].split(",")
    else:
        xList = str.split(parms.iloc[3,0])
    if len(xList) == 1:
        xData = xData.drop(xList[0],axis=1)
    else:
        for pred in xList:
            xData = xData.drop(pred,axis=1)
        
#get list of column names
masterCols = list(xData.columns)

# Encoding categorical data
if parms.iloc[4,0] == '1':
    catDict={} #is this parms dict unnecessary?
    if "," in parms.iloc[5,0]:
        catVars = parms.iloc[5,0].split(',')
    else:
        catVars = str.split(parms.iloc[5,0])
    numCatspVar = list(parms.iloc[6,0]) #num categories per variable
    for ele in range(len(catVars)):
        catDict[catVars[ele]]=int(numCatspVar[ele])
  
    for var in catDict:
        varColNum = xData.columns.get_loc(var)
        le = LabelEncoder()
        xData.iloc[:, varColNum] = le.fit_transform(xData.iloc[:, varColNum])
        onehotencoder = OneHotEncoder(categorical_features = [varColNum])
        xData = onehotencoder.fit_transform(xData).toarray()
        featureCols = list(onehotencoder.get_feature_names([var]))
        masterCols = featureCols + masterCols
        masterCols.remove(var)
        xData = pd.DataFrame(xData, columns=masterCols)
    
    
xDataVals = xData.values  
yDataVals = df.iloc[:, yColNum].values #4 is the column number of your Y

#
#TAKE CARE OF DUMMY VARIABLE TRAP?
#WHEN DO I NEED TO DO THIS??
#

# Feature Scaling
#from sklearn.preprocessing import StandardScaler
#sc_X = StandardScaler()
#X_train = sc_X.fit_transform(X_train)
#X_test = sc_X.transform(X_test)
#sc_y = StandardScaler()
#y_train = sc_y.fit_transform(y_train)

#when ready for machine learning:
#if nRow >= 20:
#    X_train, X_test, y_train, y_test = cross_validation.train_test_split(xDataVals, yDataVals, test_size = 0.2, random_state = 0)


#CREATE MODEL
#this model is better for choosing the "best" model for prediction/machine learning
regressor = LinearRegression()
model = regressor.fit(xDataVals, yDataVals)
y_pred1 = model.predict(xDataVals)
#this model is better for indicating the size of the effect of different vars
#will include this because the summary table is awesome
x0 = sm.add_constant(xDataVals)
modelSM = sm.OLS(yDataVals, xDataVals)
results = modelSM.fit()
#y_pred = results.fittedvalues


#PRINT MODEL COEFFICIENTS, ADJ R^2, ETC

outLog.write('coefficients: '+ str(model.coef_)+"\n")
outLog.write('model intercept : '+str(model.intercept_)+"\n")
outLog.write('R^2: '+str(model.score(xDataVals,yDataVals))+"\n")
#OR, RESPECTIVELY
outLog.write('Or, if using sm.OLS model:'+"\n")
outLog.write(str(results.summary()))

#
#
##PRINT ANOVA INFO
#
y_pred = model.predict(xDataVals)
#y_pred = results.fittedvalues
ei = yDataVals - y_pred
nCoeff = nCol
nPred = nCol-1
MSE = sum(ei**2)/(nRow-2)
SST = sum((yDataVals - st.mean(yDataVals))**2)
SSE = MSE * (nRow-nCoeff)
SSR = SST-SSE
MSR = SSE/(nPred)
fStat = SSR/MSE
#p_value = ss.f.cdf(fStat, nPred, (nRow-nCoeff))
#
anovaDict = {"MSE":MSE,"SST":SST,"SSE":SSE,"DoF(SSE):":nRow-nCoeff,"SSR":SSR,"DoF (SSR):":nCoeff-1,"MSR":MSR,"F Stat":fStat}
outLog.write("ANOVA Information: \n")
outLog.write(str(anovaDict) + "\n")



#DIAGNOSTIC PLOTS
#plt.xlim(-5,5)
ss.probplot(ei,plot=plt)
plt.savefig('qqPlot.png')

ctr = 0
eStar = ei/m.sqrt(MSE)
    
for pred in masterCols:
    
    fig, axs = plt.subplots(nrows=3,ncols=1,figsize=(8,15))
    #scatter plot
    axs[0].scatter(xData.iloc[:,ctr], yData) 
    axs[0].set_title('Scatter Plot of X and Y',fontsize=9)
    axs[0].set_ylabel(yName,fontsize=9)
    axs[0].set_xlabel(pred,fontsize=9)
    
    #residual plot
    axs[1].scatter(xData.iloc[:,ctr], ei) 
    axs[1].set_title('Residual Plot',fontsize=9)
    axs[1].set_ylabel("Residuals",fontsize=9)
    axs[1].set_xlabel(pred,fontsize=9)
    
    #outlier plot
    axs[2].scatter(xData.iloc[:,ctr], eStar)
    axs[2].axhline(y=4)
    axs[2].axhline(y=0)
    axs[2].axhline(y=-4)
    axs[2].set_title('Residual Outlier Plot',fontsize=9)
    axs[2].set_ylabel('Semi-Studentized Residuals',fontsize=9)
    axs[2].set_xlabel(pred,fontsize=9)
    
    
    plt.savefig('diagPlots'+pred+'.png')
    ctr = ctr + 1

##MAKING PREDICTIONS like in R SLR code
##y_new = model.predict(x_new) #given x_new from input parms
##print('predicted response:', results.predict(x_new), sep='\n')
##here, x_new can be array, not just vector, for MLR
#
##CORRELATION MATRIX
outLog.write("Correlation Matrix: \n")
outLog.write(str(xData.corr())+ '\n')

outLog.close()

#poly
#transformer = PolynomialFeatures(degree=2, include_bias=False)
#x_ = transformer.transform(x)
#model = LinearRegression().fit(x_, y)
#y_pred = model.predict(x_)
#
#plt.title('Truth or Bluff (Polynomial Regression)')
#plt.xlabel('Position level')
#plt.ylabel('Salary')
#plt.savefig('detailed_poly.png')
#plt.show()


#backwards/forwards elimination




#data mining stuff
#sklearn.linear_model.Ridge
#sklearn.linear_model.Lasso
#sklearn.linear_model.ElasticNet





