# -*- coding: utf-8 -*-
"""
Created on Wed Oct 19 14:55:13 2016

@author: zhu
"""

from sklearn import datasets
from sklearn import svm
from sklearn import svm
from sklearn import datasets

iris = datasets.load_iris()
digits = datasets.load_digits()

print(digits.data)
digits.target

digits.images[0]
clf = svm.SVC(gamma=0.001, C=100.)
 
clf.fit(digits.data[:-1], digits.target[:-1]) 
clf.predict(digits.data[-1:])
#Model persistence
clf = svm.SVC()
iris = datasets.load_iris()
X, y = iris.data, iris.target
clf.fit(X, y)  
import pickle
s = pickle.dumps(clf)
clf2 = pickle.loads(s)
clf2.predict(X[0:1])
y[0]

#Datasets
data = iris.data
data.shape

#regression
#http://lib.csdn.net/article/machinelearning/35051