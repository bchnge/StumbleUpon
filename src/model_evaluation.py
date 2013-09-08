import sklearn.cross_validation as cv
import sklearn.metrics as metrics
import pandas as pd
import numpy as np
from scipy import stats


def ensembleEvaluate(nFolds, x, y, classifiers):
    ''' utility for evaluating the classification performance (i.e. ROC_AUC) of multiple classifiers through ensembling (i.e. mode votes)
        '''
    
    #Determine cross validation samples
    kf = cv.StratifiedKFold(y, n_folds=nFolds)
    
    #Create train and cv sets
    roc = []
    for train_index, test_index in kf:
        x_train, x_test = x.values[train_index], x.values[test_index]
        y_train, y_test = y.values[train_index], y.values[test_index]
        
        #Predict using each model
        y_hat = getModeVote(classifiers, x_train, y_train, x_test)
        
        # Calculate metric for the given sample
        fpr, tpr, thresholds = metrics.roc_curve(y_test, y_hat)
        roc_auc = metrics.auc(fpr, tpr)
        roc.append(roc_auc)
    return roc

def plotROC(y_test, y_hat):
    fpr, tpr, thresholds = metrics.roc_curve(y_test,y_hat)
    roc_auc = metrics.auc(fpr, tpr)
    plot(fpr, tpr, label = 'ROC Curve (area = %0.2f)' % roc_auc)
    print roc_auc

def getModeVote(classifiers, x_train, y_train, x_test):
    """ calculate the mode vote among a list of classifiers
        """
    y_hat = pd.DataFrame(index = np.arange(0,len(x_test)))
    
    for idx, clf in enumerate(classifiers):
        clf.fit(x_train, y_train)
        yh = clf.predict(x_test)
        y_hat['class_' + str(idx)] = pd.Series(yh)
    
    mode_votes = [i[0] for i in stats.mode(y_hat, axis = 1)[0]]

    return mode_votes
