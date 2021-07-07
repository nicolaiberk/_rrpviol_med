import joblib
import pickle
import os
import pandas as pd
import numpy as np
from nltk.tokenize import RegexpTokenizer


## set wd
os.chdir("/home/nico/Documents/Projects/_rrpviol_med/")

## load classifier and vectorizer
clf = joblib.load('_dt/bitrigram_classifier.pkl')
vectorizer = pickle.load(open('_dt/bitrigram_vectorizer.pkl', mode='rb'))

## load data
mig_articles = pd.read_csv('_dt/_out/_migration_btgrams.csv')

## preprocess articles
DocTokenizer = RegexpTokenizer(r'\w+')
Tokenizer = lambda x : DocTokenizer.tokenize(x)
mig_articles['tokens'] = [' '.join(t) for t in mig_articles.text.apply(Tokenizer)]


## generate feature matrix
mtrx = vectorizer.transform(mig_articles.tokens)

## fit model
mig_articles['pred'] = clf.predict(mtrx)
mig_articles['proba'] = [est[1] for est in clf.predict_proba(mtrx)]

## save
mig_articles.to_csv('_dt/_out/_migration_slant.csv')
