print("Setup....")

import joblib
import pickle
import os
import pandas as pd
import numpy as np
from nltk.tokenize import RegexpTokenizer
from tqdm import tqdm


## set wd
os.chdir("/home/nico/Projects/_rrpviol_med/")

print("Done.")

## load classifier and vectorizer
print("Loading classifiers and vectorizers....")
clf_resampled = joblib.load('_dt/bitrigram_classifier_resampled.pkl')
clf_1819 = joblib.load('_dt/bitrigram_classifier_1819_fraktionen.pkl')
vectorizer = pickle.load(open('_dt/bitrigram_new_vectorizer.pkl', mode='rb'))
print("Done.")

## load data
print("Loading raw data...")
mig_articles = pd.read_csv('_dt/_mig_estimates/_migration_articles_BERT.csv')
print("Done.")

## preprocess articles
print("Preprocesing articles....")
DocTokenizer = RegexpTokenizer(r'\w+')
Tokenizer = lambda x : DocTokenizer.tokenize(x)
mig_articles['tokens'] = [' '.join(t) for t in mig_articles.text.apply(Tokenizer)]
del mig_articles['text']
print("Done.")

## generate feature matrix
print("Generating feature matrix....")
mtrx = vectorizer.transform(mig_articles.tokens)
del mig_articles['tokens']
print("Done.")

## generate estimates
print("Generating slant estimates...")
mig_articles['pred_resampled'] = clf_resampled.predict(mtrx)
mig_articles['proba_resampled'] = [est[1] for est in clf_resampled.predict_proba(mtrx)]

mig_articles['pred_1819'] = clf_1819.predict(mtrx)
mig_articles['proba_1819'] = [est[1] for est in clf_1819.predict_proba(mtrx)]
print("Done.")

## save
print("Saving....")
mig_articles.to_csv('_dt/_migration_slant_BERT.csv')
print("Done. Finished estimation of AfD slant.")