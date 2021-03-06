# feature extraction for migration framing

## import 
import os
import pandas as pd
import numpy as np
from _functions import TextCleaner
from sklearn.feature_extraction.text import CountVectorizer
from nltk.corpus import stopwords
sws_german = stopwords.words("german")
import pickle


## set wd
os.chdir("/home/nico/Projects/_rrpviol_med/")

## load data
print('Loading data...')
dta = pd.read_csv('_dt/_mig_estimates/_migration_articles_BERT.csv', header=0)
print('\tDone!')

## preprocess
print('Preprocessing articles...')
tokens, stems = TextCleaner(dta.text)
del dta
del stems
print('\tDone!')



## generate feature matrix of common bi- and trigrams
print('Generating dtm...')
vec = CountVectorizer(max_df=.5, 
                          min_df=100, 
                          stop_words = sws_german,
                          ngram_range = (2,3)
                         )
btmtrx = pd.DataFrame(vec.fit_transform(tokens).toarray()) # returns non-sparse (dense) matrix
btmtrx.columns = vec.get_feature_names()
print('\tDone!')

## merge
print('Saving...')

## Write bitrigram matrix to drive
btmtrx.to_csv("_dt/_migration_btgrams_BERT.csv")

del btmtrx

## save vectorizer
pickle.dump(vec, open("_dt/bitrigram_new_vectorizer.pkl",mode='wb'))
print('\tDone!')