import joblib
import pickle
import os
import pandas as pd
import csv

os.chdir('..')


clf = joblib.load('classifier.pkl')
vectorizer = pickle.load(open('vectorizer.pkl', mode='rb'))
textrow = 5
fieldnames = ['date', 'paper', 'title', 'link', 'mig_pred', 'mig_proba']

if os.path.isfile('_data/_estimations.csv'):
    i = pd.read_csv('_data/_estimations.csv').shape[0] # set starting point
else:
    i = 0


with open('_data/_merged_articles.csv', mode="r", encoding="utf-8") as fi:
    reader = csv.reader(fi)
    next(reader) # skip header
    
    for skip in range(i+1):
        next(reader)
    
    
    with open('_data/_estimations.csv', mode="a", encoding="utf-8") as fo:
        
        writer = csv.DictWriter(fo, lineterminator = '\n', fieldnames = fieldnames)
        writer.writeheader()
        
        for row in reader:
            i += 1
            mtrx = vectorizer.transform(pd.array([row[textrow]]))
            pred = clf.predict(mtrx)[0]
            proba = clf.predict_proba(mtrx)[0][1]
            writer.writerow({'date':         row[0], 
                             'paper':        row[6], 
                             'title':        row[1], 
                             'link':         row[2],
                             'mig_pred':     pred,
                             'mig_proba':    proba})
            
            print(f'Classified {i} speeches (of ~3.5M). Current Journal: {row[6]}', end="\r")

