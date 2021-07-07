# write migration articles to new file

## import
import os
import csv
import numpy as np
import re
import sys

csv.field_size_limit(sys.maxsize)

## set wd
os.chdir("/home/nico/Documents/Projects/_rrpviol_med/")

## get urls for all migration-related articles
os.chdir("_dt/_mig_estimates")

mig_urls = []
for estimate in os.listdir():
    with open(estimate, mode="r", encoding="utf-8") as fi:
        print(f"Reading {estimate}...")
        reader = csv.reader(fi)
        for row in reader:
            
            # define relative position in row based on title
            linkrow  = np.argmax([r == 'link'     for r in row])
            predrow  = np.argmax([r == 'mig_pred' for r in row])
            paperrow = np.argmax([r == 'paper'    for r in row])
            titlerow = np.argmax([r == 'title'    for r in row])
            daterow  = np.argmax([r == 'date'     for r in row])
            textrow  = np.argmax([r == 'text'     for r in row])
            
            break
        
        for row in reader:
            
            # append url if article is about migration
            
            if row[predrow] == "True":
                mig_urls.append(row[linkrow])
                
print(f"Found {len(mig_urls)} articles about migration, {len(np.unique(mig_urls))} unique.")

mig_urls = np.unique(mig_urls)

## write all mig articles to new file
os.chdir("/home/nico/Documents/Projects/_rrpviol_med/_dt/_out")

filename = "_migration_articles.csv"

m = 0

for rawfile in os.listdir("Archive"):
    with open("Archive/"+rawfile, mode="r", encoding="utf-8") as fi:
        print(f"Reading {rawfile}...")
        reader = csv.reader(fi)
        
        paper = re.match("_([a-z]*)_articles.csv", rawfile).group(1)
        i = 0
        
        for row in reader:
            # define relative position in row based on title
            linkrow = np.argmax([r == 'url'   for r in row])
            titlerow = np.argmax([r == 'title'    for r in row])
            daterow  = np.argmax([r == 'date'     for r in row])
            textrow  = np.argmax([r == 'text'     for r in row])
            
            break
            
        with open(filename, mode="a", encoding="utf-8") as fo:
            writer = csv.writer(fo)
            writer.writerow(["date", "paper", "title", "url", "text"])
            
            for row in reader:
                i += 1
                print(f"Scanning row #{i} ({paper}), so far {m} of {len(mig_urls)} about migration.", end = "\r")
                if row[linkrow] in mig_urls:
                    m += 1
                    writer.writerow([row[daterow],
                                    paper,
                                    row[titlerow],
                                    row[linkrow],
                                    row[textrow]]
                    )
        print("\nDone.\n")
                    
print(f"Finished writing {filename}")
            
            

