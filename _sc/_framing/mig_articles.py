# write migration articles to new file

## import
import os
import csv
import numpy as np
import re
import sys
from tqdm import tqdm

csv.field_size_limit(sys.maxsize)

## set wd
os.chdir("/home/nico/Projects/_rrpviol_med/")

## get urls for all migration-related articles
os.chdir("_dt")

mig_urls = []
with open('BERT_estimates_cleaned.csv', mode="r", encoding="utf-8") as fi:
    print(f"\nReading estimation file...")
    reader = csv.reader(fi)
    next(reader)
    next(reader)
    i = 0
    for row in reader:
        
        i += 1
        
        # append url if article is about migration
        if str(row[6]) == 'True':
            mig_urls.append(row[2])
                
print(f"Found {len(mig_urls)} articles about migration (of {i}), {len(np.unique(mig_urls))} unique.\n")

mig_urls = np.unique(mig_urls)

## write all mig articles to new file
os.chdir("/home/nico/Projects/_rrpviol_med/_dt")

filename = "_migration_articles_BERT.csv"

m = 0

with open(filename, mode="a", encoding="utf-8") as fo:
    writer = csv.writer(fo)
    writer.writerow(["date", "paper", "title", "url", "text"])
    i = 0
    for rawfile in os.listdir("Archive"):
        with open("Archive/"+rawfile, mode="r", encoding="utf-8") as fi:
            print(f"\nReading {rawfile}...")
            reader = csv.reader(fi)
            paper = re.match("_([a-z]*).*.csv", rawfile).group(1)
            
            for row in reader:
                # define relative position in row based on title
                linkrow  = np.argmax([r == 'url'   for r in row])
                titlerow = np.argmax([r == 'title'    for r in row])
                daterow  = np.argmax([r == 'date'     for r in row])
                textrow  = np.argmax([r == 'text'     for r in row])
                break
                
            for row in reader:
                i += 1
                if row[linkrow] in mig_urls:
                    m += 1
                    writer.writerow([row[daterow], paper, row[titlerow], row[linkrow], row[textrow]])
                    
            print(f"Done with {rawfile}: \n\t- #{i} rows scanned\n\t- so far #{m} migration articles written to '{filename}'\n")
		            
print(f"Finished writing {filename}")
		    

