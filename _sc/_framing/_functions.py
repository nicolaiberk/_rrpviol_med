# -*- coding: utf-8 -*-
"""
Created on Tue Jun 22 11:22
@author: Nicolai Berk
@title: Cleaner
"""

# Import
import nltk
from nltk.tokenize import RegexpTokenizer
from nltk.stem.snowball import SnowballStemmer
stemmer = SnowballStemmer('german')
# from sklearn.feature_extraction.text import CountVectorizer
# from sklearn.feature_extraction.text import TfidfVectorizer

# Clean Text
def TextCleaner(docs):
    
    # remove punctuation and tokenize
    DocTokenizer = RegexpTokenizer(r'\w+')
    Tokenizer = lambda x : DocTokenizer.tokenize(x)
    tokens = docs.apply(Tokenizer)
    
    # stem
    def WordStemmer(toks):
      stems = ''
      for word in toks:
        stems = stems + stemmer.stem(word) + ' '

      return stems


    stems = tokens.apply(WordStemmer)    
    
    tokens = [' '.join(t) for t in tokens] # joins into single string
    
    return tokens, stems
