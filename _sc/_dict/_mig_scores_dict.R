# ______________________________________________
# Media reactions to RR violence
# Goal: Estimate migration content with dictionary
# Procedure: load data, assign scores, save sample
# ______________________________________________
# Date:  Fri Apr 30 17:04:58 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(pbapply)

# load data ####

## load dict
dict <- read.csv("_dicts/german_glove_alt.csv")$x

papers <- c("bild", "faz", "spon", "sz", "taz", "weltonline")

for (p in papers){
  
  cat(paste("\n\nSampling", p, "..."))
  
  ## load newspaper articles
  cat("\tLoading data...")
  raw <- fread(here(paste0("_dt/_out/_", p, "_articles.csv")))
  ncols_raw <- ncol(raw)
  
  # assign scores & sample ####
  
  ## count occurences/article length
  cat("\tAssigning dictionary scores...")
  for (d in dict){
    raw[,eval(d)] <- str_count(raw$text, d)
  }
  
  raw$sum <- rowSums(raw[,(ncols_raw+1):ncol(raw)])
  raw$ntokens <- str_length(raw$text)
  raw$mig_share <- raw$sum/raw$ntokens
  
  ## pull stratified sample
  cat("\tPulling stratified sample...")
  
  ### sample 200 cases from articles low, mid, and high on migration mentions
  low_sample <- 
  mid_sample <- 
  high_sample <- 
  
  
  ## add to overall sample
  
  ## delete full set
  
}

# save sample ####


