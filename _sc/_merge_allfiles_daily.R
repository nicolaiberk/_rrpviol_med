# ______________________________________________
# Media reactions to RR violence
# Goal: Merge different datasets
# ______________________________________________
# Date:  Mon Nov 08 11:06:30 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


cat( '\14' )
rm( list = ls( ))

# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(vars)
library(tseries)
library( ggplot2 )
library(stringr)

tryCatch({
  setwd( '/Users/krausewz/Dropbox (Maestral)/_git.pprs/_rrpviol_med/' )
  }, error = function(e){
    print('Failed setting dir, trying different one...') 
  })

tryCatch({
  setwd( 'C:/Users/nicol/Dropbox/PhD/Projects/_rrpviol_med' )}, 
  error = function(e){
    print('Failed setting dir, sticking to initial...')
  })

# load data ####
load( '_dt/_mig_estimates/mig_articles_topics2022-01-31.Rdata')
load( '_dt/_out/_daily.Rdata')
afd_slant <- read.csv("_dt/_mig_estimates/_migration_slant_BERT.csv")

# aggregate newspaper estimates for each day and merge
fulltable <- 
  output %>% 
  group_by(paper, date_new) %>% 
  summarise(
    crime_tot = sum(crime),
    medit_tot = sum(medit),
    deport_tot = sum(deport),
    refnums_tot = sum(refnums),
    camps_tot = sum(camps),
    labmar_tot = sum(labmar),
    capcrime_tot = sum(capcrime),
    fullboat_tot = sum(`38`),
    n_mig     = n()
  ) %>% 
  mutate(
    date2 = date_new,
    source = paper,
    crime_share = crime_tot/n_mig,
    medit_share = medit_tot/n_mig,
    deport_share = deport_tot/n_mig,
    refnums_share = refnums_tot/n_mig,
    camps_share = camps_tot/n_mig,
    labmar_share = labmar_tot/n_mig,
    capcrime_share = capcrime_tot/n_mig,
    fullboat_share = fullboat_tot/n_mig
  ) %>% 
  full_join(x = ., y = w, by = c('date2', 'source')) %>% 
  filter(!is.na(natt))

rm(output)
rm(w)

## merge afd slant estimates
fulltable <- 
  afd_slant %>% 
  filter(date != '') %>% 
  mutate(date2 = ifelse(paper == 'bild', as.Date(date, format = '%Y-%m-%d'), as.Date(NA))) %>% # fix dates
  mutate(date2 = ifelse(paper == 'spon', as.Date(date, format = '%d.%m.%Y'), date2)) %>% 
  mutate(date2 = ifelse(!paper %in% c('bild', 'spon'),  as.Date(as.numeric(date), origin = '1970-01-01'), date2)) %>% 
  mutate(date2 = as.Date(date2)) %>% 
  dplyr::select(-date) %>% 
  mutate(source = ifelse(paper == 'weltonline', 'welt', paper)) %>% 
  group_by(source, date2) %>% 
  summarise(slant_afd_resampled_avg_pred  = mean(pred_resampled),
            slant_afd_resampled_avg_proba = mean(proba_resampled),
            slant_afd_1819_avg_pred  = mean(pred_1819),
            slant_afd_1819_avg_proba = mean(proba_1819)) %>% 
  right_join(x = ., y = fulltable, by = c('date2', 'source')) %>% 
  dplyr::select(-paper)

rm(afd_slant)

## add different operationalisations of migration estimates
bert_estimates <- read.csv('_dt/_mig_estimates/BERT_estimates_cleaned.csv')

# find papers based on urls
bert_estimates <- 
  bert_estimates %>%
  mutate(paper = str_match(link, pattern = '.*(bild)\\.de.*')[,2]) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(faz)\\.net.*')[,2]        , paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(welt)\\.de.*')[,2]        , paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(taz)\\.de.*')[,2]         , paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(spiegel)\\.de.*')[,2]     , paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(sueddeutsche)\\.de.*')[,2], paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(jetzt)\\.de.*')[,2]       , paper)) %>%
  mutate(paper = ifelse(is.na(paper), str_match(link, pattern = '.*(dctp)\\.tv.*')[,2]        , paper)) %>% 
  filter(!is.na(paper))

bert_estimates <- 
  bert_estimates %>% 
  mutate( paper = ifelse(paper == 'dctp', 'spon', paper)) %>% 
  mutate( paper = ifelse(paper == 'spiegel', 'spon', paper)) %>% 
  mutate( paper = ifelse(paper == 'sueddeutsche', 'sz', paper)) %>% 
  mutate( paper = ifelse(paper == 'jetzt', 'sz', paper))

fulltable <- 
  bert_estimates %>% 
  mutate(est = as.numeric(est)) %>% 
  mutate(date2 = ifelse(paper == 'bild', as.Date(date, format = '%Y-%m-%d'), as.Date(NA))) %>% # fix dates
  mutate(date2 = ifelse(paper == 'spon', as.Date(date, format = '%d.%m.%Y'), date2)) %>% 
  mutate(date2 = ifelse(!paper %in% c('bild', 'spon'),  as.Date(as.numeric(date), origin = '1970-01-01'), date2)) %>%
  filter(!is.na(date2)) %>% 
  mutate(date2 = as.Date(date2)) %>% 
  mutate(label = (label == 'True')) %>% 
  group_by(paper, date2) %>% 
  summarise(
    mig_pred  = mean(label, na.rm = T),
    mig_proba = mean(est, na.rm = T),
    n_tot = n()
  ) %>% 
  mutate(source = paper) %>% 
  full_join(fulltable, by = c('source', 'date2'))

save(fulltable, file = paste0('_dt/merged_daily_', Sys.Date(), '.Rdata'))
