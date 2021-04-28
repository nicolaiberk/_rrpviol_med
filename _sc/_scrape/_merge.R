# ______________________________________________
# Media reacions to RR violence
# Goal: Merge article data
# Procedure: Load each set, append to full set
# ______________________________________________
# Date:  Mon Mar 01 09:44:09 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)

filepath <- here( "_dt/_out" )
outpath <- paste0(filepath, "/_merged_articles.csv")


# SPON ####
## load
spon <- fread(paste0(filepath, "/_spon_articles.csv"))
spon$source <- 'spon'
## check data
# sum(spon$text == "")/nrow(spon) # 26.6% !
# sum(is.na(spon$text)) # 0%
# 
# hist(as.Date(spon$date, format = "%d.%m.%Y"), "weeks") # missings do not seem to be systematic


## arrange vars
### date, title, url, topic, lead, text

## write
fwrite(spon, file = outpath, append = F)
rm(spon)



# SZ ####
sz <- fread(paste0(filepath, "/_sz_articles.csv"))

## check data
# sum(sz$text == "")/nrow(sz) # 1.8%
# sum(is.na(sz$text))/nrow(sz) # 0%
# sum(is.na(sz$date))/nrow(sz) # 3.5%
# 
# hist(as.Date(sz$date, origin = "1970-01-01"), "weeks") # missings do not seem to be systematic

## arrange vars
### date (chr, format = "%d.%m.%Y"), title, url, topic, lead, text
sz$date <- strftime(x = as.Date(sz$date, origin = "1970-01-01"), format = "%d.%m.%Y")
sz$source <- 'sz'
## write
fwrite(sz, file = outpath, append = T)
rm(sz)

# taz ####
taz <- fread(paste0(filepath, "/_taz_articles.csv"))

## check data
# sum(taz$text == "")/nrow(taz) # 2%
# sum(taz$text == "NA")/nrow(taz) # 0%
# sum(is.na(taz$text))/nrow(taz) # 0%
# sum(is.na(taz$date))/nrow(taz) # 0%
# 
# hist(as.Date(taz$date, origin = "1970-01-01"), "weeks") # missings do not seem to be systematic
# 
# sum(as.Date(taz$date, 
#             origin = "1970-01-01") 
#     < as.Date("2013-01-01"), 
#     na.rm = T
#     )/nrow(taz) # 0.01%

## arrange vars
### date (chr, format = "%d.%m.%Y"), title, url, topic, lead, text
taz$date[as.Date(taz$date, origin = "1970-01-01")  < 
           as.Date("2013-01-01")] <- NA
taz$date <- strftime(x = as.Date(taz$date, origin = "1970-01-01"), format = "%d.%m.%Y")
taz <- taz[,c(3,1,2,4,5,6)]
taz$source <- 'taz'
fwrite(taz, file = outpath, append = T)
rm(taz)



# welt ####
welt <- fread(paste0(filepath, "/_weltonline_articles.csv"))

## check data
# sum(welt$text == "")/nrow(welt) # 0.4%
# sum(welt$text == "NA")/nrow(welt) # 0%
# sum(is.na(welt$text))/nrow(welt) # 0%
# sum(is.na(welt$date))/nrow(welt) # 0.1%
# 
# hist(as.Date(welt$date, origin = "1970-01-01"), "weeks") # looks a bit weird, should be discussed

## arrange vars
### date (chr, format = "%d.%m.%Y"), title, url, topic, lead, text
welt$date <- strftime(x = as.Date(welt$date, origin = "1970-01-01"), format = "%d.%m.%Y")
welt$source <- 'welt'

## write
fwrite(welt, file = outpath, append = T)
rm(welt)



# faz ####
faz <- fread(paste0(filepath, "/_faz_articles.csv"))

## check data
# sum(faz$text == "")/nrow(faz) # 0.4%
# sum(faz$text == "NA")/nrow(faz) # 0%
# sum(is.na(faz$text))/nrow(faz) # 0%
# sum(is.na(faz$date))/nrow(faz) # 0%
# 
# hist(as.Date(faz$date, origin = "1970-01-01"), "weeks") # looks fine

## arrange vars
### date (chr, format = "%d.%m.%Y"), title, url, topic, lead, text
faz$date <- strftime(x = as.Date(faz$date, origin = "1970-01-01"), format = "%d.%m.%Y")
faz$source <- 'faz'

fwrite(faz, file = outpath, append = T)
rm(faz)



# bild ####
bild <- fread(paste0(filepath, "/_bild_articles.csv"))

## check data
# sum(bild$text == "")/nrow(bild) # 7.5%
# sum(bild$text == "NA")/nrow(bild) # 0%
# sum(is.na(bild$text))/nrow(bild) # 0%
# sum(is.na(bild$date))/nrow(bild) # 0%

# hist(as.Date(bild$date, format = "%d.%m.%Y"), "weeks") # a bit weird that twice as many articles in 2016/17

## arrange vars
### date (chr, format = "%d.%m.%Y"), title, url, topic, lead, text
bild$date <- strftime(x = as.Date(bild$date, origin = "1970-01-01"), format = "%d.%m.%Y")
bild$source <- 'bild'

fwrite(bild, file = outpath, append = T)
rm(bild)
