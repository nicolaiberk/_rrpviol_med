# ______________________________________________
# Media reacions to RR violence
# Goal: Merge article data
# Procedure: Load each set, append to full set
# ______________________________________________
# Date:  Mon Mar 01 09:44:09 2021
# Author: Nicolai Berk & Werner Krause
# ______________________________________________


# Setup ####
library(dplyr)
library(magrittr)
library(here)
library(data.table)

filepath <- "/Users/krausewz/Dropbox (Maestral)/_git.pprs/_rrpviol_med" 
outpath <- paste0(filepath, "/_dt/_out/_merged_articles2.csv")


# Strings ----

afd.s <- c( 'AfD|von Storch|Höcke |Gauland|Petry|Weidel | Lucke |PEGIDA' )
imm.s <- c( 'Flüchtling|Asylbewerber|Asylant|Immigr|Migra' )
att.s <- c( 'Anschl|Attent|Angr|Brand|NSU' )
pev.s <- c( 'Parteitag' )

det.strings <- function( d ){
  library( stringr )
  d %<>% mutate( 
    afd = ifelse( str_detect( title , afd.s ) == T | str_detect( lead , afd.s ) == T , 1 , 0 )
    , imm = ifelse( str_detect( title , imm.s ) == T | str_detect( lead , imm.s ) == T , 1 , 0 )
    , att = ifelse( str_detect( title , att.s ) == T | str_detect( lead , att.s ) == T , 1 , 0 )
    , pev = ifelse( str_detect( title , pev.s ) == T | str_detect( lead , pev.s ) == T , 1 , 0 )
    , imm.woattafd = ifelse( imm == 1 & att == 0 & afd == 0 , 1 , 0 )
    , imm.woatt = ifelse( imm == 1 & att == 0 , 1 , 0 )
    , afd.wopev = ifelse( afd == 1 & pev == 0 , 1 , 0 ))
    
  d %<>% mutate( 
    afd.tx = ifelse( str_detect( text , afd.s ) == T , 1 , 0 )
      , immig.tx = ifelse( str_detect( text , imm.s ) == T , 1 , 0 ))
  return( d )
  detach( package:stringr )
}
sumup <- function( d ){
  d %<>% 
    group_by( date , source ) %>% 
    mutate( N_daysource = max( row_number( ))) %>%
    mutate_at( vars( afd : immig.tx ) , funs( daysource = sum( . , na.rm = TRUE ) )) %>%
    group_by( date ) %>% 
    mutate( N_day = max( row_number( ))) %>%
    mutate_at( vars( afd : immig.tx ) , funs( day = sum( . , na.rm = TRUE ) )) %>%
    ungroup( ) %>%
    mutate_at( vars( afd_daysource : immig.tx_daysource ) , funs( share = . / N_daysource )) %>% 
    mutate_at( vars( afd_day : immig.tx_day ) , funs( share = sum( . , na.rm = TRUE ) / N_day ))
  
}

# SPON ####
## load
spon <- fread(paste0(filepath, "/_dt/_out/_spon_articles.csv"))
spon$source <- 'spon'
## check data
sum(spon$text == "")/nrow(spon) # 26.6% !
sum(is.na(spon$text)) # 0%

spon %<>% 
  filter( !( text == '' & stringr::str_detect( url , 'fotostrecke' ))) %>%
  filter( !( text == '' & stringr::str_detect( url , '/video/' ))) %>%
  filter( text != '' | stringr::str_detect( url , 'www\\.spiegel\\.de' ) == TRUE )

sum(spon$text == "")/nrow(spon) # 26.6% !
sum(is.na(spon$text)) # 0%
hist(as.Date(spon$date, format = "%d.%m.%Y"), "weeks") # missings do not seem to be systematic

spon %<>% det.strings( . )
spon %<>% select( -text )
spon %<>% sumup( . )

## arrange vars
### date, title, url, topic, lead, text

## write
fwrite(spon, file = outpath, append = F)
rm(spon)

# SZ ####
sz <- fread(paste0(filepath, "/_dt/_out/_sz_articles.csv"))

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
sz %<>% det.strings( . )
sz %<>% select( -text )
sz %<>% sumup( . )
## write
fwrite(sz, file = outpath, append = T)
rm(sz)

# taz ####
taz <- fread(paste0(filepath, "/_dt/_out/_taz_articles.csv"))

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
taz %<>% det.strings( . )
taz %<>% select( -text )
taz %<>% sumup( . )
fwrite(taz, file = outpath, append = T)
rm(taz)



# welt ####
welt <- fread(paste0(filepath, "/_dt/_out/_weltonline_articles.csv"))

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
welt %<>% det.strings( . )
welt %<>% select( -text )
welt %<>% sumup( . )
## write
fwrite(welt, file = outpath, append = T)
rm(welt)



# faz ####
faz <- fread(paste0(filepath, "/_dt/_out/_faz_articles.csv"))

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
faz %<>% det.strings( . )
faz %<>% rename( topic = keywords )
faz %<>% select( -text )
faz %<>% sumup( . )
fwrite(faz, file = outpath, append = T)
rm(faz)



# bild ####
bild <- fread(paste0(filepath, "/_dt/_out/_bild_articles.csv"))

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
bild %<>% rename( lead = subtitle , topic = kicker )
bild %<>% det.strings( . )
bild %<>% select( -text , -time)
bild %<>% sumup( . )
fwrite(bild, file = outpath, append = T)
rm(bild)

med <- read.csv( paste0(filepath, "/_dt/_out/_merged_articles2.csv") )
table( med$source )
save( med , file = paste0(filepath, "/_dt/_out/_merged_articles2.Rdata") )
unlink(paste0(filepath, "/_dt/_out/_merged_articles2.csv"))

# CREATE DAILY DATA WITH COUNT VARIABLES ----

load( paste0( filepath , '/_dt/_out/_merged_articles2.Rdata' ))
w <- med

w %<>% mutate( date2 = as.Date( date , '%d.%m.%Y' ))
w %>% filter( is.na( date2 ) ) %>% 
  group_by( source ) %>%
  mutate( n_date_fail = max( row_number( ))) %>% ungroup( ) %>%
  select( source , n_date_fail , date ) %>% unique( )
# 5344 missing dates for SZ

w %<>% filter( !is.na( date2 ))

w %<>% mutate( week = as.numeric( lubridate::isoweek( date2 )) ,  year = as.numeric( format( date2 , '%Y' ))) 
summary( w$week ) # no NA
summary( w$year ) # no NA

att <- read.csv( paste0( filepath , '/_dt/rwt/_rr.csv') ) %>%
  mutate( date2 = as.Date( paste( iyear , imonth , iday , sep = '-'))
          , natt = 1 ) %>% 
  select( date2 , natt , nkill , nwound ) %>%
  group_by( date2 ) %>%
  mutate_at( vars( natt : nwound ) , funs( sum( . , na.rm = T ))) %>%
  ungroup( ) %>% 
  unique( )
summary( att$date2 ) # no NA

w %<>% left_join( att ) %>%
  mutate_at( vars( natt : nwound ) , funs( ifelse( is.na( . ) , 0 , . )))

w %<>% 
  select( date2 , week , year , source, afd_daysource : immig.tx_daysource 
          , afd_daysource_share : immig.tx_daysource_share 
          , natt : nwound ) %>%
  unique( )
w %<>% filter( date2 < '2019-01-01' )
w %<>% arrange( date2 )

save( w , file = paste0( filepath , '/_dt/_out/_daily.Rdata' ))
