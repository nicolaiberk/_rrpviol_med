# ______________________________________________
# Media reactions to RR violence
# Goal: Collect artices from Spiegel Online
# Approach: Collect urls and titles, then articles
# ______________________________________________
# Date:  Fri Feb 05 08:40:25 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)

# Collect urls and titles ####
date.list <- as.data.frame( seq( as.Date( '2013-01-01' ) , as.Date( '2018-12-31' ) , by = 1 ))
names( date.list )[ 1 ] <- 'V'
date.list$year <- format( as.Date( date.list$V ) , '%Y' )
date.list$month <- format( as.Date( date.list$V ) , '%m' )
date.list$day <- format( as.Date( date.list$V ) , '%d' )
date.list$date.string <- paste( date.list$day , date.list$month , date.list$year , sep = '.' )

xpath <- '//article/header/h2/a'
xpath_date <- '//footer/span[1]'
xpath_topic <- '//footer/span[3]'


out <- tibble::tibble( date = NA_character_ , title = NA_character_ , url = NA_character_, article_date = NA_character_)

for( i in date.list$date.string ){
  
  print( i )
  spon <- read_html( paste0( 'https://www.spiegel.de/nachrichtenarchiv/artikel-' , i , '.html' ))
  
  nodes <- html_nodes(spon, xpath = xpath )
  titles <- html_attr(nodes, 'title' )
  urls <- html_attr(nodes, 'href') 
  
  article_dates <- html_nodes(spon, xpath = xpath_date ) %>% html_text()
  
  topics <- html_nodes(spon, xpath = xpath_topic ) %>% html_text()

  
  out <- rbind( out[!is.na(out$date),] , 
                cbind( date = i , 
                       title = titles, 
                       url = urls,
                       article_date = as.Date(paste(substr(i, 7, 10), article_dates), "%Y %d. %B, %H.%M Uhr"),
                       topic = topics))
  
  save( out , file = here('_dt/spon_updated.Rdata'))
  
  Sys.sleep(rnorm(1, 0, 0.5)^2)

}
