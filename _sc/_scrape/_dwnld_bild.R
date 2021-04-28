# ______________________________________________
# Media reactions to RR violence
# Goal: Collect urls to scrape from Bild Online
# Approach: 
# ______________________________________________
# Date:  Fri Feb 05 08:40:25 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)
library(glue)
library(lubridate)

# define parameters ####
date.list <- as.data.frame( seq( as.Date( '2013-01-01' ) , as.Date( '2018-12-31' ) , by = 1 ))
names( date.list )[ 1 ] <- 'V'
date.list$year <- format( as.Date( date.list$V ) , '%Y' )
date.list$month <- format( as.Date( date.list$V ) , '%m' )
date.list$day <- format( as.Date( date.list$V ) , '%d' )
date.list$date.string <- paste( date.list$year , date.list$month , date.list$day , sep = '-' )

xpath <- '//div[contains(@class, "txt")]/p/a'
xpath_date <- '//div[contains(@class, "txt")]/p'
xpath_topic <- '//footer/span[3]'


out <- tibble::tibble( date = NA_character_ , title = NA_character_ , url = NA_character_, article_date = NA_character_)

for( dt in date.list$date.string){
  
  cat("\014")
  cat(dt)
  
  page <- read_html(
    glue("https://www.bild.de/archive/{year(dt)}/{month(dt)}/{day(dt)}/index.html")
    )
  nodes <- html_nodes(page, xpath = xpath )
  
  tt_raw <- html_text(nodes)
  titles <- tt_raw[(grep("\\b01\\b", tt_raw)+1):length(tt_raw)] # get rid of header
  
  urls <- html_attr(nodes, 'href') 
  urls <-  urls[(grep("\\b01\\b", tt_raw)+1):length(tt_raw)]
  
  article_time <- html_nodes(page, xpath = xpath_date ) %>% html_text()
  article_time <- 
    article_time[grep("Uhr", article_time)] %>% # match only proper times
    gsub(" Uhr.*", "", x = .)
  
  
  out <- rbind( out[!is.na(out$date),] , 
                cbind( date = dt , 
                       title = titles, 
                       url = urls,
                       article_time = as.Date(article_time, "%H:%M")
                       ))
  
  save( out , file = here('_dt/bild_articles.Rdata'))
  
  Sys.sleep(rnorm(1, 0, 0.5)^2)
  
}
