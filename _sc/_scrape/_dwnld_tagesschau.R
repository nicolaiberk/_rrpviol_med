# ______________________________________________
# Media reactions to RR violence
# Goal: Scrape tagesschau agendas
# Procedure: define dates, scrape, write
# ______________________________________________
# Date:  Fri Feb 19 08:49:44 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library(dplyr)
library( rvest )
library(data.table)
library(here)

# define dates ####


d.list <- seq( from = as.Date( '2013-04-22' ) , to = as.Date( '2020-10-01' ) , 'days' )
d.list <- stringr::str_remove_all( d.list , '-' )

firstRun <- T
for( i in d.list ){
  cat("\014", "Scraping ", i, " ...")
  
  #i <- d.list[ 1 ]
  url0 <- paste0( 'https://www.tagesschau.de/multimedia/video/videoarchiv2~_date-' , i , '.html' )
  page <- read_html( url0 )
  
  date <- page %>% 
    html_nodes(".dachzeile") %>% 
    html_text() %>% 
    as.Date(format = "%d.%m.%Y %H:%M Uhr")
  
  video_url <- page %>% 
    html_nodes( xpath = '//p[contains(@class, "teasertext")]/a' ) %>%
    html_attr("href") %>% 
    paste0("https://www.tagesschau.de", .)
    
  title <- page %>% 
    html_nodes(xpath = "//h4") %>% 
    html_text()
  
  text <- page %>% 
    html_nodes( '.teasertext' ) %>%
    html_text(trim = T)
  
  fwrite(
    data.frame(
      date = date,
      url = video_url,
      title = title,
      text = text
    ), 
    append = ifelse(firstRun == T, F, T),
  file = here('_dt/_tagesschau_raw.Rdata') )
  
  firstRun <- F
}






# scrape ####

# write ####

