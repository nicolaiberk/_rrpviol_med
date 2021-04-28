# ______________________________________________
# Media reactions to RR violence
# Goal: Scrape urls FAZ for subsequent article collections
# Procedure: Define urls, scrape, save
# ______________________________________________
# Date:  Thu Feb 11 18:08:44 2021
# Author: Nicolai Berk
# ______________________________________________



# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)
library(glue)
library(data.table)
library(lubridate)

## functions

### double-digit day
double_digits <- function(x){
  as.character(ifelse(x>9, x, paste0("0", x)))
}


# Define urls ####

## url to format
url_raw <- "https://www.faz.net/artikel-chronik/nachrichten-{url_y}-{url_m}-{url_d}/"

## dates for formatting
date.list <- as.data.frame( seq( as.Date( '2013-01-01' ) , as.Date( '2018-12-31' ) , by = 1 ))
names( date.list )[ 1 ] <- 'V'
date.list$year <- format( as.Date( date.list$V ) , '%Y' )
date.list$month <- format( as.Date( date.list$V ) , '%m' )
date.list$day <- format( as.Date( date.list$V ) , '%d' )
date.list$date.string <- paste( date.list$year , date.list$month , date.list$day , sep = '-' )


# date.list <- date.list[!date.list$date.string %in% old$date,];rm(old)



# scrape ####
cols <- c('date', 'title', 'url', 'keywords', 'lead')
for (dt in date.list$date.string){
  
  url_d <- day(dt) %>% double_digits()
  url_m <- month(dt, label = T, abbr = F)
  url_y <- year(dt)
  
  ## page to scrape
  base_url<- glue(url_raw)
  
  cat("\014")

  cat("Scraping page ", base_url, "...")
    
  ## get relevant elements on page
  elements <- 
    tryCatch(read_html(base_url),
             error = function(e){
               cat("Failed, retrying", dt)
               Sys.sleep(4)
               tryCatch(read_html(base_url),
                        error = function(e){
                          cat("\nCollection failed, skipping", dt)                          
                          NA
                        })
               }
             ) %>% 
    html_nodes('.Teaser620')
    
  article_titles <- elements %>% 
  html_nodes(".Headline") %>% 
    html_text()
    
  article_urls <- elements %>%
    html_node(".TeaserHeadLink") %>% 
    html_attr('href') %>% 
    paste0('https://www.faz.net', .)
  
  article_keywords <- elements %>% 
    html_node(".Stichwort") %>% 
    html_text()

  article_leads <- elements %>% 
    html_node(".Copy") %>% 
    html_text() %>% 
    gsub(pattern = "\n|\t|\r|  ", 
         replacement = "")
    
  fazlinks <- data.frame(dt, article_titles, article_urls, article_keywords, article_leads)
  colnames(fazlinks) <- cols
  
  # save #### 
  fwrite(fazlinks, 
         file = here('_dt/_faz_raw.csv'), 
         append = ifelse(dt == date.list$date.string[1],F,T) # overwrite file in first iteration
         )
}


