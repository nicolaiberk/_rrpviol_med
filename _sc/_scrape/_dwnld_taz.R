# ______________________________________________
# Media reactions to RR violence
# Goal: Collect article URLs from TAZ
# Procedure: define scraping url, get info, save
# ______________________________________________
# Date:  Wed Feb 10 12:48:47 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)
library(glue)
library(data.table)

# function to write single row (https://stackoverflow.com/questions/23340344/using-write-table-to-write-rows-row-by-row-to-a-csv-in-r-as-you-would-in-python)
filepath <- here("_dt/_taz_raw.csv")



# define scraping url ####

## url to format
url_raw <- "https://taz.de/!s=&eTagAb={dt}&eTagBis={dt}/"
suburl_raw <- "https://taz.de/!s=&eTagAb={dt}&eTagBis={dt}/?search_page={p}"

## dates for formatting
date.list <- as.data.frame( seq( as.Date( '2013-01-01' ) , as.Date( '2018-12-31' ) , by = 1 ))
names( date.list )[ 1 ] <- 'V'
date.list$year <- format( as.Date( date.list$V ) , '%Y' )
date.list$month <- format( as.Date( date.list$V ) , '%m' )
date.list$day <- format( as.Date( date.list$V ) , '%d' )
date.list$date.string <- paste( date.list$day , date.list$month , date.list$year , sep = '.' )


# uncomment this in case code broke down (and comment out header writing)
# filepath <- here("_dt/_taz_raw.csv")
# old <- read.csv2(filepath)
# 
# date.list <- date.list[!date.list$date.string %in% old$date,];rm(old)


firstRun <- T
# get info ####
for (dt in date.list$date.string){
  
  ## get n of pages to scrape
  base_url<- glue(url_raw)
  
  cat("\014")
  cat(glue("Determining number of subpages for {base_url}..."))
  
  n_pages<- 
    read_html(base_url) %>% 
    html_node(xpath = '//div[contains(@class, "secthead")]/h2/a') %>% 
    html_text() %>% 
    gsub(pattern = "Suchergebnis 1 - \\d+ von ", 
         replacement = "") %>% 
    as.numeric()
  
  ## scrape each subpage (one for 20 items)
  for (p in 0:round(n_pages/20, 0)){
    
    suburl <- glue(suburl_raw)
    
    cat("\n\tScraping subpage ", p, "/", round(n_pages/20, 0), ": \n\t\t", suburl)
    
    ## get relevant elements on page
    elements <- 
      read_html(suburl) %>% 
      html_nodes(xpath = '//ul[contains(@class, "news")]/li[contains(@class, "article")]')
    
    for (e in elements){
      
      ## extract title
      article_title <- 
        ifelse(
          e %>% 
            html_nodes(xpath = "a/h3") %>% 
            length()>0,
          e %>% 
            html_nodes(xpath = "a/h3") %>% 
            html_text(),
          ""
        )
      
      ## extract url
      article_url <- e %>% 
        html_nodes(xpath = "a") %>% 
        html_attr('href') %>% 
        paste0('https://taz.de', .)

      ## extract date
      article_date <- e %>% 
        html_nodes(".date") %>% 
        html_text() %>% 
        gsub(pattern = "\\..", # replace odd character
             replacement = ".") %>% 
        as.Date(format = "%d.%m.%Y")
      
      ## extract topic(s)
      article_topic <- e %>% 
        html_nodes(xpath = "div/ul[contains(@class, 'right')]/li[@class != 'ondemand']/a") %>% 
        html_text() %>% 
        paste(collapse = ", ")
      
      
      # save ####
      tazlinks <- data.frame(dt, article_title, article_url, article_date, article_topic)
      colnames(tazlinks) <- c("date", "title", "url", "article_date", "topic")
      
      # save #### 
      fwrite(tazlinks, 
             file = here('_dt/_faz_raw.csv'), 
             append = ifelse(firstRun == T,F,T) # overwrite file in first iteration
      )
      
      firstRun <- F
      
      }
  }
}



