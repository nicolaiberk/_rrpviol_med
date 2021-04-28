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
filepath <- here("_dt/_sz_raw_update.csv")

### double-digit day
double_digits <- function(x){
  as.character(ifelse(x>9, x, paste0("0", x)))
}



# define scraping url ####

## url to format
url_raw <- "https://www.sueddeutsche.de/archiv/politik/{dt_y}/{dt_m}"
suburl_raw <- "https://www.sueddeutsche.de/archiv/politik/{dt_y}/{dt_m}/page/{p}"

## dates for formatting
year.list <- 2013:2018
month.list <- 1:12 %>% double_digits()

# uncomment this in case code broke down (and comment out header writing)
# old <- read.csv2(filepath)
# 
# date.list <- date.list[!date.list$date.string %in% old$date,];rm(old)

# get info ####
firstRun <- T
for (dt_y in year.list){
  for(dt_m in month.list){
    
  
    ## get n of pages to scrape
    base_url<- glue(url_raw)
    
    cat("\014")
    cat(glue("Determining number of subpages for {base_url}..."))
    
    n_pages<- 
      read_html(base_url) %>% 
      html_nodes(xpath='//li[contains(@class, "navigation")]/ol/li') %>% 
      tail(1) %>% 
      html_text() %>% 
      gsub(pattern = "\\n *", 
           replacement = "") %>% 
      as.numeric()
    
    ## scrape each subpage (one for 20 items)
    for (p in 1:n_pages){
      
      suburl <- glue(suburl_raw)
      
      cat("\014")
      cat("Scraping page ", base_url,
          "(", p, "/", n_pages, ")")
      
      
      ## get relevant elements on page
      elements <- 
        read_html(suburl) %>% 
        html_nodes('.entrylist__entry')
      
      for (e in elements){
        
        ## extract title
        article_title <- e %>% 
          html_nodes(".entrylist__title") %>% 
          html_text()
        
        ## extract url
        article_url <- e %>% 
          html_nodes(".entrylist__link") %>% 
          html_attr('href')
        
        ## extract date
        article_date <- e %>% 
          html_nodes(".entrylist__time") %>% 
          html_text() %>% 
          gsub("\n|  ", "", .) %>% 
          as.Date(format = "%d.%m.%Y | %H:%M")
        
        
        ## extract topic(s)
        article_keyword <- e %>% 
          html_nodes(".entrylist__overline") %>% 
          html_text()
        
        
        # save ####
        if(article_title != ""){ # prevents saving weird elements with urls leading nowhere
          fwrite(
            data.frame(
              title = ifelse(length(article_title)>0,
                             article_title,
                             NA), 
              url = ifelse(length(article_url)>0,
                           article_url,
                           NA), 
              date = ifelse(length(article_date)>0,
                            article_date,
                            NA),  
              keyword = ifelse(length(article_keyword)>0,
                               article_keyword,
                               NA)),
            file = filepath,
            append = ifelse(firstRun == T, F, T)
          )
        }
        firstRun <- F
      
    }
  }
}
}



