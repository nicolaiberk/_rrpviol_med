# ______________________________________________
# Media reactions to RR violence
# Goal: Scrape TAZ articles 2013-2018
# Procedure: load links, scrape, save
# ______________________________________________
# Date:  Wed Feb 17 15:03:44 2021
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
filepath <- here("_dt/_taz_articles.csv")

# load links ####
out <- fread(here('_dt/_taz_raw.csv')) %>% data.frame()
out$date <- out$date %>% as.Date(format = "%d.%m.%Y")
# out <- out[sample(1:nrow(out), 1000),] #use subsample for testing

## in case code broke down: load old data, drop collected observations
# old <- fread(here('_dt/_taz_articles.csv'))
# out <- out[!out$url %in% old$url,];rm(old)



# scrape ####
for (id in 1:nrow(out)){
  date <- out[id, "date"]
  title <- out[id, "title"]
  article_url <- out[id, "url"]
  article_date <- out[id, "article_date"]
  topic <- out[id, "topic"]
  
  
  cat("\014")
  cat(id,"/", nrow(out),
      " (", as.integer(id*100/nrow(out)), "%):\r\n",
      article_url, "\n",
      sep = "")
  
  # tryCatch() avoids breakdow due to 404s. See https://r-lang.com/r-trycatch-function/ and https://stackoverflow.com/questions/38114066/using-trycatch-and-rvest-to-deal-with-404-and-other-crawling-errors?answertab=votes
  page <- tryCatch(read_html(article_url),
                   error = function(e){
                     cat(article_url, "failed, retrying...")
                     Sys.sleep(4)
                     tryCatch(read_html(article_url),
                              error = function(e){
                                cat(article_url, "failed, skipping url.")
                                NA
                              }
                     )}
  )
  
  lead <-
    ifelse(
      is.na(page),
      NA_character_,
      page %>%
        html_nodes(xpath = '//*[contains(@class, "intro")]/text()') %>%
        paste(collapse = " ")
    ) %>% 
    gsub(pattern = "\n|\t|  ", 
         replacement = "")
  
  
  text <-
    ifelse(
      is.na(page),
      NA_character_,
      page %>% 
        html_nodes(xpath = '//article/p[contains(@class, "article")]') %>% 
        html_text() %>% 
        paste(collapse = " ") %>% 
        gsub(pattern = "\n|\t|  ", 
             replacement = "") 
    )
  
  # if empty, try alternative xpath fitting few pages
  text <- ifelse(
    text != "",
    text,
    page %>% 
      html_nodes(xpath = '//*[contains(@class, "sect_article")]/div[@class = "sectbody"]') %>%
      html_text() %>% 
      paste(collapse = " ") %>% 
      gsub(pattern = "\n|\t|  ", 
           replacement = " ")
  )
  
  
  # save ####
  fwrite(
    data.frame(
      title    = ifelse(length(title) > 0,        title,        NA_character_), 
      url      = ifelse(length(article_url) > 0,  article_url,  NA_character_), 
      date     = ifelse(length(article_date) > 0, article_date,  NA_character_), 
      topic    = ifelse(length(topic) > 0,        topic,        NA_character_), 
      lead     = ifelse(length(lead) > 0,         lead,         NA_character_),
      text     = ifelse(length(text) > 0,         text,         NA_character_)),
    file   = filepath, 
    append=ifelse(id == 1, F, T) # overwrite in first run
  )
  
  
  Sys.sleep(rnorm(1, 0, 0.25)^2)
}

cat("Finished downloading", nrow(out), "articles.", sep = " ")







