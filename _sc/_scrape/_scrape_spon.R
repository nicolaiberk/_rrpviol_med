# ______________________________________________
# Media Reactions to RR violence
# Goal: Scraping Articles from Spiegel Online
# Procedure: load links, loop over and scrape article, save
# ______________________________________________
# Date:  Wed Feb 10 10:48:24 2021
# Author: Nicolai Berk
# ______________________________________________


# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)
library(data.table)
library(lubridate)


# function to write single row (https://stackoverflow.com/questions/23340344/using-write-table-to-write-rows-row-by-row-to-a-csv-in-r-as-you-would-in-python)
filepath <- here("_dt/_spon_articles.csv")


# load links ####
load(here("_dt/_spon_raw.Rdata"))
# out <- out[sample(1:nrow(out), 1000),] #use subsample for testing

## in case code broke down: load old data, drop collected observations
# old <- fread(filepath)
# out <- out[!out$url %in% old$url,];rm(old)

firstRun <- T

# loop over links collected, add article text to dataframe ####
for (id in 1:nrow(out)){
  article_url <- out[id, "url"]
  topic <- out[id, "topic"]
  title <- gsub(pattern  = "\n|  ",
                replacement = "",
                out[id, "title"])
  date <- out[id, "date"]
  article_date <- out[id, "article_date"]
  
  
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
        html_nodes('.leading-loose') %>%
        html_text() %>%
        paste(collapse = " ")
    ) %>% 
    gsub(pattern = "\n|\t|  ", 
         replacement = "")
  
  
  # matching text might be more specific with css selector (something like: "c-article-text c-content-container __margin-bottom--is-0 p")
  text <-
    ifelse(
      is.na(page),
      NA_character_,
      page %>% 
        html_nodes(xpath = '//div[contains(@class, "clearfix")]//p') %>% 
        html_text() %>% 
        paste(collapse = " ") %>% 
        gsub(pattern = "\n|\t|  ", 
             replacement = "")
    )
  
  
  fwrite(
    data.frame(
      date     = ifelse(length(date) > 0,         date,         NA_character_), 
      title    = ifelse(length(title) > 0,        title,        NA_character_), 
      url      = ifelse(length(article_url) > 0,  article_url,  NA_character_), 
      topic    = ifelse(length(topic) > 0,        topic,        NA_character_), 
      lead     = ifelse(length(lead) > 0,         lead,         NA_character_),
      text     = ifelse(length(text) > 0,         text,         NA_character_)),
    file   = filepath,
    append = ifelse(firstRun == T, F, T) # overwrite in first run
  )
  
  firstRun <- F
  
  Sys.sleep(rnorm(1, 0, 0.25)^2)
  
}

cat("Finished downloading", nrow(out), "articles.", sep = " ")
