# ______________________________________________
# Media reactions to RR violence
# Goal: Scrape Bild articles 2013-2018
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
library(lubridate)

# function to write single row (https://stackoverflow.com/questions/23340344/using-write-table-to-write-rows-row-by-row-to-a-csv-in-r-as-you-would-in-python)
filepath <- here("_dt/_bild_articles.csv")

# load links ####
load(here('_dt/_bild_raw.Rdata'))
# out <- out[sample(1:nrow(out), 1000),] #use subsample for testing

## in case code broke down: load old data, drop collected observations
# old <- fread(filepath)
# out <- out[!out$url %in% old$url,];rm(old)


## write header to csv (only in first run)

firstRun <- T

# scrape ####
for (id in 1:nrow(out)){
  article_url <- paste0("https://www.bild.de", out[id, "url"])
  article_time <- out[id, "article_time"] %>% as.numeric() %>% as_date()
  title <- out[id, "title"]
  date <- out[id, "date"]
  
  
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
  
  kicker <- 
    ifelse(
      is.na(page),
      NA_character_,
      page %>%
        html_node('.kicker') %>%
        html_text() %>%
        paste(collapse = " ")
    ) %>% 
    gsub(pattern = "\n|  ", 
         replacement = "")
  
  subtitle <-
    ifelse(
      is.na(page),
      NA_character_,
      page %>%
        html_nodes('.article-header__subline') %>%
        html_text() %>%
        paste(collapse = " ")
    ) %>% 
    gsub(pattern = "\n|  ", 
         replacement = "")
  
  
   text <-
    ifelse(
      is.na(page),
      NA_character_,
      page %>% 
        html_nodes(xpath = '//div[contains(@class, "article-body") or contains(@class, "txt")]/p') %>% 
        html_text() %>% 
        paste(collapse = " ") %>% 
        gsub(pattern = "\n|  ", 
             replacement = "")
    )
  
  
  # save ####
  fwrite(
    data.frame(
      date     = ifelse(length(date) > 0,         date,         NA_character_), 
      title    = ifelse(length(title) > 0,        title,        NA_character_), 
      url      = ifelse(length(article_url) > 0,  article_url,  NA_character_), 
      time     = ifelse(length(article_time) > 0, article_time, NA_character_), 
      kicker   = ifelse(length(kicker) > 0,       kicker,       NA_character_),
      subtitle = ifelse(length(subtitle) > 0,     subtitle,     NA_character_),
      text     = ifelse(length(text) > 0,         text,         NA_character_)),
    file   = filepath,
    append = ifelse(firstRun == T, F, T) # overwrite in first run
   )
  
  firstRun <- F
  
  # Sys.sleep(rnorm(1, 0, 0.25)^2)
  
}

cat("Finished downloading", nrow(out), "articles.", sep = " ")







