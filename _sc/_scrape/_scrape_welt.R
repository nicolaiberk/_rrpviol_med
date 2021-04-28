# ______________________________________________
# Media reactions to RR violence
# Goal: Scrape Welt articles
# Approach: loop over links collected, add article text to dataframe
# ______________________________________________
# Date:  Fri Feb 05 10:09:16 2021
# Author: Nicolai Berk
# ______________________________________________



# Setup ####
library( rvest )
library( dplyr )
library( magrittr )
library(here)
library(data.table)


# function to write single row (https://stackoverflow.com/questions/23340344/using-write-table-to-write-rows-row-by-row-to-a-csv-in-r-as-you-would-in-python)
filepath <- here("_dt/_weltonline_articles.csv")


# load links
load(here("_dt/_weltonline_raw.Rdata"))
# welt_all <- welt_all[sample(1:nrow(welt_all), 1000),] #use subsample for testing


## in case code broke down: load old data, drop collected observations
# old <- fread(filepath)
# welt_all <- welt_all[!welt_all$link %in% old$article_url,]
# rm(old)


# loop over links collected, add article text to dataframe ####
for (id in 1:nrow(welt_all)){
    topic <- welt_all[id, "ressort"]
    title <- gsub(pattern  = "\n|  ",
                  replacement = "",
                  welt_all[id, "title"])
    article_url <- welt_all[id, "link"]
    
    cat("\014")
    cat(id,"/", nrow(welt_all),
        " (", as.integer(id*100/nrow(welt_all)), "%):\n",
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
          html_nodes('.c-summary__intro') %>%
          html_text(trim = T) %>%
          paste(collapse = " ") %>% 
          gsub(pattern = "\n|\t|  ", 
               replacement = "") 
      )


    # matching text might be more specific with css selector (something like: "c-article-text c-content-container __margin-bottom--is-0 p")
    text <-
      ifelse(
        is.na(page),
        NA_character_,
        page %>%
          html_nodes(xpath = "//div[contains(@class, 'c-article-text')]/p") %>%
          html_text(trim = T) %>%
          paste(collapse = " ") %>% 
          gsub(pattern = "\n|\t|  ", 
               replacement = "") 
      )
    
    date <-
      ifelse(
        is.na(page),
        NA_character_,
        page %>%
          html_nodes(".c-publish-date") %>%
          html_text(trim = T) %>%
          as.Date(format = "Veröffentlicht am %d.%m.%Y")
      )
      

  
    # save ####
    fwrite(
      data.frame(
        date     = ifelse(length(date) > 0,         date,         NA_character_), 
        title    = ifelse(length(title) > 0,        title,        NA_character_), 
        url      = ifelse(length(article_url) > 0,  article_url,  NA_character_), 
        topic    = ifelse(length(topic) > 0,        topic,        NA_character_), 
        lead     = ifelse(length(lead) > 0,         lead,         NA_character_),
        text     = ifelse(length(text) > 0,         text,         NA_character_)),
      file   = filepath, 
      append=ifelse(id == 1, F, T) # overwrite in first run
    )

    Sys.sleep(rnorm(1, 0, 0.25)^2)
}

