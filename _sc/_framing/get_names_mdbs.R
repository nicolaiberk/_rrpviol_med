# ______________________________________________
# Media reactions to radical right violence
# Goal: Get names of MdBs
# ______________________________________________
# Date:  Mon Oct 11 16:28:58 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(legislatoR)

politician_names <- 
  get_core('deu') %>% 
  left_join(get_political('deu'), by = 'pageid') %>% 
  filter(session > 17) %>% 
  select(name) %>%
  unique()

# write to csv
write.csv2(politician_names, paste0(here::here('_dt/_releases'), '/politician_names.csv'))
