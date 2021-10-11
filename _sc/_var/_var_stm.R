# ______________________________________________
# Media reactions to RR violence
# Goal: Estimate VAR for migration frames
# Procedure: load data, merge, estimate var
# ______________________________________________
# Date:  Mon Sep 06 17:21:46 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(vars)
library(tseries)
library( ggplot2 )

# load data ####
load(here('_dt/_mig_estimates/mig_articles_topics.Rdata'))
load(here('_dt/_out/_daily.Rdata'))

# aggregate newspaper estimates for each day and merge

fulltable <- 
  output %>% 
  group_by(paper, date_new) %>% 
  summarise(
    crime_tot = sum(crime),
    medit_tot = sum(medit),
    deport_tot = sum(deport),
    refnums_tot = sum(refnums),
    camps_tot = sum(camps),
    labmar_tot = sum(labmar),
    capcrime_tot = sum(capcrime),
    n_mig     = n()
  ) %>% 
  mutate(
    date2 = date_new,
    source = paper,
    crime_share = crime_tot/n_mig,
    medit_share = medit_tot/n_mig,
    deport_share = deport_tot/n_mig,
    refnums_share = refnums_tot/n_mig,
    camps_share = camps_tot/n_mig,
    labmar_share = labmar_tot/n_mig,
    capcrime_share = capcrime_tot/n_mig
  ) %>% 
  full_join(x = ., y = w, by = c('date2', 'source')) %>% 
  filter(!is.na(natt))


# estimate var for each paper and issue ####

## dickey-fuller test
adf.test(fulltable$natt, k = 14) # stationary

# var
C <- 'natt'

grangertable <- 
  fulltable %>% 
  pivot_longer(c(3:10, 13:21), names_to = 'frame', values_to = 'attention') %>% 
  group_by(source, frame) %>% 
  summarise() %>% 
  mutate(granger_p = NA)

grangertable <- 
  grangertable %>% 
  mutate(frame_is = 'dependent') %>% 
  rbind(grangertable %>% 
          mutate(frame_is = 'independent') )

for (dependent in c('total', 'share')){
  if (dependent == 'share'){
    frames <- c('crime_share', 
                'medit_share',
                'deport_share',
                'refnums_share',
                'camps_share',
                'labmar_share',
                'capcrime_share')
  }else{
    frames <- c('crime_tot', 
                'medit_tot',
                'deport_tot',
                'refnums_tot',
                'camps_tot',
                'labmar_tot',
                'capcrime_tot')
  }
  
  
  # estimate 
  p <- list()
  q <- list()
  j <- 0
  
  for (Y in c(frames, 'n_mig')){
    
    for( i in unique(fulltable$source)){
      
      
      # ideally do an adf test for each var here
      
      j <- j+1
      
      t <- fulltable %>% filter( paper == i )
      s <- vars::VARselect( cbind( t[ , Y ], t[ , C ] ) , type = 'both' )
      
      
      m <- vars::VAR( y = cbind( Y = t[ , Y ], C = t[ , C ])
                      , p = s$selection[ 1 ] , type = 'both' )
      
      p1 <- vars::irf( x = m , impulse = C , response = Y , n.ahead = 14 , cumulative = T )
      
      p1.1 <- cbind( unlist( p1$irf ) , unlist( p1$Lower ) , unlist( p1$Upper )) %>% as.data.frame( ) %>%
        mutate( time = seq_along( V1 ) , mod = paste( i , 'C = ', C, 'Y = ', Y, sep = ' - ' ) )
      
      p2 <- vars::irf( x = m , impulse = Y , response = C , n.ahead = 14 , cumulative = T )
      
      p2.1 <- cbind( unlist( p2$irf ) , unlist( p2$Lower ) , unlist( p2$Upper )) %>% as.data.frame( ) %>%
        mutate( time = seq_along( V1 ) , mod = paste( i , 'C = ', Y, 'Y = ', C, sep = ' - ' ) )
      
      p[[j]] <- 
        p1.1 %>%
        ggplot( aes( time , V1 )) +
        geom_point( ) +
        geom_errorbar( aes( ymin = V2 , ymax = V3 ) , width = 0 ) +
        geom_hline( aes( yintercept = 0 ) , linetype = 2 ) +
        facet_wrap( ~mod )
      
      q[[j]] <- 
        p2.1 %>%
        ggplot( aes( time , V1 )) +
        geom_point( ) +
        geom_errorbar( aes( ymin = V2 , ymax = V3 ) , width = 0 ) +
        geom_hline( aes( yintercept = 0 ) , linetype = 2 ) +
        facet_wrap( ~mod )
    
      g_p <- lmtest::grangertest(t[Y][[1]] ~ t[C][[1]])
      grangertable <- 
        grangertable %>% 
        mutate(change = (source == i & (frame == Y) & (frame_is == 'dependent'))) %>% 
        mutate(granger_p = ifelse(change, g_p$`Pr(>F)`, granger_p))
        
      g_q <- lmtest::grangertest(t[C][[1]] ~ t[Y][[1]])
      grangertable <- 
        grangertable %>% 
        mutate(change = (source == i & (frame == Y) & (frame_is == 'independent'))) %>% 
        mutate(granger_p = ifelse(change, g_q$`Pr(>F)`, granger_p))

    }
  }
  
  gridExtra::grid.arrange(grobs = p, nrow = 8) %>% 
    ggsave(filename = here(paste0('_sc/_var/', dependent, '_frames_dv_var.png')),
         width = 24, height = 20)
  
  gridExtra::grid.arrange(grobs = q, nrow = 8) %>% 
    ggsave(filename = here(paste0('_sc/_var/', dependent, '_frames_iv_var.png')),
           width = 24, height = 20)
}

grangertable <- 
  grangertable %>% 
  dplyr::select(!change)

save(grangertable,
     file = here('_dt/grangertable.Rdata'))

