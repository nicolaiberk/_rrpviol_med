# ______________________________________________
# Media reactions to RR violence
# Goal: Estimate VAR for migration frames
# Procedure: load data, merge, estimate var
# ______________________________________________
# Date:  Mon Sep 06 17:21:46 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________

cat( '\14' )
rm( list = ls( ))

# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(vars)
library(tseries)
library( ggplot2 )

setwd( '/Users/krausewz/Dropbox (Maestral)/_git.pprs/_rrpviol_med/' )

# load data ####
load( '_dt/_mig_estimates/mig_articles_topics.Rdata')
load( '_dt/_out/_daily.Rdata')

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

pp <- readr::read_csv( '/Users/krausewz/_hu_box/_gits/_pprs/_21_rrpviol_med_active/_rrpviol_med/_dt/rr_crim_petrapau/akt. Datensatz rechtsextremistische Kriminalität nach Bundesländern.csv' )

pp %<>% filter( stringr::str_detect( Time , 'gesamt' ) == FALSE )
pp %<>% mutate( Time = as.Date( Time , '%d/%m/%y' )
                , year = as.numeric( format( Time , '%Y' ))
                , month = as.numeric( format( Time , '%m' )))

fulltable %<>% mutate( year = as.numeric( format( date2 , '%Y' ))
                , month = as.numeric( format( date2 , '%m' )))

fulltable %<>% left_join( pp )
summary( fulltable$`Pol. rechts motiviert (alle Straftaten)` )

fulltable.month <- fulltable %>%
  group_by( year , month , source ) %>%
  mutate_at( vars( crime_tot : n_mig , crime_share : capcrime_share , afd_daysource : immig.tx_daysource_share ) , funs( mean( . , na.rm = TRUE ))) %>%
  mutate_at( vars( natt : nwound ) , funs( sum( . , na.rm = TRUE ))) %>%
  ungroup(  ) %>% 
  dplyr::select( -paper , -date_new , -date2 ) %>% unique( )

fulltable.month <- cbind( fulltable.month , fastDummies::dummy_cols( fulltable.month$source ))

Y <- 'Pol. rechts motiviert (alle Straftaten)'
hist( Y )
plot( fulltable.month$Time , Y )
plot( fulltable.month$Time , fulltable.month$natt )
C <- 'n_mig'
Z <- fulltable.month %>%
  dplyr::select( `.data_bild` : `.data_welt` )


summary( vars::VAR( y = fulltable.month[, c( Y , C )] , exogen = Z ))

for( i in unique( fulltable.month$source )){
  Y <- 'Pol. rechts motiviert (alle Straftaten)'
  C <- 'n_mig'
  t <- fulltable.month %>% filter( source == i )
  
  print( i )
  print( summary( vars::VAR( y = t[, c( Y , C )])))
  
}

fulltable.month$X <- fulltable.month$`Pol. rechts motiviert (alle Straftaten)`
summary( ARDL::ardl( n_mig ~ X , fulltable.month , order = 5 , start = NULL, end = NULL ))

summary( ARDL::auto_ardl( n_mig ~ X , fulltable.month , max_order = 5 )$best_model )
summary( ARDL::auto_ardl( X ~ n_mig , fulltable.month , max_order = 12 )$best_model )


rtv <- openxlsx::read.xlsx( '/Users/krausewz/Zotero/storage/8G9A6LDB/RTV 1990-2019 full version.xlsx' ) %>%
  mutate( date = as.Date( paste( `V2:.Year` , `V3:.Month` , `V4:.Day` , sep = '-' ))) %>%
  #filter( `V5:.Country` == 15 ) %>%
  dplyr::select( country = `V5:.Country` , date , year =  `V2:.Year` , month = `V3:.Month` , day = `V4:.Day` , location = `V6:.City/village/location` 
          , type = `V7:.Incident.type` 
          # 1. Premeditated attack – incidents where perpetrators have actively pursued a predefined person or target group.
          # 2. Spontaneous attack – attacks triggered by random confrontations between perpetrator(s) and victim(s), associated with some predefined target group.
          # 3. Attack plot – planned attacks by an identifiable group or individual involving deadly weapons that were intercepted by the police before the attack was carried out.
          # 4. Preparation for armed struggle – discoveries of bomb-making materials or major arms repositories belonging to right-wing activists lacking specific attack plans.
          , perpetrator = `V8:.Perpetrator.type`
          # 1. Organised groups – known entities with five or more members whose association primarily relies on a strong commitment to right-wing politics
          # 2. Affiliated members – two or more members of organized groups acting on their own initiative
          # 3. Autonomous cells – clandestine entities of two to four members whose association primarily relies on a strong commitment to right-wing politics
          # 4. Gangs – informal groups of three or more acquaintances with a general right-wing commitment, but whose association primarily relies on social bonds
          # 5. Unorganised – two or more perpetrators with no known association to any specific right-wing group, cell, or gang
          # 6. Lone actor – single perpetrators who prepare and carry out attacks alone at their owninitiative
          # 7. Shadow groups – unresolved attacks claimed by formerly unknown groups
          # 99. unidentified perpetrator(s), but where targeting or other factors strongly indicate a farright motivation.
          , organization = `V9:.Organizational.affiliation`
          , target = `V12:.Target.group`
          # 1. Jews
          # 2. Muslims
          # 3. Immigrant/foreigner/asylum seeker/refugee
          # 4. Left-wing/anti-fascism
          # 5. Government
          # 6. LGBT+ (Lesbian, Gay, Bisexual, Transgender)
          # 7. Gypsy/Roma
          # 8. Pro-immigration activists
          # 9. Black
          # 10. Police
          # 11. Homeless/low social status
          # 12. Physically or mentally disabled
          # 13. Deserters
          # 14. Media
          # 15. Separatist
          # 16. Other
          # 99. Unknown.
          , weapon = `V14:.Weapon`
          # 1. Explosives
          # 2. Petrol bomb/fire bomb/Molotov cocktail
          # 3. Handgun
          # 4. Shotgun/rifle
          # 5. Automatic firearm
          # 6. Knife
          # 7. Letter bomb
          # 8. Beating/kicking (no weapons used)
          # 9. Tear gas
          # 10. Pepper spray
          # 11. Blunt instruments
          # 12. Chemical/biological weapon
          # 13. Arson
          # 14. Rocket launcher/grenade
          # 15. Other
          # 16. Car/vehicles
          # 99. Unknown.
          , killed = `V17:.Number.of.persons.killed`
          , wounded = `V18:.Number.of.persons.wounded`
          ) %>%
  mutate( att = 1 ) %>%
  rename_at( vars( location : att ) , funs( paste0( 'rtv_' , . )))
  

d <- rtv %>% rename( date_new = date ) %>% 
  dplyr::select( date_new , rtv_killed , rtv_wounded , rtv_att ) %>%
  filter( !is.na( date_new )) %>%
  group_by( date_new ) %>% mutate_at( vars( rtv_killed : rtv_att ) , funs( sum( as.numeric( as.character( . )) , na.rm = TRUE ))) %>% ungroup( ) %>%
  unique( ) %>%
  right_join( fulltable )

if( exists( 'rdd.rd' )){ rm( rdd.rd )}
count <- 1
for( w in d %>% filter( rtv_att > 0 ) %>%
     mutate( date_new = as.character( date_new )) %>%
     dplyr::select( date_new ) %>% unique( ) %>% as.list( ) %>% unlist( )){
  i <- as.Date( w )
  d.min <- i - 30
  d.max <- i + 30
  
  t.ds <- d %>%
    filter( date_new >= d.min & date_new <= d.max ) %>%
    mutate( treat_day = ifelse( date_new == i , 1 , 0 )
            , treated = ifelse( date_new >= i , 1 , 0 )
            , attack_id = count
            , force = date_new - i
            ) %>% arrange( date_new )
  
  t.ds %<>%
    dplyr::select( attack_id , force , date_new , treat_day , treated 
                   , source
                   , rtv_killed : n_mig 
                   , crime_share : capcrime_share 
                   , afd_daysource : immig.tx_daysource_share )
  
  t.ds <- expand.grid( date_new = seq( d.min , d.max , 1 ) , source = unique( d$source )) %>%
    as.data.frame( ) %>%
    full_join( t.ds ) %>%
    mutate( attack_id = count 
            , force = date_new - i
            , treat_day = 0
            , treated = ifelse( is.na( treated ) , lag( treated ) , treated )
            ) %>%
    mutate_at( vars( crime_tot : immig.tx_daysource_share ) , funs( ifelse( is.na( . ) , 0 , . )))
  
  if( exists( 'rdd.rd' )){
    rdd.rd <- rbind( rdd.rd , t.ds )
  } else {
    rdd.rd <- t.ds
  }
    
  count <- count + 1 
  rm( t.ds , d.min , d.max )
}
rm( i )
  


summary( lm( n_mig ~ force * treated + factor( source ) , rdd.rd ))

summary( lm( crime_tot ~ force * treated + factor( source ) , rdd.rd ))
summary( lm( crime_share ~ force * treated + factor( source ) , rdd.rd ))
summary( lm( capcrime_tot ~ force * treated + factor( source ) , rdd.rd ))
summary( lm( capcrime_share ~ force * treated + factor( source ) , rdd.rd ))

rdd.rd$capcrime_tot


