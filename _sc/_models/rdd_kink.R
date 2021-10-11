rm( list = ls( ))
cat( '\14' )

library( magrittr )
library( dplyr )

tryCatch( setwd( '/Users/krausewz/Dropbox (Maestral)/_git.pprs/_rrpviol_med/' ))
tryCatch( setwd( '/Users/base/Dropbox (Maestral)/_git.pprs/_rrpviol_med/' ))
load( '_dt/daily_combined.Rdate' )

head( d )
d$paper <- as.character( d$paper )

summary( d$date_new )
d %>% group_by( date_new ) %>% filter( max( row_number( )) != 6 )
d %>% filter( !is.na( gtd_att )) %>% tail( ) # Ends: '2018-12-31'
d %>% filter( !is.na( rtv_att )) %>% tail( ) # Ends: '2019-12-31'


# Make Sure Each Source is present every day -----

d %<>% filter( date_new >= '2013-01-01' & date_new < '2020-01-01')
d <- expand.grid( paper = unique( d$paper ) , date_new = seq.Date( as.Date( '2013-01-01' ) , as.Date( '2019-12-31' ) , 1 ) ) %>% 
  as.data.frame( ) %>%
  full_join( d )

# RDD & Kink -------


transform.d <- function( data
                         , event.var # attack indicator, e.g. rtv_att
){
  # data = d 
  # event.var = 'rtv_att'
  
  if( event.var %in% c( 'rtv_att' , 'rtv_killed' , 'rtv_wounded' )){
    data %<>% filter( date_new >= '2013-01-01' & date_new < '2020-01-01')
  } else {
    data %<>% filter( date_new >= '2013-01-01' & date_new < '2019-01-01')
    
  }
  
  data$event.var <- data[ , event.var ]
  
  data %<>% 
    select( date_new , paper , event.var , n_mig : capcrime ) %>%
    mutate( event.var = ifelse( is.na( event.var ) , 0 , event.var )) %>% # NA to for attack scores
    mutate_at( vars( n_mig : capcrime ) , funs( ifelse( is.na( . ) , 0 , . ))) # NA to 0 for newspaper scores
  
  if( exists( 'rdd.rd' )){ rm( rdd.rd )} # remove 
  count <- 1
  
  for( w in data %>% filter( event.var > 0 ) %>%
       mutate( date_new = as.character( date_new )) %>%
       dplyr::select( date_new ) %>% unique( ) %>% as.list( ) %>% unlist( )){
    i <- as.Date( w )
    d.min <- i - 30
    d.max <- i + 30
    
    t.ds <- data %>%
      filter( date_new >= d.min & date_new <= d.max ) %>%
      mutate( treat_day = ifelse( date_new == i , 1 , 0 )
              , treated = ifelse( date_new >= i , 1 , 0 )  # !!! Attention: Could also be "> i" dependent on understanding of treatment day
              , attack_id = count
              , force = date_new - i
      ) %>% arrange( date_new )
    
    t.ds %<>%
      dplyr::select( attack_id , force , date_new , treat_day , treated 
                     , paper
                     , n_mig : capcrime ) %>%
      mutate( share_mig = n_mig / n_tot )

    t.ds %<>%
      mutate( attack_id = count 
              , force = date_new - i
      ) 
    
    if( exists( 'rdd.rd' )){
      rdd.rd <- rbind( rdd.rd , t.ds )
    } else {
      rdd.rd <- t.ds
    }
    
    count <- count + 1 
    rm( t.ds , d.min , d.max )
  }
  rm( i )

  
  rdd.rd <- cbind( rdd.rd , fastDummies::dummy_cols( rdd.rd$paper ))
  
  return( rdd.rd )
  
}


rdd.rtv.natt <- transform.d( d , 'rtv_att' )
rdd.gtd.natt <- transform.d( d , 'gtd_att' )

rdd.rtv.kill <- transform.d( d , 'rtv_killed' )
rdd.gtd.kill <- transform.d( d , 'gtd_kill' )

rdd.rtv.wound <- transform.d( d , 'rtv_wounded' )
rdd.gtd.wound <- transform.d( d , 'gtd_wound' )

# RDD ------

summary( lm( n_mig ~ force*treated + paper , rdd.rtv.natt ))


rdrobust.core <- function( data , outcome , p , q , deriv ){
  data$outcome <- data[ , outcome ]
  out <- rdrobust::rdrobust( y = data$outcome , x = data$force , c = 0
                             , deriv = deriv
                             , p = p 
                             , q = q 
                             , covs = cbind( data$.data_bild 
                                             , data$.data_faz
                                             , data$.data_spon
                                             , data$.data_sz
                                             , data$.data_taz
                                             , data$.data_welt ))
  }

mod.rdd.rtv.natt <- rdrobust.core( rdd.rtv.natt , 'n_mig' , p = 1 , q = 2 , deriv = 0 )
mod.rdd.gtd.natt <- rdrobust.core( rdd.gtd.natt , 'n_mig' , p = 1 , q = 2 , deriv = 0 )

summary( mod.rdd.rtv.natt ) # no significant LATE
summary( mod.rdd.gtd.natt ) # no significant LATE

# Kink -------

mod.kink.rtv.natt <- rdrobust.core( rdd.rtv.natt , 'n_mig' , p = 1 , q = 2 , deriv = 1 )
mod.kink.rtv.kill <- rdrobust.core( rdd.rtv.kill , 'n_mig' , p = 1 , q = 2 , deriv = 1 )
mod.kink.rtv.wound <- rdrobust.core( rdd.rtv.wound , 'n_mig' , p = 1 , q = 2 , deriv = 1 )
mod.kink.gtd.natt <- rdrobust.core( rdd.gtd.natt , 'n_mig' , p = 1 , q = 2 , deriv = 1 )
mod.kink.gtd.kill <- rdrobust.core( rdd.gtd.kill , 'n_mig' , p = 1 , q = 2 , deriv = 1 )
mod.kink.gtd.wound <- rdrobust.core( rdd.gtd.wound , 'n_mig' , p = 1 , q = 2 , deriv = 1 )

summary( mod.kink.rtv.natt )  # significant Effect
summary( mod.kink.rtv.kill )  # not significant
summary( mod.kink.rtv.wound ) # significant Effect
summary( mod.kink.gtd.natt )  # significant Effect
summary( mod.kink.gtd.kill )  # not significant
summary( mod.kink.gtd.wound ) # not significant, but correct direction

mod.kink.rtv.natt.share_mig <- rdrobust.core( rdd.rtv.natt , 'share_mig' , p = 1 , q = 2 , deriv = 1 )
summary( mod.kink.rtv.natt.share_mig ) # not significant
mod.kink.gtd.natt.share_mig <- rdrobust.core( rdd.gtd.natt , 'share_mig' , p = 1 , q = 2 , deriv = 1 )
summary( mod.kink.gtd.natt.share_mig ) # not significant

for( i in c( 'crime' , 'medit' , 'deport' , 'refnums' , 'camps' , 'labmar' , 'capcrime' )){
  print( i )
  out <- rdrobust.core( rdd.rtv.natt , i , p = 1 , q = 2 , deriv = 1 )
  summary( out ) 
} # significant effect: labmar

for( i in c( 'crime' , 'medit' , 'deport' , 'refnums' , 'camps' , 'labmar' , 'capcrime' )){
  print( i )
  out <- rdrobust.core( rdd.gtd.natt , i , p = 1 , q = 2 , deriv = 1 )
  summary( out ) 
} # significant effect: crime, deport, refnums, capcrime
  # borderline significant effect: camps, labmar







