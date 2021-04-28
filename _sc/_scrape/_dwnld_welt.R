library( dplyr )
library( magrittr )
library( rvest )

#### DOWNLOAD #####
# covered: 2013-2019

out.url <- 'https://www.welt.de/schlagzeilen/nachrichten-vom-13-12-2019.html' 
url <- 'https://www.welt.de/schlagzeilen/nachrichten-vom-13-12-2019.html' 

st.t <- timestamp( )
while( !is.na( url )){
  print( url )
  
  url1 <- xml2::read_html( url )
  html.name <- stringr::str_extract( url , 'nachrichten-.*' )
  xml2::write_xml( url1 , paste0( '_dt/_var/_media/_welt_html/' , html.name ))

  url <- url1 %>%
    html_node( '.next > a:nth-child(1)' ) %>%
    html_attr( 'href' )
}
end.t <- timestamp( )
  
#### LOAD AND PREPRARE #####
# HERE ARE ALSO DOWNLOAD LINKS TO ARTICLES - NEED TO BE EXTRACTED

f.l <- list.files( '_dt/_var/_media/_welt_html/' )

out <- tibble::tibble( url = as.character( ) , ressort = as.character( ) , title  = as.character( ) , link = as.character( ))

count.no = 0 
for( f in f.l[ 2560 : 2591 ] ){
  count.no <- count.no + 1
  print( count.no )
  print( f )
  url1 <- xml2::read_html( x = paste0( '_dt/_var/_media/_welt_html/' , f ))
  for( i in seq( 1 : 200 )){
    print( i )
    ressort <- url1 %>%
      html_node( paste0( 'div.article:nth-child(' , i , ') > div:nth-child(2) > div:nth-child(1) > span:nth-child(1)' )) %>%
      html_text()
    
    title <- url1 %>%
      html_node( paste0( 'div.article:nth-child(' , i , ') > div:nth-child(2) > h4:nth-child(2)' )) %>%
      html_text()
    
    link <- url1 %>%
      html_node( paste0( 'div.article:nth-child(' , i , ') > div:nth-child(2) > h4:nth-child(2) > a:nth-child(1)' )) %>%
      html_attr( 'href' )
    out <- rbind( out , cbind( url = f , ressort , title , link ))
  }
}

# Qualitaetskontrolle

welt_all <- out
save( welt_all , file = '_tx/_apsr_rr/_dt/_var/_media/weltonline_raw.Rdata' )

load( '_dt/_var/_media/weltonline_raw.Rdata' )
welt_all %<>% unique( )
welt_all %<>% na.omit( )

head( welt_all )

welt_all %<>% mutate( date = stringr::str_remove( url , 'nachrichten-vom-' )
                     , date = stringr::str_remove( date , '.html' )
                     , date = as.Date( date , '%d-%m-%Y' )
                     ) 
d <- welt_all %>% select( date ) %>% unique( ) %>% arrange( date )
summary( d$date )
as.Date( '2020-02-04' ) - as.Date( '2013-01-01' ) # corresponds to length of dataset!

welt_all %<>% group_by( date ) %>% mutate( tot.no = max( seq_along( date ))) %>% ungroup( )

tmp1 <- welt_all %>% mutate( title1 = stringr::str_detect( pattern = "AfD|Höcke|Gauland|Petry|Storch|Weidel" , string = title )) %>%
  mutate( title2 = stringr::str_detect( pattern = "Flüchtl|ausl|Auslä|Asyl|Migra|migra" , string = title )) %>%
  mutate( title3 = stringr::str_detect( pattern = "Anschl|Attent|attent|anschl|NSU" , string = title ))

tmp1 %<>%
  mutate( welt.afd = ifelse( title1 == TRUE , 1 , 0 )) %>%
  mutate( welt.afd.immig = ifelse( title1 == TRUE | title2 == TRUE , 1 , 0 )) %>%
  mutate( welt.afd.immig.att = ifelse( title1 == TRUE | title2 == TRUE | title3 == TRUE , 1 , 0 )) %>%
  mutate( welt.afd.immig.minusatt = ifelse(( title1 == TRUE | title2 == TRUE ) & title3 == FALSE , 1 , 0 )) %>%
  mutate( welt.immig.minusafd.minusatt = ifelse( title1 == FALSE & title2 == TRUE & title3 == FALSE , 1 , 0 )) %>%
  group_by( date ) %>%
  mutate_at( vars( welt.afd : welt.immig.minusafd.minusatt ) , funs( sum( . , na.rm = TRUE ))) %>%
  ungroup( ) %>%
  select( date , welt.afd : welt.immig.minusafd.minusatt , tot.no ) %>%
  unique( )

table( tmp1$welt.afd )
table( tmp1$welt.afd.immig )
table( tmp1$welt.afd.immig.att )
table( tmp1$welt.afd.immig.minusatt )
table( tmp1$welt.immig.minusafd.minusatt )
welt <- tmp1 

save( welt , file = '_dt/_var/_media/_welt_count.Rdata' )
