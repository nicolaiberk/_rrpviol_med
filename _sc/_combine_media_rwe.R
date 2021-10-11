rm( list = ls( ))
cat( '\14' )

library( magrittr )
library( dplyr )

tryCatch( setwd( '/Users/krausewz/Dropbox (Maestral)/_git.pprs/_rrpviol_med/' ))
load( '_dt/_mig_estimates/daily_media_attention.Rdata' )

head( media_attention )

# Add RTV Data -----

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


# !!! Attention lots of subsets are possible here. E.g. type or target !!! 

d <- rtv %>% rename( date_new = date ) %>% 
  dplyr::select( date_new , rtv_killed , rtv_wounded , rtv_att ) %>%
  filter( !is.na( date_new )) %>%
  group_by( date_new ) %>% mutate_at( vars( rtv_killed : rtv_att ) , funs( sum( as.numeric( as.character( . )) , na.rm = TRUE ))) %>% ungroup( ) %>%
  unique( ) %>%
  right_join( media_attention )

head( d ) # new variables: rtv_killed, rtv_wounded, rtv_att

rm( media_attention , rtv )

# GTD coding ----

load( '_dt/_out/_daily.Rdata')
head( w )
d <- w %>% 
  mutate( date_new = as.Date( date2 )) %>%
  select( date_new , gtd_att = natt , gtd_kill = nkill , gtd_wound = nwound ) %>% unique( ) %>%
  right_join( d )
rm( w )  

# Right-Wing Extremist Crime (Monthly Data) ----

rwec <- readr::read_csv( '/Users/krausewz/_hu_box/_gits/_pprs/_21_rrpviol_med_active/_rrpviol_med/_dt/rr_crim_petrapau/akt. Datensatz rechtsextremistische Kriminalität nach Bundesländern.csv' )

rwec %<>% filter( stringr::str_detect( Time , 'gesamt' ) == FALSE )
rwec %<>% mutate( Time = as.Date( Time , '%d/%m/%y' )
                , year = as.numeric( format( Time , '%Y' ))
                , month = as.numeric( format( Time , '%m' ))) %>%
  select( year, month 
          , pp_all = `Pol. rechts motiviert (alle Straftaten)`
          , pp_viol_attacks = Gewalttaten 
          , pp_killed = Todesopfer
          , pp_wounded = `Verletzte Pers.`
          , pp_koerperverl = Körperverl.
          , pp_brandstiftung = Brandstiftung
          , pp_landfriedensbruch = Landfriedensb.
          , pp_tatverdaechtige = Tatverdächtige
          )

d %<>% mutate( year = as.numeric( format( date_new , '%Y' ))
                       , month = as.numeric( format( date_new , '%m' )))

d %<>% left_join( rwec ) 

# !!! "pp_" are monthly values   !!! 







# Save -----

save( d , file =  '_dt/daily_combined.Rdate' )

