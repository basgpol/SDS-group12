########################################################################################
################################### Test Project #######################################
########################################################################################

#############################################################################################
## NOTES

## DONE:
# created code to gather stats for offensive players

## missing:
# code to gather transferdata
# need to create code til gather stats for defensive players (modification)
# code to combine alle the specific datasæts



## load libraries
library("rvest")
library("stringr")
library("purrr")


## defines key links
base.link = "http://www.transfermarkt.co.uk/"
pl.tranfers.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
css.selector.profile = ".table-header+ .responsive-table .spielprofil_tooltip"


# helper function
helper_function = function(link, css.select){
  out = link %>% 
    html_nodes(css.select) %>% 
    html_text
  return(out)
}


# scraping function for scrabing tranfer infomation
scrape_transfer_info = function(link){
  parsed.link = link %>% 
    read_html
  name = parsed.link %>% 
    helper_function(".table-header+ .responsive-table .spielprofil_tooltip")
  #position = parsed.link %>% 
    #helper_function(".table-header+ .responsive-table .pos-transfer-cell")
  from.club = parsed.link %>% 
    helper_function(".table-header+ .responsive-table .verein-flagge-transfer-cell .vereinprofil_tooltip")
  transfer.fee = parsed.link %>% 
    helper_function(".responsive-table:nth-child(2) .rechts a")
  return(
    data.frame(name = name, 
               from.club = from.club,
               transfer.fee = transfer.fee)
  )}

# iterate over links, extract data, bind rows
transferPL.df = pl.tranfers.link %>% 
  map_df(scrape_transfer_info)



########################################

## Creates vector that indclude all links to player profiles who were transfered to PL
profile.links.partly = pl.tranfers.link %>% 
  read_html(encoding = "UTF-8") %>% #inddrager special danish character 
  html_nodes(css = css.selector.profile) %>%
  html_attr(name = 'href') #tager den attribut med navnet hret
profile.links.partly
profile.links.partly.clean = profile.links.partly[c(TRUE, FALSE)] # Remove every second character, so we have only on for each player

profile.links = paste(base.link,profile.links.partly.clean, sep ="")
profile.links[1:7]

##creates vector with links to all the players stat page
player.stats.links = str_replace(profile.links,"profil","leistungsdaten") 
player.stats.links[1:5]

## creates vector with links to all the players detailed stat page
detailed.stat.links = paste(player.stats.links,"/saison//plus/1#gesamt", sep = "")
detailed.stat.links


## Create function to find player states
scrape_playerstats = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  name = my.link %>% 
    html_nodes("h1") %>% 
    html_text()
  name = name[1]
  position = my.link %>% 
    html_nodes(".dataDaten:nth-child(2) p+ p .dataValue") %>% 
    html_text()
  position = position[1]
  age = my.link %>% 
    html_nodes(".dataDaten:nth-child(1) p:nth-child(1) .dataValue") %>% 
    html_text() 
  age = age[1]
  nationality = my.link %>% 
    html_nodes(".hide-for-small+ p .dataValue span") %>% 
    html_text()
  nationality = nationality[1]
  apps = my.link %>% 
    html_nodes(".hide+ td") %>% 
    html_text()
  apps = apps[1]
  goals = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(4)") %>% 
    html_text()
  goals = goals[1]
  assists = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(5)") %>% 
    html_text()
  assists = assists[1]
  subs_in = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(7)") %>% 
    html_text()
  subs_in = subs_in[1]
  subs_out = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(8)") %>% 
    html_text()
  subs_out = subs_in[1]
  yellowcards = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(9)") %>% 
    html_text()
  yellowcards = yellowcards[1]
  redcards = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(11)") %>% 
    html_text()
  redcards = redcards[1]
  penaltygoals = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(12)") %>% 
    html_text()
  penaltygoals = penaltygoals[1]
  minutes.pr.goal = my.link %>% 
    html_nodes("tfoot .zentriert+ .rechts") %>% 
    html_text()
  minutes.pr.goal = minutes.pr.goal[1]
  minutes.played = my.link %>% 
    html_nodes("tfoot .rechts+ .rechts") %>% 
    html_text()
  minutes.played = minutes.played[1]
  return(data.frame(name = name,
                    positions = position,
                    age = age,
                    nationality = nationality,
                    appearances = apps,
                    total.goals = goals,
                    total.assists = assists,
                    substitutions_in = subs_in,
                    substitutions_out = subs_out,
                    yellowcards = yellowcards,
                    redcards = redcards,
                    penaltygoals = penaltygoals,
                    minutes.pr.goal = minutes.pr.goal,
                    total.minutes.played = minutes.played
                    ))}


## Create data frame with player states by using function
player.stats = detailed.stat.links[1:10]  %>% 
  map_df(scrape_playerstats)



