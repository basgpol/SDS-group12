########################################################################################
################################### Test Project #######################################
########################################################################################

#############################################################################################
## NOTES

## DONE:
# created code to gather stats for field players
# code to gather transferdata

## TO DO:
# need to create code til gather stats for defensive players (modification)
# code to combine alle the specific datas?ts
# code to combine alle the specific datas?ts

## MISSING:
# need to create code til gather stats for goalkeepers players (modification)
# need code to add google searches


## load libraries
library("rvest")
library("stringr")
library("purrr")
library("dplyr")

## defines key links
base.link = "http://www.transfermarkt.co.uk/"
pl.tranfers.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
bl.transfer.link = "http://www.transfermarkt.co.uk/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
ll.transfer.link = "http://www.transfermarkt.co.uk/laliga/transfers/wettbewerb/ES1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
sa.tranfer.link = "http://www.transfermarkt.co.uk/serie-a/transfers/wettbewerb/IT1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
l1.transfer.link = "http://www.transfermarkt.co.uk/ligue-1/transfers/wettbewerb/FR1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
css.selector.profile = ".table-header+ .responsive-table .spielprofil_tooltip"
css.selector.transfer = ".table-header+ .responsive-table .rechts a"
all.transferlinks = c(pl.tranfers.link, bl.transfer.link, ll.transfer.link, sa.tranfer.link, l1.transfer.link)

## creating a function that finds all the links to transfered players in all major european football leagues
link.collector = function(vector){
  out = vector %>% 
    read_html(encoding = "UTF-8") %>% #inddrager special danish character 
    html_nodes(css = css.selector.profile) %>%
    html_attr(name = 'href') #tager den attribut med navnet hret
  return (out)
}

# applying the function and thereby creating a vector of all the links to transfered players
all.tranferlinks.partly = lapply(all.transferlinks, link.collector)
all.profiles.partly = unlist(all.tranferlinks.partly) # transform from list to vector
all.profile.links.partly.clean = all.profiles.partly[c(TRUE, FALSE)] # removing copies of links
all.profile.links.partly.clean

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

profile.links = paste(base.link,all.profile.links.partly.clean, sep ="")
profile.links[1:200]


##creates vector with links to all the players stat page
player.stats.links = str_replace(profile.links,"profil","leistungsdaten") 
player.stats.links[1:200]

## creates vector with links to all the players detailed stat page for season 14/15
season.stat.links = paste(player.stats.links,"/plus/1?saison=2014", sep = "")
season.stat.links[1:200]

## Create function to find player stats
scrape_playerstats = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  name = my.link %>% 
    html_nodes("h1") %>% 
    html_text()
  name = name[1]
  position = my.link %>% 
    html_nodes(".dataDaten:nth-child(2) p:nth-child(2) .dataValue") %>% 
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
  secondyellow = my.link %>% 
    html_nodes("tfoot .zentriert:nth-child(10)") %>% 
    html_text()
  secondyellow = secondyellow[1]
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
                    secondyellow = secondyellow,
                    redcards = redcards,
                    penaltygoals = penaltygoals,
                    minutes.pr.goal = minutes.pr.goal,
                    total.minutes.played = minutes.played
  ))}


## Create data frame with player states by using function
player.stats.season = season.stat.links[190:200]  %>% 
  map_df(scrape_playerstats)

######### Transferdata

## creating a function that finds all the links to all transfers in the major european football leagues
link.collector2 = function(vector){
  out = vector %>% 
    read_html(encoding = "UTF-8") %>% #inddrager special danish character 
    html_nodes(css = css.selector.transfer) %>%
    html_attr(name = 'href') #tager den attribut med navnet hret
  return (out)
}

# applying the function and thereby creating a vector of alle the links to transfered players
all.tranferlinks2.partly = lapply(all.transferlinks, link.collector2)
all.transfers.partly = unlist(all.tranferlinks2.partly) # transform from list to vector

## merging to get the full links to the transfers
transfer.links = paste(base.link,all.transfers.partly, sep ="")
transfer.links[190:200]

## Create function to find transfer stats
scrape_transferstats = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  name = my.link %>% 
    html_nodes("h1") %>% 
    html_text()
  name = name[1]
  transfer.date = my.link %>% 
    html_nodes(".table-highlight a") %>% 
    html_text()
  transfer.date = transfer.date[1]
  club.from = my.link %>% 
    html_nodes(".no-border-rechts br+ .vereinprofil_tooltip") %>% 
    html_text()
  club.from = club.from[1]
  transfer.fee = my.link %>% 
    html_nodes(".hauptfact") %>% 
    html_text()
  transfer.fee = transfer.fee[1]
  return(data.frame(name = name,
                    transfer.date = transfer.date,
                    club.from = club.from,
                    transfer.fee = transfer.fee
  ))}

transfer.stats = transfer.links[190:200]  %>% 
  map_df(scrape_transferstats)


#### Merging playing and transferstats
final_player_data = left_join(transfer.stats, player.stats.season)

