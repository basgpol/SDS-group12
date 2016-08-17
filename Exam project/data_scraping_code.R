########################################################################################
################################### Test Project #######################################
########################################################################################

#############################################################################################
## NOTES

#########DATA SCRAPPING#########
## DONE:
# created code to gather stats for field players
# code to gather transferdata
# code to combine all the specific datas?ts
# converting transfer.fee to numeric value 

##DOING
#creating club categories

## TO DO:
# need to create code til gather stats for goalkeepers players (modification)
# need code to add google searches

#########VISUALISATION#########
##TO DO
#average transfer value per league, 
#age vs transfer value, 
#position vs transfer value, 
#goals per minute vs transfer value
#visualise the best models

#########MODELISATION#########
##TO DO
#Linear model
#LASSO and RIDGE
#Cross validation using Kfold
#Decision tree
#Random forest
#test and compare

#############################################################################################
## CODE

##=========================================================================================
##------------------------- Table of contents ---------------------------------------------
##=========================================================================================
# 1. Data gathering
#   1.1 Loading R-packages
#   1.2 Defining key links
#   1.3 Defining CSS Selectors
#   1.4 Bulding functions (link collection and scraping)
#   1.5 Applying functions to generate links
#   1.6 Applying functions to scrape the performance and transfer stats
#   1.7 Importing table rangig for the major leauges in season 14/15
#   1.8 Mergig data frames
# 2. Cleaning
#   2.1 Cleaning player data and creating useful predictors
#   2.2 Cleaning club data and creating useful predictors

##=========================================================================================
##------------------------- 1. Data Gathering--- ------------------------------------------
##=========================================================================================

##========================== 1.1 Loading R-packages =======================================

library("rvest")
library("stringr")
library("purrr")
library("dplyr")

##========================== 1.2 Defining key links =======================================

## Links from transfermarkt.co.uk to overviews of transfers in season 14/15 in  the five major football leagues  
base.link = "http://www.transfermarkt.co.uk/"
pl.tranfers.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
bl.transfer.link = "http://www.transfermarkt.co.uk/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
ll.transfer.link = "http://www.transfermarkt.co.uk/laliga/transfers/wettbewerb/ES1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
sa.tranfer.link = "http://www.transfermarkt.co.uk/serie-a/transfers/wettbewerb/IT1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
l1.transfer.link = "http://www.transfermarkt.co.uk/ligue-1/transfers/wettbewerb/FR1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
all.transferlinks = c(pl.tranfers.link, bl.transfer.link, ll.transfer.link, sa.tranfer.link, l1.transfer.link)

## Links from wikipedia.com sites for info om final tables in the 
pl.table14.link = "https://en.wikipedia.org/wiki/2014-15_Premier_League"
bl.table14.link = "https://en.wikipedia.org/wiki/2014-15_Bundesliga"
ll.table14.link = "https://en.wikipedia.org/wiki/2014-15_La_Liga"
sa.table14.link = "https://en.wikipedia.org/wiki/2014-15_Serie_A"
l1.table14.link = "https://en.wikipedia.org/wiki/2014-15_Ligue_1"

##========================== 1.3 Defining CSS selectors =======================================

## Define CSS selectors from transfermarkt
css.selector.profile = ".table-header+ .responsive-table .hide-for-small .spielprofil_tooltip"
css.selector.transfer = ".table-header+ .responsive-table .rechts a"
# (delete if no problem) css.selector.table = ".responsive-table .hauptlink .vereinprofil_tooltip"

## Define CSS selectors for league tables from Wikipedia
css.pl.table14 = ".wikitable:nth-child(26)"
css.bl.table14 = ".wikitable:nth-child(22)"
css.ll.table14 = ".wikitable:nth-child(29)"
css.sa.table14 = ".wikitable:nth-child(28)"
css.l1.table14 = ".wikitable:nth-child(19)"


##================= 1.4 Bulding functions (link collection and scraping) =====================

## Function 1: creating collector-function that finds all the links to transfered players in all major european football leagues
link.collector = function(vector){
  out = vector %>% 
    read_html(encoding = "UTF-8") %>% #inddrager special danish character 
    html_nodes(css = css.selector.profile) %>%
    html_attr(name = 'href') #tager den attribut med navnet hret
  return (out)
}

## Function 2: creating collector-function that finds all the links to all transfers in the major european football leagues
link.collector2 = function(vector){
  out = vector %>% 
    read_html(encoding = "UTF-8") %>% #inddrager special danish character 
    html_nodes(css = css.selector.transfer) %>%
    html_attr(name = 'href') #tager den attribut med navnet hret
  return (out)
}

## Function 3: Creating scraper-function to scrape all performance stats from detailed stat page
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

## Function 4: Creating scraper-function to scrape all transfer stats
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
  club.to = my.link %>% 
    html_nodes(".no-border-links br+ .vereinprofil_tooltip") %>% 
    html_text()
  club.to = club.to[1]
  transfer.fee = my.link %>% 
    html_nodes(".hauptfact") %>% 
    html_text()
  transfer.fee = transfer.fee[1]
  return(data.frame(name = name,
                    transfer.date = transfer.date,
                    club.from = club.from,
                    club.to = club.to,
                    transfer.fee = transfer.fee
  ))}

##===================== 1.5 Applying functions to generate links =============================

# applying function 1 and thereby creating a vector of all the links to transfered players
all.tranferlinks.partly = lapply(all.transferlinks, link.collector)
all.profiles.partly = unlist(all.tranferlinks.partly) # transform from list to vector
profile.links = paste(base.link,all.profiles.partly, sep ="") # creating full link
profile.links[1:300] # showing the first 300 links

##creates vector with links to all the players stat page
player.stats.links = str_replace(profile.links,"profil","leistungsdaten") 
player.stats.links[1:200]

## creates vector with links to all the players detailed stat page for season 14/15
season.stat.links = paste(player.stats.links,"/plus/1?saison=2014", sep = "")
season.stat.links[1:200] # showing the first 200 links to transfered players performance stats for 14/15

# applying the function 2 and thereby creating a vector of all the links to transfer details
all.tranferlinks2.partly = lapply(all.transferlinks, link.collector2)
all.transfers.partly = unlist(all.tranferlinks2.partly) # transform from list to vector

## merging to get the full links to the transfer details
transfer.links = paste(base.link,all.transfers.partly, sep ="")
transfer.links[100:200]

##=========== 1.6 Applying functions to scrape the performance and transfer stats ============

## Create data frame with performance stats using function 3
player.stats.season = season.stat.links[150:200]  %>% 
  map_df(scrape_playerstats)

## Create data frame with transfer stats using function 4
transfer.stats = transfer.links[150:200]  %>% 
  map_df(scrape_transferstats)

##============ 1.7 Importing table rangig for the major leauges in season 14/15 ==============

pl.table14 = pl.table14.link %>%
  read_html() %>% 
  html_node(css.pl.table14) %>% 
  html_table() %>%  # then convert the HTML table into a data frame
  mutate(league = "Premier league") # adding a new column with the league name

bl.table14 = bl.table14.link %>%
  read_html() %>% 
  html_node(css.bl.table14) %>% 
  html_table() %>%  
  mutate(league = "Bundesliga")
            
ll.table14 = ll.table14.link %>%
  read_html() %>% 
  html_node(css.ll.table14) %>% 
  html_table() %>%
  mutate(league = "La Liga")
            
sa.table14 = sa.table14.link %>%
  read_html() %>% 
  html_node(css.sa.table14) %>% 
  html_table() %>%  
  mutate(league = "Serie A")
            
l1.table14 = l1.table14.link %>%
  read_html() %>% 
  html_node(css.l1.table14) %>% 
  html_table() %>% 
  mutate(league = "Ligue 1")
            


##============================ 1.8 Merging data frames ====================================

###Changing pound character and converting transfer fee to numeric value
####!!!!! TO change to right dataset !!!#####


## merging the performance and transfer data frames into one player data frame
player_data = left_join(transfer.stats, player.stats.season)

## merging the league specific data frames into one club data frame
club.data = rbind(pl.table14, bl.table14, ll.table14, sa.table14, l1.table14)





##=========================================================================================
##--------------------------------- 2. Cleaning -------------------------------------------
##=========================================================================================

##================ 2.1 Cleaning player data and creating useful predictors ================

###Changing pound character and converting transfer fee to numeric value

player_data_clean<- player_data

###Removing pound character
player_data_clean$transfer.fee = str_replace(player_data_clean$transfer.fee,"£","")

###Transforming free transfer to O (not used because we want to remove these obs)
#player_data_2$transfer.fee = str_replace(player_data_2$transfer.fee,"Free transfer","0")

###Converting to numeric value
player_data_clean$transfer.fee = str_replace(player_data_clean$transfer.fee,"\\.","") #removing the dots
player_data_clean$transfer.fee = str_replace(player_data_clean$transfer.fee,"m","0000") #removing the m 
player_data_clean$transfer.fee = str_replace(player_data_clean$transfer.fee,"k","000") #removing the k

player_data_clean = transform(player_data_clean, transfer.fee = as.numeric(transfer.fee)) #convert character string in numeric string
head(player_data_clean)



##================ 2.2 Cleaning club data and creating useful predictors ================

## Giving different status to different clubs depending on which position they ended at in the league.
attach(club.data)
club.data$Status[Pos <= 5] = "Top Club"
club.data$Status[Pos <= 15 & Pos > 5] = "Middle Club"
club.data$Status[Pos >= 16] = "Buttom Club"
detach(club.data)


