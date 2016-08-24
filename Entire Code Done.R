########################################################################################
################################### Test Project #######################################
########################################################################################


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
# 3. Vizualization 
#   3.1
#   3.2
#   3.3
#   3.4
# 4. Prediction Models
#   4.1 Dividing into a tran and test sample
#   4.2 Create evaluation function
#   4.3 Baseline Model: Simple average from training sample
#   4.4 Ordinary least square model
#   4.5 Lasso model
#   4.6 Decision tree
#   4.7 Random forrest

##=========================================================================================
##------------------------- 1. Data Gathering--- ------------------------------------------
##=========================================================================================

##========================== 1.1 Loading R-packages =======================================
library("rvest")
library("stringr")
library("purrr")
library("dplyr")
library("RCurl")
library("XML")
library("glmnet")
library("caret")
library("plotly")
library("ggplot2")
library("plotly")

#
#
# ##========================== 1.2 Defining key links =======================================
# 
# ## Links from transfermarkt.co.uk to overviews of transfers in season 14/15 in  the five major football leagues  
# base.link = "http://www.transfermarkt.co.uk/"
# pl.tranfers.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
# bl.transfer.link = "http://www.transfermarkt.co.uk/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
# ll.transfer.link = "http://www.transfermarkt.co.uk/laliga/transfers/wettbewerb/ES1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
# sa.tranfer.link = "http://www.transfermarkt.co.uk/serie-a/transfers/wettbewerb/IT1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
# l1.transfer.link = "http://www.transfermarkt.co.uk/ligue-1/transfers/wettbewerb/FR1/plus/?saison_id=2015&s_w=&leihe=0&intern=0"
# all.transferlinks = c(pl.tranfers.link, bl.transfer.link, ll.transfer.link, sa.tranfer.link, l1.transfer.link)
# 
# ## Links from wikipedia.com sites for info om final tables in the 
# pl.table14.link = "https://en.wikipedia.org/wiki/2014-15_Premier_League"
# bl.table14.link = "https://en.wikipedia.org/wiki/2014-15_Bundesliga"
# ll.table14.link = "https://en.wikipedia.org/wiki/2014-15_La_Liga"
# sa.table14.link = "https://en.wikipedia.org/wiki/2014-15_Serie_A"
# l1.table14.link = "https://en.wikipedia.org/wiki/2014-15_Ligue_1"
# 
# ##========================== 1.3 Defining CSS selectors =======================================
# 
# ## Define CSS selectors from transfermarkt
# css.selector.profile = ".table-header+ .responsive-table .hide-for-small .spielprofil_tooltip"
# css.selector.transfer = ".table-header+ .responsive-table .rechts a"
# # (delete if no problem) css.selector.table = ".responsive-table .hauptlink .vereinprofil_tooltip"
# 
# ## Define CSS selectors for league tables from Wikipedia
# css.pl.table14 = ".wikitable:nth-child(26)"
# css.bl.table14 = ".wikitable:nth-child(22)"
# css.ll.table14 = ".wikitable:nth-child(29)"
# css.sa.table14 = ".wikitable:nth-child(28)"
# css.l1.table14 = ".wikitable:nth-child(19)"
# 
# 
# ##================= 1.4 Bulding functions (link collection and scraping) =====================
# 
# ## Function 1: creating collector-function that finds all the links to transfered players in all major european football leagues
# link.collector = function(vector){
#   out = vector %>% 
#     read_html(encoding = "UTF-8") %>% #inddrager special danish character 
#     html_nodes(css = css.selector.profile) %>%
#     html_attr(name = 'href') #tager den attribut med navnet hret
#   return (out)
# }
# 
# ## Function 2: creating collector-function that finds all the links to all transfers in the major european football leagues
# link.collector2 = function(vector){
#   out = vector %>% 
#     read_html(encoding = "UTF-8") %>% #inddrager special danish character 
#     html_nodes(css = css.selector.transfer) %>%
#     html_attr(name = 'href') #tager den attribut med navnet hret
#   return (out)
# }
# 
# ## Function 3: Creating scraper-function to scrape all performance stats from detailed stat page
# scrape_playerstats = function(link){
#   my.link = link %>% 
#     read_html(encoding = "UTF-8")
#   name = my.link %>% 
#     html_nodes("h1") %>% 
#     html_text()
#   name = name[1]
#   position = my.link %>% 
#     html_nodes(".dataDaten:nth-child(2) p:nth-child(2) .dataValue") %>% 
#     html_text()
#   position = position[1]
#   age = my.link %>% 
#     html_nodes(".dataDaten:nth-child(1) p:nth-child(1) .dataValue") %>% 
#     html_text()
#   age = age[1]
#   nationality = my.link %>% 
#     html_nodes(".hide-for-small+ p .dataValue span") %>% 
#     html_text()
#   nationality = nationality[1]
#   birth_place = my.link %>% 
#     html_nodes(".hide-for-small .dataValue span") %>% 
#     html_text()
#   birth_place = birth_place[1]
#   apps = my.link %>% 
#     html_nodes(".hide+ td") %>% 
#     html_text()
#   apps = apps[1]
#   goals = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(4)") %>% 
#     html_text()
#   goals = goals[1]
#   assists = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(5)") %>% 
#     html_text()
#   assists = assists[1]
#   subs_in = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(7)") %>% 
#     html_text()
#   subs_in = subs_in[1]
#   subs_out = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(8)") %>% 
#     html_text()
#   subs_out = subs_in[1]
#   yellowcards = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(9)") %>% 
#     html_text()
#   yellowcards = yellowcards[1]
#   secondyellow = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(10)") %>% 
#     html_text()
#   secondyellow = secondyellow[1]
#   redcards = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(11)") %>% 
#     html_text()
#   redcards = redcards[1]
#   penaltygoals = my.link %>% 
#     html_nodes("tfoot .zentriert:nth-child(12)") %>% 
#     html_text()
#   penaltygoals = penaltygoals[1]
#   minutes.pr.goal = my.link %>% 
#     html_nodes("tfoot .zentriert+ .rechts") %>% 
#     html_text()
#   minutes.pr.goal = minutes.pr.goal[1]
#   minutes.played = my.link %>% 
#     html_nodes("tfoot .rechts+ .rechts") %>% 
#     html_text()
#   minutes.played = minutes.played[1]
#   return(data.frame(name = name,
#                     positions = position,
#                     age = age,
#                     nationality = nationality,
#                     birth_place = birth_place,
#                     appearances = apps,
#                     total.goals = goals,
#                     total.assists = assists,
#                     substitutions_in = subs_in,
#                     substitutions_out = subs_out,
#                     yellowcards = yellowcards,
#                     secondyellow = secondyellow,
#                     redcards = redcards,
#                     penaltygoals = penaltygoals,
#                     minutes.pr.goal = minutes.pr.goal,
#                     total.minutes.played = minutes.played
#   ))}
# 
# ## Function 4: Creating scraper-function to scrape all transfer stats
# scrape_transferstats = function(link){
#   my.link = link %>% 
#     read_html(encoding = "UTF-8")
#   name = my.link %>% 
#     html_nodes("h1") %>% 
#     html_text()
#   name = name[1]
#   transfer.date = my.link %>% 
#     html_nodes(".table-highlight a") %>% 
#     html_text()
#   transfer.date = transfer.date[1]
#   club.from = my.link %>% 
#     html_nodes(".no-border-rechts br+ .vereinprofil_tooltip") %>% 
#     html_text()
#   club.from = club.from[1]
#   club.to = my.link %>% 
#     html_nodes(".no-border-links br+ .vereinprofil_tooltip") %>% 
#     html_text()
#   club.to = club.to[1]
#   transfer.fee = my.link %>% 
#     html_nodes(".hauptfact") %>% 
#     html_text()
#   transfer.fee = transfer.fee[1]
#   contract.period.left = my.link %>% 
#     html_nodes("tr:nth-child(9) .zentriert") %>% 
#     html_text()
#   contract.period.left = contract.period.left[1]  
#   return(data.frame(name = name,
#                     transfer.date = transfer.date,
#                     club.from = club.from,
#                     club.to = club.to,
#                     transfer.fee = transfer.fee,
#                     contract.period.left = contract.period.left
#   ))}
# 
# ##===================== 1.5 Applying functions to generate links =============================
# 
# # applying function 1 and thereby creating a vector of all the links to transfered players
# all.tranferlinks.partly = lapply(all.transferlinks, link.collector)
# all.profiles.partly = unlist(all.tranferlinks.partly) # transform from list to vector
# profile.links = paste(base.link,all.profiles.partly, sep ="") # creating full link
# profile.links[1:300] # showing the first 300 links
# 
# ##creates vector with links to all the players stat page
# player.stats.links = str_replace(profile.links,"profil","leistungsdaten") 
# player.stats.links[1:200]
# 
# ## creates vector with links to all the players detailed stat page for season 14/15
# season.stat.links = paste(player.stats.links,"/plus/1?saison=2014", sep = "")
# season.stat.links[1:200] # showing the first 200 links to transfered players performance stats for 14/15
# 
# # applying the function 2 and thereby creating a vector of all the links to transfer details
# all.tranferlinks2.partly = lapply(all.transferlinks, link.collector2)
# all.transfers.partly = unlist(all.tranferlinks2.partly) # transform from list to vector
# 
# ## merging to get the full links to the transfer details
# transfer.links = paste(base.link,all.transfers.partly, sep ="")
# transfer.links[100:200]
# 
# ##=========== 1.6 Applying functions to scrape the performance and transfer stats ============
# 
# ## Create data frame with performance stats using function 3
# player.stats.season = season.stat.links  %>% 
#   map_df(scrape_playerstats)
# 
# ## Create data frame with transfer stats using function 4
# transfer.stats = transfer.links  %>% 
#   map_df(scrape_transferstats)
#   
# 
# ##============ 1.7 Importing table rangig for the major leauges in season 14/15 ==============
# 
# pl.table14 = pl.table14.link %>%
#   read_html() %>% 
#   html_node(css.pl.table14) %>% 
#   html_table() %>%  # then convert the HTML table into a data frame
#   mutate(league = "Premier league") # adding a new column with the league name
# 
# bl.table14 = bl.table14.link %>%
#   read_html() %>% 
#   html_node(css.bl.table14) %>% 
#   html_table() %>%  
#   mutate(league = "Bundesliga")
#             
# ll.table14 = ll.table14.link %>%
#   read_html() %>% 
#   html_node(css.ll.table14) %>% 
#   html_table() %>%
#   mutate(league = "La Liga")
#             
# sa.table14 = sa.table14.link %>%
#   read_html() %>% 
#   html_node(css.sa.table14) %>% 
#   html_table() %>%  
#   mutate(league = "Serie A")
#             
# l1.table14 = l1.table14.link %>%
#   read_html() %>% 
#   html_node(css.l1.table14) %>% 
#   html_table() %>% 
#   mutate(league = "Ligue 1")
#             
# 
# 
# ##============================ 1.8 Merging data frames ====================================
# 
# ## merging the performance and transfer data frames into one player data frame
# player.data = left_join(transfer.stats, player.stats.season)
# 
# ## saving uncleaned player data as csv
# write.table(player.data, file = "player_data_unclean.csv",
#             sep = ",", col.names = NA, qmethod = "double")
# 
# ## merging the league specific data frames into one club data frame
# club.data = rbind(pl.table14, bl.table14, ll.table14, sa.table14, l1.table14)
# 
# ## saving uncleaned club data as csv
# write.table(club.data, file = "club_data_unclean.csv",
#              sep = ",", col.names = NA, qmethod = "double")


##=========================================================================================
##--------------------------------- 2. Cleaning -------------------------------------------
##=========================================================================================


##================ 2.1 Cleaning player data and creating useful predictors ================
player.data.cleaning = read.csv("player_data_unclean.csv", encoding = "Latin1") # loading saved version of uncleaned player data

## 2.1.1: Cleaning transfer fee variable
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"?","")
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"\\.","") #removing the dots
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"m","0000") #removing the m 
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"k","000") #removing the k
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"-", NA) #removing the - and turn into NA
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"\\?", NA) #removing ? and turn into NA
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"Free transfer","0") # Setting free transfer to be equal to 0

## 2.1.2: Cleaning goals pr. minutes
player.data.cleaning$minutes.pr.goal = str_sub(player.data.cleaning$minutes.pr.goal, start=1, end=-2)
player.data.cleaning$minutes.pr.goal = str_replace(player.data.cleaning$minutes.pr.goal,"\\.","")

## 2.1.3: Cleaning total minutes played
player.data.cleaning$total.minutes.played = str_sub(player.data.cleaning$total.minutes.played, start=1, end=-2)
player.data.cleaning$total.minutes.played = str_replace(player.data.cleaning$total.minutes.played,"\\.","")

## 2.1.4: Cleaning variable for transferdate
player.data.cleaning$transfer.year = sub(".*,", "", player.data.cleaning$transfer.date) # subtract the year
player.data.cleaning$transfer.month = str_sub(player.data.cleaning$transfer.date, start=1, end=-9) #subtract month
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Jan", 01) #month by number instead of character
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Feb", 02)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Mar", 03)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Apr", 04)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "May", 05)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Jun", 06)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Jul", 07)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Aug", 08)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Sep", 09)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Oct", 10)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Nov", 11)
player.data.cleaning$transfer.month = str_replace(player.data.cleaning$transfer.month, "Dec", 12)
player.data.cleaning$transfer.day = str_sub(player.data.cleaning$transfer.date, start=4, end=-7) #subtract day
player.data.cleaning$transfer.date = paste(player.data.cleaning$transfer.year,"/", 
                                           player.data.cleaning$transfer.month,"/",
                                           player.data.cleaning$transfer.day) ## full transferdate in right format

player.data.cleaning$transfer.date = str_replace_all(player.data.cleaning$transfer.date, " ","") 

## 2.1.5: Cleaning contract length left
player.data.cleaning$contract.period.left = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", player.data.cleaning$contract.period.left, perl=T) #extract the date when the contract expires

player.data.cleaning$end.year = sub(".*,", "", player.data.cleaning$contract.period.left) # subtract the year
player.data.cleaning$end.month = str_sub(player.data.cleaning$contract.period.left, start=1, end=-9) #subtract month
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Jan", 01) #month by number instead of character
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Feb", 02)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Mar", 03)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Apr", 04)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "May", 05)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Jun", 06)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Jul", 07)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Aug", 08)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Sep", 09)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Oct", 10)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Nov", 11)
player.data.cleaning$end.month = str_replace(player.data.cleaning$end.month, "Dec", 12)
player.data.cleaning$end.day = str_sub(player.data.cleaning$contract.period.left, start=4, end=-7) #subtract day
player.data.cleaning$contract.end.date = paste(player.data.cleaning$end.year,"/", 
                                               player.data.cleaning$end.month,"/",
                                               player.data.cleaning$end.day) ## full date in right format

player.data.cleaning$contract.end.date = str_replace_all(player.data.cleaning$contract.end.date, " ","") #remove whitespaces
player.data.cleaning$contract.end.date = str_replace_all(player.data.cleaning$contract.end.date, "//",NA) # label missing data NA
player.data.cleaning$contract.end.date = str_replace_all(player.data.cleaning$contract.end.date, "NA/NA/NA",NA) # label missing data NA

# calculating the number of days left of the contract 
player.data.cleaning$contract.left = as.Date(as.character(player.data.cleaning$contract.end.date), format = "%Y/%m/%d") -
  as.Date(as.character(player.data.cleaning$transfer.date), format = "%Y/%m/%d")

player.data.cleaning$contract.left.month = player.data.cleaning$contract.left / 30.4375 ## rescaling from days to months

## mistakes at transfermarkt.co.uk does that we recieve some observations with a negative contract length 
## we turn them into NA
player.data.cleaning$contract.left.month[player.data.cleaning$contract.left.month < 0] = NA 

## 2.1.6: Cleaning age variabel
player.data.cleaning$birth.date = player.data.cleaning$age
player.data.cleaning$birth.date = str_replace_all(player.data.cleaning$birth.date, " ","")
player.data.cleaning$birth.date = str_sub(player.data.cleaning$birth.date, start=1, end=-6) #birth date

player.data.cleaning$birth.year = sub(".*,", "", player.data.cleaning$birth.date) # subtract the year

player.data.cleaning$birth.month = substring(player.data.cleaning$birth.date, 1,4)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Jan", 01) #month by number instead of character
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Feb", 02)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Mar", 03)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Apr", 04)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "May", 05)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Jun", 06)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Jul", 07)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Aug", 08)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Sep", 09)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Oct", 10)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Nov", 11)
player.data.cleaning$birth.month = str_replace(player.data.cleaning$birth.month, "Dec", 12)

player.data.cleaning$birth.day = substring(player.data.cleaning$birth.date, 5,6) #subtract day
player.data.cleaning$birth.day = str_replace(player.data.cleaning$birth.day, ",","") #removing unneccesary

player.data.cleaning$birth.date = paste(player.data.cleaning$birth.year,"/",player.data.cleaning$birth.month,"/",player.data.cleaning$birth.day) ## full birthdate in right format


player.data.cleaning$birth.date = str_replace_all(player.data.cleaning$birth.date," ","") #remove whitespaces
player.data.cleaning$birth.date = str_replace(player.data.cleaning$birth.date," ","")
player.data.cleaning$birth.date = str_replace(player.data.cleaning$birth.date,"\n","")

## Calculating the age at the transferdate 
player.data.cleaning$transferage = as.Date(as.character(player.data.cleaning$transfer.date), format = "%Y/%m/%d") -
  as.Date(as.character(player.data.cleaning$birth.date), format = "%Y/%m/%d")

player.data.cleaning$transferage = player.data.cleaning$transferage / 365.25 #rescaling from days to years


### 2.1.7: Creating variable that group players in defenders, midfielders and attackers 
#player.data.cleaning$positions = str_replace(player.data.cleaning$positions," Defender","Defender")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Right-Back","Defender")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Left-Back","Defender")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Centre Back","Defender")
#player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"-","Defender") # one observation has a -, but is defender - AMER?
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Attacking Midfield","Midfield")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Central Midfield","Midfield")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Defensive Midfield","Midfield")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Left Midfield","Midfield")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Right Midfield","Midfield")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Right Wing","Attacker")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Left Wing","Attacker")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Centre Forward","Attacker")
player.data.cleaning$positions = str_replace(player.data.cleaning$positions,"Secondary Striker","Attacker")


### 2.1.8: General cleaning 
## Setting "-" equal to 0 for the performance variables
perform.var = c("appearances", "total.goals", "total.assists", 
                "substitutions_in", "substitutions_out", "yellowcards", "secondyellow",
                "redcards", "penaltygoals", "minutes.pr.goal","total.minutes.played")
player.data.cleaning[perform.var] = 
  sapply(player.data.cleaning[perform.var], as.character) #  transforming performance var into character variables 

player.data.cleaning[perform.var][player.data.cleaning[perform.var] == "-"] = 0 #replacing - with 0 for performance variables

## Removing rows where positions are unknown.
player.data.cleaning = player.data.cleaning[-c(30, 67, 78, 83, 105, 178, 457, 659), ]

## Removing players who appear double in the sample
player.data.cleaning = subset(player.data.cleaning, select=-c(X))
player.data.cleaning = player.data.cleaning[!duplicated(player.data.cleaning), ] ## removing dublicated observations

## Removing rows which contain transfer.fee = NA
player.data.cleaning = subset(player.data.cleaning, !is.na(transfer.fee))

## Removing keepers
player.data.cleaning = player.data.cleaning[!grepl("Keeper", player.data.cleaning$positions),]

## Putting NA in all blank cells 
player.data.cleaning[player.data.cleaning == ""] = NA 


### Transforming variables containing numbers into numeric variables
sapply(player.data.cleaning, class) #inspecting the class of all variables

var.to.numeric = c("age", "transfer.fee", "appearances", "total.goals", "total.assists", 
                   "substitutions_in", "substitutions_out", "yellowcards", "secondyellow",
                   "redcards", "penaltygoals", "minutes.pr.goal","total.minutes.played") #creating a 
#vector with the names of the variables that we want numeric

player.data.cleaning[var.to.numeric] = 
  sapply(player.data.cleaning[var.to.numeric], as.numeric) #  transforming the selected var into numeric

## Rescaling transfer fees into million punds 
player.data.cleaning$transfer.fee = player.data.cleaning$transfer.fee / 1000000


## Removing column X.1 and X, which are not useful.
names(player.data.cleaning)
player.data.clean = subset(player.data.cleaning, select=-c(contract.period.left, age, 
                                                           transfer.year, transfer.month, 
                                                           transfer.day, end.year, end.month,
                                                           end.day, contract.end.date, contract.left,
                                                           birth.year, birth.month, birth.day))


### 2.2: Adding a variable with the number of google hits on the player

## Creating a function that find the 
GoogleHits <- function(input)  #Function that seach for the input specified
{
  input = gsub(" ", "+", input)
  #url <- paste("https://www.google.com/search?q=\"",
  #             input, "\"", sep = "")
  url = paste0("https://www.google.com/search?q=",
               input)
  
  CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
  script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
  doc <- htmlParse(script)
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
  #cat("\nNo. of Hits:\n")
  return(as.integer(gsub("[^0-9]", "", res)))
}

##
search.1=dQuote(player.data.clean$name)  #Put quotation marks around name of the player
search.2=paste(search.1,"footballer", sep=" ") #Paste name of footballer, the word footballer and nationality
search.2

# OBS!
# Google as maximum number of searches that you can make on one day. Therefore we splittet our
# search in three parts and did the searches from different IP-addresses. 

player.data.clean.pt1=player.data.clean[1:250,]
list.pt.1=lapply(search.2[1:250], GoogleHits)
player.data.clean.pt1$searchresults=unlist(list.pt.1) #New column reporting number of search results

player.data.clean.pt2=player.data.clean[251:500,]
list.pt.2=lapply(search.2[251:500], GoogleHits)
player.data.clean.pt2$searchresults=unlist(list.pt.2) #New column reporting number of search results

player.data.clean.pt3=player.data.clean[501:696,]
list.pt.3=lapply(search.2[501:696], GoogleHits)
player.data.clean.pt3$searchresults=unlist(list.pt.3) #New column reporting number of search results

player.data.clean = rbind(player.data.clean.pt1, player.data.clean.pt2, player.data.clean.pt3)

## saving cleaned player data as csv
write.table(player.data.clean, file = "player_data_clean.search.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")


##================ 2.2 Cleaning club data and creating useful predictors ================
club.data = read.csv("club_data_unclean.csv", encoding="latin1") # loading saved version of uncleaned club data
club.data.cleaning = club.data

## Giving different status to different clubs depending on which position they ended at in the league.
attach(club.data.cleaning)
club.data.cleaning$Status[Pos <= 5] = "Top Club"
club.data.cleaning$Status[Pos <= 15 & Pos > 5] = "Middle Club"
club.data.cleaning$Status[Pos >= 16] = "Bottom Club"
detach(club.data.cleaning)

## Changing the name of the team name variable
names(club.data.cleaning)[names(club.data)=="Team..v.t.e"] <- "Team"
names(club.data.cleaning)

## Renaming clubs in Wikipedia-tabel first

club.data.cleaning$Team=recode(club.data.cleaning$Team,"Barcelona (C)"="FC Barcelona", "Valencia"="Valencia CF", "M?laga"="M?laga CF", "Elche[d](R)"="Elche CF", 
                               "Levante"="Levante UD", "Getafe"="Getafe CF", "Deportivo La Coru?a"="Dep. La Coru?a", "Granada"="Granada CF",
                               "Eibar"="SD Eibar", "Almer?a (R)"="UD Almer?a", "C?rdoba (R)"="C?rdoba CF", "Sevilla"="Sevilla FC",
                               "Villarreal" = "Villarreal CF", "Celta Vigo" = "Celta de Vigo","Juventus (C)"="Juventus", "Cargliari (R)"="Cagliari Calcio", "Parma[c](R)"="Parma", "Cesena (R)"="Cesena",
                               "Internazionale"="Inter", "Genoa[b]"="Genoa", "Roma"="AS Roma", "Napoli"="SSC Napoli", "Milan"="AC Milan",
                               "Palermo"="US Palermo", "Chievo"="Chievo Verona", "Empoli"="FC Empoli", "Udinese"="Udinese Calcio",
                               "Cagliari (R)"="Cagliari Calcio","Paris Saint-Germain (C)"="Paris SG", "Evian (R)"="Evian", "Metz (R)"="FC Metz", "Lyon"="Olympique Lyon",
                               "Bordeaux"="G. Bordeaux", "Lille"="LOSC Lille", "Nice"="OGC Nice", "Caen"="SM Caen", "Nantes"="FC Nantes",
                               "Lorient"="FC Lorient", "Bordeaux"="G. Bordeaux", "Lens[b](R)"="RC Lens", "Bastia"="SC Bastia","Bayern Munich (C)"="Bayern Munich ", "SC Freiburg (R)"="SC Freiburg", "SC Paderborn 07 (R)"="SC Paderborn",
                               "Hamburger SV (O)"="Hamburger SV", "Borussia M?nchengladbach"="Bor. M'gladbach", "Schalke 04"="FC Schalke 04",
                               "Bayer Leverkusen"="Bay. Leverkusen", "Eintracht Frankfurt"="E. Frankfurt", "Borussia Dortmund"="Bor. Dortmund",
                               "1899 Hoffenheim" = "TSG Hoffenheim", "FSV Mainz 05"="1.FSV Mainz 05","Chelsea (C)"="Chelsea", "Hull City (R)"="Hull City", "Burnley"="Burnley FC", "Queens Park Rangers (R)"="QPR",
                               "West Bromwich Albion"="West Brom", "Tottenham Hotspur"="Spurs","Swansea City"="Swansea", 
                               "Manchester United"="Manchester Utd.", "West Ham United"="West Ham", "Leicester City"="Leicester", 
                               "Newcastle United"="Newcastle")


# Selecting the useful clubvariables
names(club.data.cleaning)
club.data.clean = subset(club.data.cleaning, select=c(Team, league, Status))

## saving cleaned player data as csv
write.table(club.data.clean, file = "club_data_clean.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")          

# ##================ 2.3 Merging player and club data into one tidy data frame ================
# setwd("/Users/guillaumeslizewicz/Documents/SDS-group12/Exam_project")
# player.data.clean= read.csv(file="player_data_clean.csv", encoding = "latin1")
# iconv(player.data.clean, from = "latin1", to = "UTF8", sub = NA, mark = TRUE, toRaw = FALSE)
# 
# club.data.clean=read.csv(file = "club.data.clean.csv", encoding="UTF8")

transferdata.tidy=merge(player.data.clean,club.data.clean, by.x=c("club.to"),by.y=c("Team"), all.x=TRUE)

## Handeling promoted clubs
transferdata.tidy$Status[is.na(transferdata.tidy$Status)] = "Promoted"
transferdata.tidy$league[(transferdata.tidy$club.to == "Watford") | 
                           (transferdata.tidy$club.to == "Norwich")|
                           (transferdata.tidy$club.to == "Bournemouth")] = "Premier league"
transferdata.tidy$league[(transferdata.tidy$club.to == "FC Ingolstadt") | 
                           (transferdata.tidy$club.to == "SV Darmstadt 98")] = "Bundesliga"
transferdata.tidy$league[(transferdata.tidy$club.to == "UD Las Palmas")| 
                           (transferdata.tidy$club.to == "Sporting Gij?n")|
                           (transferdata.tidy$club.to == "Real Betis")] = "La Liga"
transferdata.tidy$league[(transferdata.tidy$club.to == "Bologna")| 
                           (transferdata.tidy$club.to == "Carpi")|
                           (transferdata.tidy$club.to == "Frosinone")] = "Serie A"
transferdata.tidy$league[(transferdata.tidy$club.to == "SCO Angers")| 
                           (transferdata.tidy$club.to == "Troyes")|
                           (transferdata.tidy$club.to == "G. Ajaccio")] = "Ligue 1"

## Saving the tidy final data set 
write.table(transferdata.tidy, file = "transferdata.final.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")

##=========================================================================================
##--------------------------------- 3. Visualization---------------------------------------
##=========================================================================================

transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.tidy.csv", encoding = "UTF8", header = TRUE)
transfer.data = subset(transfer.data, select=-c(X.1, X.2,X))

##================ 3.1 XXXXX  ================

##================ 3.2 XXXXX  ================

##================ 3.3 XXXXX  ================

##================ 3.4 XXXXX  ================




##=========================================================================================
##--------------------------------- 4. Prediction Models _---------------------------------
##=========================================================================================

## Loading the final data set
transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", 
                         encoding = "UTF8", header = TRUE)

## Due to the descriptive analysis we create a new variable where age is squared
transfer.data$transferage_sq = transfer.data$transferage^2

## creating a vector with selected predictors for transferfee
predicting.var = c("transfer.fee", "positions", "appearances", "total.goals", "total.assists", 
                   "total.minutes.played", "contract.left.month","transferage",
                   "league", "Status", "searchresults","transferage_sq")

## Removing observations where contract lenght is unknown
transfer.data = filter(transfer.data, is.na(contract.left.month) == FALSE) 


##================ 4.1 Cross validation: Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data)) # creates a vector 

## setting seed to enable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data[train.indicator,predicting.var] # selecting observations with a train indicator
test_sample = transfer.data[-train.indicator, predicting.var] # selecting observations without a train indicator


##================ 4.2 Create evaluation function  ================
## Creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 4.3 Baseline model: Simple average from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1
get.rmse(test_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 

### 4.3.1: Illustrating our prediction model 1
## Create new data frame for the illustration
train_sample.1<- train_sample %>% 
  select(transfer.fee,league) 
train_sample.1<- train_sample.1%>% 
  mutate(index=1:258)

## Creating GGplot for visualisation
p = ggplot(train_sample.1, aes(x = index , y = transfer.fee))+
  geom_segment(aes(x= index, xend=index, y=transfer.fee, yend=estimate_M1), color="red") +
  geom_point(aes(x = index, y = transfer.fee, color = "black"))   +
  geom_line(aes(x = index, y = estimate_M1), color="green", size =1)+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks= element_line(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text=element_text(family="Goudy Old Style"))

gg <- ggplotly(p)  #using plotly to make it interactive
gg

##================ 4.4 Ordinary least square model  ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample)) # generating linear model on training data
summary(Model_2)

estimate_M2 = predict(Model_2, test_sample) # calculating estimate from model 2

get.rmse(test_sample$transfer.fee, estimate_M2) # calculating RMSE from estimate on test sample 

##================ 4.5 Lasso model  ================
## Creating matrices with all regressors beacuse the glmnet function only works with matrices
RegressorMatrix_train=model.matrix(transfer.fee~ ., train_sample)
RegressorMatrix_test=model.matrix(transfer.fee~.,test_sample)



## Training Lasso
Model_3 = glmnet(x = RegressorMatrix_train, y = train_sample$transfer.fee)
Model_3

# Calculating RSME for each lambda
lambda_values = Model_3$lambda

performance_Lasso = data.frame()

for (lambda in lambda_values){
  performance_Lasso = rbind(performance_Lasso,
                            data.frame(lambda = lambda,
                                       RMSE = get.rmse(predict(Model_3, RegressorMatrix_test, s = lambda),
                                                       test_sample$transfer.fee)))
}
performance_Lasso

##Visualization of RSME as a function of lamda
ggplot(performance_Lasso, aes(x = lambda, y = RMSE))+
  geom_point() + 
  geom_line() + 
  theme_minimal()

## Identifying lambda with the lowest RMSE
best.lambda = performance_Lasso$lambda[performance_Lasso$RMSE == min(performance_Lasso$RMSE)]

## Coefficients for best models
coef(Model_3, s = best.lambda)

## RMSE for best model
Estimate_M3=predict(Model_3, RegressorMatrix_test, s=best.lambda)
get.rmse(Estimate_M3, test_sample$transfer.fee)

##================ 4.6 Decision tree  ================
set.seed(123)
Model_4=tree(transfer.fee~.,data=train_sample, method="anova")
Model_4$frame
plot(Model_4,niform=TRUE, 
     main="Regression Tree for Transfer fee ")
text(Model_4,pretty=0,use.n=TRUE, cex=.5)


##Estimating transfer fee for test data
Estimate_M4=predict(Model_4,test_sample)
Estimate_M4
##Calculating RMSE 
get.rmse(test_sample$transfer.fee,Estimate_M4)  #6.600


## Cross validation to find the optimal number of terminal nodes
cv.Model_4 = cv.tree(Model_4, FUN = prune.tree)
plot(cv.Model_4$size, cv.Model_4$dev, type = "b") #Optimal number with cross validation is 7
best.size=cv.Model_4$size[which.min(cv.Model_4$dev)]
prune.Model_4=prune.tree(Model_4,best = best.size)
plot(prune.Model_4);text(prune.Model_4)
## Calculating estimates with pruned model
pruned.estimate=predict(prune.Model_4,test_sample)
get.rmse(pruned.estimate,test_sample$transfer.fee)
## Higher RMSE with pruned model than the original


##================ 4.7 Random Forest  ================
## Gode link: http://www.listendata.com/2014/11/random-forest-with-r.html
# install.packages("randomForest")
library(randomForest)
set.seed(1)

### More complex attempt
## First, find the best number of trees
Model_5a = randomForest(transfer.fee ~ ., data = train_sample, ntree = 200)
print(Model_5a)
Model_5b = randomForest(transfer.fee ~ ., data = train_sample, ntree = 300)
print(Model_5b)
Model_5c = randomForest(transfer.fee ~ ., data = train_sample, ntree = 500)
print(Model_5c)
Model_5d = randomForest(transfer.fee ~ ., data = train_sample, ntree = 750)
print(Model_5d)
Model_5e = randomForest(transfer.fee ~ ., data = train_sample, ntree =1000) ## gives the lowest residuals
print(Model_5e)
Model_5f = randomForest(transfer.fee ~ ., data = train_sample, ntree =2000)
print(Model_5f)

## Second, find the number of variables in each split with the lowest OOB-error 
mtry = tuneRF(train_sample[-1],train_sample$transfer.fee, ntreeTry=1000,
              stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, na.action=na.roughfix)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m) ##the best mtry is 3

## Run the RF with the best numbers of trees and variables
set.seed(1)
Model_5 = randomForest(transfer.fee ~ .,
                       data = train_sample,
                       ntree = 1000,
                       mtry = best.m)
print(Model_5)

estimate_M5 = predict(Model_5, test_sample) # calculating estimate from model 5
get.rmse(test_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample

importance(Model_5) #calculate the importance of the different variables

## the simple way:
Model_5 = randomForest(transfer.fee ~ ., data = train_sample, importance = TRUE)
print(Model_5)

estimate_M5 = predict(Model_5, test_sample) # calculating estimate from model 5
get.rmse(test_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample

var.list = importance(Model_5, type = 1) #calculate the variables influence
varImpPlot(Model_5) # plots the variables influence
var.list

#plot(test_sample$transfer.fee, estimate_M6)
#abline(a=0, b=1.0)

