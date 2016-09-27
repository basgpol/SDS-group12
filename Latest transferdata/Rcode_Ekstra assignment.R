############################################################################################
####################################### SDS Project ########################################
######################### Putting a price tag on football players ##########################
############################################################################################


##=========================================================================================
##------------------------- Table of contents ---------------------------------------------
##=========================================================================================
# 1. Replicating prediction models
#   1.1 Loading R-packages
#   1.2 Loading former data frame and replicate prediction models
#       1.2.1 Dividing into a train and test sample
#       1.2.2 Create evaluation function
#       1.2.3 Model 1: Simple average from training sample
#       1.2.4 Model 2: Ordinary least square model
#       1.2.5 Model 3: Lasso model
#       1.2.6 Model 4: Decision tree
#       1.2.6 Model 4: Decision tree
# 2. Data gathering
#   2.2 Defining key links
#   2.3 Defining CSS Selectors
#   2.4 Bulding functions (link collection and scraping)
#   2.5 Applying functions to generate links
#   2.6 Applying functions to scrape the performance and transfer stats
#   2.7 Importing table ranking for the major leauges in season 14/15
#   2.8 Mergig data frames
# 3. Cleaning
#   3.1 Cleaning player data and creating useful predictors
#   3.2 Adding a variable with the number of google hits on the player
#   3.3 Cleaning club data and creating useful predictors
#   3.4 Merging player and club data into one tidy data frame
# 4. Prediction Models
#   4.1 Dividing into a train and test sample
#   4.2 Create evaluation function
#   4.3 Baseline Model: Simple average from training sample
#   4.4 Ordinary least square model
#   4.5 Lasso model
#   4.6 Decision tree
#   4.7 Random forrest

# [3. Vizualization 
# 3.1 Club map with transfer spending
# 3.2 Club map with transfer paths 
# 3.3 Transfer fee by age scatter plot
# 3.4 Transfer fee by time left on contract scatter plot
# 3.5 Average spending per club per league
# 3.6 Average spending per player per league 
# 3.7 Average spending per player per club status]


##=========================================================================================
##------------------------- 1. Replicating prediction models ------------------------------
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
library ("tree")
library("randomForest")

##=============== 1.2 Loading former data frame and replicate prediction models =============

## Loading the final data set on old transferdata
transfer.data.former = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", 
                         encoding = "UTF8", header = TRUE)

## new variable where age is squared
transfer.data.former$transferage_sq = transfer.data.former$transferage^2

## creating a vector with selected predictors for transferfee
predicting.var = c("transfer.fee", "positions", "appearances", "total.goals", "total.assists", 
                   "total.minutes.played", "contract.left.month","transferage",
                   "league", "Status", "searchresults","transferage_sq")

## Removing observations where contract lenght is unknown
transfer.data.former = filter(transfer.data.former, is.na(contract.left.month) == FALSE) 


##================ 1.2.1 Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data.former)) # creates a vector 

## setting seed to enable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data.former)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data.former[train.indicator,predicting.var] # selecting observations with a train indicator
test_sample_old = transfer.data.former[-train.indicator, predicting.var] # selecting observations without a train indicator


##================ 1.2.2 Create evaluation function  ================
## Creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 1.2.3 Model 1: Simple average from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1

##================ 1.2.4 Model 2: Ordinary least square model  ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample)) # generating linear model on training data
summary(Model_2)
estimate_M2 = predict(Model_2, test_sample_old) # calculating estimate from model 2

get.rmse(test_sample_old$transfer.fee, estimate_M2) # calculating RMSE from estimate on test sample 

##================ 1.2.5 Model 3: Lasso model  ================
## Creating matrices with all regressors beacuse the glmnet function only works with matrices
RegressorMatrix_train=model.matrix(transfer.fee~ ., train_sample)
RegressorMatrix_test.old=model.matrix(transfer.fee~.,test_sample_old)

## Training Lasso
Model_3 = glmnet(x = RegressorMatrix_train, y = train_sample$transfer.fee)
Model_3

# # Calculating RSME for each lambda
# lambda_values = Model_3$lambda
# 
# performance_Lasso = data.frame()
# 
# for (lambda in lambda_values){
#   performance_Lasso = rbind(performance_Lasso,
#                             data.frame(lambda = lambda,
#                                        RMSE = get.rmse(predict(Model_3, RegressorMatrix_test, s = lambda),
#                                                        test_sample$transfer.fee)))
# }
# performance_Lasso
# 
# ##Visualization of RSME as a function of lamda
# ggplot(performance_Lasso, aes(x = lambda, y = RMSE))+
#   geom_point() + 
#   geom_line() + 
#   theme_minimal()
# 
# ## Identifying lambda with the lowest RMSE
# best.lambda = performance_Lasso$lambda[performance_Lasso$RMSE == min(performance_Lasso$RMSE)]
# 
# ## Coefficients for best models
# coef(Model_3, s = best.lambda)
# 
# ## RMSE for best model
# Estimate_M3=predict(Model_3, RegressorMatrix_test, s=best.lambda)
# get.rmse(Estimate_M3, test_sample$transfer.fee)

##================ 1.2.6 Model 4: Decision tree  ================
set.seed(123)
Model_4=tree(transfer.fee~.,data=train_sample, method="anova")
Model_4$frame
plot(Model_4,niform=TRUE, 
     main="Regression Tree for Transfer fee ")
text(Model_4,pretty=0,use.n=TRUE, cex=.5)


##================ 1.2.7 Model 5: Random Forest  ================
set.seed(1)

Model_5 = randomForest(transfer.fee ~ ., data = train_sample, importance = TRUE)
print(Model_5)

##=========================================================================================
##--------------------------------- 2. Data gathering  ------------------------------------
##=========================================================================================

##========================== 2.1 Defining key links =======================================
#Remove # for several lines by the shortcut ctrl/command + shift + c

## Links from transfermarkt.co.uk to overviews of transfers in the summer 16 in  the five major football leagues
base.link = "http://www.transfermarkt.co.uk/"
pl.tranfers.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2016&s_w=&leihe=0&intern=0"
bl.transfer.link = "http://www.transfermarkt.co.uk/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=2016&s_w=&leihe=0&intern=0"
ll.transfer.link = "http://www.transfermarkt.co.uk/laliga/transfers/wettbewerb/ES1/plus/?saison_id=2016&s_w=&leihe=0&intern=0"
sa.tranfer.link = "http://www.transfermarkt.co.uk/serie-a/transfers/wettbewerb/IT1/plus/?saison_id=2016&s_w=&leihe=0&intern=0"
l1.transfer.link = "http://www.transfermarkt.co.uk/ligue-1/transfers/wettbewerb/FR1/plus/?saison_id=2016&s_w=&leihe=0&intern=0"
all.transferlinks = c(pl.tranfers.link, bl.transfer.link, ll.transfer.link, sa.tranfer.link, l1.transfer.link)

## Links from wikipedia.com sites for info om final tables in the
pl.table15.link = "https://en.wikipedia.org/wiki/2015-16_Premier_League"
bl.table15.link = "https://en.wikipedia.org/wiki/2015-16_Bundesliga"
ll.table15.link = "https://en.wikipedia.org/wiki/2015-16_La_Liga"
sa.table15.link = "https://en.wikipedia.org/wiki/2015-16_Serie_A"
l1.table15.link = "https://en.wikipedia.org/wiki/2015-16_Ligue_1"

# ##========================== 2.2 Defining CSS selectors =======================================

## Define CSS selectors from transfermarkt
css.selector.profile = ".table-header+ .responsive-table .hide-for-small .spielprofil_tooltip"
css.selector.transfer = ".table-header+ .responsive-table .rechts a"

## Define CSS selectors for league tables from Wikipedia
css.pl.table15 = ".wikitable:nth-child(36)"
css.bl.table15 = ".wikitable:nth-child(17)"
css.ll.table15 = ".wikitable:nth-child(30)"
css.sa.table15 = ".wikitable:nth-child(25)"
css.l1.table15 = ".wikitable:nth-child(16)"


##================= 2.3 Bulding functions (link collection and scraping) =====================

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
  birth_place = my.link %>%
    html_nodes(".hide-for-small .dataValue span") %>%
    html_text()
  birth_place = birth_place[1]
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
                    birth_place = birth_place,
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
  contract.period.left = my.link %>%
    html_nodes("tr:nth-child(9) .zentriert") %>%
    html_text()
  contract.period.left = contract.period.left[1]
  return(data.frame(name = name,
                    transfer.date = transfer.date,
                    club.from = club.from,
                    club.to = club.to,
                    transfer.fee = transfer.fee,
                    contract.period.left = contract.period.left
  ))}

##===================== 2.4 Applying functions to generate links =============================

# applying function 1 and thereby creating a vector of all the links to transfered players
all.tranferlinks.partly = lapply(all.transferlinks, link.collector)
all.profiles.partly = unlist(all.tranferlinks.partly) # transform from list to vector
profile.links = paste(base.link,all.profiles.partly, sep ="") # creating full link
profile.links[1:300] # showing the first 300 links

##creates vector with links to all the players stat page
player.stats.links = str_replace(profile.links,"profil","leistungsdaten")
player.stats.links[1:200]

## creates vector with links to all the players detailed stat page for season 15/16
season.stat.links = paste(player.stats.links,"/plus/1?saison=2015", sep = "")
season.stat.links[1:200] # showing the first 200 links to transfered players performance stats for 14/15

# applying the function 2 and thereby creating a vector of all the links to transfer details
all.tranferlinks2.partly = lapply(all.transferlinks, link.collector2)
all.transfers.partly = unlist(all.tranferlinks2.partly) # transform from list to vector

## merging to get the full links to the transfer details
transfer.links = paste(base.link,all.transfers.partly, sep ="")
transfer.links[100:200]

##=========== 2.5 Applying functions to scrape the performance and transfer stats ============

## Create data frame with performance stats using function 3
player.stats.season = season.stat.links  %>%
  map_df(scrape_playerstats)

## Create data frame with transfer stats using function 4
transfer.stats = transfer.links  %>%
  map_df(scrape_transferstats)


##============ 2.6 Importing table ranking for the major leauges in season 15/16 ==============

pl.table15 = pl.table15.link %>%
  read_html() %>%
  html_node(css.pl.table15) %>%
  html_table() %>%  # then convert the HTML table into a data frame
  mutate(league = "Premier league") # adding a new column with the league name

bl.table15 = bl.table15.link %>%
  read_html() %>%
  html_node(css.bl.table15) %>%
  html_table() %>%
  mutate(league = "Bundesliga")

ll.table15 = ll.table15.link %>%
  read_html() %>%
  html_node(css.ll.table15) %>%
  html_table() %>%
  mutate(league = "La Liga")

sa.table15 = sa.table15.link %>%
  read_html() %>%
  html_node(css.sa.table15) %>%
  html_table() %>%
  mutate(league = "Serie A")

l1.table15 = l1.table15.link %>%
  read_html() %>%
  html_node(css.l1.table15) %>%
  html_table() %>%
  mutate(league = "Ligue 1")



# ##============================ 2.7 Merging data frames ====================================

## merging the performance and transfer data frames into one player data frame
player.data = left_join(transfer.stats, player.stats.season)

## saving uncleaned player data as csv
write.table(player.data, file = "player_data16_unclean.csv",
            sep = ",", col.names = NA, qmethod = "double")

## merging the league specific data frames into one club data frame
club.data = rbind(pl.table15, bl.table15, ll.table15, sa.table15, l1.table15)

## saving uncleaned club data as csv
write.table(club.data, file = "club_data16_unclean.csv",
             sep = ",", col.names = NA, qmethod = "double")


##=========================================================================================
##--------------------------------- 3. Cleaning -------------------------------------------
##=========================================================================================


##================ 3.1 Cleaning player data and creating useful predictors ================
player.data.cleaning = read.csv("player_data16_unclean.csv", encoding = "Latin1") # loading saved version of uncleaned player data

## 3.1.1: Cleaning transfer fee variable
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"�","")
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"\\.","") #removing the dots
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"m","0000") #removing the m 
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"k","000") #removing the k
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"-", NA) #removing the - and turn into NA
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"\\?", NA) #removing ? and turn into NA
player.data.cleaning$transfer.fee = str_replace(player.data.cleaning$transfer.fee,"Free transfer","0") # Setting free transfer to be equal to 0

## 3.1.2: Cleaning goals pr. minutes
player.data.cleaning$minutes.pr.goal = str_sub(player.data.cleaning$minutes.pr.goal, start=1, end=-2)
player.data.cleaning$minutes.pr.goal = str_replace(player.data.cleaning$minutes.pr.goal,"\\.","")

## 3.1.3: Cleaning total minutes played
player.data.cleaning$total.minutes.played = str_sub(player.data.cleaning$total.minutes.played, start=1, end=-2)
player.data.cleaning$total.minutes.played = str_replace(player.data.cleaning$total.minutes.played,"\\.","")

## 3.1.4: Cleaning variable for transferdate
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

## 3.1.5: Cleaning contract length left
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

## 3.1.6: Cleaning age variabel
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

### 3.1.7: Creating variable that group players in defenders, midfielders and attackers 
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

### 3.1.8: General cleaning 
## Setting "-" equal to 0 for the performance variables
perform.var = c("appearances", "total.goals", "total.assists", 
                "substitutions_in", "substitutions_out", "yellowcards", "secondyellow",
                "redcards", "penaltygoals", "minutes.pr.goal","total.minutes.played")
player.data.cleaning[perform.var] = 
  sapply(player.data.cleaning[perform.var], as.character) #  transforming performance var into character variables 

player.data.cleaning[perform.var][player.data.cleaning[perform.var] == "-"] = 0 #replacing - with 0 for performance variables

## Removing rows where positions are unknown.
#player.data.cleaning = player.data.cleaning[-c(30, 67, 78, 83, 105, 178, 457, 659), ]

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


### 3.2: Adding a variable with the number of google hits on the player

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

player.data.clean.pt3=player.data.clean[501:563,]
list.pt.3=lapply(search.2[501:563], GoogleHits)
player.data.clean.pt3$searchresults=unlist(list.pt.3) #New column reporting number of search results

player.data.clean = rbind(player.data.clean.pt1, player.data.clean.pt2, player.data.clean.pt3)

## saving cleaned player data as csv
write.table(player.data.clean, file = "player_data16_clean.search.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")


##================ 3.3 Cleaning club data and creating useful predictors ================
club.data = read.csv("club_data16_unclean.csv", encoding="latin1") # loading saved version of uncleaned club data
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

club.data.cleaning$Team=recode(club.data.cleaning$Team,"Barcelona (C)"="FC Barcelona", "Valencia"="Valencia CF", "M�laga"="M�laga CF", "Elche[d](R)"="Elche CF", "Las Palmas"="UD Las Palmas", 
                               "Levante (R)"="Levante UD", "Getafe (R)"="Getafe CF","Rayo Vallecano (R)"="Rayo Vallecano", "Deportivo La Coru�a"="Dep. La Coru�a", "Granada"="Granada CF",
                               "Eibar"="SD Eibar", "Almer?a (R)"="UD Almer?a", "C?rdoba (R)"="C?rdoba CF", "Sevilla"="Sevilla FC",
                               "Villarreal" = "Villarreal CF", "Celta Vigo" = "Celta de Vigo","Juventus (C)"="Juventus", "Cargliari (R)"="Cagliari Calcio", "Parma[c](R)"="Parma", "Cesena (R)"="Cesena",
                               "Carpi (R)"="Carpi", "Frosinone (R)"="Frosinone", "Hellas Verona (R)"="Hellas Verona",
                               "Internazionale"="Inter", "Genoa"="Genoa", "Roma"="AS Roma", "Napoli"="SSC Napoli", "Milan"="AC Milan",
                               "Palermo"="US Palermo", "Chievo"="Chievo Verona", "Empoli"="FC Empoli", "Udinese"="Udinese Calcio",
                               "Cagliari (R)"="Cagliari Calcio","Paris Saint-Germain (C)"="Paris SG", "Evian (R)"="Evian", "Metz (R)"="FC Metz", "Lyon"="Olympique Lyon",
                               "Bordeaux"="G. Bordeaux", "Lille"="LOSC Lille", "Nice"="OGC Nice", "Caen"="SM Caen", "Nantes"="FC Nantes", "Angers"="SCO Angers",
                               "Lorient"="FC Lorient", "Bordeaux"="G. Bordeaux", "Lens[b](R)"="RC Lens", "Bastia"="SC Bastia","Bayern Munich (C)"="Bayern Munich ", "SC Freiburg (R)"="SC Freiburg", "SC Paderborn 07 (R)"="SC Paderborn",
                               "Hamburger SV"="Hamburger SV", "Borussia M�nchengladbach"="Bor. M'gladbach", "Schalke 04"="FC Schalke 04", "Darmstadt 98"="SV Darmstadt 98",
                               "Bayer Leverkusen"="Bay. Leverkusen", "Eintracht Frankfurt (O)"="E. Frankfurt", "Borussia Dortmund"="Bor. Dortmund",
                               "1899 Hoffenheim" = "TSG Hoffenheim", "Mainz 05"="1.FSV Mainz 05","Chelsea"="Chelsea", "Hull City (R)"="Hull City", "Burnley"="Burnley FC", "Queens Park Rangers (R)"="QPR",
                               "West Bromwich Albion"="West Brom", "Tottenham Hotspur"="Spurs","Swansea City"="Swansea", 
                               "Manchester United"="Manchester Utd.", "West Ham United"="West Ham", "Leicester City (C)"="Leicester", 
                               "Newcastle United (R)"="Newcastle", "Newcastle United (R)"="Norwich", "Aston Villa (R)"="Aston Villa", "AFC Bournemouth"="Bournemouth")


# Selecting the useful clubvariables
names(club.data.cleaning)
club.data.clean = subset(club.data.cleaning, select=c(Team, league, Status))

## saving cleaned player data as csv
write.table(club.data.clean, file = "club_data16_clean.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")          

# ##================ 3.4 Merging player and club data into one tidy data frame ================
# setwd("/Users/guillaumeslizewicz/Documents/SDS-group12/Exam_project")
player.data.clean= read.csv(file="player_data_clean.csv", encoding = "latin1")
# iconv(player.data.clean, from = "latin1", to = "UTF8", sub = NA, mark = TRUE, toRaw = FALSE)
# 
# club.data.clean=read.csv(file = "club.data.clean.csv", encoding="UTF8")

transferdata.tidy=merge(player.data.clean,club.data.clean, by.x=c("club.to"),by.y=c("Team"), all.x=TRUE)

## Handeling promoted clubs
transferdata.tidy$Status[is.na(transferdata.tidy$Status)] = "Promoted"
transferdata.tidy$league[(transferdata.tidy$club.to == "Burnley FC") | 
                           (transferdata.tidy$club.to == "Hull City")|
                           (transferdata.tidy$club.to == "Middlesbrough")] = "Premier league"
transferdata.tidy$league[(transferdata.tidy$club.to == "RB Leipzig") | 
                           (transferdata.tidy$club.to == "SC Freiburg")] = "Bundesliga"
transferdata.tidy$league[(transferdata.tidy$club.to == "Alav�s")| 
                           (transferdata.tidy$club.to == "CD Legan�s")|
                           (transferdata.tidy$club.to == "CA Osasuna")] = "La Liga"
transferdata.tidy$league[(transferdata.tidy$club.to == "Cagliari Calcio")| 
                           (transferdata.tidy$club.to == "Crotone")|
                           (transferdata.tidy$club.to == "Pescara")] = "Serie A"
transferdata.tidy$league[(transferdata.tidy$club.to == "AS Nancy")| 
                           (transferdata.tidy$club.to == "Dijon")|
                           (transferdata.tidy$club.to == "FC Metz")] = "Ligue 1"

## Saving the tidy final data set 
write.table(transferdata.tidy, file = "transferdata16.final.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")

##=========================================================================================
##--------------------------------- 4. Prediction Models _---------------------------------
##=========================================================================================

## Loading the final data set from website
#transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", 
#                         encoding = "UTF8", header = TRUE)

## Loading the previous saved final data set 
transfer.data = read.csv("transferdata16.final.csv", encoding = "UTF-8") # loading saved version of uncleaned player data

## Due to the descriptive analysis we create a new variable where age is squared
transfer.data$transferage_sq = transfer.data$transferage^2

## creating a vector with selected predictors for transferfee
predicting.var = c("transfer.fee", "positions", "appearances", "total.goals", "total.assists", 
                   "total.minutes.played", "contract.left.month","transferage",
                   "league", "Status", "searchresults","transferage_sq")

## Removing observations where contract lenght is unknown
transfer.data = filter(transfer.data, is.na(contract.left.month) == FALSE) 

test16_sample = transfer.data[predicting.var] 

## Removing unused factor levels
test16_sample$positions = factor(test16_sample$positions)

##================ 4.1 Model 1: Simple average from training sample  ================
result_m1_16 = get.rmse(test16_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 
## RMSE = 10.5


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

##================ 4.2 Model 2: Ordinary least square model  ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample)) # generating linear model on training data
summary(Model_2)
estimate_M2 = predict(Model_2, test16_sample, na.rm=TRUE) # calculating estimate from model 2
estimate_M2 = na.omit(estimate_M2) #udelader enkelt NA-observation
result_m2_16 = get.rmse(test16_sample$transfer.fee[1:315], estimate_M2) # calculating RMSE from estimate on test sample 
## RMSE = 7,9

##================ 4.5 Lasso model  ================

## Creating matrices with all regressors beacuse the glmnet function only works with matrices
RegressorMatrix_train=model.matrix(transfer.fee~ ., train_sample)
RegressorMatrix_test=model.matrix(transfer.fee~ ., test16_sample)

Model_3 = glmnet(x = RegressorMatrix_train, y = train_sample$transfer.fee)
Model_3

#Calculating the RMSE
Estimate_M3=predict(Model_3, RegressorMatrix_test)
get.rmse(test16_sample$transfer.fee[1:315], Estimate_M3)
## RMSE = 8.2

## Trying to finde the best Lambda
# Calculating RSME for each lambda
lambda_values = Model_3$lambda

performance_Lasso = data.frame()

for (lambda in lambda_values){
  performance_Lasso = rbind(performance_Lasso,
                            data.frame(lambda = lambda,
                                       RMSE = get.rmse(predict(Model_3, RegressorMatrix_test, s = lambda),
                                                       test16_sample$transfer.fee[1:315])))
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
result_m3_16 = get.rmse(test16_sample$transfer.fee[1:315], Estimate_M3)
## RMSE = 7.9

##================ 4.6 Decision tree  ================
set.seed(123)
Model_4=tree(transfer.fee~.,data=train_sample, method="anova")
Model_4$frame
plot(Model_4,niform=TRUE, 
     main="Regression Tree for Transfer fee ")
text(Model_4,pretty=0,use.n=TRUE, cex=.5)


##Estimating transfer fee for test data
Estimate_M4=predict(Model_4,test16_sample)
Estimate_M4
##Calculating RMSE 
get.rmse(test16_sample$transfer.fee,Estimate_M4)  #RMSE = 9.2


## Cross validation to find the optimal number of terminal nodes
cv.Model_4 = cv.tree(Model_4, FUN = prune.tree)
plot(cv.Model_4$size, cv.Model_4$dev, type = "b") #Optimal number with cross validation is 7
best.size=cv.Model_4$size[which.min(cv.Model_4$dev)]
prune.Model_4=prune.tree(Model_4,best = best.size)
plot(prune.Model_4);text(prune.Model_4)
## Calculating estimates with pruned model
pruned.estimate=predict(prune.Model_4,test16_sample)
result_m4_16 = get.rmse(pruned.estimate,test16_sample$transfer.fee)
## Lower RMSE with pruned model: 9.1


##================ 4.7 Random Forest  ================
library(randomForest)
set.seed(1)

Model_5 = randomForest(transfer.fee ~ ., data = train_sample, importance = TRUE)
print(Model_5)

estimate_M5 = predict(Model_5, test16_sample) # calculating estimate from model 5
estimate_M5 = na.omit(estimate_M5) #udelader enkelt NA-observation
result_m5_16 = get.rmse(test16_sample$transfer.fee[1:315], estimate_M5) # calculating the RMSE on test sample = 8.1

var.list = importance(Model_5, type = 1) #calculate the variables influence
varImpPlot(Model_5) # plots the variables influence
var.list

##================ 4.8 Normalizing RMSE  ================
## Finding max and min values for transfer fee 
max16 = max(test16_sample$transfer.fee)
min16 = min(test16_sample$transfer.fee)

max15 = max(test_sample_old$transfer.fee)
min15 = min(test_sample_old$transfer.fee)

## finding normalised RMSE for new data
nrmse_m1 = result_m1_16/(max16-min16)
nrmse_m2 = result_m2_16/(max16-min16)
nrmse_m3 = result_m3_16/(max16-min16)
nrmse_m4 = result_m4_16/(max16-min16)
nrmse_m5 = result_m5_16/(max16-min16)

## finding normalised RMSE for previous data
nrmse_m1_15 = 9.06/(max15-min15)
nrmse_m2_15 = 6.34/(max15-min15)
nrmse_m3_15 = 6.34/(max15-min15)
nrmse_m4_15 = 6.60/(max15-min15)
nrmse_m5_15 = 6.26/(max15-min15)


####### KAN SLETTES HERFRA
# ##================ Merging new and former data sets=======
# View(transfer.data.former)
# train_sample
# train_sample["Indicator"] = "train15"
# test_sample_old
# test_sample_old["Indicator"] = "test15"
# test16_sample
# test16_sample["Indicator"] = "test16"
# total.dataset = rbind(train_sample, test_sample_old, test16_sample)
# 
# train15 = subset(total.dataset, total.dataset$Indicator == "train15")
# test16  = subset(total.dataset, total.dataset$Indicator == "test15")
# 
# library(randomForest)
# set.seed(1)
# 
# Model_5 = randomForest(transfer.fee ~ ., data = train15, importance = TRUE)
# print(Model_5)
# 
# estimate_M5 = predict(Model_5, test16) # calculating estimate from model 5
# get.rmse(test16_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample
# 
# var.list = importance(Model_5, type = 1) #calculate the variables influence
# varImpPlot(Model_5) # plots the variables influence
# var.list
# 
# head(test16$positions)
# levels(test16$positions)
# head(train15$positions)
# levels(test_sample_old$positions)
# levels(test16_sample$positions)
# ny_train
# 
# test16$positions = factor(test16$positions)

##=========================================================================================
##------------------------- 3. Visualisation --- ------------------------------------------
##=========================================================================================
##LIBRARIES
library(plotly)
#install.packages("extrafont")
#install.packages("Cairo")
library(extrafont)
library("ggmap")# getting maps and coordinates from google
library(maptools)# getting maps and coordinates from google
library(maps)# getting maps and coordinates from google
library("ggplot2")# plotting the data
library(dplyr)#tidying dataset
library(rworldmap)
library(stringr)#dealing with strings and replacing strings in observations
library(RCurl)


#creating dataset
df.viz<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", encoding = "UTF8", header = TRUE)

####remove player with transfer fee= 0 et contract time left=0
df.viz<- filter(df.viz,transfer.fee>0)
####remove player with transfer fee= 0 et contract time left=0
#df.stats<- filter(df.stats,transfer.fee>0 | is.na(contract.left.month)==FALSE)


##GETTING STARTING MAP
#Watercolor
myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
                 source="stamen", maptype="watercolor", crop=FALSE,zoom=4)
ggmap(myMap)

#B&W
myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
                 source="stamen",maptype="toner", crop=FALSE,zoom=4)
ggmap(myMap)

##================ 3.1 CLUB MAP WITH TRANSFER SPENDING ================

# #grouping by clubs
# df.spending.club = df.viz %>%
#   group_by(club.to,league)%>%
#   dplyr::summarise(transfer.fee.total = sum(transfer.fee))
# 
# #tidying data frame
# colnames(df.spending.club)[1] <- "team"#change "club to" to "team"
# 
# #TEAM
# 
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
# df.spending.club$team = str_replace(df.spending.club$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
# df.spending.club$team = str_replace(df.spending.club$team,"FC Augsburg","Augsburg FC")
# df.spending.club$team = str_replace(df.spending.club$team,"FC Köln","Cologne FC")
# df.spending.club$team = str_replace(df.spending.club$team,"VfB Stuttgart","Stuttgart VfB")
# df.spending.club$team = str_replace(df.spending.club$team,"Hellas Verona","Verona FC")
# df.spending.club$team = str_replace(df.spending.club$team,"BSC","Berlin")
# df.spending.club$team = str_replace(df.spending.club$team,"Juventus","Juventus Turin")
# df.spending.club$team = str_replace(df.spending.club$team,"Inter","Inter Milan")
# df.spending.club$team = str_replace(df.spending.club$team,"US","FC")
# df.spending.club$team = str_replace(df.spending.club$team,"\\.","")
# df.spending.club$team = str_replace(df.spending.club$team," *\\(.*?\\) *","") #remove (C) for champions
# 
# #class transforming to numeric value or character value
# df.spending.club$transfer.fee.total <- as.numeric(df.spending.club$transfer.fee.total)
# df.spending.club$team <- as.character(df.spending.club$team)
# 
# ###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)
# 
# df.spending.club$team <- with(df.spending.club, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
#                                                        ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
#                                                               ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
#                                                                      ifelse(league=="Premier league", paste(team,"UK", sep = " "),
#                                                                             ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))
# 
# 
# #geocode team
# # geocodes <- geocode(as.character(df.spending.club$team))
# # write_csv(geocodes,"geocodes.csv")
# read_csv("geocodes.csv")
# 
# #new dataframe with geocode
# df.spending.club <- data.frame(df.spending.club[1:3],geocodes)
# 
# out.of.europe<-filter(df.spending.club, lon < -10 |lat < 35)
# out.of.europe.2<- filter(df.spending.club, lon>20 |lat>60)
# out.full= rbind(out.of.europe.2, out.of.europe)
# 
# write_csv(df.spending.club,"df_spending_club_with_geo.csv")

#GETTING DATA
df.spending.club<-read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/df_spending_club_with_geo.csv", encoding = "UTF8", header = TRUE)

#WITH GGPLOT
map.clubs <- ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, size=transfer.fee.total), data =df.spending.club,col="red", alpha=0.4)+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.line = element_line(color = NA),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Total transfer spending for clubs in Europe")+
  labs(size="")+
  scale_size(breaks = c(5.0e+7,1.0e+8,1.5e+8),labels = c("50M£","100M£","150M£"))

map.clubs
#####with plotly
# m <- list(
#   colorbar = list(title = "Total transfer spending"),
#   size = 10, opacity = 0.8, symbol = 'circle'
# )
# 
# # geo styling
# g <- list(
#   scope = 'europe',
#   projection = list(type = 'mercator'),
#   showland = TRUE,
#   landcolor = toRGB("gray95"),
#   subunitcolor = toRGB("gray85"),
#   countrycolor = toRGB("gray85"),
#   countrywidth = 0.5,
#   subunitwidth = 0.5
# )
# g
# 
# plot_ly(df.spending.club, lat = lat, lon = lon,  color = transfer.fee.total,
#         #text = team,
#         hoverinfo = "text" ,
#         text=paste("Team = ", df.spending.club$team,"\n", "Total transfer = ", df.spending.club$transfer.fee.total),
#         type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers', 
#         marker = m) %>%
#   layout(title = 'Football teams in Europe and transfer spending', geo = g)

#================ 3.2 CLUB MAP WITH TRANSFER PATHS ================

# #Getting data from df
# player.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned player data
# transfer.data = player.data
# 
# # transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,# taking a random 50 sample
# #                           replace=FALSE),]
# 
# transfer.data.geo<-transfer.data %>% 
#   group_by(club.from) %>% 
#   dplyr::summarise()
# 
# 
# transfer.data.geo= completeFun(transfer.data.geo,"club.from") #function applied to transfer data to remove unknown origin
# 
# ###Looping for club coordinate
# 
# geocodes.club.from <- geocode(as.character(transfer.data.geo$club.from))
# #transfer.path.origin <- data.frame(transfer.data,geocodes.club.from)
# 
# #transfer.path.origin= data.frame(rep(i, nrow(transfer.data)), transfer.data$V1, transfer.data$lon, transfer.data$lat) #creating a dataset with destination coordinates
# transfer.path.origin= data.frame( player.data.geo$club.from, geocodes.club.from$lon, geocodes.club.from$lat) #creating a dataset with destination coordinates
# colnames(transfer.path.origin)[1] <- "club.from"#change "club.from"
# 
# 
# ###CODE that takes the geocode from 1st visualisation
# #transfer path destination
# colnames(transfer.data)[2] <- "team"#change "club to" to "team"
# #colnames(transfer.data)[5] <- "club.from"#change "club to" to "team"
# 
# #TEAM
# 
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
# transfer.data$team = str_replace(transfer.data$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
# transfer.data$team = str_replace(transfer.data$team,"FC Augsburg","Augsburg FC")
# transfer.data$team = str_replace(transfer.data$team,"FC Köln","Cologne FC")
# transfer.data$team = str_replace(transfer.data$team,"VfB Stuttgart","Stuttgart VfB")
# transfer.data$team = str_replace(transfer.data$team,"Hellas Verona","Verona FC")
# transfer.data$team = str_replace(transfer.data$team,"BSC","Berlin")
# transfer.data$team = str_replace(transfer.data$team,"Juventus","Juventus Turin")
# transfer.data$team = str_replace(transfer.data$team,"Inter","Inter Milan")
# transfer.data$team = str_replace(transfer.data$team,"US","FC")
# transfer.data$team = str_replace(transfer.data$team,"\\.","")
# transfer.data$team = str_replace(transfer.data$team," *\\(.*?\\) *","") #remove (C) for champions
# 
# #class transforming to numeric value or character value
# transfer.data$transfer.fee <- as.numeric(transfer.data$transfer.fee)
# transfer.data$team <- as.character(transfer.data$team)
# 
# ###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)
# 
# transfer.data$team <- with(transfer.data, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
#                                                        ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
#                                                               ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
#                                                                      ifelse(league=="Premier league", paste(team,"UK", sep = " "),
#                                                                             ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))
# 
# 
# transfer.data= completeFun(transfer.data,"lon")#applying the function to remove NA/unidentified to transfer.path
# 
# #transfer path origin
# 
# transfer.path1<-dplyr::left_join(transfer.data, transfer.path.origin, by = "club.from")
# 
# 
# #read geocode and adding it to data frame
# geocodes<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/geocodes.csv", encoding = "UTF8", header = TRUE)
# transfer.data.destination <- data.frame(df.spending.club[1:3],geocodes)
# 
# #building destination list
# transfer.path2<-dplyr::left_join(transfer.data, transfer.data.destination, by = "team")
# 
# 
# 
# 
# #BINDING
# transfer.path1$index<-c(1:nrow(transfer.path1))
# colnames(transfer.path1)[27] <- "lon"#change "club to" to "team"
# colnames(transfer.path1)[28] <- "lat"#change "club to" to "team"
# 
# transfer.path2$index<-c(1:nrow(transfer.path2))
# transfer.path2
# 
# transfer.path.full<-bind_rows(transfer.path1,transfer.path2)
# 
# 
# transfer.path.full<-transfer.path.full %>%
#   select(lon,lat,name,team,club.from,transfer.fee, league,index)#selecting the columns
# 
# transfer.path.full= arrange(transfer.path.full, desc(index))# organising in descending order
#write.csv(transfer.path.full, "transfer_path_full.csv")

transfer.path.full = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transfer_path_full.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned player data

##MAAPING 

path.map<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name)), #putting paths on the map
            colour="red", data = transfer.path.full, alpha=0.4)+
  theme(axis.title.x=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.text= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Transfer for season 2014/2015")

path.map


###BOTH MAPS SUPERPOSED
full.map<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name)), #putting paths on the map
            colour="orange", data = transfer.path.full, alpha=0.4)+
  geom_point(aes(x = lon, y = lat, size=transfer.fee.total), data =df.spending.club,col="red", alpha=0.4)+
  theme(axis.title.x=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.text= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Transfer for season 2014/2015")+
  labs(size="Transfer spending\n per club")+
  scale_size(breaks = c(5.0e+7,1.0e+8,1.5e+8),labels = c("50M£","100M£","150M£"))


full.map

#================ 3.3 TRANSFER FEE BY AGE SCATTER PLOT ================

p.age = ggplot(df.viz, aes(x = transferage , y = transfer.fee))
p.age<-p.age + geom_point(stat = "identity",col="red",alpha=0.4,aes(text = paste("Name:",name)))+ #to use for ggplot
  geom_smooth(aes(colour = transferage, fill = transferage))+
  ggtitle("Age repartition of transfers in European leagues")+
  labs(y="Transfer price\nin M£",x="Age") +
  theme(axis.ticks.y= element_line(color=NA),
        axis.ticks.x=element_line(colour="#CACACA", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        axis.title.x =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = -1,
                                   hjust = 1 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))
p.age

#plotly.p.age<-ggplotly(p.age)
#plotly.p.age

#================ 3.4 TRANSFER FEE BY TIME LEFT ON CONTRACT SCATTER PLOT ================

p.time <- ggplot(data=df.viz, aes(x = contract.left.month , y = transfer.fee)) +
  geom_point(stat = "identity",col="red",alpha=0.4,aes(text = paste("Name:",name)))+
  #geom_point(aes(text = paste(name, " to ", club.to)), size = 4) +
  geom_smooth(aes(colour = contract.left.month, fill = contract.left.month))+
  ggtitle("Time left on contract seems to be positively correlated with transfer fees")+
  labs(y="Transfer price\nin M£",x="Time left on contract\nin months")+
  scale_x_continuous(breaks=seq(0,58,12))+
  theme(axis.ticks.y= element_line(color=NA),
        axis.ticks.x=element_line(colour="#CACACA", size=0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        axis.title.x =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = -1,
                                   hjust = 1 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2),
        panel.grid.major.x =element_blank(), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))

p.time

# plotly.p.time <- ggplotly(p.time)
# plotly.p.time

#================ 3.5 AVERAGE SPENDING PER CLUB PER LEAGUE  ================

#finding mean for every league
df.viz.ave<-df.viz %>% 
  group_by(league) %>% 
  dplyr::summarise(
    sum.transfer=sum(transfer.fee))

df.viz.ave<-df.viz.ave %>% 
  mutate(number.of.clubs = ifelse(league=="Bundesliga",18,20)) %>% 
  mutate(average.spending=sum.transfer/number.of.clubs)
#ordering
df.viz.ave <- transform(df.viz.ave, 
                        league = reorder(league, average.spending))
#plotting it
total.league = ggplot(df.viz.ave, aes( x =league, y=average.spending, fill=league))
total.league<-total.league + geom_bar(stat="identity",alpha=1)+
  theme(axis.title.x=element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Premier League Clubs spend far more on average than any other leagues' clubs\n")+
  scale_fill_manual("National leagues",
                    values=c( "#F2E7DA", "#E89090","#92A0B0", "#C0CFAE", "#525252", "#DEE3DC"))+
  labs(y="Average transfer spending\nper league in M£")

total.league

#================ 3.6 AVERAGE SPENDING PER PLAYER PER LEAGUE  ================


#finding mean spending per player for different leagues
df.viz.player.league<-df.viz %>% 
  group_by(league) %>% 
  dplyr::summarise(average.spending.per.player=mean(transfer.fee))

#ordering
df.viz.player.league <- transform(df.viz.player.league, 
                                  league = reorder(league, average.spending.per.player))
#plotting it
ave.player = ggplot(df.viz.player.league, aes( x =league, y=average.spending.per.player, fill=league))
ave.player<-ave.player + geom_bar(stat="identity",alpha=1)+
  theme(axis.title.x=element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        text=element_text(family="LM Roman 10"))+
  ggtitle("Premier League Clubs spend far more on average than any other leagues' clubs\n")+
  scale_fill_manual("National leagues",
                    values=c( "#E89090","#F2E7DA","#C0CFAE","#92A0B0",  "#525252", "#DEE3DC"))+
  labs(y="Average transfer price\nper player in M£") 
ave.player

#================ 3.7 AVERAGE SPENDING PER PLAYER PER CLUB STATUS  ================


df.viz.status<-df.viz %>% 
  group_by(Status) %>% 
  dplyr::summarise(mean.transfer=mean(transfer.fee))

#Ordering
df.viz.status <- transform(df.viz.status, 
                           Status = reorder(Status, mean.transfer))

#Plotting

p.club = ggplot(df.viz.status, aes(y= mean.transfer, x = Status, fill=Status))
p.club<- p.club + geom_bar(stat = "identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_text(size  = 9),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1),
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text=element_blank(),
        legend.title=element_blank(),
        legend.position="none",
        text=element_text(family="LM Roman 10"))+
  ggtitle("Top Club spend far more\non average than other leagues' clubs\n")+
  scale_fill_manual(values=c( "#CFF09E", "#A8DBA8", "#79BD9A", "#3B8686"))+
  labs(y="Average transfer price\nper player in M£") 

p.club


