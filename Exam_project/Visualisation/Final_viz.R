################################################################################################################################################
################################################################################################################################################
####################################                                             ###############################################################          
####################################              FINAL VISUALISATIONS           ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################
################################################################################################################################################library(plotly)

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
font_import()
font_import(pattern = "/Library/Fonts/lmroman10-bold.ttf")

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
################################################################################################################################################
####################################                                             ###############################################################          
####################################         CLUB MAP W TRANSFER SPENDING        ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################


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


################################################################################################################################################
####################################                                             ###############################################################          
####################################         MAPS WITH TRANSFER PATHS            ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################

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

################################################################################################################################################
####################################                                             ###############################################################          
####################################         TRANSFER FEE BY AGE SCATTER PLOT    ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################

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

################################################################################################################################################
####################################                                             ###############################################################          
####################################         TRANSFER FEE BY TIME LEFT PLOT      ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################

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

################################################################################################################################################
####################################                                             ###############################################################          
####################################         AVERAGE SPENDING PER CLUB           ############################################################### 
####################################                PER LEAGUE                   ###############################################################
####################################                                             ###############################################################
################################################################################################################################################

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

################################################################################################################################################
####################################                                             ###############################################################          
####################################         AVERAGE SPENDING PER PLAYER         ############################################################### 
####################################                PER LEAGUE                   ###############################################################
####################################                                             ###############################################################
################################################################################################################################################

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

################################################################################################################################################
####################################                                             ###############################################################          
####################################         AVERAGE SPENDING PER PLAYERS        ############################################################### 
####################################                 PER CLUB STATUS             ###############################################################
####################################                                             ###############################################################
################################################################################################################################################

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
