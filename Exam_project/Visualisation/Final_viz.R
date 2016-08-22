################################################################################################################################################
################################################################################################################################################
####################################                                             ###############################################################          
####################################              FINAL VISUALISATIONS           ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################
################################################################################################################################################library(plotly)

##LIBRARIES
library("ggmap")# getting maps and coordinates from google
library(maptools)# getting maps and coordinates from google
library(maps)# getting maps and coordinates from google
library("ggplot2")# plotting the data
library(dplyr)#tidying dataset
library(rworldmap)
library(stringr)#dealing with strings and replacing strings in observations

#creating dataset
df.viz<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", encoding = "UTF8", header = TRUE)

####remove player with transfer fee= 0 et contract time left=0
df.viz<- filter(df.viz,transfer.fee>0)
####remove player with transfer fee= 0 et contract time left=0
#df.stats<- filter(df.stats,transfer.fee>0 | is.na(contract.left.month)==FALSE)


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
#geocode team
geocodes <- geocode(as.character(df.spending.club$team))

#new dataframe with geocode
df.spending.club <- data.frame(df.spending.club[1:3],geocodes)

out.of.europe<-filter(df.spending.club, lon < -10 |lat < 35)
out.of.europe.2<- filter(df.spending.club, lon>20 |lat>60)
out.full= rbind(out.of.europe.2, out.of.europe)

write_csv(df.spending.club,"df_spending_club_with_geo.csv")
read_csv(("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/df_spending_club_with_geo.csv", encoding = "UTF8", header = TRUE)
)
#####with plotly
m <- list(
  colorbar = list(title = "Total transfer spending"),
  size = 10, opacity = 0.8, symbol = 'circle'
)

# geo styling
g <- list(
  scope = 'europe',
  projection = list(type = 'mercator'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)
g

plot_ly(df.spending.club, lat = lat, lon = lon,  color = transfer.fee.total,
        #text = team,
        hoverinfo = "text" ,
        text=paste("Team = ", df.spending.club$team,"\n", "Total transfer = ", df.spending.club$transfer.fee.total),
        type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers', 
        marker = m) %>%
  layout(title = 'Football teams in Europe and transfer spending<br>(Hover for airport)', geo = g)


################################################################################################################################################
####################################                                             ###############################################################          
####################################         TRANSFER FEE BY AGE SCATTER PLOT    ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################

p.age = ggplot(df.viz, aes(x = transferage , y = transfer.fee))
p.age<-p.age + geom_point(stat = "identity")+
          #geom_point(aes(text = paste("Name:",name))+
          geom_smooth(aes(colour = transferage, fill = transferage))+
          ggtitle("Age repartition of transfers in European leagues????")

p.age
plotly.p.age<-ggplotly(p.age)
plotly.p.age

################################################################################################################################################
####################################                                             ###############################################################          
####################################         TRANSFER FEE BY TIME LEFT PLOT      ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################

p.time <- ggplot(data=df.viz, aes(x = contract.left.month , y = transfer.fee)) +
  geom_point(stat = "identity")+
  #geom_point(aes(text = paste(name, " to ", club.to)), size = 4) +
  geom_smooth(aes(colour = contract.left.month, fill = contract.left.month))+
  ggtitle("Time left on contract seems to be positively correlated with transfer fees")
p.time

plotly.p.time <- ggplotly(p.time)
plotly.p.time

################################################################################################################################################
####################################                                             ###############################################################          
####################################         AVERAGE SPENDING BY CLUB            ############################################################### 
####################################                PER LEAGUE                   ###############################################################
####################################                                             ###############################################################
################################################################################################################################################

#finding mean for every league
df.viz.ave<-df.viz %>% 
  group_by(league) %>% 
  dplyr::summarise(
    mean.transfer=mean(transfer.fee))

#plotting it
df.viz.ave$mean.transfer<-as.numeric(df.viz.ave$mean.transfer)
p = ggplot(df.viz.ave, aes( x =league, y=mean.transfer))
p<-p + geom_bar(stat="identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_text(size  = 7,
                                  angle = 45,
                                  hjust = 1,
                                  vjust = 1),
        axis.ticks= element_line(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Goudy Old Style"))+
  ggtitle("Premier League Clubs spend far more on average than other leagues' clubs")
p

################################################################################################################################################
####################################                                             ###############################################################          
####################################         AVERAGE SPENDING BY CLUB            ############################################################### 
####################################                  STATUS                     ###############################################################
####################################                                             ###############################################################
################################################################################################################################################

df.viz.status<-df.viz %>% 
  group_by(Status) %>% 
  dplyr::summarise(mean.transfer=mean(transfer.fee))

p = ggplot(df.viz.status, aes(y= mean.transfer, x = Status))
p + geom_bar(stat = "identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_text(size  = 7,
                                  angle = 45,
                                  hjust = 1,
                                  vjust = 1),
        axis.ticks= element_line(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Goudy Old Style"))+
  ggtitle("Top Club spend far more on average than other leagues' clubs")

################################################################################################################################################
####################################                                             ###############################################################          
####################################         MAPS WITH TRANSFER PATHS            ############################################################### 
####################################                                             ###############################################################
################################################################################################################################################
# 
# #Getting data from df
# player.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="latin1") # loading saved version of uncleaned player data
# transfer.data = player.data 
# 
# # transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,# taking a random 50 sample
# #                           replace=FALSE),]
# 
# completeFun <- function(data, desiredCols) { #function to remove empty/NA
#   completeVec <- complete.cases(data[, desiredCols])
#   return(data[completeVec, ])
# }
# transfer.data= completeFun(transfer.data,"club.to") #function applied to transfer data to remove unknown origin
# 
# 
# ###Looping for club coordinate
# geocodes.club.to <- geocode(as.character(transfer.data$club.to))
# transfer.path.origin <- data.frame(transfer.data,geocodes.club.to)
# 
# geocodes.club.from <- geocode(as.character(transfer.data$club.from))
# transfer.path.destination <- data.frame(transfer.data,geocodes.club.from)

# transfer.path.destination= data.frame(rep(i, nrow(transfer.data)), transfer.data$V1, transfer.data$lon, transfer.data$lat) #creating a dataset with destination coordinates

#BINDING

# transfer.path.full= rbind(transfer.path.origin, transfer.path.destination)#binding
# transfer.path.full<-transfer.path.full %>% 
#   select(lon,lat,name,club.to,club.from,transfer.fee)#selecting the columns
# transfer.path.full= arrange(transfer.path.full, desc(index))# organising in descending order
# write.csv(transfer.path.full, "transfer_path_full.csv")

read_csv("transfer_path_full.csv", encoding = "UTF8", header = TRUE) # must be tweaked so only club.to is in europe

transfer.path.full= completeFun(transfer.path.full,"lon")#applying the function to remove NA/unidentified to transfer.path


gg<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name)), #putting paths on the map
            colour="red", data = transfer.path.full, alpha=0.3)
gg

