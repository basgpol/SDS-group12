#######################################################################################################################
###################################################  MAP   ############################################################
#######################################################################################################################
#intalling packages

#install.packages("ggmap")
#install.packages("maptools")

#calling libraries
library(plotly)
library("ggmap")
library(maptools)
library(maps)
library("ggplot2")
library(dplyr)
library(rworldmap)
library(stringr)


#Getting starting map

myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
source="stamen", maptype="watercolor", crop=FALSE,,zoom=3)
ggmap(myMap)


####################example############################
#Munich<-geocode("Munich")
#ggmap(myMap)+
#  geom_point(aes(Munich[1],Munich[2]), data = Munich,
#             alpha = .5, color="darkred", size = 3)
#######################################################

#Getting data from club
club.data<-read.csv("club_data_unclean.csv",header=TRUE, stringsAsFactors=TRUE, fileEncoding="latin1")
mydata = club.data
#mydata <- mydata[sample(1:nrow(mydata), 50,# taking a random 50 sample
#                        replace=FALSE),]

#tidying data frame
colnames(mydata)[3] <- "team"#change "team/N/E/V" to "team"

#POINTS

mydata$Pts = str_replace(mydata$Pts,"[fecd]","")#removing the unwanted characters
mydata$Pts = str_replace(mydata$Pts,"*\\[.*?\\] *","")#removing the unwanted characters between brackets

#TEAM

mydata$team = str_replace(mydata$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
mydata$team = str_replace(mydata$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
mydata$team = str_replace(mydata$team,"FC Augsburg","Augsburg FC")
mydata$team = str_replace(mydata$team,"FC Köln","Cologne FC")
mydata$team = str_replace(mydata$team,"VfB Stuttgart","Stuttgart VfB")
mydata$team = str_replace(mydata$team,"Hellas Verona","Verona FC")
mydata$team = str_replace(mydata$team,"BSC","Berlin")
mydata$team = str_replace(mydata$team,"Juventus","Juventus Turin")
mydata$team = str_replace(mydata$team,"\\.","")
mydata$team = str_replace(mydata$team," *\\(.*?\\) *","") #remove (C) for champions

#class transforming to numeric value or character value
mydata$Pts <- as.numeric(mydata$Pts)
mydata$team <- as.character(mydata$team)

###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)

mydata$team <- with(mydata, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
                                   ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
                                          ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
                                                 ifelse(league=="Premier league", paste(team,"UK", sep = " "),
                                                        ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))


#Looping for club coordinate
for (i in 1:nrow(mydata)) {
  latlon = geocode(mydata[i,3],source=c("google","dsk"))
  mydata$lon[i] = as.numeric(latlon[1])
  mydata$lat[i] = as.numeric(latlon[2])
  
}

#coordinates should be between lon [-10,20] and lat [35:60]
out.of.europe<-filter(mydata, lon < -10 |lat < 35)
out.of.europe.2<- filter(mydata, lon>20 |lat>60)
out.full= rbind(out.of.europe.2, out.of.europe)

# write(mydata,file="club_for_viz.csv")
# Creating a cleaner dataset
clubs.points = data.frame(mydata$Pts, mydata$lon, mydata$lat,mydata$team)
colnames(clubs.points) = c('Pts','lon','lat','team')#naming the variables

# Getting data on the map

####former way###

#ggmap(myMap)+
#geom_point(aes(x=lon, y=lat), data=clubs.points, col="red", alpha=0.4, size=clubs.points$Pts*1/10) + 
#scale_size_continuous(range=range(clubs.points$Pts))+
# theme(axis.title=element_blank(),
#        axis.title.y=element_blank(),
#        axis.text.y=element_blank(),
#        axis.text=element_blank(),
#        axis.ticks= element_line(color=NA),
#        axis.line = element_line(color = NA))

###new way###

mapclubs <- ggmap(myMap) +
     geom_point(aes(x = lon, y = lat, size=Pts^2), data =clubs.points,col="red", alpha=0.4)+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.line = element_line(color = NA))
mapclubs

#########trying with plotly

m <- list(
  colorbar = list(title = "Points per team"),
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

plot_ly(clubs.points, lat = lat, lon = lon, text = team, color = Pts,
        type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers',
        marker = m) %>%
  layout(title = 'Football teams in Europe and points<br>(Hover for airport)', geo = g)

?scope
??scope
#############################################################################################
################################ CREATING PLAYER PATHS ####################################
#############################################################################################

#Getting data from df
player.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="latin1") # loading saved version of uncleaned player data
transfer.data = player.data 

# transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,# taking a random 50 sample
#                           replace=FALSE),]

completeFun <- function(data, desiredCols) { #function to remove empty/NA
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
transfer.data= completeFun(transfer.data,"club.to") #function applied to transfer data to remove unknown origin


###Looping for club coordinate
geocodes.club.to <- geocode(as.character(transfer.data$club.to))
transfer.path.origin <- data.frame(transfer.data,geocodes.club.to)

geocodes.club.from <- geocode(as.character(transfer.data$club.from))
transfer.path.destination <- data.frame(transfer.data,geocodes.club.from)

# transfer.path.origin<-for (i in 1:nrow(transfer.data)) { #getting origin coordinates
#   latlon = geocode(transfer.data[i,4])
#   transfer.data$lon[i] = as.numeric(latlon[1])
#   transfer.data$lat[i] = as.numeric(latlon[2])
# }
# 
# transfer.path.origin = data.frame(rep(i, nrow(transfer.data)), transfer.data$V1, transfer.data$lon, transfer.data$lat) #creating a dataset with origin
# 
# transfer.path.destination<-for (i in 1:nrow(transfer.data)) { #getting destination coordinates
#   latlon = geocode(transfer.data[i,5])
#   transfer.data$lon[i] = as.numeric(latlon[1])
#   transfer.data$lat[i] = as.numeric(latlon[2])
# }
# 
# transfer.path.destination= data.frame(rep(i, nrow(transfer.data)), transfer.data$V1, transfer.data$lon, transfer.data$lat) #creating a dataset with destination coordinates

#BINDING

transfer.path.full= rbind(transfer.path.origin, transfer.path.destination)#binding
transfer.path.full<-transfer.path.full %>% 
  select(lon,lat,name,club.to,club.from,transfer.fee)#selecting the columns
transfer.path.full= arrange(transfer.path.full, desc(index))# organising in descending order
write.csv(transfer.path.full, "transfer_path_full.csv")

transfer.path.full= completeFun(transfer.path.full,"lon")#applying the function to remove NA/unidentified to transfer.path

gg<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name), size=transfer.fee), #putting paths on the map
            colour="red", data = transfer.path.full, alpha=0.3)
gg
