#######################################################################################################################
###################################################  MAP   ############################################################
#######################################################################################################################
#intalling packages

#install.packages("ggmap")
#install.packages("maptools")

#calling libraries

library("ggmap")
library(maptools)
library(maps)
library("ggplot2")
library(dplyr)
library(rworldmap)



#Getting starting map

myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
source="stamen", maptype="watercolor", crop=FALSE,,zoom=5)
ggmap(myMap)


####################example############################
#Munich<-geocode("Munich")
#ggmap(myMap)+
#  geom_point(aes(Munich[1],Munich[2]), data = Munich,
#             alpha = .5, color="darkred", size = 3)
#######################################################

#Getting data from club
mydata = club.data

#tidying data frame
colnames(mydata)[2] <- "team"#change "team/N/E/V" to "team"

#POINTS

mydata$Pts = str_replace(mydata$Pts,"[fecd]","")#removing the unwanted characters
mydata$Pts = str_replace(mydata$Pts,"*\\[.*?\\] *","")#removing the unwanted characters between brackets

#TEAM

mydata$team = str_replace(mydata$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"[1234567890]","")
mydata$team = str_replace(mydata$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
mydata$team = str_replace(mydata$team,"Mönchen","München")
mydata$team = str_replace(mydata$team,"Borussia Münchengladbach","Münchengladbach Borussia")
mydata$team = str_replace(mydata$team,"FC Augsburg","Augsburg FC")
mydata$team = str_replace(mydata$team,"FC Köln","Köln FC")
mydata$team = str_replace(mydata$team,"VfB Stuttgart","Stuttgart VfB")
mydata$team = str_replace(mydata$team,"Hellas Verona","Hellas Verona FC")
mydata$team = str_replace(mydata$team,"BSC","Berlin")
mydata$team = str_replace(mydata$team,"\\.","")
mydata$team = str_replace(mydata$team," *\\(.*?\\) *","") #remove (C) for champions

#class
mydata$Pts <- as.numeric(mydata$Pts)
mydata$team <- as.character(mydata$team)

#Looping for club coordinate
for (i in 1:nrow(mydata)) {
  latlon = geocode(mydata[i,2])
  mydata$lon[i] = as.numeric(latlon[1])
  mydata$lat[i] = as.numeric(latlon[2])
  
}

# Creating a cleaner dataset
clubs.points = data.frame(mydata$Pts, mydata$lon, mydata$lat)
colnames(clubs.points) = c('Pts','lon','lat')

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


#############################################################################################
################################ CREATING PLAYER PATHS ####################################
#############################################################################################

#Getting data from df
player.data = read.csv("player_data_unclean.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1") # loading saved version of uncleaned player data
transfer.data = player.data 
transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,
                          replace=FALSE),]
#Looping for club coordinate
for (i in 1:nrow(transfer.data)) { #getting origin coordinates
  latlon = geocode(transfer.data[i,4])
  transfer.data$lon[i] = as.numeric(latlon[1])
  transfer.data$lat[i] = as.numeric(latlon[2])
}

transfer.path = data.frame(rep(i, nrow(transfer.data)), transfer.data$lon, transfer.data$lat) #creating a dataset with origin

for (i in 1:nrow(transfer.data)) { #getting destination coordinates
  latlon = geocode(transfer.data[i,5])
  transfer.data$lon[i] = as.numeric(latlon[1])
  transfer.data$lat[i] = as.numeric(latlon[2])
}

transfer.path2= data.frame(rep(i, nrow(transfer.data)), transfer.data$lon, transfer.data$lat) #creating a dataset with destination coordinates

#BINDING

transfer.path.full= rbind(transfer.path2, transfer.path)#binding
colnames(transfer.path.full) = c('index','lon','lat')#naming the columns
transfer.path.full= arrange(transfer.path.full, desc(index))# organising in descending order

completeFun <- function(data, desiredCols) { #function to remove NA
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
transfer.path.full= completeFun(transfer.path.full,"lon")#applying the function to transfer.path

ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(index)), #putting paths on the map
            colour="red", data = transfer.path.full, alpha=0.3)

