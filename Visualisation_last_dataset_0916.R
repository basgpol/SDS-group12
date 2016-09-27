##=========================================================================================
##------------------------- 3. Visualisation --- ------------------------------------------
##=========================================================================================
##LIBRARIES
install.packages("plotly")
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
df.viz<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Latest%20transferdata/transferdata16.final.csv", encoding = "UTF8", header = TRUE)

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
df.spending.club = df.viz %>%
  group_by(club.to,league)%>%
  dplyr::summarise(transfer.fee.total = sum(transfer.fee))

#tidying data frame
colnames(df.spending.club)[1] <- "team"#change "club to" to "team"

#TEAM

df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
df.spending.club$team = str_replace(df.spending.club$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
df.spending.club$team = str_replace(df.spending.club$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
df.spending.club$team = str_replace(df.spending.club$team,"FC Augsburg","Augsburg FC")
df.spending.club$team = str_replace(df.spending.club$team,"FC Köln","Cologne FC RheinEnergieStadion")
df.spending.club$team = str_replace(df.spending.club$team,"VfB Stuttgart","Stuttgart VfB")
df.spending.club$team = str_replace(df.spending.club$team,"Hellas Verona","Verona FC")
df.spending.club$team = str_replace(df.spending.club$team,"Celta de Vigo","Sede Real Club Celta de Vigo")
df.spending.club$team = str_replace(df.spending.club$team,"BSC","Berlin")
df.spending.club$team = str_replace(df.spending.club$team,"FC Schalke","Schalke 04 FC ")
df.spending.club$team = str_replace(df.spending.club$team,"FC Lorient","Lorient FC")
df.spending.club$team = str_replace(df.spending.club$team,"OGC Nice","Nice OGC")
df.spending.club$team = str_replace(df.spending.club$team,"Burnley FC","Burnley Football Club")
df.spending.club$team = str_replace(df.spending.club$team,"Juventus","Juventus Turin")
df.spending.club$team = str_replace(df.spending.club$team,"Inter","Inter Milan")
df.spending.club$team = str_replace(df.spending.club$team,"SM Caen","Stade Malherbe Caen")
df.spending.club$team = str_replace(df.spending.club$team,"TSG Hoffenheim","Hoffenheim")
df.spending.club$team = str_replace(df.spending.club$team,"Sevilla FC","Sevilla Fútbol Club")
df.spending.club$team = str_replace(df.spending.club$team,"Real Betis","Real Betis Sevilla")
df.spending.club$team = str_replace(df.spending.club$team,"US","FC")
df.spending.club$team = str_replace(df.spending.club$team,"\\.","")
df.spending.club$team = str_replace(df.spending.club$team," *\\(.*?\\) *","") #remove (C) for champions

#class transforming to numeric value or character value
df.spending.club$transfer.fee.total <- as.numeric(df.spending.club$transfer.fee.total)
df.spending.club$team <- as.character(df.spending.club$team)

###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)

df.spending.club$team <- with(df.spending.club, ifelse(league=="Bundesliga", paste(team,"Germany", sep = " "),
                                                       ifelse(league=="Ligue 1", paste(team,"France", sep = " "),
                                                              ifelse(league=="Serie A", paste(team,"Italy", sep = " "),
                                                                     ifelse(league=="Premier league", paste(team,"UK", sep = " "),
                                                                            ifelse(league=="La Liga", paste(team,"Spain", sep = " "),""))))))


#geocode team
# geocodes <- geocode(as.character(df.spending.club$team))
#write.csv(geocodes,"geocodes.csv")
#read_csv("geocodes.csv")

#new dataframe with geocode
df.spending.club <- data.frame(df.spending.club[1:3],geocodes)

out.of.europe<-filter(df.spending.club, lon < -10 |lat < 35)
out.of.europe.2<- filter(df.spending.club, lon>20 |lat>60)
out.full= rbind(out.of.europe.2, out.of.europe)
df.spending.club[35,5] = 51.558816
df.spending.club[35,6] = 7.063850
df.spending.club[55,c(5,6)]= c(43.683284, 7.197926)
df.spending.club[19,c(5,6)]= c(42.230694, -8.720006)
df.spending.club[84,c(5,6)]= c(28.100462, -15.456771)
df.spending.club$lat = str_replace(df.spending.club$lat,"40.37573","40.38000")
df.spending.club$lat = as.numeric(df.spending.club$lat)

write.csv(df.spending.club,"df_spending_club_with_geo.csv")

#GETTING DATA
df.spending.club<-read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Final%20documents/df_spending_club_with_geo.csv", encoding = "UTF8", header = TRUE)

#data.frame[row_number, column_number] = new_value


#WITH GGPLOT
# map.clubs <- ggmap(myMap) +
#   geom_point(aes(x = lon, y = lat, size=transfer.fee.total), data =df.spending.club,col="red", alpha=0.4)+
#   theme(axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks= element_line(color=NA),
#         axis.line = element_line(color = NA),
#         text=element_text(family="LM Roman 10"))+
#   ggtitle("Total transfer spending for clubs in Europe")+
#   labs(size="")+
#   scale_size(breaks = c(5.0e+7,1.0e+8,1.5e+8),labels = c("50M£","100M£","150M£"))
# 
# map.clubs
####with plotly

# m <- list(
#   colorbar = list(title = "Total transfer spending"),
#   size = log(df.spending.club$transfer.fee.total)*6 , opacity = 0.8, symbol = 'circle'
# )

f <- list(
  family = "Goudy Old Style, serif",
  size = 16,
  color = "#525252"
)

mar = list(
  l = 10,
  r = 10,
  b = 30,
  t = 70,
  pad = 4
)

#geo styling
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


p2<- plot_ly(df.spending.club, lat = lat, lon = lon,  color = transfer.fee.total,
            marker=list(size = log(df.spending.club$transfer.fee.total)*6, colorbar=list(title = "M£", borderwidth=0,outlinecolor="white")),
            # marker=list(size =18),
            opacity=0.5,
            #text = team,
            hoverinfo = "text" ,
            text=paste("", df.spending.club$team, "<br>", df.spending.club$transfer.fee.total,"M£"),
            type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers',
            marker = m) %>%
  layout(title = 'Transfer spendings per<br>Football teams in Europe', font=f, margin=mar, geo = g)
p2

#########
# p<- plot_ly(df.spending.club, lat = lat, lon = lon,  color = transfer.fee.total, marker=list(size = transfer.fee.total),opacity=0.7,
#         #text = team,
#         hoverinfo = "text" ,
#         text=paste("", df.spending.club$team, "<br>", df.spending.club$transfer.fee.total,"M£"),
#         type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers',
#         marker = m) %>%
#   layout(title = 'Football teams in Europe and transfer spending', geo = g)
# p
#================ 3.2 CLUB MAP WITH TRANSFER PATHS ================

#Getting data from df
player.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Latest%20transferdata/transferdata16.final.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned player data
transfer.data = player.data

# transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,# taking a random 50 sample
#                           replace=FALSE),]
# geocodes.club.from <- geocode(as.character(transfer.data$club.from))
# transfer.path.origin <- data.frame(transfer.data,geocodes.club.from)
#write.csv(transfer.path.origin,"transfer.path.origin.csv")
transfer.path.origin <-read.csv("/Users/guillaumeslizewicz/Documents/SDS-group12/Final documents/transfer.path.origin.csv")

# transfer.path.destination<-left_join(df.spending.club,transfer.data,by="team")
#write.csv(transfer.path.destination,"transfer.path.destination.csv")
transfer.path.destination<-read.csv("/Users/guillaumeslizewicz/Documents/SDS-group12/Final documents/transfer.path.destination.csv")

transfer.path.destination.clean<-data.frame( transfer.path.destination$name, transfer.path.destination$team, transfer.path.destination$club.from,transfer.path.destination$league.x, transfer.path.destination$transfer.fee, transfer.path.destination$lon, transfer.path.destination$lat)
colnames(transfer.path.destination.clean)[1] <- "name"#change "club to" to "team"
colnames(transfer.path.destination.clean)[2] <- "team"#change "club to" to "team"
colnames(transfer.path.origin.clean)[6] <- "lon_d"#change "club to" to "team"
colnames(transfer.path.origin.clean)[7] <- "lat_d"

transfer.path.origin.clean<-data.frame( transfer.path.origin$name, transfer.path.origin$team, transfer.path.origin$club.from,transfer.path.origin$league, transfer.path.origin$transfer.fee, transfer.path.origin$lon, transfer.path.origin$lat)
colnames(transfer.path.origin.clean)[1] <- "name"#change "club to" to "team"
colnames(transfer.path.origin.clean)[2] <- "team"#change "club to" to "team"
colnames(transfer.path.origin.clean)[6] <- "lon_o"#change "club to" to "team"
colnames(transfer.path.origin.clean)[7] <- "lat_o"

#BINDING

#transfer.path.full<-bind_rows(transfer.path.origin.clean,transfer.path.destination.clean)
transfer.path.full<-left_join(transfer.path.origin.clean,transfer.path.destination.clean, by="name")
#write.csv(transfer.path.full, "transfer_path_full16.csv")
#transfer.path.full<-read.csv("/Users/guillaumeslizewicz/Documents/SDS-group12/Final documents/transfer_path_full16.csv")
colnames(transfer.path.full)[12] <- "lon_d"#change "club to" to "team"
colnames(transfer.path.full)[13] <- "lat_d"
transfer.path.full<-transfer.path.full[complete.cases(transfer.path.full[,c(6,7,12,13)]),]
id <- rownames(transfer.path.full)
transfer.path.full <- cbind(id=id, transfer.path.full)

#transfer.path.full = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transfer_path_full.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned player data


##MAAPING 

geo <- list(
  scope = 'Europe',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_geo(locationmode = 'Europe', color = I("red")) %>%
  add_markers(
    data = transfer.path.full[1:10,], x = ~lon_o, y = ~lat_o, text = ~name, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(transfer.path.full[1:10,],id),
    x = ~lon_o, xend = ~lon_d,
    y = ~lat_o, yend = ~lat_d,
    alpha = 0.3, size = I(1), hoverinfo = "text"
  ) %>%
  layout( title = 'E T 2016',
    geo = geo, showlegend = FALSE, height=800
  )

# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

plot_geo(locationmode = 'USA-states', color = I("red")) %>%
  add_markers(
    data = air, x = ~long, y = ~lat, text = ~airport,
    size = ~cnt, hoverinfo = "text", alpha = 0.5
  ) %>%
  add_segments(
    data = group_by(flights, id),
    x = ~start_lon, xend = ~end_lon,
    y = ~start_lat, yend = ~end_lat,
    alpha = 0.3, size = I(1), hoverinfo = "none"
  ) %>%
  layout(
    title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
    geo = geo, showlegend = FALSE, height=800
  )
#================ 3.3 TRANSFER FEE BY AGE SCATTER PLOT ================

# p.age = ggplot(df.viz, aes(x = transferage , y = transfer.fee))
# p.age<-p.age + geom_point(stat = "identity",alpha=0.4,aes(text = paste("",name)))+ #to use for ggplot
#   geom_smooth(aes(colour = transferage, fill = transferage))+
#   ggtitle("Age repartition of transfers in European leagues")+
#   labs(y="Transfer price\nin M£",x="Age") +
#   theme(axis.ticks.y= element_line(color=NA),
#         axis.ticks.x=element_line(colour="#CACACA", size=0.2),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.title.y =element_text(angle = 0,
#                                    colour="#525252",
#                                    vjust = 1,
#                                    hjust = 0 ),
#         axis.title.x =element_text(angle = 0,
#                                    colour="#525252",
#                                    vjust = -1,
#                                    hjust = 1 ),
#         panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
#         axis.title.y=element_blank(),
#         text=element_text(family="Goudy Old Style"))
# p.age

# #plotly.p.age<-ggplotly(p.age)
# plotly.p.age<-ggplotly(
#   p.age, x = ~transferage, y = ~transfer.fee.total,
#   hoverinfo = "text",
#   text = paste("", name, "<br> Age=", transferage,"<br> Transfer price=", transfer.fee.total),
#   text = ~paste("", name),
#   color = ~df.viz$league
# )
# plotly.p.age

f <- list(
  family = "Goudy Old Style, serif",
  size = 16,
  color = "#525252"
)
x <- list(
  title = "Age",
  titlefont = f
)
y <- list(
  title ="Transfer fee in M£", 
  titlefont = f
)

m = list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

p <- df.viz %>% 
  plot_ly(x = transferage, y = transfer.fee, mode = "markers",color = league , marker = list(size = 15), opacity=0.6,
          hoverinfo = "text",
          text = paste("", name, "<br>", round(df.viz$transferage, digits = 1), "years old","<br>", df.viz$transfer.fee, "M£", "<br>", df.viz$club.to)) %>% 
  layout(title ="Transfer price per age", xaxis = x, yaxis = y, font = f, margin=m)
p


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

total.league2<- plot_ly(
    x = c("giraffes", "orangutans", "monkeys"),
    y = c(20, 14, 23),
    name = "SF Zoo",
    type = "bar"
  )
total.league2
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
