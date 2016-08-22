####################################################################################################################################
########## New visualisation maps of transfer spending per club##############################################################################
####################################################################################################################################

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
myMap <- get_map(location= myLocation)

#creating dataset
df.spending<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", encoding = "UTF8", header = TRUE)

#grouping by clubs
df.spending.club = df.spending %>%
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
df.spending.club$team = str_replace(df.spending.club$team,"FC Köln","Cologne FC")
df.spending.club$team = str_replace(df.spending.club$team,"VfB Stuttgart","Stuttgart VfB")
df.spending.club$team = str_replace(df.spending.club$team,"Hellas Verona","Verona FC")
df.spending.club$team = str_replace(df.spending.club$team,"BSC","Berlin")
df.spending.club$team = str_replace(df.spending.club$team,"Juventus","Juventus Turin")
df.spending.club$team = str_replace(df.spending.club$team,"Inter","Inter Milan")
df.spending.club$team = str_replace(df.spending.club$team,"US","FC")
df.spending.club$team = str_replace(df.spending.club$team,"\\.","")
df.spending.club$team = str_replace(df.spending.club$team," *\\(.*?\\) *","") #remove (C) for champions

#class transforming to numeric value or character value
df.spending.club$transfer.fee.total <- as.numeric(df.spending.club$transfer.fee.total)
df.spending.club$team <- as.character(df.spending.club$team)

###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)

df.spending.club$team <- with(df.spending.club, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
                                   ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
                                          ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
                                                 ifelse(league=="Premier league", paste(team,"UK", sep = " "),
                                                        ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))

#geocode team
geocodes <- geocode(as.character(df.spending.club$team))

#new dataframe with geocode
df.spending.club <- data.frame(df.spending.club[1:3],geocodes)

out.of.europe<-filter(df.spending.club, lon < -10 |lat < 35)
out.of.europe.2<- filter(df.spending.club, lon>20 |lat>60)
out.full= rbind(out.of.europe.2, out.of.europe)

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

plot_ly(df.spending.club, lat = lat, lon = lon, text = team, color = transfer.fee.total,
        type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers', 
        marker = m) %>%
  layout(title = 'Football teams in Europe and transfer spending<br>(Hover for airport)', geo = g)

