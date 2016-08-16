if (!require("pacman")) install.packages("pacman")
pacman::p_load(mapDK, rattle, metricsgraphics, plyr,dplyr, ggplot2, RColorBrewer, 
               htmlwidgets, htmltools, stringr, mapDK, tm, quanteda, quanteda,
               RTextTools, glmnet,lars, scatterplot3d,e1071,rpart , rpart.plot,
               ggmap, Matrix,randomForest,readr)

##=========================================================================================
##------------------------- Table of contents ---------------------------------------------
##=========================================================================================
# 1. Data gathering
#   1.1 Defining CSS Selectors
#   1.2 Building the scraper function
#   1.3 Looping the function
#   1.4 Finding the correct date
#
# 2. Cleaning
#   2.1 Cleaning and splitting into usefull predictors
#   2.2 Generating distances to City Square Hall
#
# 3. Text analysis
#   3.1 Converting to textcorpus and datafrequency table
#   3.2 Choosing words by Lasso
#   3.3 Converting to Principal Components
#
# 4. Initial Plotting and data characteristics
#   4.1 Preparation for plots
#   4.2 Graphing data characteristics
#   4.3 Mapping the data
#
# 5. Models
#   5.1 Splitting in test and training set and showing correlations
#   5.2 Baseline model and evaluation function
#   5.3 M2 - Linear Model
#   5.4 M3 - Lasso 
#   5.5 M4 - Support Vector Machine
#   5.6 M5 - Decision tree
#   5.7 M6 - RandomForest 
#   5.8 Comparing predictions

# OBS! OBS! OBS! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# As the data gathering for this assignment has been done on multiple days in order to create as large a
# dataset as possible, and afterwards cleaned to avoid duplicates, both the scraping and cleaning part of
# the code is blocked/commented out. We have included the scraper and cleaner to show the methodology, but
# afterwards loads the finished dataset, containing a month of individual datasets combined, in order to give 
# our models better prediction power. If need be, the full code can be run be removing the "commentout" 
# functions, but the sample size will then be much smaller.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


##=========================================================================================
##------------------------- 1. Data Gathering--- ------------------------------------------
##=========================================================================================

#--------- The data gahtering is comment out in this program. 
# commentout_datagatering
#   
# ##===================== 1.1 Defining page and CSS.selectors================================
# 
# # We want the the number of pages about housing for rent and the total number of housing for rent. 
# 
# link_1 = "http://www.boligportal.dk/lejebolig/soeg_leje_bolig.php"
# 
# site = read_html(link_1, encoding ="UTF-8")
# Numbers.of.pages = site %>% html_nodes(css = "#lejebolig_navbar td+ td") %>% html_text()
# 
# pages         = str_extract(str_extract(Numbers.of.pages[1], "(af)+[ ]+.+"),"[0-9]+")
# housing.count = str_extract(Numbers.of.pages[2],"[0-9]+")
# 
# #Showing count af house for rent and the number af pages
# housing.count
# pages
# 
# #CSS.selectors
# 
# css.selector.1 = "#bprice"
# css.selector.2 = "#btime" 
# css.selector.3 = "#btype" 
# css.selector.4 = "#bsize"
# css.selector.5 = "#bpost"
# css.selector.6 = "#bdesc"
# css.selector.7 = "#tekst a"
# 
# ##===================== 1.2 Building the scraper function ================================
# 
# extraction_function = function(link) {
#   
#   
#   site = read_html(link)
#   rent               = site %>% html_nodes(css = css.selector.1) %>% html_text()
#   date               = site %>% html_nodes(css = css.selector.2) %>% html_text()
#   accommodation.type = site %>% html_nodes(css = css.selector.3) %>% html_text()
#   kvm                = site %>% html_nodes(css = css.selector.4) %>% html_text()
#   location           = site %>% html_nodes(css = css.selector.5) %>% html_text()
#   description        = site %>% html_nodes(css = css.selector.6) %>% html_text()
#   title              = site %>% html_nodes(css = css.selector.7) %>% html_text()
#   href               = site %>% html_nodes(css = css.selector.7) %>% html_attr(name = 'href')
#   
#   
#   remove <- c ("KVM","HUSLEJE/MD", "DATO ???", "BOLIGTYPE")
#   
#   date = date [! date %in% remove]
#   rent = rent [! rent %in% remove]
#   kvm = kvm   [! kvm  %in% remove]
#   accommodation.type = accommodation.type [! accommodation.type %in% remove]
#   
#   
#   return(cbind(rent, date, accommodation.type, kvm, location, description, title, href))
#   
#   
# }
# 
# ##===================== 1.3 Extracting the data ================================
# 
# link = paste0(link_1, "?page=")
# 
# page <- c(seq(1,as.numeric(pages),1))
# 
# 
# #creating a vector consisting of URL links to the first 100 pages
# listoflinks = gsub(" ","",paste(link,page))
# 
# data = list()
# 
# #Showing count af house for rent and the number af pages
# housing.count
# pages
# 
# for (i in seq_along(listoflinks)) {
#   data[[i]] = extraction_function(listoflinks[i])
#   print(i)
#   Sys.sleep(5)
# }
# #Converting to dataframe:
# House.rent.data = ldply(data)
# 
# ##===================== 1.4 Finding the correct date ================================
# 
# Idag = Sys.Date()
# 
# gem = gsub("Idag",Idag, "your_folder/Idag.txt")
# 
# write.table(House.rent.data, gem, row.names=F)
# 
# files = list.files(path = "your_folder", pattern = NULL, all.files = FALSE,
#                    full.names = FALSE, recursive = FALSE,
#                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# 
# Hent.filer = files[grep("txt", files)]
# 
# 
# Samle.data = list()
# for (i in seq_along(Hent.filer)) {
#   NR=Hent.filer[i]
#   
#   Hvilken = as.Date(gsub(".txt","",NR[1]))
#   
#   
#   if (as.Date(Hvilken) < as.Date("2015-11-26")) {
#     
#     Hent = gsub("FIL",NR, "your_folder/FIL")
#     Hent <- read.table(Hent, header = TRUE)
#     Hent$href = ""
#   }
#   
#   if (as.Date(Hvilken) >= as.Date("2015-11-26")) {
#     
#     Hent = gsub("FIL",NR, "your_folder/FIL")
#     Hent <- read.table(Hent, header = TRUE)
#     
#  }
#   
#  Hent$indleast = gsub(".txt","", Hent.filer[i])
# 
#  dag  = gsub(".txt","",NR[1])
#  igaar = gsub(" ", "",paste(substr(dag, start=1, stop=8),as.numeric(substr(dag, start=9, stop=10))-1))
#   
#   
# # Finder den korrekte dato
# Hent$Date_d = as.numeric(str_extract(Hent$date, "[0-9]+"))
# Hent$Date_d = ifelse(Hent$Date_d <= 10, paste("0", Hent$Date_d, sep = ""), Hent$Date_d)
#   
# Hent$Date_m = str_extract(Hent$date, "[A-å].+")
#   
# Hent$Date_m = gsub("Jun", "2015-06-", Hent$Date_m)
# Hent$Date_m = gsub("Jul", "2015-07-", Hent$Date_m)
# Hent$Date_m = gsub("Aug", "2015-08-", Hent$Date_m)
# Hent$Date_m = gsub("Sep", "2015-09-", Hent$Date_m)
# Hent$Date_m = gsub("Okt", "2015-10-", Hent$Date_m)
# Hent$Date_m = gsub("Nov", "2015-11-", Hent$Date_m) 
# Hent$Date_m = gsub("Dec", "2015-12-", Hent$Date_m) 
#   
# Hent$Date_c = ifelse(is.na(Hent$Date_d),Hent$Date_m,paste(Hent$Date_m,Hent$Date_d))
#   
# Hent$Date_c = gsub("I dag", dag, Hent$Date_c) 
# Hent$Date_c = gsub("I går", dag, Hent$Date_c) 
# Hent$Date_c = as.Date(gsub(" ", "", Hent$Date_c))
#   
# Hent = Hent %>%  select(rent, accommodation.type, kvm, location, description , title, Date_c, indleast, href) 
#   
# Samle.data[[i]] =Hent
#   
# }
# 
# House.data = ldply(Samle.data)
# 
# write.table(House.data, file = "your_folder/House.data.csv",
#             sep = ",", col.names = NA, qmethod = "double")
# 



##=========================================================================================
##------------------------- 2. Cleaning ---------------------------------------------------
##=========================================================================================
# commentout_cleaning 
#   
# House.data = read.table("your_folder/House.data.csv", header = TRUE, sep = ",", row.names = 1)
# 
# ##===================== 2.1 Cleaning and splitting into usefull predictors =================
# 
# cleanning = House.data %>% 
#   mutate( kvm_c  = as.numeric(gsub(" ","",gsub("mÂ²","",kvm))), 
#             rent_c = as.numeric(gsub("[[:punct:]]", "", gsub("kr.","",rent))),
#             accommodation.type_c = gsub("ÃƒÂ¦","Ã¦",accommodation.type),
#             id = str_extract(href, "(id-)+[0-9]+"),
#             indleast = as.Date(indleast),
#             Date_c   = as.Date(str_trim(Date_c)))
#   
#   
# #------------ Location 
# cleanning$street = str_extract(cleanning$location, "[A-Ã¥].+(?=,)")
# cleanning$location_c = str_trim(cleanning$location)
# cleanning$location_c = gsub("ÃƒÂ¦","Ã¦",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ¸","Ã¸",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ¥","Ã¥",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ…rhus","Ã…rhus",cleanning$location_c)
# cleanning$location_c = gsub("KÃ¸benhavn ÃƒÂ˜","KÃ¸benhavn Ã˜",cleanning$location_c)
# cleanning$location_c = gsub("AllÃƒÂ©","AllÃ©",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ˜restads","Ã˜restads",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ˜rstedsvej","Ã˜rstedsvej",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ˜rsteds ","Ã˜rsteds ",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ˜sterbrogade","Ã˜sterbrogade ",cleanning$location_c)
# cleanning$location_c = gsub("ÃƒÂ˜restads","Ã˜restads ",cleanning$location_c)
# cleanning$location_c = gsub("GÃƒÂ¶teborg","GÃ¸teborg",cleanning$location_c)
# cleanning$location_c = gsub("   "," ",cleanning$location_c)
# cleanning$location_c = gsub("  "," ",cleanning$location_c)
#   
# cleanning$street_c = str_extract(cleanning$location_c, "[A-Ã¥].+(?=,)")
# cleanning$city   = str_extract(cleanning$location_c, "(,).+[A-Ã¥].+")
# cleanning$postnr = as.numeric(str_extract(cleanning$city, "[0-9]+"))
#   
# #----------------- Only Copenhagen postalcodes 
#   cleanning = filter(cleanning, (postnr < 2501) )
# #----------------- Reserved
# cleanning$Reserveret   = str_extract(cleanning$city, "(Reservere).+")
# cleanning$Reserveret   = ifelse(is.na(cleanning$Reserveret),0,1)
#   
# cleanning$city   = gsub("Reserveret","",cleanning$city)
# cleanning$city   = str_trim(str_extract(cleanning$city, "[A-Ã¥].+"))
#   
# cleanning$location_c = str_trim(gsub("Reserveret","",cleanning$location_c))
# #------------ description 
#   
# cleanning$description_c = as.character(cleanning$description)
# cleanning$description_c = gsub("ÃƒÂ¦","Ã¦",cleanning$description_c)
# cleanning$description_c = gsub("ÃƒÂ¸","Ã¸",cleanning$description_c)
# cleanning$description_c = gsub("ÃƒÂ¥","Ã¥",cleanning$description_c)
# cleanning$description_c = gsub("ÃƒÂ…rhus","Ã…rhus",cleanning$description_c)
# cleanning$description_c = gsub("ÃƒÂ˜restads","Ã˜restads",cleanning$description_c)
#   
# #Til brug ved tekstanalysen:
# cleanning$description2 = tolower(cleanning$description)
# cleanning$description2 = gsub("ã~|ã¸|ãfâ¸","ø", cleanning$description2) 
# cleanning$description2 = gsub("ãfâ¥|ã¥","å", cleanning$description2) 
# cleanning$description2 = gsub("ã¦|ãfâ¦|ã???","æ", cleanning$description2)
# cleanning$description2 = gsub("ã©", "é", cleanning$description2)
#   
# cleanning$description_c = cleanning$description2
# #------------ title 
#   
# cleanning$title_c = as.character(cleanning$title)
# cleanning$title_c = str_trim(cleanning$title_c)
# cleanning$title_c = gsub("ÃƒÂ¦","Ã¦",cleanning$title_c)
# cleanning$title_c = gsub("ÃƒÂ¸","Ã¸",cleanning$title_c)
# cleanning$title_c = gsub("ÃƒÂ¥","Ã¥",cleanning$title_c)
# cleanning$title_c = gsub("ÃƒÂ…rhus","Ã…rhus",cleanning$title_c)
# cleanning$title_c = gsub("ÃƒÂ˜restads","Ã˜restads",cleanning$title_c)
#   
# #Til brug ved tekstanalysen:
#   
# cleanning$title_c = tolower(cleanning$title)
# cleanning$title_c = gsub("ã~|ã¸|ãfâ¸","ø", cleanning$title_c) 
# cleanning$title_c = gsub("ãfâ¥|ã¥","å", cleanning$title_c) 
# cleanning$title_c = gsub("ã¦|ãfâ¦","æ", cleanning$title_c)
# cleanning$title_c = gsub("ã©", "é", cleanning$title_c)
# #------------------------------ Rooms 
# 
# cleanning$accommodation.type.c = gsub("Ã¦|ÃfÂ¦", "æ", cleanning$accommodation.type_c)
#   
# cleanning$rooms = tolower(cleanning$accommodation.type.c)
#   
# mean(cleanning[cleanning$rooms == "værelse","rent_c"])
# mean(cleanning[cleanning$rooms == "1 vær. lejlighed","rent_c"])
#   
# mean(cleanning[cleanning$rooms == "hus/villa","rent_c"])
# mean(cleanning[cleanning$rooms == "4+ vær. lejlighed","rent_c"])
#   
# mean(cleanning[cleanning$rooms == "delebolig","rent_c"])
#   
# #Room determining algorithm
#   
# cleanning$rooms = ifelse(
#   cleanning$rooms == "værelse", "1",ifelse(
#     cleanning$rooms == "1 vær. lejlighed", "1.2", ifelse(
#       cleanning$rooms == "delebolig", "1.5", ifelse(
#         cleanning$rooms == "2 vær. lejlighed", "2", ifelse(
#             cleanning$rooms == "3 vær. lejlighed", "3", ifelse(
#               cleanning$rooms == "4+ vær. lejlighed", "4", ifelse(
#                 cleanning$rooms == "hus/villa", "hus", "Amalienborg..."
#               )))))))
#   
# #----------------- Selecting variables 
# cleanning = distinct(select(cleanning, rent_c, kvm_c, accommodation.type_c, title_c, description_c, 
#                               indleast, Date_c, href, id, location_c, location, street, city, postnr, Reserveret, rooms))
#   
# #------- Finding ID's for all observations  
# # First the data is devided into a group with ID's and one group without ID 
# No_ID = filter(cleanning,is.na(id)==1) %>%
#   select(rent_c, accommodation.type_c, kvm_c, description_c , title_c, Date_c, location_c,
#          city, postnr, street, Reserveret, indleast, rooms) 
#   
# # Finding all unique ID's. This will be used to give old observations an ID, if
# # there are new observations with an ID.  
# with_ID = filter(cleanning,is.na(id)==0) 
# with_ID = distinct(select(with_ID,id, title_c, description_c, rent_c, accommodation.type_c, kvm_c, 
#                           city, postnr, street ,Date_c, rooms))
#   
# # Giving the group without ID's a new ID's, where it is possibly. 
# Yes_ID = inner_join(No_ID,with_ID,
#                     by =c("rent_c", "accommodation.type_c", "kvm_c", "description_c", 
#                           "title_c", "city", "postnr", "street", "Date_c", "rooms"))
#   
# # Finding all observations with ID's.
# with_ID = filter(cleanning,is.na(id)==0)  %>% 
#     select(id, title_c, description_c, rent_c, accommodation.type_c, kvm_c, location_c,
#            city, postnr, street , Reserveret , Date_c, indleast, rooms)
#   
# # Combining all observation that now have ID's.
# AlleMedID = rbind(Yes_ID ,with_ID)
#   
# # Preparing data to be save.
# unikke =  distinct(select(AlleMedID ,rent_c, accommodation.type_c, kvm_c, description_c, location_c, 
#                             title_c, Date_c, city, postnr, street, id, Reserveret, rooms))
# 
# ##===================== 2.2 Generating distance variables  ================================
#   
# ## Getting playground data 
# playground = read.table("your_folder\legeplads.csv", header = TRUE, fill = TRUE, sep = ",")
#  
# ##lat and lon for any adress
# unikke$location <- as.character(paste(unikke$street, unikke$postnr, sep=", "))
#   
#   
# ##### Finding adress 
#   
# findgeo = distinct(select(unikke, location, id))
#   
# Liste <- c(seq(1,nrow(findgeo),1))
#   
#   
# GetGeoCode = function(adress) {
#     
#   geocodedk = geocode(adress, source="google")
#   return(cbind(adress,geocodedk))
#     
# }
#   
#   
# Adresser = list()
#   
# for (i in seq_along(Liste)) {
#     Adresser[[i]] = GetGeoCode(findgeo[i,1])
#     print(findgeo[i,1])
#     print(Adresser[[i]])
#     print(i)
#     Sys.sleep(2)
# }
#   
# AdressList = ldply(Adresser)
#   
# AdressList$lat = as.numeric(as.character(AdressList$lat))
# AdressList$lon = as.numeric(as.character(AdressList$lon))
# lat_max <- 55.8 
# subset(AdressList, AdressList[ ,"lat"] < lat_max)
# lon_min <- 12.0
# AdressList <- subset(AdressList, AdressList[ ,"lon"] > lon_min)
#   
# AdressList = na.omit(AdressList)
#   
# ####playground
# playground = playground %>% select(wkb_geometry)
# playground = na.omit(playground)
# playground$wkb_geometry = gsub("\\(|\\)", "", playground$wkb_geometry)
# playground$wkb_geometry = str_extract(playground$wkb_geometry, "[0-9].+")
# 
# z = str_split(playground$wkb_geometry, pattern = " ")
# z = do.call(rbind.data.frame, z)
#   
# playground = bind_cols(playground, z)
# 
# names(playground) = c("wbk_geometry", "lon", "lat")
#   
# playground$lat = as.numeric(as.character(playground$lat))
# playground$lon = as.numeric(as.character(playground$lon))
#   
# 
# get_shortest_distance <- function(address_lat=0, address_lon=0){
#     
#   for(x in seq_len(nrow(playground))) {
#     
#     playground_row = playground[x,]
#       
#     l[x] <- c(distm(c(address_lat, address_lon), c(playground_row$lat, playground_row$lon), fun=distHaversine))
#   }
#   minimum_distance <- c(l[which.min(l)])
#   return(minimum_distance)
# }
#   
# # which.min() giver index nr - dvs. pladsen i en vector som indeholder den laveste vÃ¦rdi
# # get_shortest_distance()[which.min(get_shortest_distance())]
#   
# l <- vector()
# closest_distance <- vector()
# adress <- as.character()
#   
# AdressList = na.omit(AdressList)
#   
# for(i in seq_len(nrow(AdressList))) {
#   address_row = (AdressList[i,])
#     
#     
#   closest_distance[i] <- c(get_shortest_distance(address_row$lat,address_row$lon))
#   adress[i] <- as.character(address_row$adress)
#     
# }
#   
# d <- data.frame(adress, closest_distance)
#   
# View(d)
#   
# d$street = str_extract(d$adress, "[A-Ã¥].+(?=,)")
# d$postnr = as.numeric(str_extract(d$adress, "[0-9]+"))
# d$adress <- as.character(paste(d$street, d$postnr, sep=", "))
#   
# AdressList$street = str_extract(AdressList$adress, "[A-Ã¥].+(?=,)")
# AdressList$postnr = as.numeric(str_extract(AdressList$adress, "[0-9]+"))
# 
# AdressList$adress_old <- AdressList$adress
# AdressList$adress <- as.character(paste(AdressList$street, AdressList$postnr, sep=", "))
#   
# AdressList <- merge(AdressList, d, by="adress")
#   
# #-------------City Square Hall
# AdressList$adress_old = as.character(AdressList$adress_old)
# from <- AdressList$adress_old
# to <- c("City Square Hall, Copenhagen")
#   
# for (i in 1:nrow(kmnew)){
#   nn[i,] <- which.min(kmnew[i,]) 
# }
#   
# centrum_dist = data.frame()
#   
# for (i in seq_along(from))
# {
#   centrum_dist[i,3] = mapdist(from[[i]] , "City Square Hall, Copenhagen" , mode = "walking")
#   Sys.sleep(2)
# }
#   
# mapd
# test = data.frame()
# test = cbind(from,to)
# test_matrix = as.matrix(test)
#   
# test$from = gsub("æ|?f¦|????", "?", test$from) 
# test$from = gsub("ø|?~|?f?~", "?", test$from) 
# test$from = gsub("å|?f¥", "?", "?", test$from) 
#   
# df_text = df_text %>% 
#   mutate( description_total= gsub("æ|?f¦|????", "ae", description_total),
#           description_total= gsub("ø|?~|?f?~", "oe", description_total),
#           description_total= gsub("å|?f¥", "aa", description_total)
#       )
# 
# centrum_dist <- mapdist(from, to, mode = "walking", Sys.sleep(1))
#   
# names(centrum_dist) = c("adress", names(centrum_dist[-1]), Sys.sleep(1))
#   
# Adress_dist <- merge(AdressList, centrum_dist, by="adress")
# Adress_dist <- Adress_dist %>% distinct("adress")
# 
# unikke$adress <- as.character(paste(unikke$street, unikke$postnr, sep=", "))
#   
# total <- merge(Adress_dist, unikke, by="adress")
#   
# write.table(total, file = "C:\\Users\\Andreas\\Documents\\GitHub\\SDS_Group_14_Exam\\boss2.csv",
#               sep = ",", col.names = NA, qmethod = "double")
# 

#=========================================================================================
#-------------------------- 3. Text analysis ---------------------------------------------
#=========================================================================================
Get_it = read.csv("https://raw.githubusercontent.com/PhilipLemaitre/SDS_Group_14_Exam/master/boss2.csv",
                  fill = TRUE, sep = ",", header = TRUE)

Get_it = Get_it[complete.cases(Get_it),]

Get_it <- Get_it %>% filter(rooms %in% c("1", "1.5", "2", "3", "4", "hus"))

df <- distinct(Get_it, id)

df_text = data.frame(df$rent_c, df$kvm_c, df$description_c, df$title_c)

df_text$kvmpris = df_text$df.rent_c/df_text$df.kvm_c
df_text$kvmpris = as.numeric(df_text$kvmpris)

##===================== 3.1 Converting to textcorpus and datafrequency table =================

## Substituting the 3 infinite values with the mean kvmprice

is.na(df_text) <- sapply(df_text, is.infinite)
mean = mean(df_text$kvmpris, na.rm = TRUE)
df_text$kvmpris[is.na(df_text$kvmpris)] = mean
mean
#Combining the two textstrings: title and description

df_text$description_total = as.character(paste(df_text$df.title_c, df_text$df.description_c, sep= "  "))

head(df_text$description_total)

#Cleaning special characters

df_text$description_total <- iconv(df_text$description_total,"WINDOWS-1252","UTF-8")

df_text$description_total = tolower(df_text$description_total)

head(df_text$description_total)

## create corpus (through quanteda) based on vector + document variables
textcorpus <- corpus(df_text$description_total, docvars = df_text[, 5:6])


## inspect corpus
tail(summary(textcorpus, verbose = FALSE))[, 1:6]

neutral_terms  <- c("lejlighed", "lejligheden", "apartment", "ved", "skibbroen",
                    "havneholmen","bolig","december","henvendelse","vaerelse",
                    "ejendom","m2","kvm","kan","samt","f?","m?neder","lejes","st","november","maj",
                    "ejendommen","april", "flere","kontakt","m","kun","januar", "vaerelses", "udlejes",
                    "angiv", "room","vaerelset","lejer","lejligheder","udlejer",
                    "s?ger", "vaer", "vaerelser", "v", "paa")


textdfm <- dfm(textcorpus, language = "danish", 
               toLower = TRUE,
               removePunc = TRUE,
               removeSeparators = TRUE,
               stem = TRUE,
               ignoredFeatures = c(stopwords("danish"), stopwords("english"), neutral_terms),
               verbose = FALSE
)

#Present in at least 5% of documents:
mindoc = nrow(df)/100*5
textdfm <- trim(textdfm, minDoc = mindoc) 
head(textdfm) 
dim(textdfm)

## We convert the datafrequency table into a matrix of word regressors by converting to a "TM" object,
#and then converting to a matrix

tm_textdfm = convert(textdfm, to = "tm")
dim(tm_textdfm)

#We convert the tm table into a wordmatrix to use in the lasso below

wordmatrix = as.matrix(tm_textdfm)
dim(wordmatrix)

##===================== 3.2 Choosing words by Lasso ==========================================

train_idx <- sample(1:nrow(df_text),1000,replace=FALSE)
##Building the Lasso regression:
wm_train = wordmatrix[train_idx,]
wm_test = wordmatrix[-train_idx,]

df_train = df_text[train_idx,]
df_test = df_text[-train_idx,]

glm_text = glmnet(x = wm_train, y = df_train$kvmpris)
lambda_values = glm_text$lambda

performance = data.frame()


#----- Evaluation 
rmse <- function (true, estimate) {
  return(sqrt(mean((true-estimate)^2)))
}


for (lambda in lambda_values){
  performance = rbind(performance,
                      data.frame(lambda = lambda,
                                 RMSError = rmse(true = df_test$kvmpris,
                                                 estimate = predict(glm_text, wm_test, s = lambda)
                                 )))
}

best.lambda = performance$lambda[performance$RMSError == min(performance$RMSError)]
best.lambda

glmnet.fit = glmnet(wordmatrix, df_text$kvmpris)
coef = coef(glmnet.fit, s = best.lambda)
coef

##===================== 3.3 Converting to Principal Components ===============================

## Basing the Principal Component Analysis on the words choosen by the Lasso
# Turning the GLMNET object into a matrix
coef = as.data.frame(                               
  as.matrix(                             
    coef(glmnet.fit, s = best.lambda)))  

## Removing coefficients of zero and intercept and transforming into words as header
coef$coefficients = coef[,1]
coef = coef[!(coef[1,] == 0),]
coef = as.data.frame(t(coef))
coef = coef[,-1]

## Creating vector of words determined by the Lasso

pca_vector = colnames(coef, do.NULL = TRUE, prefix = "Words")

## Creating new text matrix/data.frame with only lasso words

df_wm = as.data.frame(wordmatrix)
df.lasso_words = df_wm[, which(names(df_wm) %in% pca_vector)]

## Modelling ans summarising the principal components 

pca = prcomp(df.lasso_words)
summary(pca)

#_____________________________________ plotting by PCA_______________________________

loadings = pca$rotation

comp1  = loadings[,1]
comp2  = loadings[,2]
comp3  = loadings[,3]
comp4  = loadings[,4]
comp5  = loadings[,5]
comp6  = loadings[,6]
comp7  = loadings[,7]
comp8  = loadings[,8]
comp9  = loadings[,9]
comp10 = loadings[,10]
comp11 = loadings[,11]
comp12 = loadings[,12]

df_plot = cbind.data.frame(comp1, comp2, comp3, comp4, comp5, comp6, 
                           comp7, comp8, comp9, comp10, comp11, comp12)

## Plot Words along Principal components to highlight possible groupings and similarities

# Two dimensional Scatterplot
TwoD = ggplot(df_plot, aes(comp1, comp2)) + 
  geom_point(color = "grey", size = 0.1) + 
  theme_minimal() + 
  labs(title="Principal component analysis", x = "Princomp 1", y = "Princomp2") + 
  geom_text(aes(label = row.names(df_plot)), hjust = 0, vjust = 0, size=4) 
TwoD


# ------ Three dimensional Scatterplot
with(df_plot, {
  ThreeD_plot = scatterplot3d(comp1, comp2, comp3,  # Axis of the components
                              color = "blue", pch = 19,           # Color and size
                              main= "Principal Components",       # Headline and Axis titles:
                              xlab= "Wordgroup 1",                
                              ylab= "Wordgroup 2",                
                              zlab= "Wordgroup 3")               
  coords = ThreeD_plot$xyz.convert(comp1, comp2, comp3) 
  text(coords$x, coords$y,
       labels = row.names(df_plot),                # text to plot
       cex=1, pos=4)                              # Place text to right of points
})


## Turning both text matrix and PCA analysis into exportable data.frame to use in larger model

#Constructing dataframe with first 12PCA's
df.pca = as.data.frame(pca$x[,1:12])

df.export = cbind(df, df_text, df.pca, df.lasso_words)

df.words = df.export[,(ncol(df.export) - ((length(pca_vector)-1))):ncol(df.export)]
df.words = data.frame(lapply(df.words, factor))

df.export$KmCityHall = df.export$km
df.export$MinutesCityHall = df.export$minutes
df.export$MilesCityHall = df.export$miles
df.export$postnr = df.export$postnr.y

df.export = select(df.export, kvmpris, KmCityHall, MinutesCityHall, MilesCityHall, postnr,
                   lon, lat, rent_c, kvm_c, description_total, rooms, adress, city, id)

df_wm_fac = lapply(df_wm, factor)

BruceWayne = cbind(df.export, df.pca, df_wm_fac)

names(BruceWayne)

##============================================================================================
#-------------------------- 4. Initial plotting and characteristics --------------------------
##============================================================================================

##===================== 4.1 Preparation for plots ============================================
total <- BruceWayne

total <- distinct(BruceWayne, id)

total$street = str_extract(total$adress, "[A-å].+(?=,)")
total$postnr = as.numeric(str_extract(total$adress, "[0-9]+"))
total$m <- total$KmCityHall*1000

df = distinct(select(total, rent_c, kvm_c, m,
                     id, street, lat, lon, postnr, rooms))

#------------  Making postalcodes usable for mapDK 
df <- df %>% 
  mutate(postnr_simpelt=
           ifelse((1000 <= postnr & postnr < 1100),1000, 
                  ifelse((1100 <= postnr & postnr< 1200),1100, 
                         ifelse((1200 <= postnr & postnr< 1300),1200,
                                ifelse((1300 <= postnr & postnr< 1400),1300,
                                       ifelse((1400 <= postnr & postnr< 1500),1400,
                                              ifelse((1500 <= postnr & postnr< 1600),1500,
                                                     ifelse((1600 <= postnr & postnr< 1700),1600,
                                                            ifelse((1700 <= postnr & postnr< 1800),1700,
                                                                   ifelse((1800 <= postnr & postnr< 1900),1800,
                                                                          ifelse((1900 <= postnr & postnr< 2000),1900,
                                                                                 ifelse(( postnr == 2150), 2100, postnr))))))))))))

df[df == 0] <- NA
df = na.omit(df)

#------kvm_pris
df$price_square <- df$rent_c/df$kvm_c


#------ Grouping by id, postalcodes, city, street, rooms, kvm_c 

df_postnr <- df %>%
  group_by(postnr_simpelt) %>% 
  summarise(antal_obs = n(), sumpris = sum(rent_c), avg_price_square = mean(price_square), sumkvm=sum(kvm_c)) 


df_postnr$avgprice <- df_postnr$sumpris / df_postnr$antal_obs 

#removing observations with type NA (renamed to Amalienborg earlier)
df_udenNA <- df %>% filter(rooms %in% c("1", "1.5", "2", "3", "4", "hus"))


#-----Graphs
# Average price pr. sq. meter

kvm_min <- 20
df_kvm  <- df
df_kvm2 <- df
df_kvm  <- subset(df_kvm, df_kvm[ ,"kvm_c"] > kvm_min)

df_kvm2 <- subset(df_kvm2, df_kvm2[ ,"kvm_c"] < kvm_min)

gns_min <- mean(df_kvm2$price_square)
print(gns_min)

gns_maks <- mean(df_kvm$price_square)
print(gns_maks)

df_total = merge(df_postnr, df, by="postnr_simpelt")

##===================== 4.2 Plotting characteristics ============================================
#shows that our dependent variable should price pr. sq. meter rather than absolut price
j = ggplot(data = df_udenNA, aes(x = kvm_c, y=rent_c, size=rooms))
j + geom_point() + labs(list(title = "Relationsship between price and size", 
                             x = "Size of apartment (m^2)", y = "Absolut rental price (DKK)")) + scale_fill_discrete(name="Experimental\nCondition") + theme_minimal()


#plottin distribution of price pr. sq. meter, in the appendix
r = ggplot(data=df_total, aes(x=price_square))
r = r + geom_histogram()
r  + theme(axis.text.x = element_blank()) + labs(list(title = "Distribution of price pr. square meter (DKK)", x = "Price pr. square meter (DDK)", y = "Count")) + scale_fill_discrete(name="Experimental\nCondition") + theme_minimal()


#Relationsship between price and size
df_udenNA$postnr_farve <- as.character(df_udenNA$postnr_simpelt)

k = ggplot(data = df_udenNA, aes(x = kvm_c, y=price_square, size=rooms))
k + geom_point() + labs(list(title = "Relationsship between price and size", x = "Size of apartment (m^2)", y = "Price pr. square meter (DKK)")) + scale_fill_discrete(name="Experimental\nCondition") + theme_minimal()

#distance to city square hall, this goes in the appendix
t = ggplot(data = df_udenNA, aes(x = m, y=price_square, size = rooms))
t + geom_point() + labs(list(title = "Distance to City Sqaure Hall's effect on price", x = "Distance to City Square Hall", y = "Price pr. squatre meter")) + scale_fill_discrete(name="Experimental\nCondition") + theme_minimal()

#Average price in different neighborhoods
f <- ggplot(data = df_postnr, aes(x=reorder(postnr_simpelt, avg_price_square), y=avg_price_square))
f + geom_bar(stat="identity") + theme(axis.text.x = element_blank()) + labs(list(title = "Average price pr. square meter in different neighborhoods", x = "Zip Code", y = "Average price pr. sq.m (DKK)")) + scale_fill_discrete(name="Experimental\nCondition") + theme_minimal()

##===================== 4.3 Mapping the data ============================================

#price in neighborhood on map, maybe in the appendix?
mapDK(values = "avg_price_square", id = "postnr_simpelt", 
      data = data.frame(df_postnr),
      detail = "zip", show_missing = FALSE,
      guide.label = "gns pris")


#plotting map showing varians in price in neighborhood
cph.map <- ggmap(get_map(location = c(12.57,55.68), maptype = "toner", source="stamen", crop=TRUE, zoom = 13))
#removing most expensive
sq_maks <- 500
df_maks <- df
df_maks <- subset(df_maks, df_maks[ ,"price_square"] < sq_maks)
p <- cph.map + geom_point(aes(x = lon, y = lat, size=price_square), data = df_maks, color=I("red"))
p + labs(list(title = "Distribution of price pr. square meter (DKK) in different neighborhoods", x = " ", y = " ")) + scale_fill_discrete(name="Experimental\nCondition")

#appendix, showing relationship between price and size holds, when removing most expensive apartments
t = ggplot(data = df_maks, aes(x = kvm_c, y=price_square))
t + geom_point() 


#appendix, showing clearly that average price falls when rented rooms are excluded

rooms_min <- 1

df_udenv <- df_maks
df_udenv$rooms <- as.numeric(as.character(df_udenv$rooms))
df_udenv = na.omit(df_udenv)

df_udenv <- subset(df_udenv, df_udenv[ ,"rooms"] > rooms_min)

q <- cph.map + geom_point(aes(x = lon, y = lat, size=price_square), color=I("red"), data = df_udenv)
q

##============================================================================================
## ------------------------ 5. Models --------------------------------------------------------
##============================================================================================
##===================== 5.1 Splitting in test and training set and showing correlations ======
set.seed(110)




BruceWayne$postnr = factor(BruceWayne$postnr)
BruceWayne$rooms = factor(as.character(BruceWayne$rooms))


little.BruceWayne = filter(BruceWayne, is.na(KmCityHall)==0)
little.BruceWayne = filter(little.BruceWayne, rooms !="Amalienborg...")
little.BruceWayne = little.BruceWayne[complete.cases(little.BruceWayne),]

all_models_var = select(little.BruceWayne, KmCityHall, postnr, lon,lat, rooms, adress, city)

#Using word-vector from Lasso to split the set

prince_var = select(little.BruceWayne, PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12)

text_anal = little.BruceWayne[,(ncol(little.BruceWayne) - ((length(pca_vector)-2))):ncol(little.BruceWayne)]

Regressor_matrix = as.data.frame(data.matrix(cbind(all_models_var, prince_var, text_anal)))

#----- Creating train-set and test-set 
train_idx <- sample(1:nrow(little.BruceWayne), 600,replace=FALSE)

train <- little.BruceWayne[train_idx,] # select all these rows
test  <- little.BruceWayne[-train_idx,] # select all but these rows



##===================== 5.2 Baseline model and evaluation function ===========================

#----- Evaluation function: RootMeanSquareError
rmse <- function (true, estimate) {
  return(sqrt(mean((true-estimate)^2)))
}

#----- Baseline model, just using the average price as a predictor
mean(train$kvmpris)
rmse(mean(test$kvmpris),train$kvmpris)

##===================== 5.3 M2 - Linear Model ================================================
# OBS!!
# As we do not have enough observations to predict have the same level of postal codes represented
# in the data, when using te postal codes as factors, this conflicts with the linear model, and we therefore 
# chose to remove postal codes for this model. This also happens to the words "stort", "koekken and "adgang".
# We have tried our best to find a way to dynamically exclude variables with varying factor levels, but have found no solution
# in any book, fora or blog. Most suggestions on StackExchange discard it as a possibility to standardize levels


drops <- c("rent_c","kvm_c","MinutesCityHall", "MilesCityHall", "description_total", "adress", "id", "postnr")

little.BruceWayne_linMod = little.BruceWayne[,!(names(little.BruceWayne) %in% drops)]

train_lin <- little.BruceWayne_linMod[train_idx,] # select all these rows
test_lin  <- little.BruceWayne_linMod[-train_idx,] # select all but these rows

#train_l_fac = lapply(train_lin[19:43], factor)
train_lin = train_lin[1:18]
#train_lin_2 = cbind(train_l_fir ,train_l_fac)



names(train_lin)
M2 <- lm(kvmpris ~ ., data = train_lin)

summary(M2)


kvmpris_hat_M2 <- predict(M2,test_lin)

#Evaluation
rmse(kvmpris_hat_M2,test$kvmpris)

##===================== 5.4 M3 - Lasso ======================================================= 
#___________________________ Fitting the Lasso ___________________________________________________________

# As the Lasso syntax needs a seperated data.matrix of regressors and a vector of outcomes, we split the set vertically:
RegressorMatrix_train = as.matrix(Regressor_matrix[train_idx,])

RegressorMatrix_test = as.matrix(Regressor_matrix[-train_idx,])

#Training Lasso
M3_Lasso = glmnet(x = RegressorMatrix_train, y = train$kvmpris)

#Testing Lasso
rmse(predict(M3_Lasso, RegressorMatrix_test), test$kvmpris)

#Tuning Lamda__________________________-
lambda_values = M3_Lasso$lambda

performance_Lasso = data.frame()

for (lambda in lambda_values){
  performance_Lasso = rbind(performance_Lasso,
                            data.frame(lambda = lambda,
                                       RMSError = rmse(predict(M3_Lasso, RegressorMatrix_test, s = lambda),
                                                       test$kvmpris)))
}

#Lambda Performance (Like in class) 
ggplot(performance_Lasso, aes(x = lambda, y = RMSError))+
  geom_point() + 
  geom_line() + 
  theme_minimal()

## ----------------------- Final Model and Performance 
best.lambda = performance$lambda[performance$RMSError == min(performance$RMSError)]

coef(M3_Lasso, s = best.lambda)

rmse(predict(M3_Lasso, RegressorMatrix_test, s=best.lambda), test$kvmpris)

kvmpris_hat_Lasso = predict(M3_Lasso,RegressorMatrix_test, s = best.lambda)

##===================== 5.5 M4 - Support Vector Machine ======================================
# As we have the same Factor problem here as in the linear model, we use the same predictors we found above

M4_SVM <- svm(kvmpris ~. , data = train_lin)

kvmpris_hat_M4 <- predict(M4_SVM,test_lin)

plot(test$kvmpris, kvmpris_hat_M4)

abline(a=0, b=1.0)

rmse(kvmpris_hat_M4,test$kvmpris)

##===================== 5.6 M5 - Decision tree ===============================================
# Problem with æ ø å format
Var_to_tree_train = train_lin
Var_to_tree_train$city = gsub("ø","oe", Var_to_tree_train$city)
Var_to_tree_train$city = gsub("Ø","OE", Var_to_tree_train$city)
Var_to_tree_train$city = gsub("æ","ae", Var_to_tree_train$city)
Var_to_tree_train$city = gsub("å","aa", Var_to_tree_train$city)
Var_to_tree_train$city = gsub("Å","AA", Var_to_tree_train$city)

Var_to_tree_test = test_lin
Var_to_tree_test$city = gsub("ø","oe", Var_to_tree_test$city)
Var_to_tree_test$city = gsub("Ø","OE", Var_to_tree_test$city)
Var_to_tree_test$city = gsub("æ","ae", Var_to_tree_test$city)
Var_to_tree_test$city = gsub("å","aa", Var_to_tree_test$city)
Var_to_tree_test$city = gsub("Å","AA", Var_to_tree_test$city)
#--


M5_tree <- rpart(kvmpris ~ . , data = Var_to_tree_train)

par(mar=c(4,4,4,4))
prp(M5_tree ,fallen.leaves=TRUE , type=0)
summary(M5_tree)


kvmpris_hat_M5_tree <- predict(M5_tree,Var_to_tree_test)
plot(test$kvmpris,kvmpris_hat_M5_tree, xlim=c(0, 1000), ylim=c(0, 1000))
abline(a=0, b=1.0)

rmse(test$kvmpris,kvmpris_hat_M5_tree)

##===================== 5.7 M6 - RandomForest ================================================

names(train_lin)

M6_randomForest <- randomForest(kvmpris ~ ., data = train_lin)

summary(M6_randomForest)


kvmpris_hat_M6_randomForest <- predict(M6_randomForest,test_lin)

plot(test$kvmpris,kvmpris_hat_M6_randomForest)
abline(a=0, b=1.0)

rmse(predict(M6_randomForest,test),test$kvmpris)

##===================== 5.8 Comparing predictions ============================================

#Baseline/Average
rmse(mean(test$kvmpris),train$kvmpris)
# Linear model
rmse(kvmpris_hat_M2,test_lin$kvmpris)
# Lasso
rmse(predict(M3_Lasso, RegressorMatrix_test, s=best.lambda), test_lin$kvmpris)
# Support Vector Machine
rmse(kvmpris_hat_M4,test_lin$kvmpris)
# Decision Tree
rmse(test_lin$kvmpris,kvmpris_hat_M5_tree)
# RandomForest
rmse(predict(M6_randomForest,test_lin),test$kvmpris)