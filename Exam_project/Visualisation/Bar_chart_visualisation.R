######################################################################################################################
######################################### BAR CHART ##################################################################
######################################################################################################################
######################################################################################################################
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)

####LOAD the DATA

df.stats<-read.csv("/Users/guillaumeslizewicz/Documents/SDS-group12/Exam_project/transferdata.tidy.csv")
df.stats$transfer.fee<-as.numeric(df.stats$transfer.fee) #set as numeric for transfer fees

####change transfer fees in categories

df.stats.simp<- df.stats
df.stats.simp$transfer.fee<-cut(x=df.stats.simp$transfer.fee,c(0,10^5,10^6,5000000,10^7,2*10^7,10^9,10^15))

#### Counting the occurences of each cat and creating a new dataframe with only 2 variables

# df.stats.simp.occ = df.stats.simp %>% 
#   count(transfer.fee) %>% 
#   arrange(-n) %>% 
#   ungroup() %>% 
#   mutate(
#     #transfer_fee_read = reorder(as.factor(transfer.fee), n)
#   )


###Filter NA
# df.stats.simp.occ<-df.stats.simp.occ %>% 
#   filter(!is.na(transfer.fee))

##Add

# df.stats.simp.occ<-  df.stats.simp.occ %>% 
#   mutate( transfer.fee.cat= as.numeric(transfer.fee))

#### df.stats.simp.occ<-  df.stats.simp.occ %>%
####  mutate( transfer_free_label=replace(transfer_fee_label, transfer_fee_label==5,"more than 10m" ), replace(transfer_fee_label, transfer_fee_label==4,"between 3m and 10m"))

##create new labels depending on 
# df.stats.simp.occ<-  df.stats.simp.occ %>% 
# mutate(transfer.fee.label = ifelse(transfer.fee.cat==5,"Between 10m and 20m",
#                                     ifelse(transfer.fee.cat==4,"Between 5m and 10m",
#                                            ifelse( transfer.fee.cat==2,"Between 100k and 1m", 
#                                                    ifelse( transfer.fee.cat==3,"Between 1m and 5m",
#                                                            ifelse(transfer.fee.cat==1,"Less than 100k",
#                                                                   ifelse(transfer.fee.cat==6,"More than 20m",  transfer.fee.cat )))))))
# 
# 
# df.stats.simp.occ<-arrange(df.stats.simp.occ,transfer.fee.cat)
# ##########Working bar chart 1############
# library("ggplot2")
# #change label order
# df.stats.simp.occ$transfer.fee.label <- factor(df.stats.simp.occ$transfer.fee.label, levels = df.stats.simp.occ$transfer.fee.label[order(df.stats.simp.occ$transfer.fee.cat)])
# #draw graph
# p = ggplot(df.stats.simp.occ, aes(x =transfer.fee.label, y = n))
# 
# #change graph appearance
# p + geom_bar(stat = "identity", position = "identity")+ 
#   theme(#axis.title.x=element_blank(),
#     axis.text.x =element_text(size  = 7,
#                               angle = 45,
#                               hjust = 1,
#                               vjust = 1),
#     axis.ticks= element_line(color=NA),
#     axis.ticks= element_line(color=NA),
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.title.y=element_blank(),
#     axis.title.x=element_blank())
# 
# 
# 
# #######################################
# #######################################
# #######################################
# library("ggplot2")
# p = ggplot(df.stats.simp.occ, aes(x = transfer.fee.cat, y = n, fill=factor(transfer.fee.label)))
# p + geom_bar(stat = "identity", position = "identity")+ 
#   theme(axis.title.x=element_blank(),
#         axis.text.x =element_blank(),
#         # element_text(size  = 7,
#         #        angle = 45,
#         #         hjust = 1,
#         #       vjust = 1),
#         axis.ticks= element_line(color=NA),
#         axis.ticks= element_line(color=NA),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.title.y=element_blank(),
#         text=element_text(family="Goudy Old Style"))+
#   scale_fill_manual("Transfer fees",
#                     values=c("grey", "#E08E79", "#F1D4AF", "#ECE5CE", "#C5E0DC", "#774F38"),
#                     breaks=c("Less than 100k","Between 100k and 1m","Between 1m and 5m","Between 5m and 10m","Between 10m and 20m","More than 20m"))+
#   ggtitle("Number of transfers per transfer fee")
# 
# levels(df.stats.simp.occ$transfer.fee.cat)
# df.stats.simp.occ$transfer.fee.cat<-as.numeric(df.stats.simp.occ$transfer.fee.cat)
# 
# ##############################################################################
# #######################################
# #######################################
# ####TRANSFER FOR APPEARANCE####
# p.stats = ggplot(df.stats, aes(x = appearances , y = transfer.fee))
# p.stats + geom_point(stat = "identity")
# 
# ####TRANSFER FEE FOR TOTAL GOALS####
# p.stats = ggplot(df.stats, aes(x = total.goals , y = transfer.fee))
# p.stats + geom_point(stat = "identity") +
#   scale_y_sqrt() 
# 
# filter
# ?arrange
# ###AGREGATE MAX###
# #df.agg <- aggregate(minutes.pr.goal ~ positions, df.stats, max)
# #df.max <- merge(df.agg, df.stats)
# #p.stats
# #summary(df.stats)
# #df.stats$transfer.fee<-as.numeric(df.stats$transfer.fee)
# #summary(df.stats)

##########################################################################
##########################################################################
##########################################################################
##########################################################################
###################Chart for leagues and clubs on top of cat##############
############################# New data frame #############################
##########################################################################
##########################################################################


###Filter NA
df.stats.simp<-df.stats.simp%>% 
   filter(!is.na(transfer.fee))

##Add transfer fee cat

 df.stats.simp<-  df.stats.simp %>% 
  mutate( transfer.fee.cat= as.numeric(transfer.fee))

##create new labels depending on transfer fees
df.stats.simp<-  df.stats.simp%>% 
  mutate(transfer.fee.label = ifelse(transfer.fee.cat==5,"Between 10m and 20m",
                                     ifelse(transfer.fee.cat==4,"Between 5m and 10m",
                                            ifelse( transfer.fee.cat==2,"Between 100k and 1m", 
                                                    ifelse( transfer.fee.cat==3,"Between 1m and 5m",
                                                            ifelse(transfer.fee.cat==1,"Less than 100k",
                                                                   ifelse(transfer.fee.cat==6,"More than 20m",  transfer.fee.cat )))))))


#df.stats.simp<-arrange(df.stats.simp,transfer.fee.cat)
############################################################################################    
####################### BAR CHART NUMBER OF TRANSFER PER CAT PER LEAGUE##########################################################  
############################################################################################    
#change chart order
 df.stats.simp$transfer.fee.label <- factor(df.stats.simp$transfer.fee.label, levels = df.stats.simp$transfer.fee.label[order(df.stats.simp$transfer.fee.cat)])

#ggplot(df.stats.simp, aes(x = transfer.fee.cat)) + geom_bar()
p = ggplot(df.stats.simp, aes(x = transfer.fee.label, fill=league))
p + geom_bar()+
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
  scale_fill_manual("National leagues", values=c("grey", "#E08E79", "#F1D4AF", "#ECE5CE", "#C5E0DC", "#774F38"))+
  ggtitle("Number of transfers per transfer fee")

########################################################################################################################
############################################################################################################################################
###### GET PERCENTAGES ####################################################################################################
#########################################################################################################################
#####################################################################################################################################

#create my DF

mydf<-df.stats.simp %>% 
  select( name,club.to ,transfer.fee.cat, transfer.fee, transfer.fee.label,league)

#mean cat per league
mean.cat<-mydf %>% 
  group_by(league)%>%
  dplyr::summarise(
    fre = n(),
    category = mean(transfer.fee.cat, na.rm = TRUE)
  )
#frequence by league and cat
leak<-mydf %>% 
  group_by(transfer.fee.cat,league,transfer.fee.label)%>%
  dplyr::summarise(
    fre = n()
  )
#percentage
percentage_df<-leak %>% 
  group_by(transfer.fee.cat)%>%
  dplyr::mutate(
    percentages = fre/sum(fre)*100)
  )
percentage_df$percentages<-round(percentage_df$percentages, digit=1)

########################################################################################################################
############################################################################################################################################
###### NEW BAR CHART WITH PERCENTAGES ####################################################################################################
#########################################################################################################################
#####################################################################################################################################

# order the bar chart so it's increasing cats.
percentage_df$transfer.fee.label <- factor(percentage_df$transfer.fee.label, levels = percentage_df$transfer.fee.label[order(percentage_df$transfer.fee.cat)])

# add new variable for positionning geom_text in the middle of the bar chart
percentage_df <- ddply(percentage_df, .(transfer.fee.label), mutate, csum = cumsum(fre)-fre/2)

# creating the bar chart
percentage_df<-arrange(percentage_df,fre,transfer.fee.label,transfer.fee.cat,league,percentages)
p = ggplot(percentage_df, aes(x = transfer.fee.label, y=fre, fill=league))

#plotting it
p + geom_bar(stat="identity")+
  scale_fill_manual("National leagues",
                    values=c("grey", "#E08E79", "#F1D4AF", "#ECE5CE", "#C5E0DC", "#774F38"))+
  ggtitle("Number of transfers per transfer fee")+
  geom_text(aes(y = csum, ymax=fre, ymin=fre, label =paste(round(percentages),"%",sep=""), fontfamily = "Garamond"), colour="white",  #adding percentages
            size = 2.5, hjust = 0.5, vjust = 0.5)+
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
        text=element_text(family="Goudy Old Style"))

########################################################################################################################
############################################################################################################################################
###### NEW BAR CHART WITH FACETS ####################################################################################################
#########################################################################################################################
#####################################################################################################################################

p = ggplot(percentage_df, aes(x = transfer.fee.label, y=fre, fill=league))
p + geom_bar(stat="identity")+
  scale_fill_manual("National leagues",
                    values=c( "#BF9692", "#FFAA00", "#3B8686", "#1f3057","#779C00", "#DEE3DC"))+
  ggtitle("Number of transfers per transfer fee")+
  #geom_text(aes(y = fre, ymax=fre, ymin=fre, label =paste(round(percentages),"%",sep=""), fontfamily = "Garamond"),
  #          size = 2, position="dodge", vjust=-0.5)+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_text(size  = 7,
                                  angle = 45,
                                  hjust = 1,
                                  vjust = 1),
        axis.ticks= element_line(color=NA),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Goudy Old Style"))+
  facet_wrap(~ league)

##############################################################################################################################
#################################################################################################################   
####################### BAR CHART NUMBER OF TRANSFER PER transfer CAT PER club cat##########################################################  
#################################################################################################################    
#########################################################################################################
#loading df
df.club.cat<- df.stats
#change chart order
df.stats.simp$transfer.fee.label <- factor(df.stats.simp$transfer.fee.label, levels = df.stats.simp$transfer.fee.label[order(df.stats.simp$transfer.fee.cat)])

#change legend order/names
df.stats.simp$Status <- as.character(df.stats.simp$Status)
df.stats.simp$Status[df.stats.simp$Status == "Promoted"] <- "Accessed league this year (Promoted)"
df.stats.simp$Status[df.stats.simp$Status == "Buttom Club"] <- "Bottom clubs"
df.stats.simp$Status <- as.factor(df.stats.simp$Status)


#ggplot(df.stats.simp, aes(x = transfer.fee.cat)) + geom_bar()
p = ggplot(df.stats.simp, aes(x = transfer.fee.label, fill=Status))
p + geom_bar()+
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
  scale_fill_manual("National leagues", values=c("grey", "#E08E79", "#F1D4AF", "#ECE5CE", "#C5E0DC", "#774F38"))+
  ggtitle("Number of transfers per transfer fee")

