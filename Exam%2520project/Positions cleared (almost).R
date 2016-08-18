setwd("~/SDS-group12/Exam project")
player.data.partclean=read.csv(file="player_data_partclean.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
library("stringr")
library("dplyr")

# Creating Defender
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Right-Back","Defender")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Left-Back","Defender")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Centre Back","Defender")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"-","Defender")

# Creating Midfield
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Attacking Midfield","Midfield")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Central Midfield","Midfield")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Defensive Midfield","Midfield")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Left Midfield","Midfield")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Right Midfield","Midfield")


# Creating Attacker
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Right Wing","Attacker")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Left Wing","Attacker")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Centre Forward","Attacker")
player.data.partclean$positions = str_replace(player.data.partclean$positions,"Secondary Striker","Attacker")

# Setting free transfer to be equal to 0
player.data.partclean$transfer.fee = str_replace(player.data.partclean$transfer.fee,"Free transfer","0")

# Setting goals = - equal to 0
player.data.partclean$total.goals = str_replace(player.data.partclean$total.goals,"-","0")

# Removing rows where positions are unknown.
player.data.partclean = player.data.partclean[-c(67, 78, 83, 105, 178, 457, 659), ]

# Removing double players
player.data.partclean = player.data.partclean[-c(59, 118, 131, 157, 161, 184, 291, 335, 357, 377, 379, 385, 394, 499, 521, 528, 557, 572, 578, 598, 645, 662, 745, 753), ]

# Remocing column X.1 and X, which are not useful.

subset(player.data.partclean, select=-c(X.1,X))
player.data.partclean = subset(player.data.partclean, select=-c(X.1,X))

# Removing rows which contain transfer.fee = NA
player.data.partclean = subset(player.data.partclean, !is.na(transfer.fee))

# Removing keepers
player.data.partclean = player.data.partclean[!grepl("Keeper", player.data.partclean$positions),]


# Removing all NA
#player.data.partclean = na.omit(player.data.partclean)


# Creating new dataframe for uploading

write.csv(player.data.partclean, file="Player.data.positions.nokeepers.partclean.csv")




