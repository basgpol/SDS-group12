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

# Removing rows where positions are unknown.
player.data.partclean = player.data.partclean[-c(67, 78, 83, 105, 178, 457, 659), ]

# Remocing column X.1 and X, which are not useful.

subset(player.data.partclean, select=-c(X.1,X))
player.data.partclean = subset(player.data.partclean, select=-c(X.1,X))

# Creating new dataframe

write.csv(player.data.partclean, file="Player.data.positions.partclean1.csv")



