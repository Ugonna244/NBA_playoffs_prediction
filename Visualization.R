require(ggplot2)
require(RColorBrewer)
# library(reshape)
require(dplyr)
require(plotrix)

seasons <- read.csv("NBA data base 82-83_16-17.csv")


#Average points of each team from all seasons made it to the playoffs round
teamMeanPoints <- seasons[seasons$Playoff == 1,] %>% group_by(Team) %>% summarise(PTS = mean(PTS))
teamMeanPoints$PTS <- round(teamMeanPoints$PTS,digits = 2)
avgPtsPlot <- ggplot(teamMeanPoints, aes(x=Team, y=PTS)) + 
  geom_bar(aes(fill=Team),stat = "identity") +
  scale_x_discrete()+ ggtitle("Average Points of each team made it to the playoff (all seasons)")  +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.title.x = element_blank(),legend.text=element_text(size=8),legend.position="none") +
  theme(axis.text=element_text(size=8),
        
        axis.title=element_text(size=10,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
avgPtsPlot


# Precentage of playoff appearances per team
numberOfPlayoffPerTeam <- seasons %>% group_by(Team) %>% summarise(Playoff = sum(Playoff),
                                                                       numberOfSeasons = sum(Rk-Rk+1))
numberOfPlayoffPerTeam$PlayyoffPrecentage <- (numberOfPlayoffPerTeam$Playoff/numberOfPlayoffPerTeam$numberOfSeasons)*100
numberOfPlayoffPerTeam.Plot <- ggplot(numberOfPlayoffPerTeam,aes(Team,PlayyoffPrecentage)) +
  geom_bar(aes(fill=Team),stat = "identity") +
  ggtitle("Precentage Of Playoffs Per Team")  +
  theme(plot.title = element_text(hjust = 0),legend.text=element_text(size=8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=10,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
numberOfPlayoffPerTeam.Plot