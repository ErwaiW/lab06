# ===================================================================
# Title: Cleaning Data
# Description:
#   This script performs cleaning tasks and transformations on 
#   various columns of the raw data file.
# Input(s): data file 'raw-data.csv'
# Output(s): data file 'clean-data.csv'
# Author: Yueling Wu
# Date: 10-03-2018
# ===================================================================

library(readr)
library(dplyr)
library(ggplot2)

# Exporting some data tables
nba2018 = read_csv("../data/nba2018-players.csv")
warriors <- data.frame(arrange(filter(nba2018,team == "GSW"),salary))
write.csv(warriors,"../data/warriors.csv",row.names=FALSE)
lakers <- data.frame(arrange(filter(nba2018,team == "LAL"),desc(experience)))
write_csv(lakers,"../data/lakers.csv")

# Exporting some R output
sink(file = "../output/data-structure.txt")
str(nba2018)
sink()

sink(file = "../output/summary-warriors.txt")
summary(warriors)
sink()

sink(file = "../output/summary-lakers.txt")
summary(lakers)
sink()

# Exporting some "base" graphs
png(filename = "../images/scatterplot-height-weight1.png")
plot(nba2018$height,nba2018$weight,col= blues9,pch=20,xlab="Height",ylab="Weight")
dev.off()

png(filename = "../images/scatterplot-height-weight2.png",res=100)
plot(nba2018$height,nba2018$weight,col= blues9,pch=20,xlab="Height",ylab="Weight")
dev.off()

jpeg(filename = "../images/histogram-age.jpeg",width=600,height=400)
hist(nba2018$age,xlab="Age",main = paste("Histogram of age"),col=blues9)
dev.off()

pdf(file = "../images/histogram-age.pdf",width=7,height=5)
hist(nba2018$age,xlab="Age",main = paste("Histogram of age"),col=blues9)
dev.off()

# Exporting some ggplots
ggplot(nba2018,aes(x=points,y=salary))+geom_point()
ggsave("../images/points_salary.pdf",width=7,height=5)
dev.off()

gg_ht_wt_positions = ggplot(nba2018,aes(x=height,y=weight))+geom_point()+facet_wrap(~ position)
ggsave("../images/height_weight_by_position.pdf",width=6,height=4)
dev.off()

# More "dplyr"
nba2018$player[nba2018$team=="LAL"]

nba2018%>%
  filter(team=="GSW"&position=="PG")%>%
  select(player,salary)

nba2018%>%
  filter(experience>10&salary<=10000000)%>%
  select(player,team,height,weight)

nba2018%>%
  filter(experience==0&age==20&games==5)%>%
  select(player,team,height,weight)

nba2018$min_per_game = nba2018$minutes/nba2018$games
gsw_mpg <- nba2018%>%
  filter(team=="GSW")%>%
  select(player,experience,min_per_game)%>%
  arrange(desc(min_per_game))%>%
  data.frame()

nba2018%>%
  group_by(team)%>%
  summarise(meanp3=mean(points3))%>%
  arrange(meanp3)%>%
  head(5)

nba2018%>%
  filter(position=="PF"&experience>=5&experience<=10)%>%
  summarise(meanage=mean(age),sdage=sd(age))
