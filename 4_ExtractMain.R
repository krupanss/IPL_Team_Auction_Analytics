rm(list = ls())
source("UserFunctions.R")
library(XML)
setwd("F:/Krupan/Documents/2_Freelance/3_Github_Projects/IPL_Team_Auction_Analytics/")
#***************************************Read All Matches Scorecard URLs****************************************************************
ipl2016url = "http://www.espncricinfo.com/indian-premier-league-2016/engine/series/968923.html"
ipl2015url = "http://www.espncricinfo.com/indian-premier-league-2015/engine/series/791129.html"
ipl2014url = "http://www.espncricinfo.com/indian-premier-league-2014/engine/series/695871.html"

urls1 = XML::getHTMLLinks(ipl2016url)
urls2 = XML::getHTMLLinks(ipl2015url)
urls3 = XML::getHTMLLinks(ipl2014url)

urls = c(urls1,urls2,urls3) 
urls = unique(urls)

scorecardUrls1 = as.character(urls[grep("/indian-premier-league-2016/engine/match/",urls)])
scorecardUrls2 = as.character(urls[grep("/indian-premier-league-2015/engine/match/",urls)])
scorecardUrls3 = as.character(urls[grep("/indian-premier-league-2014/engine/match/",urls)])

scorecardUrls = c(scorecardUrls1, scorecardUrls2, scorecardUrls3)

scorecardUrls = paste0("http://www.espncricinfo.com",scorecardUrls)

# Match Abandoned Url
scorecardUrls = scorecardUrls[scorecardUrls != "http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829755.html"]
scorecardUrls = scorecardUrls[scorecardUrls != "http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829763.html"]
scorecardUrls = scorecardUrls[scorecardUrls != "http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829813.html"]


rm(ipl2016url)
rm(ipl2015url)
rm(ipl2014url)
rm(urls)
rm(urls1)
rm(urls2)
rm(urls3)
rm(scorecardUrls1)
rm(scorecardUrls2)
rm(scorecardUrls3)

# scorecardUrls = as.list(scorecardUrls)
# rm(urls)
# scorecardUrls[15]
source("1_ExtractTeamStats.R")
# fn_getTeamStats

# Extraction
team.stats.complete = lapply(scorecardUrls, FUN = fn_getTeamStats)

team.stats.complete = do.call(rbind,team.stats.complete)

# Transformation
team.stats.complete$Team = as.character(team.stats.complete$Team)
team.stats.complete$MatchNo = as.character(team.stats.complete$MatchNo)
team.stats.complete$VenueGround = as.character(team.stats.complete$VenueGround)
team.stats.complete$VenueCity = as.character(team.stats.complete$VenueCity)
team.stats.complete$Time = as.character(team.stats.complete$Time)
team.stats.complete$Winner = as.character(team.stats.complete$Winner)
team.stats.complete$WonBy = as.character(team.stats.complete$WonBy)
team.stats.complete$LostBy = as.character(team.stats.complete$LostBy)
team.stats.complete$FOW = as.character(team.stats.complete$FOW)
team.stats.complete$TossWon = as.character(team.stats.complete$TossWon)
team.stats.complete$PlayerOfMatch = as.character(team.stats.complete$PlayerOfMatch)
team.stats.complete$PlayerOfMatchTeam = as.character(team.stats.complete$PlayerOfMatchTeam)
team.stats.complete$ScorecardUrl = as.character(team.stats.complete$ScorecardUrl)

# Check for Missing Values
sapply(team.stats.complete, function(x) sum(is.na(x)))

team.stats.complete[team.stats.complete$ScorecardUrl == 
                      "http://www.espncricinfo.com/indian-premier-league-2014/engine/match/729315.html",]
team.stats.complete[team.stats.complete$ScorecardUrl == 
                      "http://www.espncricinfo.com/indian-premier-league-2014/engine/match/729315.html" & 
                      team.stats.complete$Team == "Kolkata Knight Riders" ,]$LostBy = "by the one-over eliminator"

#Save Data to rda file
save(team.stats.complete,file = "TeamStats.rda")



# load("TeamStats.rda")
# Structure of Data
str(team.stats.complete)
summary(team.stats.complete)
View(team.stats.complete)


# Check for outliers
team.stats.complete[team.stats.complete$OversBattingPlayed < 15,]$ScorecardUrl
team.stats.complete[grep("D/L",team.stats.complete$WonBy),]$ScorecardUrl

# Below four matches are played less than 10 overs because of D/L or Reduced Overs
match.outlier = c('http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980989.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829771.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829807.html',
                  'http://www.espncricinfo.com/indian-premier-league-2014/engine/match/733993.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980943.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980997.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980999.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829743.html')

teams.numeric.stats = subset(team.stats.complete, !team.stats.complete$ScorecardUrl %in% match.outlier)

# Subset only Numeric Data
teams.numeric.stats = select_if(teams.numeric.stats, is.numeric)

# Removing Redundant Dimensions
teams.numeric.stats$Year = NULL
teams.numeric.stats$ActualOvers = NULL

# Removing Numerically Categorical Dimensions
teams.numeric.stats$IsWinner = NULL
teams.numeric.stats$IsTossWon = NULL
teams.numeric.stats$BattingFirst = NULL
teams.numeric.stats$BowlingFirst = NULL

#Check for Missing Values before clustering.
sapply(teams.numeric.stats, function(x) sum(is.na(x)))

# Choosing Dimensions for Clustering
str(teams.numeric.stats)
# Runrate is a redundant dimension which can be derived from Runs and Oversplayed.
# Same with the case of Runrate conceeded and can be checked with the correlation plot below.
# Check for Colinearity
library(Hmisc)
rcorr(as.matrix(teams.numeric.stats), type="pearson")

library(corrplot)
stats.cor = cor(teams.numeric.stats)
corrplot(stats.cor, method="number")


# Lower Variance Dimensions
summary(teams.cluster.data)


# vif(teams.cluster.data)

# Clustering of data.
# ***********************************************************************************************************************
set.seed(11)
# teams.cluster.data = teams.numeric.stats[,c(1,2,3,5,6,7,9)]
teams.cluster.data = teams.numeric.stats[,c(1,2,3)]
str(teams.cluster.data)
teams.cluster = kmeans(teams.cluster.data, 4)
# teams.cluster$cluster

# Plotting Clusters 
# Method 1
plot(teams.cluster.data,col=teams.cluster$cluster)
points(teams.cluster$center,col=1:ncol(teams.cluster.data),pch=8,cex=1)

# Cluster Plot against 1st 2 principal components
library(cluster) 
clusplot(teams.cluster.data, teams.cluster$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(teams.cluster.data, teams.cluster$cluster)
# ***********************************************************************************************************************

