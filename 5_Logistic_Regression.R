rm(list = ls())
source("UserFunctions.R")
# Logistic Regression for Output of Win or Loss
# ***********************************************************************************************************************
library(dplyr)
# stats.leagues = team.stats.complete %>% filter(grepl("match",MatchNo))

load("TeamStats.rda")
# Get Copy of Dataset 
lr.stats = team.stats.complete

#Check for Missing Values.
sapply(lr.stats, function(x) sum(is.na(x)))

# Check for outliers
lr.stats[team.stats.complete$OversBattingPlayed < 15,]$ScorecardUrl
lr.stats[grep("D/L",team.stats.complete$WonBy),]$ScorecardUrl

# Below four matches are played less than 10 overs because of D/L or Reduced Overs
match.outlier = c('http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980989.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829771.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829807.html',
                  'http://www.espncricinfo.com/indian-premier-league-2014/engine/match/733993.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980943.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980997.html',
                  'http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980999.html',
                  'http://www.espncricinfo.com/indian-premier-league-2015/engine/match/829743.html')

lr.stats = subset(lr.stats, !lr.stats$ScorecardUrl %in% match.outlier)

# Converting Numeric Data to Factors
lr.stats$IsTossWon = as.factor(lr.stats$IsTossWon)
lr.stats$IsWinner = as.factor(lr.stats$IsWinner)
lr.stats$BattingFirst = as.factor((lr.stats$BattingFirst))
lr.stats$BowlingFirst = as.factor(lr.stats$BowlingFirst)


# Runrate is a redundant dimension which can be derived from Runs and Oversplayed.
# Same with the case of Runrate conceeded and can be checked with the correlation plot below.
# Check for Colinearity
# Get Numeric Data and Categorical Data 
lr.stats.numeric = select_if(lr.stats, is.numeric)
lr.stats.category = lr.stats[,!names(lr.stats) %in% names(lr.stats.numeric)]

# Pre-processing Numerical Data
# Dimension Reduction
# Year, ActualOvers doesn't contribute any information
# Removing Redundant Dimensions
lr.stats.numeric$Year = NULL
lr.stats.numeric$ActualOvers = NULL


# Runrate is a redundant dimension which can be derived from Runs and Oversplayed.
# Same with the case of Runrate conceeded and can be checked with the correlation plot below.
# Check for Colinearity
library(Hmisc)
rcorr(as.matrix(lr.stats.numeric), type="pearson")

library(corrplot)
stats.cor = cor(lr.stats.numeric)
corrplot(stats.cor, method="number")

# Removing Runrate dimensions
lr.stats.numeric$RunRateScored = NULL
lr.stats.numeric$RunRateConceeded = NULL
# View(lr.stats.numeric)

# Pre-processing Categorical Data
# Dimension Reduction
# ScorecardUrl, MatchNo, Date doesn't contribute any information
lr.stats.category$ScorecardUrl = NULL
lr.stats.category$MatchNo = NULL
lr.stats.category$Date = NULL
lr.stats.category$Winner = NULL
lr.stats.category$TossWon = NULL
lr.stats.category$WonBy = NULL
lr.stats.category$LostBy = NULL
lr.stats.category$FOW = NULL
lr.stats.category$PlayerOfMatch = NULL
lr.stats.category$PlayerOfMatchTeam = NULL
lr.stats.category$Team = NULL

# Venue City is correlated with Venue Playgroud
lr.stats.category$VenueCity = NULL

# Converting Character to Factor
lr.stats.category$VenueGround = as.factor(lr.stats.category$VenueGround)
lr.stats.category$Time = as.factor(lr.stats.category$Time)

# Check for Categorical Colinearity
library(polycor)
polychor(lr.stats.category$BattingFirst, lr.stats.category$BowlingFirst, ML = TRUE, control = list(), std.err = FALSE, maxcor=.9999)

# We can see 99.99% Negative Correalation between Batting first and Bowling First as both are opposite derived dimensions. 
# We'll remove Bowling First Variable
lr.stats.category$BowlingFirst = NULL

# Histograms
stats.hist.numeric = lapply(names(lr.stats.numeric), FUN = fn_Hist_Plot_Con, histdata = lr.stats.numeric)
stats.hist.category = lapply(names(lr.stats.category), FUN = fn_Hist_Plot_Cat, histdata = lr.stats.category)
fn_Hist_Plot_Cat(xnames = "VenueGround", histdata = lr.stats.category)
table(lr.stats.category$VenueGround)
library(gridExtra)
do.call("grid.arrange", c( c(stats.hist.numeric,stats.hist.category), ncol=4))

# Box Plots
stats.box.numeric = lapply(names(lr.stats.numeric), fn_BoxPlotCon, bdata = lr.stats.numeric, x_var =1)
# stats.box.category = lapply(names(lr.stats.category), fn_BoxPlotCat, bdata = lr.stats.category)

lr.stats.data = cbind(lr.stats.category, lr.stats.numeric)

# Group Plots
str(lr.stats.data)
stats.hist.RunScored_Win = fn_Hist_Plot_ConGroup(xnames = "RunsScored", histdata = lr.stats.data, hgroup = "IsWinner")
plot(stats.hist.RunScored_Win)
stats.box.RunScored_Win = fn_BoxPlotConGroup(y_names = "RunsScored", bdata = lr.stats.data, x_var = "IsWinner")
plot(stats.box.RunScored_Win)
sdata.hist.BattingFirst_Win = fn_Hist_Plot_CatGroup(xnames = "BattingFirst", histdata = lr.stats.data, hgroup = "IsWinner")
plot(sdata.hist.BattingFirst_Win)

set.seed(11)

# Train and Test Data Partition based on Outcome variable
library(caTools)
split = sample.split(lr.stats.data$IsWinner, SplitRatio = 0.75)
head(split,20)

lr.stats.train = subset(lr.stats.data, split == TRUE)
table(lr.stats.train$IsWinner)

lr.stats.test = subset(lr.stats.data, split == FALSE)
table(lr.stats.test$IsWinner)

# Train and Test Data partition randomly
# 75% of the sample size
smp_size <- floor(0.75 * nrow(lr.stats.data))

set.seed(11)
train_ind = sample(seq_len(nrow(lr.stats.data)), size = smp_size)

lr.stats.train = lr.stats.data[train_ind, ]
lr.stats.test = lr.stats.data[-train_ind, ]

# caret package Data Partition
library(caret)
train = createDataPartition(y = lr.stats.data$IsWinner, times = 1, p = 0.7, list = FALSE)

lr.stats.train = lr.stats.data[train,]
lr.stats.test = lr.stats.data[-train,]


stats.glm<-glm(IsWinner ~ ., data=lr.stats.train, family="binomial"(link="logit"))
summary(stats.glm)
confint(stats.glm)

## odds ratios only
coef(stats.glm)
exp(coef(stats.glm))


#developing prediction on the Testing dataset
lr.stats.test$PredictWin<-predict.glm(stats.glm, newdata=lr.stats.test,type="response")

# Evaluating the model
head(lr.stats.test)

library(SDMTools)
confusion.matrix(lr.stats.test$IsWinner, lr.stats.test$PredictWin, threshold = 0.5)

#Predictive ability of the model using ROC curve

library(pROC)
lr.stats.test.roc<-roc(IsWinner~PredictWin, data=lr.stats.test)

attributes(lr.stats.test.roc)
plot(lr.stats.test.roc)
lr.stats.test.roc$auc

library(BCA)
cellPhone.test$Churn <- as.factor(cellPhone.test$Churn)
lift.chart("cellPhone.glm", data=cellPhone.test, targLevel=1, trueResp = .28)


