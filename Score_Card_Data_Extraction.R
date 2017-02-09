library(rvest)
scorecardurl = "http://www.espncricinfo.com/indian-premier-league-2016/engine/match/980929.html"
scorecard <-  html(scorecardurl)
str(scorecard)
# ****************************Match No********************************
match.no = scorecard %>%  
  html_node(css = ".space-top-bottom-5") %>%
  html_text()
match.no = unlist(strsplit(match.no,"-"))
match.no = gsub("^\\s+|\\s+$", "", match.no[2])

# ****************************Team Names********************************
team.names = scorecard %>%  
  html_node(css = ".team-1-name") %>%
  html_text()
team.names
team.names = gsub('\n', '', team.names)
team.names = gsub("^\\s+|\\s+$", "", team.names)
team.names = unlist(strsplit(team.names, " v "))
team.names
team.names = gsub("^\\s+|\\s+$", "", team.names)
team.data = data.frame(Team1 = team.names[1], Team2 = team.names[2])
# team.names
# **************************End of Team Names***************************

# ****************************Play Ground********************************
play.city.ground = scorecard %>% 
  html_nodes(css = ".match-information") %>% 
  html_nodes(css = ".headLink") %>%
  html_text()
play.city.ground = play.city.ground[3]
play.city.ground = unlist(strsplit(play.city.ground, ","))

play.ground = play.city.ground[1]
play.city = play.city.ground[2]
play.ground = gsub("^\\s+|\\s+$", "", play.ground)
play.city = gsub("^\\s+|\\s+$", "", play.city)

play.ground
play.city

team.data = cbind(team.data, Venue = play.ground)
# play.ground
# ************************End of Play Ground*****************************
# ************************Match Date, Time, Overs******************************
match.time = scorecard %>%  
  html_nodes(css = ".space-top-bottom-5") %>%
  html_text()

match.time = match.time[4]
match.time = gsub('\n', '', match.time)
match.time = gsub("^\\s+|\\s+$", "", match.time)
match.time = unlist(strsplit(match.time," - "))

match.date = match.time[1]
match.date <- as.Date(as.character(match.date), "%d %b %Y") 

match.year = as.numeric(format(match.date,'%Y'))


match.time = match.time[2]
match.time = unlist(strsplit(match.time," "))
match.overs = match.time[3]
match.time = match.time[1]

match.overs = gsub('[(]','',match.overs)
match.overs = unlist(strsplit(match.overs,"-"))
match.overs = match.overs[1]

# match.date
# match.time
# match.overs

team.data = cbind(team.data, Venue = play.ground, MatchDate = match.date, MatchTime = match.time, MatchOvers = match.overs)
# team.data
# ************************End of Match Date, Time, Overs******************************
# ************************Match Winner, WonBy************************
match.winner = scorecard %>%  
  html_nodes(css = ".innings-requirement") %>%
  html_text()

match.winner = unlist(strsplit(match.winner, "won"))
match.winner = gsub("^\\s+|\\s+$", "", match.winner)
match.wonby = match.winner[2]
match.winner = match.winner[1]
match.winner
match.wonby
team.data = cbind(team.data, MatchWinner = match.winner, MatchWonBy = match.wonby) 
team.data
# ************************End of Match Winner, WonBy************************
# ************************Total Wickets, Overs, Runs, RunPerOver************************
total.details = scorecard %>%  
  html_nodes(css = ".total-wrap") %>%
  html_text()  

total.details = gsub("^\\s+|\\s+$", "", total.details)
total.details = unlist(strsplit(total.details, "\n"))
total.details
team1.wickets.overs = total.details[2]
team1.wickets.overs = gsub("^\\s+|\\s+$|[(]|[)]|wickets|overs", "", team1.wickets.overs)
team1.wickets.overs = unlist(strsplit(team1.wickets.overs, ";"))

team1.wickets = team1.wickets.overs[1]
team1.wickets = gsub("^\\s+|\\s+$", "", team1.wickets)
if(team1.wickets == "all out"){
  team1.wickets = 10
}

team1.overs = team1.wickets.overs[2]
team1.overs = gsub("^\\s+|\\s+$", "", team1.overs)

team1.score = total.details[3]
team1.score = gsub("^\\s+|\\s+$", "", team1.score)

team1.runrate = total.details[4]
team1.runrate = gsub("^\\s+|\\s+$|[(]|[)]|runs per over", "", team1.runrate)
team1.runrate = gsub("^\\s+|\\s+$", "", team1.runrate)

# team1.wickets
# team1.overs
# team1.score
# team1.runrate

team2.wickets.overs = total.details[6]
team2.wickets.overs = gsub("^\\s+|\\s+$|[(]|[)]|wickets|wicket|overs", "", team2.wickets.overs)
team2.wickets.overs = unlist(strsplit(team2.wickets.overs, ";"))

team2.wickets = team2.wickets.overs[1]
team2.wickets = gsub("^\\s+|\\s+$", "", team2.wickets)
if(team2.wickets == "all out"){
  team2.wickets = 10
}

team2.overs = team2.wickets.overs[2]
team2.overs = gsub("^\\s+|\\s+$", "", team2.overs)

team2.score = total.details[7]
team2.score = gsub("^\\s+|\\s+$", "", team2.score)

team2.runrate = total.details[8]
team2.runrate = gsub("^\\s+|\\s+$|[(]|[)]|runs per over", "", team1.runrate)
team2.runrate = gsub("^\\s+|\\s+$", "", team1.runrate)

team2.wickets
team2.overs
team2.score
team2.runrate

# ************************End of Total Wickets, Overs, Runs, RunPerOver************************
# ************************Total Extras************************
match.total.extras = scorecard %>%  
  html_nodes(css = ".extra-wrap") %>% html_nodes(css = ".bold") %>% 
  html_text()
match.total.extras = match.total.extras[match.total.extras!=""]
team1.totalextras = match.total.extras[1]
team2.totalextras = match.total.extras[2]

# ************************End of Total Extras************************
# ************************Extras Details************************
match.extras = scorecard %>%  
  html_nodes(css = ".extra-wrap") %>% html_nodes(css = ".extra-details") %>% 
  html_text()
match.extras = gsub("^\\s+|\\s+$|[(]|[)]", "", match.extras)
team1.extras = match.extras[1]
team2.extras = match.extras[2]

team1.extras = unlist(strsplit(team1.extras,","))
team1.extras = gsub("^\\s+|\\s+$", "", team1.extras)
team1.byes = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("b","",team1.extras[grep("^b",team1.extras)]))
team1.legbyes = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("lb","",team1.extras[grep("^lb",team1.extras)]))
team1.wides = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("w","",team1.extras[grep("^w",team1.extras)]))
team1.noballs = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("nb","",team1.extras[grep("^nb",team1.extras)]))

if(length(team1.byes) == 0){
  team1.byes = 0
}
if(length(team1.legbyes) == 0){
  team1.legbyes = 0
}
if(length(team1.wides) == 0){
  team1.wides = 0
}
if(length(team1.noballs) == 0){
  team1.noballs = 0
}

team1.byes
team1.legbyes
team1.wides
team1.noballs

team2.extras = unlist(strsplit(team2.extras,","))
team2.extras = gsub("^\\s+|\\s+$", "", team2.extras)
team2.byes = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("b","",team2.extras[grep("^b",team2.extras)]))
team2.legbyes = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("lb","",team2.extras[grep("^lb",team2.extras)]))
team2.wides = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("w","",team2.extras[grep("^w",team2.extras)]))
team2.noballs = gsub("^\\s+|\\s+$|[(]|[)]", "", gsub("nb","",team2.extras[grep("^nb",team2.extras)]))

if(length(team2.byes) == 0){
  team2.byes = 0
}
if(length(team2.legbyes) == 0){
  team2.legbyes = 0
}
if(length(team2.wides) == 0){
  team2.wides = 0
}
if(length(team2.noballs) == 0){
  team2.noballs = 0
}

team2.byes
team2.legbyes
team2.wides
team2.noballs
# ************************End of Extras Details************************
# ************************Fall of Wickets************************
match.fow = scorecard %>%  
  html_nodes(css = ".fow") %>% html_nodes(css = ".fowLink") %>% 
  html_text()
match.fow
if(team1.wickets == "0" & team1.wickets == "0"){
  team1.fow = "NA"
  team2.fow = "NA"
} else if(team1.wickets != "0" & team1.wickets == "0") {
  team1.fow = match.fow[1]
  team2.fow = "NA"
} else if(team1.wickets == "0" & team1.wickets != "0") {
  team1.fow = "NA"
  team2.fow = match.fow[1]
} else if(team1.wickets != "0" & team1.wickets != "0") {
  team1.fow = match.fow[1]
  team2.fow = match.fow[2]
}


team1.fow
team2.fow

# ************************End of Fall of Wickets************************
# ************************Match Toss, Player of the Match************************
match.toss.pom = scorecard %>%  
  html_nodes(css = ".match-information") %>% 
  html_nodes(css = ".space-top-bottom-10") %>%
  html_text()

match.toss = match.toss.pom[1]
match.toss = unlist(strsplit(match.toss,"\n"))
match.toss = match.toss[grep("Toss",match.toss)]
match.toss = gsub("Toss|-","",match.toss)
match.toss = gsub("^\\s+|\\s+$", "", match.toss)

grep("Player of the match",match.pom.team)+1

match.pom.team = match.toss.pom[2]
match.pom.team = unlist(strsplit(match.pom.team,"\n"))
match.pom.team = match.pom.team[grep("Player of the match",match.pom.team)+1]
match.pom.team = gsub("^\\s+|\\s+$|-", "", match.pom.team)
match.pom.team = unlist(strsplit(match.pom.team,"[(]"))
match.pom = gsub("^\\s+|\\s+$|", "", match.pom.team[1])
match.pomteam = gsub("^\\s+|\\s+$|[)]", "", match.pom.team[2])

match.toss
match.pom
match.pomteam
str(team.stats)
team.stats = data.frame(Team1 = as.character(team.names[1]), Team2 = as.character(team.names[2]), MatchNo = as.character(match.no), 
                        Venue = as.character(play.ground), Date = match.date, Time = as.character(match.time), Overs = as.numeric(match.overs), 
                        Winner = as.character(match.winner), WonBy = as.character(match.wonby), Team1Wickets = as.numeric(team1.wickets), 
                        Team1Overs = as.numeric(team1.overs), Team1.Score = as.numeric(team1.score), Team1RunRate = as.numeric(team1.runrate), 
                        Team2Wickets = as.numeric(team2.wickets), Team2Overs = as.numeric(team2.overs), Team2Score = as.numeric(team2.score), 
                        Team2RunRate = as.numeric(team2.runrate), Team1Extras = as.numeric(team1.totalextras), 
                        Team2Extras = as.numeric(team2.totalextras), Team1Byes = as.numeric(team1.byes), Team1LegByes = as.numeric(team1.legbyes), 
                        Team1Wides = as.numeric(team1.wides), Team1Noballs = as.numeric(team1.noballs), Team2Byes = as.numeric(team2.byes),
                        Team2LegByes = as.numeric(team2.legbyes), Team2Wides = as.numeric(team2.wides), Team2Noballs = as.numeric(team2.noballs), 
                        Team1FOW = as.character(team1.fow), Team2FOW = as.character(team2.fow), TossWon = as.character(match.toss), 
                        PlayerOfMatch = as.character(match.pom), PlayerOfMatchTeam = as.character(match.pomteam))


team1.extras = unlist(strsplit(team1.extras, ","))
team1.extras 
team1.byes = team1.extras[grep("[b]",team1.extras)]
team1.byes
tables=readHTMLTable(scorecardurl,stringsAsFactors = F)
tables.length = length(tables)
names(tables)[1:tables.length] = paste("Table",1:tables.length,sep="")
names(tables)

team1.batting = tables[1]$Table1
team2.bowling = tables[2]$Table2
team2.batting = tables[3]$Table3
team1.bowling = tables[4]$Table4
View(team1.batting)
team1.batting
team1.batting = subset(team1.batting, !is.na(team1.batting$V4))
team1.batting$
View(a)
is.na(a$`O`)
a = subset(a, !is.na(a$`O`))
a$` ` = NULL
a$` .1`= NULL
View(na.omit(a))
