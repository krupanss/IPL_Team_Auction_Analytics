# ******************************************Libraries***********************************************
library(rvest)

# ******************************************Function***********************************************
fn_getTeamStats = function(scorecardurl)
{
  scorecard <-  read_html(scorecardurl)
  print(paste0("Processing Url: ", scorecardurl))
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
  team.names = gsub('\n', '', team.names)
  team.names = gsub("^\\s+|\\s+$", "", team.names)
  team.names = unlist(strsplit(team.names, " v "))
  team.names = gsub("^\\s+|\\s+$", "", team.names)
  
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

  # ************************Match Date, Time, Overs******************************
  match.time = scorecard %>%  
    html_nodes(css = ".space-top-bottom-5") %>%
    html_text()
  
  match.time = match.time[4]
  match.time = gsub('\n', '', match.time)
  match.time = gsub("^\\s+|\\s+$", "", match.time)
  match.time = unlist(strsplit(match.time," - "))
  
  match.date = match.time[1]
  match.date = as.Date(as.character(match.date), "%d %b %Y") 
  
  match.year = as.numeric(format(match.date,'%Y'))
  
  match.time = match.time[2]
  match.time = unlist(strsplit(match.time," "))
  match.overs = match.time[3]
  match.time = match.time[1]
  
  match.overs = gsub('[(]','',match.overs)
  match.overs = unlist(strsplit(match.overs,"-"))
  match.overs = match.overs[1]
  
  # ************************Match Winner, WonBy************************
  match.winner = scorecard %>%  
    html_nodes(css = ".innings-requirement") %>%
    html_text()
  
  match.winner = unlist(strsplit(match.winner, "won"))
  match.winner = gsub("^\\s+|\\s+$", "", match.winner)
  match.wonby = match.winner[2]
  match.winner = match.winner[1]
  
  # ************************Total Wickets, Overs, Runs, RunPerOver************************
  total.details = scorecard %>%  
    html_nodes(css = ".total-wrap") %>%
    html_text()  
  total.details
  total.details = gsub("^\\s+|\\s+$", "", total.details)
  total.details = unlist(strsplit(total.details, "\n"))
  
  team1.wickets.overs = total.details[2]
  team1.wickets.overs = gsub("^\\s+|\\s+$|[(]|[)]|wickets|wicket|overs", "", team1.wickets.overs)
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
  team2.runrate = gsub("^\\s+|\\s+$|[(]|[)]|runs per over", "", team2.runrate)
  team2.runrate = gsub("^\\s+|\\s+$", "", team2.runrate)
  
  # ************************Total Extras************************
  match.total.extras = scorecard %>%  
    html_nodes(css = ".extra-wrap") %>% html_nodes(css = ".bold") %>% 
    html_text()
  match.total.extras = match.total.extras[match.total.extras!=""]
  team1.totalextras = match.total.extras[2]
  team2.totalextras = match.total.extras[1]
  
  # ************************Extras Details************************
  match.extras = scorecard %>%  
    html_nodes(css = ".extra-wrap") %>% html_nodes(css = ".extra-details") %>% 
    html_text()
  match.extras = gsub("^\\s+|\\s+$|[(]|[)]", "", match.extras)
  team1.extras = match.extras[2]
  team2.extras = match.extras[1]
  
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
  
  # ************************Fall of Wickets************************
  match.fow = scorecard %>%  
    html_nodes(css = ".fow") %>% html_nodes(css = ".fowLink") %>% 
    html_text()
  
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
  
  
  match.pom.team = match.toss.pom[2]
  match.pom.team = unlist(strsplit(match.pom.team,"\n"))
  match.pom.team = match.pom.team[grep("Player of the match",match.pom.team)+1]
  match.pom.team = gsub("^\\s+|\\s+$|-", "", match.pom.team)
  match.pom.team = unlist(strsplit(match.pom.team,"[(]"))
  match.pom = gsub("^\\s+|\\s+$|", "", match.pom.team[1])
  match.pomteam = gsub("^\\s+|\\s+$|[)]", "", match.pom.team[2])
  
  team1.stats = data.frame(Team = team.names[1], MatchNo = match.no, VenueGround = play.ground, VenueCity = play.city, Date = match.date, Year = match.year,
                           Time = as.character(match.time), ActualOvers = as.numeric(match.overs), IsWinner = ifelse(team.names[1] == match.winner, 1, 0),
                           Winner = as.character(match.winner), TossWon = as.character(match.toss), IsTossWon = ifelse(team.names[1] == match.toss, 1, 0),
                           BattingFirst = as.numeric(1), BowlingFirst = as.numeric(0))
  
  
  team1.stats = cbind(team1.stats, WonBy = ifelse(team1.stats$IsWinner == 1, as.character(match.wonby), "--"), 
                      LostBy = ifelse(team1.stats$IsWinner ==1, "--", as.character(match.wonby)))
  
  team1.stats = cbind(team1.stats, RunsScored = as.numeric(team1.score), OversBattingPlayed = as.numeric(team1.overs), 
                      WicketsFell = as.numeric(team1.wickets), RunRateScored = as.numeric(team1.runrate), FOW = as.character(team1.fow),
                      RunsConceeded = as.numeric(team2.score), OversBowled = as.numeric(team2.overs), WicketsTaken = as.numeric(team2.wickets), 
                      RunRateConceeded = as.numeric(team2.runrate), ExtrasGiven = as.numeric(team1.totalextras), ByesGiven = as.numeric(team1.byes), 
                      LegByesGiven = as.numeric(team1.legbyes), WidesGiven = as.numeric(team1.wides), NoballsGiven = as.numeric(team1.noballs),  
                      PlayerOfMatch = match.pom, PlayerOfMatchTeam = match.pomteam, ScorecardUrl = scorecardurl)
  
  team2.stats = data.frame(Team = team.names[2], MatchNo = match.no, VenueGround = play.ground, VenueCity = play.city, Date = match.date, Year = match.year,
                           Time = as.character(match.time), ActualOvers = as.numeric(match.overs), IsWinner = ifelse(team.names[2] == match.winner, 1, 0),
                           Winner = as.character(match.winner), TossWon = as.character(match.toss), IsTossWon = ifelse(team.names[2] == match.toss, 1, 0),
                           BattingFirst = as.numeric(0), BowlingFirst = as.numeric(1))
  
  
  team2.stats = cbind(team2.stats, WonBy = ifelse(team2.stats$IsWinner == 1, as.character(match.wonby), "--"), 
                      LostBy = ifelse(team2.stats$IsWinner ==1, "--", as.character(match.wonby)))
  
  team2.stats = cbind(team2.stats, RunsScored = as.numeric(team2.score), OversBattingPlayed = as.numeric(team2.overs), 
                      WicketsFell = as.numeric(team2.wickets), RunRateScored = as.numeric(team2.runrate), FOW = as.character(team2.fow),
                      RunsConceeded = as.numeric(team1.score), OversBowled = as.numeric(team1.overs), WicketsTaken = as.numeric(team1.wickets), 
                      RunRateConceeded = as.numeric(team1.runrate), ExtrasGiven = as.numeric(team2.totalextras), ByesGiven = as.numeric(team2.byes), 
                      LegByesGiven = as.numeric(team2.legbyes), WidesGiven = as.numeric(team2.wides), NoballsGiven = as.numeric(team2.noballs), 
                      PlayerOfMatch = match.pom, PlayerOfMatchTeam = match.pomteam, ScorecardUrl = scorecardurl)
  
  team.stats = rbind(team1.stats, team2.stats)
  
  # team.stats = data.frame(Team1 = as.character(team.names[1]), Team2 = as.character(team.names[2]), MatchNo = as.character(match.no), 
  #                         Venue = as.character(play.ground), Date = match.date, Time = as.character(match.time), Overs = as.numeric(match.overs), 
  #                         Winner = as.character(match.winner), WonBy = as.character(match.wonby), Team1Wickets = as.numeric(team1.wickets), 
  #                         Team1Overs = as.numeric(team1.overs), Team1.Score = as.numeric(team1.score), Team1RunRate = as.numeric(team1.runrate), 
  #                         Team2Wickets = as.numeric(team2.wickets), Team2Overs = as.numeric(team2.overs), Team2Score = as.numeric(team2.score), 
  #                         Team2RunRate = as.numeric(team2.runrate), Team1Extras = as.numeric(team1.totalextras), 
  #                         Team2Extras = as.numeric(team2.totalextras), Team1Byes = as.numeric(team1.byes), Team1LegByes = as.numeric(team1.legbyes), 
  #                         Team1Wides = as.numeric(team1.wides), Team1Noballs = as.numeric(team1.noballs), Team2Byes = as.numeric(team2.byes),
  #                         Team2LegByes = as.numeric(team2.legbyes), Team2Wides = as.numeric(team2.wides), Team2Noballs = as.numeric(team2.noballs), 
  #                         Team1FOW = as.character(team1.fow), Team2FOW = as.character(team2.fow), TossWon = as.character(match.toss), 
  #                         PlayerOfMatch = as.character(match.pom), PlayerOfMatchTeam = as.character(match.pomteam))
  return(team.stats)
}