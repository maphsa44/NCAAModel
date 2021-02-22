library(dplyr)
library(tidyverse)
library(stringr)
library(reshape)
library(rattle)
library(readxl)
library(MASS)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(DT)
library(gridExtra)
library(knitr)
library(rmarkdown)


require(MASS)
require(dplyr)

#setwd("~/2021S EM -623-WS/Final/march-madness-2021/data")

####################
###Setup Functions
####################

#Pull Data
LocationGameStats <- function(Location) {
  ActualGameResults<-left_join(
    left_join(
      SeasonResults %>% dplyr::filter(WLoc == Location), 
      TeamConferences %>% dplyr::filter(Season >= 2003), by =c("WTeamID"="TeamID", "Season")),
    TeamConferences %>% dplyr::filter(Season >= 2003), by =c("LTeamID"="TeamID", "Season")) %>% 
    mutate('WOffEff' = ((WFGM*2)+WFGM3+WFTM+WOR)/(WFGA+(WFTA/2)+WTO)*100,
           'WDefEff' = ((LFGM*2)+LFGM3+LFTM+LOR)/(LFGA+(LFTA/2)+LTO)*100,
           'LOffEff' = ((LFGM*2)+LFGM3+LFTM+LOR)/(LFGA+(LFTA/2)+LTO)*100,
           'LDefEff' = ((WFGM*2)+WFGM3+WFTM+WOR)/(WFGA+(WFTA/2)+WTO)*100,
           GameType = case_when(ConfAbbrev.x==ConfAbbrev.y ~"C", ConfAbbrev.x!=ConfAbbrev.y ~"NC"))

  if(Location == 'H') {
    ActualGameResults<-ActualGameResults %>% dplyr::rename(HomeConfAbbrev = ConfAbbrev.x,
                                                           HomeTeamName = TeamName.x,
                                                           AwayConfAbbrev = ConfAbbrev.y,
                                                           AwayTeamName = TeamName.y)
    colnames(ActualGameResults) <- sub("W", "Home", colnames(ActualGameResults))
    colnames(ActualGameResults) <- sub("L", "Away", colnames(ActualGameResults))
    ActualGameResults<-ActualGameResults %>% dplyr::select(1, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 36, 37, 40, 41,
                                                           6, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 38, 39, 42, 43, 35, 2)
  }
  if(Location == 'A') {
    ActualGameResults<-ActualGameResults %>% dplyr::rename(AwayConfAbbrev = ConfAbbrev.x,
                                                           AwayTeamName = TeamName.x,
                                                           HomeConfAbbrev = ConfAbbrev.y,
                                                           HomeTeamName = TeamName.y)
    colnames(ActualGameResults) <- sub("W", "Away", colnames(ActualGameResults))
    colnames(ActualGameResults) <- sub("L", "Home", colnames(ActualGameResults))
    ActualGameResults<-ActualGameResults %>% dplyr::select(1, 6, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 38, 39, 42, 43,
                                                           4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 36, 37, 40, 41, 35, 2)
  }
  if(Location == 'N') {
    ActualGameResults<-ActualGameResults %>% dplyr::rename(HomeConfAbbrev = ConfAbbrev.x,
                                                           HomeTeamName = TeamName.x,
                                                           AwayConfAbbrev = ConfAbbrev.y,
                                                           AwayTeamName = TeamName.y)
    colnames(ActualGameResults) <- sub("W", "Home", colnames(ActualGameResults))
    colnames(ActualGameResults) <- sub("L", "Away", colnames(ActualGameResults))
    ActualGameResults<-ActualGameResults %>% dplyr::select(1, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 36, 37, 40, 41,
                                                           6, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 38, 39, 42, 43, 35, 2)
  }
  return(ActualGameResults)
}



GetTeamStats <- function(day=NULL, season=NULL, home_or_away=NULL, team=NULL, current_year=NULL) {
  if(is.null(day) && is.null(season) && is.null(home_or_away) && is.null(team) && is.null(current_year)) {
    HomeTeamStats<-aggregate(ActualGameResults[,-c(1, 16, 17, 21:37, 40, 41, 42, 43)], ActualGameResults[,c(1, 16, 17)], FUN=sum) %>% mutate(gamesplayed = homewin+homelost)
    
    colnames(HomeTeamStats) <- sub("HomeScore", "OffPoints", colnames(HomeTeamStats))
    colnames(HomeTeamStats) <- sub("AwayScore", "DefPoints", colnames(HomeTeamStats))
    colnames(HomeTeamStats) <- sub("Home", "", colnames(HomeTeamStats), ignore.case = TRUE)
    
    
    AwayTeamStats<-aggregate(ActualGameResults[,c(20:33, 36:37, 2, 40, 41)], ActualGameResults[,c(1, 34, 35)], FUN=sum) %>% mutate(gamesplayed = awaywin+awaylost)
    colnames(AwayTeamStats) <- sub("AwayScore", "OffPoints", colnames(AwayTeamStats))
    colnames(AwayTeamStats) <- sub("HomeScore", "DefPoints", colnames(AwayTeamStats))
    colnames(AwayTeamStats) <- sub("Away", "", colnames(AwayTeamStats), ignore.case = TRUE)
    
    TeamStats<- aggregate(as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[-c(1:3)], as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[c(1:3)], FUN = sum)
    
    TeamStats<-cbind(
      TeamStats[,c(1:3, 21:22)],
      TeamStats[,4:20]/TeamStats[,23]) %>%
      mutate(FGP = FGM/FGA,
             FGP3 = FGM3/FGA3,
             FTP = FTM/FTA)
    
    return(TeamStats)
  break
  }
  if(is.null(current_year)) {
    PastActualGameResults<-ActualGameResults[ActualGameResults$Season<season,]
    ActualGameResults<-ActualGameResults[ActualGameResults$Season==season,]
    ActualGameResults<-ActualGameResults[ActualGameResults$DayNum<day,]
    ActualGameResults<-rbind(PastActualGameResults,ActualGameResults)
    
    HomeTeamStats<-aggregate(ActualGameResults[,-c(1, 16, 17, 21:37, 40, 41, 42, 43)], ActualGameResults[,c(1, 16, 17)], FUN=sum) %>% mutate(gamesplayed = homewin+homelost)
    
    colnames(HomeTeamStats) <- sub("HomeScore", "OffPoints", colnames(HomeTeamStats))
    colnames(HomeTeamStats) <- sub("AwayScore", "DefPoints", colnames(HomeTeamStats))
    colnames(HomeTeamStats) <- sub("Home", "", colnames(HomeTeamStats), ignore.case = TRUE)
    
    
    AwayTeamStats<-aggregate(ActualGameResults[,c(20:33, 36:37, 2, 40, 41)], ActualGameResults[,c(1, 34, 35)], FUN=sum) %>% mutate(gamesplayed = awaywin+awaylost)
    colnames(AwayTeamStats) <- sub("AwayScore", "OffPoints", colnames(AwayTeamStats))
    colnames(AwayTeamStats) <- sub("HomeScore", "DefPoints", colnames(AwayTeamStats))
    colnames(AwayTeamStats) <- sub("Away", "", colnames(AwayTeamStats), ignore.case = TRUE)
    
    TeamStats<- aggregate(as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[-c(1:3)], as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[c(1:3)], FUN = sum)
    
    TeamStats<-cbind(
      TeamStats[,c(1:3, 21:22)],
      TeamStats[,4:20]/TeamStats[,23]) %>%
      mutate(FGP = FGM/FGA,
             FGP3 = FGM3/FGA3,
             FTP = FTM/FTA)
    
    return(TeamStats)
  break
  } 
  
  if(!is.null(day)) {
    ActualGameResults<-ActualGameResults[ActualGameResults$DayNum <= day, ]
  } 
  
  if(!is.null(season)) {
    ActualGameResults<-ActualGameResults[ActualGameResults$Season == season, ]
  } 
  
  if(!is.null(team)) {
    ActualGameResults<-ActualGameResults[ActualGameResults$HomeTeamName == team |ActualGameResults$AwayTeamName == team, ]
  } 
  
  
  HomeTeamStats<-aggregate(ActualGameResults[,-c(1, 16, 17, 21:37, 40, 41, 42, 43)], ActualGameResults[,c(1, 16, 17)], FUN=sum) %>% mutate(gamesplayed = homewin+homelost)
  
  colnames(HomeTeamStats) <- sub("HomeScore", "OffPoints", colnames(HomeTeamStats))
  colnames(HomeTeamStats) <- sub("AwayScore", "DefPoints", colnames(HomeTeamStats))
  colnames(HomeTeamStats) <- sub("Home", "", colnames(HomeTeamStats), ignore.case = TRUE)
  
  
  AwayTeamStats<-aggregate(ActualGameResults[,c(20:33, 36:37, 2, 40, 41)], ActualGameResults[,c(1, 34, 35)], FUN=sum) %>% mutate(gamesplayed = awaywin+awaylost)
  colnames(AwayTeamStats) <- sub("AwayScore", "OffPoints", colnames(AwayTeamStats))
  colnames(AwayTeamStats) <- sub("HomeScore", "DefPoints", colnames(AwayTeamStats))
  colnames(AwayTeamStats) <- sub("Away", "", colnames(AwayTeamStats), ignore.case = TRUE)
  
  TeamStats<- aggregate(as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[-c(1:3)], as.data.frame(rbind(HomeTeamStats,AwayTeamStats))[c(1:3)], FUN = sum)
  
  
  
  if(is.null(home_or_away)) {
    TeamStats<-cbind(
      TeamStats[,c(1:3, 21:22)],
      TeamStats[,4:20]/TeamStats[,23]) %>%
      mutate(FGP = FGM/FGA,
             FGP3 = FGM3/FGA3,
             FTP = FTM/FTA)
    if(is.null(team)) {
      return(TeamStats)
    } else {
      TeamStats[TeamStats$TeamName==team, ]
    }
    
  } else if(home_or_away == 'home') {
    TeamStats<-cbind(
      HomeTeamStats[,c(1:3, 21:22)],
      HomeTeamStats[,4:20]/HomeTeamStats[,23]) %>%
      mutate(FGP = FGM/FGA,
             FGP3 = FGM3/FGA3,
             FTP = FTM/FTA)
    if(is.null(team)) {
      return(TeamStats)
    } else {
      TeamStats[TeamStats$TeamName==team, ]
    }
  } else {
    TeamStats<-cbind(
      AwayTeamStats[,c(1:3, 21:22)],
      AwayTeamStats[,4:20]/AwayTeamStats[,23]) %>%
      mutate(FGP = FGM/FGA,
             FGP3 = FGM3/FGA3,
             FTP = FTM/FTA)
    if(is.null(team)) {
      return(TeamStats)
    } else {
      TeamStats[TeamStats$TeamName==team, ]
    }
  }
}




#Pull Win and Lose Team Stats


#Setup Stats to build Model
ModelSetup <- function(table, Stats) {
  Team1Stats<-Stats
  names(Team1Stats)[-c(1)]<-paste(names(Team1Stats)[-c(1)], "1", sep="")
  
  
  Team2Stats<-Stats
  names(Team2Stats)[-c(1)]<-paste(names(Team2Stats)[-c(1)], "2", sep="")
  
  left_join(
    left_join(
      table %>% dplyr::select(Season, HomeTeamName, AwayTeamName, HomeScore, AwayScore, game_id, DayNum),
      Team1Stats, by = c("HomeTeamName"= "TeamName1", "Season" = "Season")
    )
    ,
    Team2Stats, by = c("AwayTeamName"= "TeamName2", "Season" = "Season")
  )
}


####################
###Create Team for Model Function
####################

###Model1
Team1.1 <- function(ModelTable) {
  ModelTable %>% mutate(Score = HomeScore,
                        OffEffNorm = OffEffNorm1-OffEffNorm2,
                        DefEffNorm = DefEffNorm1-DefEffNorm2,
                        DRNorm = DRNorm1-DRNorm2,
                        TONorm = TONorm1-TONorm2,
                        FGPNorm = (FGPNorm1-FGPNorm2)*100,
                        OffPointsNorm = OffPointsNorm1-OffPointsNorm2,
                        DefPointsNorm = DefPointsNorm1-DefPointsNorm2) %>% 
    dplyr::select(Score, DRNorm, TONorm, FGPNorm, OffEffNorm, DefEffNorm, 
                  OffPointsNorm, DefPointsNorm)
}

Team2.1 <- function(ModelTable) {
  ModelTable %>% mutate(Score = AwayScore,
                        OffEffNorm = OffEffNorm2-OffEffNorm1,
                        DefEffNorm = DefEffNorm2-DefEffNorm1,
                        DRNorm = DRNorm2-DRNorm1,
                        TONorm = TONorm2-TONorm1,
                        FGPNorm = (FGPNorm2-FGPNorm1)*100,
                        OffPointsNorm = OffPointsNorm2-OffPointsNorm1,
                        DefPointsNorm = DefPointsNorm2-DefPointsNorm1) %>% 
    dplyr::select(Score, DRNorm, TONorm, FGPNorm, OffEffNorm,DefEffNorm, 
                  OffPointsNorm, DefPointsNorm)
}

###Model2
Team1.2 <- function(ModelTable) {
  Team1.2<-ModelTable %>% mutate(DRNorm = DRNorm1-DRNorm2,
                                 OffEffNorm = OffEffNorm1-OffEffNorm2,
                                 DefEffNorm = DefEffNorm1-DefEffNorm2,
                                 TONorm = TONorm1-TONorm2,
                                 FGPNorm = FGPNorm1-FGPNorm2,
                                 OppOffEffNorm = OppOffEffNorm1-OppOffEffNorm2,
                                 OppDefEffNorm = OppDefEffNorm1-OppDefEffNorm2,
                                 OffPointsNorm = OffPointsNorm1-OffPointsNorm2,
                                 DefPointsNorm = DefPointsNorm1-DefPointsNorm2,
                                 ConfScoreNorm = ConfScoreNorm1-ConfScoreNorm2, 
                                 Score = HomeScore) %>%
    dplyr::select(HomeTeamName, AwayTeamName, Score, OffEffNorm, DefEffNorm, DRNorm, TONorm, FGPNorm, OppOffEffNorm, OppDefEffNorm, OffPointsNorm, DefPointsNorm, ConfScoreNorm)
  return(Team1.2)
}


Team2.2 <- function(ModelTable) {
  Team2.2<-ModelTable %>% mutate(DRNorm = DR2-DR1,
                                 OffEffNorm = OffEffNorm2-OffEffNorm1,
                                 DefEffNorm = DefEffNorm2-DefEffNorm1,
                                 TONorm = TONorm2-TONorm1,
                                 FGPNorm = FGPNorm2-FGPNorm1,
                                 OppOffEffNorm = OppOffEffNorm2-OppOffEffNorm1,
                                 OppDefEffNorm = OppDefEffNorm2-OppDefEffNorm1,
                                 OffPointsNorm = OffPointsNorm2-OffPointsNorm1,
                                 DefPointsNorm = DefPointsNorm2-DefPointsNorm1,
                                 ConfScoreNorm = ConfScoreNorm2-ConfScoreNorm1, 
                                 Score = AwayScore) %>%
    dplyr::select(HomeTeamName, AwayTeamName, Score, OffEffNorm, DefEffNorm, DRNorm, TONorm, FGPNorm, OppOffEffNorm, OppDefEffNorm, OffPointsNorm, DefPointsNorm, ConfScoreNorm)
  return(Team2.2)
}


####################
###Creating Model Functions
####################

LinearModel1 <- function(normalizedata) {
  
  lm1 <- lm(Score~DRNorm+TONorm+FGPNorm+OffEffNorm+DefEffNorm+OffPointsNorm+DefPointsNorm, 
            data = normalizedata)
  return(lm1)
}

NormalizeData <- function(x) {
  normalized<-round((x-min(x))/(max(x)-min(x)), 4)
  return(normalized)
}

NormalizeTeamStates <- function(stats, startcolumn, endcolumn) {
  lst <- lapply(stats[-c(startcolumn:endcolumn)], function(x) round((x-min(x))/(max(x)-min(x)), 4))
  normalizeddata <- cbind(stats[c(startcolumn:endcolumn)], do.call(cbind.data.frame, Map(cbind , stats[-c(startcolumn:endcolumn)], lst)))
  names(normalizeddata)[-c(startcolumn:endcolumn)] <- rbind(names(stats)[-c(startcolumn:endcolumn)], paste(names(stats)[-c(startcolumn:endcolumn)], "Norm", sep=""))
  return(normalizeddata)
}

LinearModel2 <- function(normalizedata) {
  #Building Model2
  lm1 <- lm(Score~DRNorm+TONorm+FGPNorm+OffEffNorm+DefEffNorm+ConfScoreNorm+OffPointsNorm+DefPointsNorm+
              OppOffEffNorm+OppDefEffNorm, 
            data = normalizedata)
  return(lm1)
}

LinearModel3.1 <- function(normalizedata) {
  #Building Model2
  lm1 <- lm(TotalScore~DRNorm1+TONorm1+FGPNorm1+OffEffNorm1+DefEffNorm1+DRNorm2+TONorm2+FGPNorm2+OffEffNorm2+DefEffNorm2+
              +OffPointsNorm1+DefPointsNorm1++OffPointsNorm2+DefPointsNorm2, 
            data = normalizedata)
  return(lm1)
}


LinearModel3.2 <- function(normalizedata) {
  #Building Model2
  lm1 <- lm(TotalScore~DRNorm1+TONorm1+FGPNorm1+OffEffNorm1+DefEffNorm1+ConfScoreNorm1+
              OppOffEffNorm1+OppDefEffNorm1+DRNorm2+TONorm2+FGPNorm2+OffEffNorm2+DefEffNorm2+ConfScoreNorm2+
              OppOffEffNorm2+OppDefEffNorm2+OffPointsNorm1+DefPointsNorm1++OffPointsNorm2+DefPointsNorm2, 
            data = normalizedata)
  return(lm1)
}

Team_Correlation <- function(Comp, season, team, home_or_away, pred_or_corr) {
  SeasonStats<-Comp[Comp$Season==season,]
  testData<-SeasonStats[SeasonStats$HomeTeamName==team | SeasonStats$AwayTeamName == team,]
  
  homenormalizedata2<-Team1.2(testData)
  awaynormalizedata2<-Team2.2(testData)
  
  
  homePred <- predict(homelm2, homenormalizedata2)  # predict score
  awayPred <- predict(awaylm2, awaynormalizedata2)  # predict score
  
  
  if(home_or_away == "home") {
    actuals_preds <- data.frame(cbind(actuals=testData$HomeScore, predicteds=homePred))
    correlation_accuracy <- cor(actuals_preds)
    if(pred_or_corr == "pred") {
      return(actuals_preds)
    } else {
      return(correlation_accuracy)
    }
  } else {
    actuals_preds <- data.frame(cbind(actuals=testData$AwayScore, predicteds=awayPred))
    correlation_accuracy <- cor(actuals_preds)
    if(pred_or_corr == "pred") {
      return(actuals_preds)
    } else {
      return(correlation_accuracy)
    }
  }
}


build_correlation_accuracy <- function(Comp, season) {
  Correlation_Accuracy_df <- data.frame(Team=character(),
                                        Location=character(), 
                                        Correlation=integer()) 
  for(team in union(unique(Comp$HomeTeamName), unique(Comp$AwayTeamName))) {
    Correlation<-round(as.data.frame(Team_Correlation(Comp, season, team, "home", "corr"))$predicteds[1], 2)
    location<-"home"
    Correlation_Accuracy_df<-rbind(Correlation_Accuracy_df,
                                   as.data.frame(cbind(team, location, Correlation)))
    Correlation<-round(as.data.frame(Team_Correlation(Comp, season, team, "away", "corr"))$predicteds[1], 2)
    location<-"away"
    Correlation_Accuracy_df<-rbind(Correlation_Accuracy_df,
                                   as.data.frame(cbind(team, location, Correlation)))
    
  }
  return(Correlation_Accuracy_df)
}

####################
###Creating Matchup Functions
####################

#actual games played
BuildMatchups <- function(season, HomeTeam, AwayTeam, CompletedGames, Neutral) {
  if(CompletedGames == 'Y') {
    if(Neutral == "Y") {
      Result1<-Comp2 %>% filter(Season == season, HomeTeamName == HomeTeam, AwayTeamName == AwayTeam) %>% dplyr::select(HomeTeamName, Team1Score, HomeScore, AwayTeamName, Team2Score, AwayScore)
      Result2<-Comp2 %>% filter(Season == season, HomeTeamName == AwayTeam, AwayTeamName == HomeTeam) %>% dplyr::select(HomeTeamName, Team1Score, HomeScore, AwayTeamName, Team2Score, AwayScore)
      Output<-rbind(Result1,Result2)
      return(Output)
    }
    
    if(Neutral == "N") {
      Output<-Comp2 %>% filter(Season == season, HomeTeamName == HomeTeam, AwayTeamName == AwayTeam) %>% dplyr::select(HomeTeamName, Team1Score, HomeScore, AwayTeamName, Team2Score, AwayScore)
      return(Output)
    }

  }
  if(CompletedGames == 'N') {
    if(Neutral == "Y") {
      Result1<-Comp3 %>% filter(Season == season, HomeTeamName == HomeTeam, AwayTeamName == AwayTeam) %>% dplyr::select(HomeTeamName, Team1Score, AwayTeamName, Team2Score)
      Result2<-Comp3 %>% filter(Season == season, HomeTeamName == AwayTeam, AwayTeamName == HomeTeam) %>% dplyr::select(HomeTeamName, Team1Score, AwayTeamName, Team2Score)
      Output<-rbind(Result1,Result2) %>% mutate(Spread = Team1Score-Team2Score)
      return(Output)
    }
    
    if(Neutral == "N") {
      Output<-Comp3 %>% filter(Season == season, HomeTeamName == HomeTeam, AwayTeamName == AwayTeam) %>% dplyr::select(HomeTeamName, Team1Score, AwayTeamName, Team2Score)%>% mutate(Spread = Team1Score-Team2Score)
      return(Output)
    }
  }
} 



#Creating All Matchup for single season
SeasonMatchups<- function(season) {
  TeamStatsMatchup<-FinalTeamStats %>% 
    dplyr::filter(Season == season, !is.na(OffPoints))
  
  n <-nrow(TeamStatsMatchup)-1
  
  TeamNameMatchups<-expand.grid(rep(list(TeamStatsMatchup$TeamName), 2)) %>%
    mutate(HomeScore = 0,
           AwayScore = 0,
           Season = season) %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::rename(TeamName1 = Var1,
                  TeamName2 = Var2)
  
  return(TeamNameMatchups)
}


####################
###Pulling Stats from created matchups from matchup funcitons
####################
MatchupStats <- function(Matchups, season, Rankings, Stats) {

  FinalStats<-left_join(
    Rankings,
    Stats %>% 
      filter(Season==season) %>% 
      dplyr::select(TeamName, OffPointsNorm, DefPointsNorm, FGMNorm, FGANorm, FGPNorm, OffEffNorm, DefEffNorm, FTPNorm, FGM3Norm, FGP3Norm, FTMNorm, ORNorm, DRNorm, AstNorm, TONorm, StlNorm, BlkNorm),
    by = "TeamName"
  )  
  
  
  Team1<-left_join(
    Matchups,
    FinalStats,
    by = c("TeamName1" = "TeamName", "Season" = "Season")
  )
  
  names(Team1)[-c(1:5)]<-paste(names(Team1)[-c(1:5)], "1", sep="")
  
  Team2<-left_join(
    Matchups,
    FinalStats,
    by = c("TeamName2" = "TeamName", "Season" = "Season"))
  
  names(Team2)[-c(1:5)]<-paste(names(Team2)[-c(1:5)], "2", sep="")
  
  ErrorMatchup1.1<-left_join(
    Team1,
    Team2 %>% dplyr::select(-c(3:5)), by = c("TeamName1" = "TeamName1", "TeamName2" = "TeamName2")
  ) 
  
  
  
  ErrorMatchup1.1<-ErrorMatchup1.1 %>% mutate(AdjEM1 = OffEff1-DefEff1,
                             OppAdjEM1 = OppOffEff1-OppDefEff1,
                             AdjEM2 = OffEff2-DefEff2,
                             OppAdjEM2 = OppOffEff2-OppDefEff2) %>% dplyr::rename(HomeTeamName = TeamName1,
                                                                                  AwayTeamName = TeamName2)
  
  return(ErrorMatchup1.1)
}



CreateScheduleScoreModelStats <-function(ModelTable, Rankings) {  
  #Recalcuate Model with ScheduleScore
  Matchup<- cbind(left_join(
    ModelTable,
    Rankings,
    by = c("HomeTeamName" = "TeamName", "Season" = "Season")
  ) %>% mutate(AdjEM1 = OffEff1-DefEff1,
               OppAdjEM1 = OppOffEff-OppDefEff) %>%
    dplyr::rename(Rank1 = Rank,
                  OppOffEffNorm1 = OppOffEffNorm, 
                  OppDefEffNorm1 = OppDefEffNorm, 
                  ConfScoreNorm1 = ConfScoreNorm,
                  ScheduleScore1 = Team1Score,
                  ScheduleScore2 = Team2Score,
                  TotalModelScore1 = TotalModelScore,
                  OppOffEff1 = OppOffEff,
                  OppDefEff1 = OppDefEff,
                  ConfScore1 = ConfScore)
  ,
  left_join(
    ModelTable %>%
      dplyr::select(Season, HomeTeamName, AwayTeamName, OffEff2, DefEff1, OffEff1, DefEff2),
    Rankings,
    by = c("AwayTeamName" = "TeamName", "Season" = "Season") 
  ) %>% mutate(AdjEM2 = OffEff2-DefEff2,
               OppAdjEM2 = (OppOffEff-OppDefEff)) %>%
    dplyr::rename(Rank2 = Rank,
                  OppOffEffNorm2 = OppOffEffNorm, 
                  OppDefEffNorm2 = OppDefEffNorm, 
                  ConfScoreNorm2 = ConfScoreNorm,
                  OppOffEff2 = OppOffEff,
                  OppDefEff2 = OppDefEff,
                  ConfScore2 = ConfScore) %>%
    dplyr::select(Rank2, OppOffEff2, OppDefEff2, ConfScore2, AdjEM2, OppAdjEM2, OppOffEffNorm2, OppDefEffNorm2, ConfScoreNorm2))
  
  return(Matchup)
}

TeamSchedule <- function(season, team) {
  SeasonStats<-Comp2[c(13, 8, 1, 10, 9, 2, 11)][Comp2$Season == season,] %>% arrange(11)
  TeamStats<-SeasonStats[SeasonStats$HomeTeamName == team | SeasonStats$AwayTeamName == team,]
  TeamStats$Result <-rep(NA, nrow(TeamStats))
  for(i in 1:nrow(TeamStats)) {
    if(TeamStats$Team1Score[i]>TeamStats$Team2Score[i]) {
      if(TeamStats$HomeScore[i]>TeamStats$AwayScore[i]) {
        TeamStats$Result[i]<-"Correct"
      } else {
        TeamStats$Result[i]<-"InCorrect"
      }
    } else if(TeamStats$HomeScore[i]>TeamStats$AwayScore[i]) {
      TeamStats$Result[i]<-"InCorrect"
    } else {
      TeamStats$Result[i]<-"Correct"
    }
  }
  return(TeamStats)
}


####################
###Creating Model Result Functions
####################
ModelResults1 <- function(df, Matchup, model) {

  df1<-df[1:(length(df[ , 1])/2),]
  df2<-df[((length(df[ , 1])/2)+1):length(df[ , 1]), ]
  
  
  #Predict Score
  predict(model, df1)->result_regress_Team1
  predict(model, df2)->result_regress_Team2
  
  
  Final<- cbind(as.data.frame(cbind(Team1Score=result_regress_Team1,
                                    Team2Score=result_regress_Team2)), 
                Matchup)%>%
    mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
           Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"))
  
  return(Final)
}


ModelResults2 <- function(homedf, awaydf, Matchup, homemodel, awaymodel, totalscoremodel) {
  
  #Predict Score
  predict(homemodel, homedf)->result_regress_Team1
  predict(awaymodel, awaydf)->result_regress_Team2
  
  predict(totalscoremodel, Matchup)->result_regress_TotalScore
  
  final<-as.data.frame(cbind(
    cbind(Team1Score=result_regress_Team1,
          Team2Score=result_regress_Team2),
    TotalModelScore=result_regress_TotalScore
  ))
  
  Final<- cbind(final, Matchup) %>% 
    mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
           Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"))
  
  return(Final)
}

ModelResults3 <- function(homedf, awaydf, Matchup, homemodel, awaymodel, totalscoremodel) {
  
  #Predict Score
  predict(homemodel, homedf)->result_regress_Team1
  predict(awaymodel, awaydf)->result_regress_Team2
  
  predict(totalscoremodel, Matchup)->result_regress_TotalScore
  
  Team1Score <- (result_regress_TotalScore/2)+((result_regress_Team1-result_regress_Team2)/2)
  Team2Score <- (result_regress_TotalScore/2)-((result_regress_Team1-result_regress_Team2)/2)
  
  
  final<-as.data.frame(cbind(Team1Score,
          Team2Score,
    TotalModelScore=result_regress_TotalScore
  ))
  
  Final<- cbind(final, Matchup) %>% 
    mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
           Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"))
  
  return(Final)
}



####################
###CalculateError Functions
####################
CalculateError <- function(Final) {
  
  Comp1<- Final %>% mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
                           Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect")) %>%
    dplyr::select(Season, HomeTeamName, Team1Score, HomeScore, AwayTeamName, Team2Score, AwayScore, Result, Value)
  
  
  error1.1 <- Comp1 %>% mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
                               Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"),
                               Spread = abs(Team1Score-Team2Score),
                               SpreadBucket = case_when(Spread>10 ~"10+", 
                                                        Spread>9.5 ~"9.5-10", Spread>9 ~"9-9.5", 
                                                        Spread>8.5 ~"8.5-9", Spread>8 ~"8-8.5", 
                                                        Spread>7.5 ~"7.5-8", Spread>7 ~"7-7.5", 
                                                        Spread>6.5 ~"6.5-7", Spread>6 ~"6-6.5", 
                                                        Spread>5.5 ~"5.5-6", Spread>5 ~"5-5.5", 
                                                        Spread>4.5 ~"4.5-5", Spread>4 ~"4-4.5", 
                                                        Spread>3.5 ~"3.5-4", Spread>3 ~"3-3.5", 
                                                        Spread>2.5 ~"2.5-3", Spread>2 ~"2-2.5",
                                                        Spread>1.5 ~"1.5-2", Spread>1 ~"1-1.5", 
                                                        Spread>.5 ~".5-1", Spread>0 ~"0-.5")) %>%
    dplyr::select(Season, HomeTeamName, Team1Score, HomeScore, AwayTeamName, Team2Score, AwayScore, Spread, SpreadBucket, Result, Value)
  return(error1.1)
}

CalculateError2 <-function(Final) {
  
  colnames(Final) <- sub(".x", "1", colnames(Final))
  colnames(Final) <- sub(".y", "2", colnames(Final))
  
  Comp2.2<- Final %>% mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
                             Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"))
  
  error1.1 <- Comp2.2 %>% mutate(Value = case_when(Team1Score>Team2Score ~1, Team1Score<Team2Score ~0),
                                 Result = case_when(Team1Score>Team2Score ~"Correct", Team1Score<Team2Score ~"Incorrect"),
                                 Spread = abs(Team1Score-Team2Score),
                                 SpreadBucket = case_when(Spread>30 ~"30+", 
                                                          Spread>29.5 ~"29.5-30", Spread>30 ~"29-29.5",
                                                          Spread>28.5 ~"28.5-29", Spread>29 ~"28-28.5",
                                                          Spread>27.5 ~"27.5-28", Spread>28 ~"27-27.5",
                                                          Spread>26.5 ~"26.5-27", Spread>27 ~"26-26.5",
                                                          Spread>25.5 ~"25.5-26", Spread>26 ~"25-25.5",
                                                          Spread>24.5 ~"24.5-25", Spread>25 ~"24-24.5",
                                                          Spread>23.5 ~"23.5-24", Spread>24 ~"23-23.5",
                                                          Spread>22.5 ~"22.5-23", Spread>23 ~"22-22.5",
                                                          Spread>21.5 ~"21.5-22", Spread>22 ~"21-21.5",
                                                          Spread>20.5 ~"20.5-21", Spread>21 ~"20-20.5",
                                                          Spread>19.5 ~"19.5-20", Spread>19 ~"19-19.5",
                                                          Spread>18.5 ~"18.5-19", Spread>18 ~"18-18.5",
                                                          Spread>17.5 ~"17.5-18", Spread>17 ~"17-17.5",
                                                          Spread>16.5 ~"16.5-17", Spread>16 ~"16-16.5",
                                                          Spread>15.5 ~"15.5-16", Spread>15 ~"15-15.5",
                                                          Spread>14.5 ~"14.5-15", Spread>14 ~"14-14.5",
                                                          Spread>13.5 ~"13.5-14", Spread>13 ~"13-13.5",
                                                          Spread>12.5 ~"12.5-13", Spread>12 ~"12-12.5",
                                                          Spread>11.5 ~"11.5-12", Spread>11 ~"11-11.5",
                                                          Spread>10.5 ~"10.5-11", Spread>10 ~"10-10.5",
                                                          Spread>9.5 ~"9.5-10", Spread>9 ~"9-9.5", 
                                                          Spread>8.5 ~"8.5-9", Spread>8 ~"8-8.5", 
                                                          Spread>7.5 ~"7.5-8", Spread>7 ~"7-7.5", 
                                                          Spread>6.5 ~"6.5-7", Spread>6 ~"6-6.5", 
                                                          Spread>5.5 ~"5.5-6", Spread>5 ~"5-5.5", 
                                                          Spread>4.5 ~"4.5-5", Spread>4 ~"4-4.5", 
                                                          Spread>3.5 ~"3.5-4", Spread>3 ~"3-3.5", 
                                                          Spread>2.5 ~"2.5-3", Spread>2 ~"2-2.5",
                                                          Spread>1.5 ~"1.5-2", Spread>1 ~"1-1.5", 
                                                          Spread>.5 ~".5-1", Spread>0 ~"0-.5"))
  
  return(error1.1)
}


probability <- function(Comp) {
  probability<-spread((CalculateError2(Comp) %>%  group_by(SpreadBucket, Result) %>% count(Result)), key = "Result", value = "n") %>% 
    mutate(Probability = Correct/(Correct+Incorrect))
  
  probability$Incorrect[is.na(probability$Incorrect)] <- 0
  probability$Probability[is.na(probability$Probability)] <- 1
  
  return(probability)
}



####################
###Ranking Functions
####################
TeamRankings <- function(Comp) {
  Rankings<-as.data.frame(left_join(
    rbind(
      Comp %>% 
        dplyr::select(Season, HomeTeamName, Team1Score, AwayTeamName, win1, lost1, ConfAbbrev1, OffEff1, DefEff1, OffEff2, DefEff2, OffPoints1, DefPoints1, FGP1, DR1, TO1) %>%
        dplyr::rename(TeamName = HomeTeamName, ScheduleScore = Team1Score, win=win1, lost=lost1, ConfAbbrev = ConfAbbrev1, 
                      Opponent = AwayTeamName, OffEff = OffEff1, DefEff = DefEff1, OppOffEff = OffEff2, OppDefEff = DefEff2, OffPoints = OffPoints1, DefPoints = DefPoints1, FGP = FGP1, DR = DR1, TO = TO1),
      Comp %>% 
        dplyr::select(Season, AwayTeamName, Team2Score, HomeTeamName, win2, lost2, ConfAbbrev2, OffEff2, DefEff2, OffEff1, DefEff1, OffPoints2, DefPoints2, FGP2, DR2, TO2) %>%
        dplyr::rename(TeamName = AwayTeamName, ScheduleScore = Team2Score, win =win2, lost=lost2, ConfAbbrev = ConfAbbrev2, 
                      Opponent = HomeTeamName, OffEff=OffEff2, DefEff = DefEff2, OppOffEff = OffEff1, OppDefEff = DefEff1, OffPoints = OffPoints2, DefPoints = DefPoints2, FGP = FGP2, DR = DR2, TO = TO2)) %>%
      group_by(Season, TeamName, win, lost, ConfAbbrev, OffEff, DefEff, OffPoints, DefPoints, FGP, DR, TO) %>% 
      summarise(ScheduleScore = sum(ScheduleScore)/length(TeamName),
                OppOffEff = sum(OppOffEff)/length(TeamName),
                OppDefEff = sum(OppDefEff)/length(TeamName)) %>%
      dplyr::select(Season, TeamName, ScheduleScore, win, lost, ConfAbbrev, OffEff, DefEff, OppOffEff, OppDefEff, OffPoints, DefPoints, FGP, DR, TO)
    ,
    rbind(
      Comp %>% filter(ConfAbbrev1 != ConfAbbrev2) %>%
        dplyr::select(Season, ConfAbbrev1, Team1Score) %>%
        dplyr::rename(ConfAbbrev = ConfAbbrev1, ConfScore = Team1Score),
      Comp %>% filter(ConfAbbrev1 != ConfAbbrev2) %>%
        dplyr::select(Season, ConfAbbrev2, Team2Score) %>%
        dplyr::rename(ConfAbbrev = ConfAbbrev2, ConfScore = Team2Score)) %>%
      group_by(Season, ConfAbbrev) %>% summarise(ConfScore = sum(ConfScore)/length(ConfAbbrev)) %>%
      dplyr::select(Season, ConfAbbrev, ConfScore), by = c('Season', 'ConfAbbrev'))) %>%
    group_by(Season) %>%
    mutate(Rank = order(order(ScheduleScore, decreasing=TRUE))) %>%
    arrange(desc(ScheduleScore))
  
  Rankings$OppOffEffNorm<-NormalizeData(Rankings$OppOffEff)
  Rankings$OppDefEffNorm<-NormalizeData(Rankings$OppDefEff)
  Rankings$ConfScoreNorm<-NormalizeData(Rankings$ConfScore)
  
  return(Rankings)
}


TournmamentBracket <- function(season) {
  Season <-season
  Seed.x <-0
  TeamName1 <-'TeamName1'
  Team1Score <-0
  Team1Score <-0
  Score1 <-0
  Seed.y <-0
  TeamName2 <-'TeamName1'
  Team2Score <-0
  Score2 <-0
  
  TournamentMatchups <- data.frame(Season, Seed.x, TeamName1, Team1Score, Team1Score, Score1, Seed.y, TeamName2, Team2Score, Score2)
  
  Results<-NCAATourneyDetailedResults[NCAATourneyDetailedResults$Season==season,][c(1, 3:6)]
  Seeds<-NCAATourneySeeds[NCAATourneySeeds$Season==season,][c(3, 2)]
  
  for (team in Results$WTeamID) {
    Results$TeamName1[Results$WTeamID==team]<-Teams[Teams$TeamID==team,]$TeamName
    Results$Seed1[Results$WTeamID==team]<-Seeds[Seeds$TeamID==team,]$Seed
    }
  for (team in Results$LTeamID) {
    Results$TeamName2[Results$LTeamID==team]<-Teams[Teams$TeamID==team,]$TeamName
    Results$Seed2[Results$LTeamID==team]<-Seeds[Seeds$TeamID==team,]$Seed
  }
  
  NCAATourneyProjectedResults<-Results
    
  
  df<-rbind(
    left_join(
      NCAATourneyProjectedResults[2:7]
      ,
      Comp3, by = c('TeamName1' = 'HomeTeamName', 'TeamName2' = 'AwayTeamName')
    ) %>%
      dplyr::select(Season, Seed.x, TeamName1, Team1Score, Score1, Seed.y, TeamName2, Team2Score, Score2)
    ,
    left_join(
      NCAATourneyProjectedResults[2:7]
      ,
      Comp3, by = c('TeamName1' = 'AwayTeamName', 'TeamName2' = 'HomeTeamName')
    ) %>% dplyr::rename(Team1Score = Team2Score, Team2Score = Team1Score) %>%
      dplyr::select(Season, Seed.x, TeamName1, Team1Score, Team1Score, Score1, Seed.y, TeamName2, Team2Score, Score2)
  )
  
  TournamentMatchups<-aggregate(df[,c(4, 8)]/2, df[,-c(4, 8)], FUN=sum)
  
  return(TournamentMatchups)
}


TournamentProjections <- function(season) {
  FinalTournamenProjections<-
    left_join(
      CalculateError2(TournmamentBracket(season)),
      probability(Comp2) %>% dplyr::select(SpreadBucket, Probability),
      by = c("SpreadBucket" = "SpreadBucket")) %>%
    dplyr::select(Seed1, TeamName1, Score1, Seed2, TeamName2, Score2, Result, Probability)
  return(FinalTournamenProjections)
}



GetFinalRankings <- function(season) {
  
  Results<-NCAATourneyDetailedResults[NCAATourneyDetailedResults$Season==season,][c(1, 3:6)]
  ModelStats3<-MatchupStats(SeasonMatchups(season), season, TeamRankings2, FinalTeamStats)
  homenormalizedata3<-Team1.2(ModelStats3)
  awaynormalizedata3<-Team2.2(ModelStats3)
  Comp3<-ModelResults2(homenormalizedata3, awaynormalizedata3, ModelStats3, homelm2, awaylm2, TotalScorelm2)
  TeamRankings3<-TeamRankings(Comp3) 
  OppStrength<-TeamRankings2[TeamRankings2$Season==season,][c(2,9,10)]

  for (i in 1:nrow(TeamRankings3)) {
    TeamRankings3$OppOffEff[i]<-OppStrength$OppOffEff[TeamRankings3$TeamName[i]==OppStrength$TeamName]
    TeamRankings3$OppDefEff[i]<-OppStrength$OppDefEff[TeamRankings3$TeamName[i]==OppStrength$TeamName]
  }

  if(nrow(Results)>0) {
    bracket<-fillactualbracket(season)
    Team1Games<-count(bracket[1:63,][6], bracket[1:63,][6], name = "Games")
    Team2Games<-count(bracket[1:63,][8], bracket[1:63,][8], name = "Games")
    Champion<-count(bracket[63,][14], bracket[63,][14], name = "Games")
    names(Team1Games)[1]<-"TeamName"
    names(Team2Games)[1]<-"TeamName"
    names(Champion)[1]<-"TeamName"
    games<-rbind(Team1Games, Team2Games, Champion)
    TournamentWins<-aggregate(games[2], games[1], FUN = sum)
    Team2Games<-count(bracket[1:63,][14], bracket[1:63,][8], name = "Games")

    FinalRankings<-
      left_join(
        TeamRankings3,
        TournamentWins, by = "TeamName") %>%
      mutate(TournamentResult = ifelse(Games == 7, "Champion", 
                                       ifelse(Games == 6, "Runner-Up",
                                              ifelse(Games == 5, "Final 4",
                                                     ifelse(Games == 4, "Elite 8",
                                                            ifelse(Games == 3, "Sweet 16",
                                                                   ifelse(Games == 2, "Round of 32",
                                                                          ifelse(Games == 1, "Round of 64", "Not In"))))))))
    
    names(FinalRankings)[3:6]<-c("Score", "Wins", "Loses", "Conference")
    FinalRankings2<-FinalRankings[-c(18:20)]
    FinalRankings[c(3, 7:16)]<-round(FinalRankings[c(3, 7:16)], 2)
    
    
  } else {
    names(TeamRankings3)[3:6]<-c("Score", "Wins", "Loses", "Conference")
    TeamRankings3<-TeamRankings3[-c(18:20)]
    TeamRankings3[c(3, 7:16)]<-round(TeamRankings3[c(3, 7:16)], 2)
    FinalRankings<-TeamRankings3
    }
  return(FinalRankings)
}


listofseasons <- function() {
  seasons<-unique(Comp1[4])

  for(i in 1:nrow(seasons)) {
    seasons$website[i]<-paste("[", seasons$Season[i], "](https://maprankings.000webhostapp.com/", seasons$Season[i], "Rankings) |", sep = "")
  }

  seasons$website<-gsub('"', '', seasons$website)
  return(seasons$website)
}

updatepastrankings <- function() {
  
  seasons<-unique(Comp1[3])
  for (i in 1:nrow(seasons)) {
    print(i)
    print(seasons$Season[i])
    ModelStats3<-MatchupStats(SeasonMatchups(seasons$Season[i]), seasons$Season[i], TeamRankings2, FinalTeamStats)
    normalizedata3<-rbind(Team1.2(ModelStats3), Team2.2(ModelStats3))
    Comp3<-ModelResults1(normalizedata3, ModelStats3, lm2)
    TeamRankings3<-TeamRankings(Comp3)
    Rankings<-GetFinalRankings(seasons$Season[i])
    if(length(Rankings)>15) {
      RankingsTable<-kable(Rankings[c(15, 2:14, 16:17)], format = "markdown")
      filename<-paste(seasons$Season[i], "Rankings.Rmd", sep = "")
      htmlformat<-paste("---\ntitle: ", seasons$Season[i], "MapRankings\noutput: html_document\n---\n\n", sep = "")
      link<-listofseasons()
      gap<-"\n---"
      cat(c(htmlformat,link, gap, RankingsTable),
          sep="\n",file=filename)
      render(filename)
    } else {
      RankingsTable<-kable(Rankings[c(15, 2:14)], format = "markdown")
      filename<-paste(seasons$Season[i], "Rankings.Rmd", sep = "")
      htmlformat<-paste("---\ntitle: ", seasons$Season[i], "MapRankings\noutput: html_document\ndate: '`r Sys.time()`'\n---\n\n")
      link<-listofseasons()
      gap<-"\n---"
      cat(c(htmlformat,link, gap, RankingsTable),sep="\n",file=filename)
      render(filename)
    }
    
  }
}

createhtmlrankings <- function(season) {
  Rankings<-GetFinalRankings(season)
  if(length(Rankings)>17) {
    RankingsTable<-kable(Rankings[c(17, 2:16, 21:22)], format = "markdown")
    filename<-paste(season, "Rankings.Rmd", sep = "")
    htmlformat<-paste("---\ntitle: ", season, " MapRankings\noutput: html_document\n---\n\n", sep = "")
    link<-listofseasons()
    gap<-"\n---"
    Teamfilter<-"<input type='text' id='TeamInput' onkeyup='TeamFunction()' placeholder='Search for TeamName...' title='Type in a Team'>"
    Conferencefilter<-"<input type='text' id='ConferenceInput' onkeyup='ConferenceFunction()' placeholder='Search for Conference...' title='Type in a Conference'>\n"
    filterTeamfunction <-"<script>
  function TeamFunction() {
    var input, filter, table, tr, td, i, txtValue;
    input = document.getElementById('TeamInput');
    filter = input.value.toUpperCase();
    table = document.getElementsByClassName('table table-condensed')[0];
    tr = table.getElementsByTagName('tr');
    for (i = 0; i < tr.length; i++) {
      td = tr[i].getElementsByTagName('td')[1];
      if (td) {
        txtValue = td.textContent || td.innerText;
        if (txtValue.toUpperCase().indexOf(filter) > -1) {
          tr[i].style.display = '';
        } else {
          tr[i].style.display = 'none';
        }
      }       
    }
  }
  </script>\n\n"
    filterConferencefunction<-"<script>
  function ConferenceFunction() {
    var input, filter, table, tr, td, i, txtValue;
    input = document.getElementById('ConferenceInput');
    filter = input.value.toUpperCase();
    table = document.getElementsByClassName('table table-condensed')[0];
    tr = table.getElementsByTagName('tr');
    for (i = 0; i < tr.length; i++) {
      td = tr[i].getElementsByTagName('td')[5];
      if (td) {
        txtValue = td.textContent || td.innerText;
        if (txtValue.toUpperCase().indexOf(filter) > -1) {
          tr[i].style.display = '';
        } else {
          tr[i].style.display = 'none';
        }
      }       
    }
  }
  </script>\n\n"
    cat(c(htmlformat,link, gap, Teamfilter, Conferencefilter, RankingsTable, filterTeamfunction, filterConferencefunction),
        sep="\n",file=filename)
    render(filename)
  } else {
    RankingsTable<-kable(Rankings[c(17, 2:16)], format = "markdown")
    filename<-paste(season, "Rankings.Rmd", sep = "")
    htmlformat<-paste("---\ntitle: ", season, " MapRankings\noutput: html_document\ndate: '`r Sys.time()`'\n---\n\n")
    link<-listofseasons()
    link2<-"[Today's Matchups](https://maprankings.000webhostapp.com/Matchups) | 
  [Detailed Matchups](https://maprankings.000webhostapp.com/DetailedMatchups)"
    gap<-"\n---\n"
    Teamfilter<-"<input type='text' id='TeamInput' onkeyup='TeamFunction()' placeholder='Search for TeamName...' title='Type in a Team'>"
    Conferencefilter<-"<input type='text' id='ConferenceInput' onkeyup='ConferenceFunction()' placeholder='Search for Conference...' title='Type in a Conference'>\n"
    filterTeamfunction <-"<script>
  function TeamFunction() {
    var input, filter, table, tr, td, i, txtValue;
    input = document.getElementById('TeamInput');
    filter = input.value.toUpperCase();
    table = document.getElementsByClassName('table table-condensed')[0];
    tr = table.getElementsByTagName('tr');
    for (i = 0; i < tr.length; i++) {
      td = tr[i].getElementsByTagName('td')[1];
      if (td) {
        txtValue = td.textContent || td.innerText;
        if (txtValue.toUpperCase().indexOf(filter) > -1) {
          tr[i].style.display = '';
        } else {
          tr[i].style.display = 'none';
        }
      }       
    }
  }
  </script>\n\n"
    filterConferencefunction<-"<script>
  function ConferenceFunction() {
    var input, filter, table, tr, td, i, txtValue;
    input = document.getElementById('ConferenceInput');
    filter = input.value.toUpperCase();
    table = document.getElementsByClassName('table table-condensed')[0];
    tr = table.getElementsByTagName('tr');
    for (i = 0; i < tr.length; i++) {
      td = tr[i].getElementsByTagName('td')[5];
      if (td) {
        txtValue = td.textContent || td.innerText;
        if (txtValue.toUpperCase().indexOf(filter) > -1) {
          tr[i].style.display = '';
        } else {
          tr[i].style.display = 'none';
        }
      }       
    }
  }
  </script>\n\n"
    cat(c(htmlformat, link, gap, link2, gap, Teamfilter, Conferencefilter, RankingsTable, filterTeamfunction, filterConferencefunction),sep="\n",file=filename)
    render(filename)
  }
}

TeamScheduleHTML <- function(season) {
  SeasonStats<-Comp2[c(11, 6, 1, 8, 7, 2, 9)][Comp2$Season == season,] %>% arrange(11)
  SeasonStats$Result <-rep(NA, nrow(SeasonStats))
  for(i in 1:nrow(SeasonStats)) {
    if(SeasonStats$Team1Score[i]>SeasonStats$Team2Score[i]) {
      if(SeasonStats$HomeScore[i]>SeasonStats$AwayScore[i]) {
        SeasonStats$Result[i]<-"Correct"
      } else {
        SeasonStats$Result[i]<-"InCorrect"
      }
    } else if(SeasonStats$HomeScore[i]>SeasonStats$AwayScore[i]) {
      SeasonStats$Result[i]<-"InCorrect"
    } else {
      SeasonStats$Result[i]<-"Correct"
    }
  }
  SeasonTable<-kable(SeasonStats, format = "markdown")
  filename<-paste(season, "SeasonTable.Rmd", sep = "")
  htmlformat<-paste("---\ntitle: ", season, " TeamScheduleResults\noutput: html_document\n---\n\n", sep = "")
  gap<-"\n---"
  link<-"[home](https://maprankings.000webhostapp.com) | [Today's Matchups](https://maprankings.000webhostapp.com/Matchups) | 
  [Detailed Matchups](https://maprankings.000webhostapp.com/DetailedMatchups) | [Past 5 Days](https://maprankings.000webhostapp.com/Past5Days)"
  Teamfilter<-"<input type='text' id='TeamInput' onkeyup='TeamFunction()' placeholder='Enter Full TeamName...' title='Type in a Full Team Name'>\n"
  filterTeamfunction <-"<script>
  function TeamFunction() {
    var input, filter, table, tr, td1, td2, i, txtValue1;
    input = document.getElementById('TeamInput');
    filter = input.value.toUpperCase();
    table = document.getElementsByClassName('table table-condensed')[0];
    tr = table.getElementsByTagName('tr');
    for (i = 0; i < tr.length; i++) {
      td1 = tr[i].getElementsByTagName('td')[1];
      td2 = tr[i].getElementsByTagName('td')[4];
      if (td1) {
        txtValue1 = td1.textContent;
        txtValue2 = td2.textContent;
        if (txtValue1.toUpperCase() === filter || txtValue2.toUpperCase() === filter ) {
          tr[i].style.display = '';
        } else {
          tr[i].style.display = 'none';
        }
      }       
    }
  }
  </script>\n\n"
  cat(c(htmlformat, link, gap, Teamfilter, SeasonTable, filterTeamfunction),
      sep="\n",file=filename)
  render(filename)
}

season<-2021
YYYYMMDD<-20210222

GetTodayMatchupsv2<- function(season, YYYYMMDD){
  file<-paste("./Basketball/NCAA_Spread_", YYYYMMDD, ".csv", sep = "")
  TeamAbbrev<-read.csv("./Basketball/NCAA_Spread_team_abbrev.csv")
  TodayMatchups<-read.csv(file)
  TodayMatchups$Away<-gsub('é', 'e', TodayMatchups$Away)
  TodayMatchups$Home<-gsub('é', 'e', TodayMatchups$Home)
  TodayMatchups$Matchup<-paste(tolower(TodayMatchups$Away) , "-", tolower(TodayMatchups$Home))
  
  ModelStats3<-MatchupStats(SeasonMatchups(season), season, TeamRankings2, FinalTeamStats)
  homenormalizedata3<-Team1.2(ModelStats3)
  awaynormalizedata3<-Team2.2(ModelStats3)
  Comp3<-ModelResults3(homenormalizedata3, awaynormalizedata3, ModelStats3, homelm2, awaylm2, TotalScorelm2)
  Comp3$Matchup<-paste(tolower(Comp3$AwayTeamName), "-", tolower(Comp3$HomeTeamName))
  
  TodayMatchups$HomeModelScore<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$HomeRecord<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$AwayModelScore<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$AwayRecord<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelOverUnder<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$HomeModelCorrelation<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$AwayModelCorrelation<-rep(NA, nrow(TodayMatchups))
  
  for (i in 1:nrow(TodayMatchups)) {
    try({
      TodayMatchups$HomeModelScore[i]<-round(Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$Team1Score, 1)
      TodayMatchups$AwayModelScore[i]<-round(Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$Team2Score, 1)
      TodayMatchups$ModelOverUnder[i]<-round(Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$TotalModelScore, 1)
      TodayMatchups$HomeRecord[i]<-paste(Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$win1, Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$lost1, sep = "-")
      TodayMatchups$AwayRecord[i]<-paste(Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$win2, Comp3[TodayMatchups$Matchup[i]==Comp3$Matchup,]$lost2, sep = "-") 
      TodayMatchups$HomeModelCorrelation[i]<-Correlation_Accuracy_df[TodayMatchups$Home[i]==Correlation_Accuracy_df$team, ]$home
      TodayMatchups$AwayModelCorrelation[i]<-Correlation_Accuracy_df[TodayMatchups$Away[i]==Correlation_Accuracy_df$team, ]$away
    })
  }
  
  TodayMatchups<-TodayMatchups[complete.cases(TodayMatchups), ][-5]
  
  TodayMatchups$VegasWinner<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$VegasLoser<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$VegasSpread<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelWinner<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelLoser<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelSpread<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelPick<-rep(NA, nrow(TodayMatchups))
  TodayMatchups$ModelGap<-rep(NA, nrow(TodayMatchups))
  
  
  for (i in 1:nrow(TodayMatchups)) {
    if(is.na(unlist(strsplit(TodayMatchups$VegasLine, " ")[i])[1])) {
      TodayMatchups$VegasWinner[i]<-unlist(strsplit(TodayMatchups$VegasLine, " ")[i])[1]
      TodayMatchups$VegasSpread[i]<-as.numeric(unlist(strsplit(TodayMatchups$VegasLine, " ")[i])[2]) 
    } else {
      try({
        TodayMatchups$VegasWinner[i]<-unlist(strsplit(TodayMatchups$VegasLine, " ")[i])[1]
        if(TodayMatchups$VegasWinner[i]=="EVEN") {
          TodayMatchups$VegasWinner[i]<-TodayMatchups$HomeTeamName[i]
          TodayMatchups$VegasLoser[i]<-TodayMatchups$AwayTeamName[i]
          TodayMatchups$VegasSpread[i]<-0
        } else {
          TodayMatchups$VegasSpread[i]<-as.numeric(unlist(strsplit(TodayMatchups$VegasLine, " ")[i])[2]) 
          TodayMatchups$VegasWinner[i]<-TeamAbbrev[TeamAbbrev$abbrev==TodayMatchups$VegasWinner[i],]$teamname
          if(TodayMatchups$VegasWinner[i]==TodayMatchups$Home[i]) {
            TodayMatchups$VegasLoser[i]<-TodayMatchups$Away[i]
          } else {
            TodayMatchups$VegasLoser[i]<-TodayMatchups$Home[i]
          }
        }
        
      })
    }
  }
  
  TodayMatchups<-TodayMatchups[complete.cases(TodayMatchups[1:13]), ]
  
  
  for (i in 1:nrow(TodayMatchups)) {
    if(TodayMatchups$HomeModelScore[i]>TodayMatchups$AwayModelScore[i]) {
      TodayMatchups$ModelWinner[i]<-TodayMatchups$Home[i]
      TodayMatchups$ModelLoser[i]<-TodayMatchups$Away[i]
      if(TodayMatchups$ModelWinner[i]==TodayMatchups$VegasWinner[i]) {
        TodayMatchups$ModelSpread[i]<-TodayMatchups$AwayModelScore[i]-TodayMatchups$HomeModelScore[i]
        TodayMatchups$ModelGap[i]<-round(abs(TodayMatchups$VegasSpread[i]-TodayMatchups$ModelSpread[i]), 0)
      } else {
        TodayMatchups$ModelSpread[i]<-TodayMatchups$HomeModelScore[i]-TodayMatchups$AwayModelScore[i]
        TodayMatchups$ModelGap[i]<-round(abs(TodayMatchups$VegasSpread[i]-TodayMatchups$ModelSpread[i]), 0)
      }
    } else {
      TodayMatchups$ModelWinner[i]<-TodayMatchups$Away[i]
      TodayMatchups$ModelLoser[i]<-TodayMatchups$Home[i]
      if(TodayMatchups$ModelWinner[i]==TodayMatchups$VegasWinner[i]) {
        TodayMatchups$ModelSpread[i]<-TodayMatchups$HomeModelScore[i]-TodayMatchups$AwayModelScore[i]
        TodayMatchups$ModelGap[i]<-round(abs(TodayMatchups$VegasSpread[i]-TodayMatchups$ModelSpread[i]), 0)
      } else {
        TodayMatchups$ModelSpread[i]<-TodayMatchups$AwayModelScore[i]-TodayMatchups$HomeModelScore[i]
        TodayMatchups$ModelGap[i]<-round(abs(TodayMatchups$VegasSpread[i]-TodayMatchups$ModelSpread[i]), 0)
      }
    }
  }
  
  
  for (i in 1:nrow(TodayMatchups)) {
    try({
      if(TodayMatchups$ModelWinner[i]==TodayMatchups$VegasWinner[i] && TodayMatchups$ModelSpread[i]<TodayMatchups$VegasSpread[i]) {
        TodayMatchups$ModelPick[i]<-TodayMatchups$ModelWinner[i]
      } else if(TodayMatchups$ModelWinner[i]!=TodayMatchups$VegasWinner[i]) {
        TodayMatchups$ModelPick[i]<-TodayMatchups$ModelWinner[i] 
      } else {TodayMatchups$ModelPick[i]<-TodayMatchups$ModelLoser[i]
      }
    })
  }
  TodayMatchups$Home<-paste(TodayMatchups$Home, TodayMatchups$HomeRecord, sep = " ")
  TodayMatchups$Away<-paste(TodayMatchups$Away, TodayMatchups$AwayRecord, sep = " ") 
  TodayMatchups<-TodayMatchups[c(2, 12, 3, 11, 4, 10, 19, 18, 20)] %>% arrange(desc(ModelGap))
  #TodayMatchups<-TodayMatchups[c(2:4, 17, 16, 18)] %>% arrange(desc(ModelGap))
  Matchups<-kable(TodayMatchups, format = "markdown")
  filename<-"Matchups.Rmd"
  note<-"\n***Games in green have teams with above average predicted score correlation vs actuals"
  formateddate<-format(Sys.time(), '%B %d, %Y')
  htmlformat<-paste("---\ntitle: ", formateddate, " Matchups\noutput: html_document\n---\n\n", sep = "")
  link<-"[home](https://maprankings.000webhostapp.com) | [Detailed Matchups](https://maprankings.000webhostapp.com/DetailedMatchups) | 
  [Past 14 Days](https://maprankings.000webhostapp.com/Past14Days)"
  rowstyle<-"<style>.selected{
  background-color:green;
}

</style>\n\n"
  rowformat<-"<script>
  $(function(){
  $('tr').each(function(){
    var col_val1 = $(this).find('td:eq(1)').text();
    var col_val2 = $(this).find('td:eq(3)').text();
    if (col_val1 >= .5 && col_val2 >= .5){
      $(this).addClass('selected');  //the selected class colors the row green//
    }
  });
});
  </script>\n\n"
  gap<-"\n---\n"
  cat(c(htmlformat,link, gap, note, gap, Matchups, rowstyle, rowformat),
      sep="\n",file=filename)
  render(filename)
}


######################
#Pull Historical Predictions
######################

GetModelGapAnalysis <- function(day, season) {
  PastTeamStatsData <- GetTeamStats(day, season)
  
  PastTeamStatsData[rowSums(is.na(PastTeamStatsData))!=0,]
  
  PastTeamStatsData<-PastTeamStatsData[complete.cases(PastTeamStatsData), ]
  
  PastFinalTeamStats<-NormalizeTeamStates(PastTeamStatsData, 1, 5)
  
  PastModelStats1<-ModelSetup(ActualGameResults, PastFinalTeamStats)
  PastModelStats1$TotalScore<-PastModelStats1$HomeScore+PastModelStats1$AwayScore
  PastModelStats1<-PastModelStats1[complete.cases(PastModelStats1), ]
  
  pasthomenormalizedata1<-Team1.1(PastModelStats1)
  pastawaynormalizedata1<-Team2.1(PastModelStats1)
  pasthomelm1<-LinearModel1(pasthomenormalizedata1)
  pastawaylm1<-LinearModel1(pastawaynormalizedata1)
  pastTotalScorelm1<-LinearModel3.1(PastModelStats1)
  
  PastComp1<-ModelResults2(pasthomenormalizedata1, pastawaynormalizedata1, PastModelStats1, pasthomelm1, pastawaylm1, pastTotalScorelm1)
  PastTeamRankings1<-TeamRankings(PastComp1)
  
  PastModelStats2<-CreateScheduleScoreModelStats(PastComp1, PastTeamRankings1) 
  PastModelStats2$TotalScore<-PastModelStats2$HomeScore+PastModelStats2$AwayScore
  
  pasthomenormalizedata2<-Team1.2(PastModelStats2)
  pastawaynormalizedata2<-Team2.2(PastModelStats2)
  pasthomelm2<-LinearModel2(pasthomenormalizedata2)
  pastawaylm2<-LinearModel2(pastawaynormalizedata2)
  pastTotalScorelm2<-LinearModel3.2(PastModelStats2)
  
  PastComp2<-ModelResults3(pasthomenormalizedata2, pastawaynormalizedata2, PastModelStats2, pasthomelm2, pastawaylm2, pastTotalScorelm2)

  PastCorrelation_Accuracy_df_location<-build_correlation_accuracy(PastComp2, 2021)
  PastCorrelation_Accuracy_df<-spread(Correlation_Accuracy_df_location, key = 'location', value = 'Correlation')
  
  
  HistoricalSpreads<-read.csv("./Basketball/NCAA_Spread_historical.csv")
  TeamAbbrev<-read.csv("./Basketball/NCAA_Spread_team_abbrev.csv")
  HistoricalSpreads$game_id<-substr(HistoricalSpreads$game_id, 58, 66)
  
  DayResults<-PastComp2[PastComp2$Season==season,]
  DayResults$game_id<-substr(DayResults$game_id, 62, 70)
  DayResults<-DayResults[DayResults$DayNum==day,][c(8, 10, 1, 9, 11, 2, 3, 12, 13)]
  DayResults$VegasLine<-rep(NA, nrow(DayResults))
  DayResults$HomeModelCorrelation<-rep(NA, nrow(DayResults))
  DayResults$AwayModelCorrelation<-rep(NA, nrow(DayResults))
  
  
  
  names(DayResults)[c(3,6)]<-c("HomeModelScore", "AwayModelScore")
  
  DayResults$HomeModelScore<-round(DayResults$HomeModelScore, 2)
  DayResults$AwayModelScore<-round(DayResults$AwayModelScore, 2)
  DayResults$TotalModelScore<-round(DayResults$TotalModelScore, 2)
  
  i<-1
  for (i in 1:nrow(DayResults)) {
    try({
      DayResults[i,]$VegasLine<-HistoricalSpreads[HistoricalSpreads$game_id==DayResults$game_id[i],]$spread
      DayResults[i,]$HomeModelCorrelation<-PastCorrelation_Accuracy_df[PastCorrelation_Accuracy_df$team==DayResults$HomeTeamName[i], ]$home
      DayResults[i,]$AwayModelCorrelation<-PastCorrelation_Accuracy_df[PastCorrelation_Accuracy_df$team==DayResults$AwayTeamName[i], ]$away
      })
  }
  
  DayResults<-DayResults[complete.cases(DayResults), ]
  
  DayResults$VegasWinner<-rep(NA, nrow(DayResults))
  DayResults$VegasLoser<-rep(NA, nrow(DayResults))
  DayResults$VegasSpread<-rep(NA, nrow(DayResults))
  DayResults$ModelWinner<-rep(NA, nrow(DayResults))
  DayResults$ModelLoser<-rep(NA, nrow(DayResults))
  DayResults$ModelSpread<-rep(NA, nrow(DayResults))
  DayResults$ModelPick<-rep(NA, nrow(DayResults))
  DayResults$ModelGap<-rep(NA, nrow(DayResults))
  DayResults$ActualWinner<-rep(NA, nrow(DayResults))
  DayResults$ActualSpread<-rep(NA, nrow(DayResults))
  DayResults$Result<-rep(NA, nrow(DayResults))
  
  
  for (i in 1:nrow(DayResults)) {
    try({
      DayResults$VegasWinner[i]<-unlist(strsplit(DayResults$VegasLine, " ")[i])[2]
      if(DayResults$VegasWinner[i]=="EVEN") {
        DayResults$VegasWinner[i]<-DayResults$HomeTeamName[i]
        DayResults$VegasLoser[i]<-DayResults$AwayTeamName[i]
        DayResults$VegasSpread[i]<-0
      } else {
        DayResults$VegasSpread[i]<-as.numeric(unlist(strsplit(DayResults$VegasLine, " ")[i])[3]) 
        DayResults$VegasWinner[i]<-TeamAbbrev[TeamAbbrev$abbrev==DayResults$VegasWinner[i],]$teamname
        if(DayResults$VegasWinner[i]==DayResults$HomeTeamName[i]) {
          DayResults$VegasLoser[i]<-DayResults$AwayTeamName[i]
        } else {
          DayResults$VegasLoser[i]<-DayResults$HomeTeamName[i]
        }
      }
      
    })
    
  }

  
  for (i in 1:nrow(DayResults)) {
    if(DayResults$HomeModelScore[i]>DayResults$AwayModelScore[i]) {
      DayResults$ModelWinner[i]<-DayResults$HomeTeamName[i]
      DayResults$ModelLoser[i]<-DayResults$AwayTeamName[i]
      if(DayResults$ModelWinner[i]==DayResults$VegasWinner[i]) {
        DayResults$ModelSpread[i]<-DayResults$AwayModelScore[i]-DayResults$HomeModelScore[i]
        DayResults$ModelGap[i]<-round(abs(DayResults$VegasSpread[i]-DayResults$ModelSpread[i]), 0)
      } else {
        DayResults$ModelSpread[i]<-DayResults$HomeModelScore[i]-DayResults$AwayModelScore[i]
        DayResults$ModelGap[i]<-round(abs(DayResults$VegasSpread[i]-DayResults$ModelSpread[i]), 0)
      }
    } else {
      DayResults$ModelWinner[i]<-DayResults$AwayTeamName[i]
      DayResults$ModelLoser[i]<-DayResults$HomeTeamName[i]
      if(DayResults$ModelWinner[i]==DayResults$VegasWinner[i]) {
        DayResults$ModelSpread[i]<-DayResults$HomeModelScore[i]-DayResults$AwayModelScore[i]
        DayResults$ModelGap[i]<-round(abs(DayResults$VegasSpread[i]-DayResults$ModelSpread[i]), 0)
      } else {
        DayResults$ModelSpread[i]<-DayResults$AwayModelScore[i]-DayResults$HomeModelScore[i]
        DayResults$ModelGap[i]<-round(abs(DayResults$VegasSpread[i]-DayResults$ModelSpread[i]), 0)
      }
    }
  }
  
  
  
  for (i in 1:nrow(DayResults)) {
    try({
      if(DayResults$HomeScore[i]>DayResults$AwayScore[i]) {
        DayResults$ActualWinner[i]<-DayResults$HomeTeamName[i]
        DayResults$ActualSpread[i]<-DayResults$AwayScore[i]-DayResults$HomeScore[i]
      } else {
        DayResults$ActualWinner[i]<-DayResults$AwayTeamName[i]
        DayResults$ActualSpread[i]<-DayResults$HomeScore[i]-DayResults$AwayScore[i]
      }
      
      if(DayResults$ActualWinner[i]==DayResults$VegasWinner[i] && DayResults$VegasSpread[i]>DayResults$ActualSpread[i]) {
        DayResults$ActualWinner[i]<-DayResults$VegasWinner[i]
      } else if(DayResults$VegasSpread[i]==DayResults$ActualSpread[i]) {
        DayResults$ActualWinner[i]<-"Push"
      } else {
        DayResults$ActualWinner[i]<-DayResults$VegasLoser[i]
      }
    })
  }
  
  
  for (i in 1:nrow(DayResults)) {
    try({
      if(DayResults$ModelWinner[i]==DayResults$VegasWinner[i] && DayResults$ModelSpread[i]<DayResults$VegasSpread[i]) {
        DayResults$ModelPick[i]<-DayResults$ModelWinner[i]
      } else if(DayResults$ModelWinner[i]!=DayResults$VegasWinner[i]) {
        DayResults$ModelPick[i]<-DayResults$ModelWinner[i] 
      } else {DayResults$ModelPick[i]<-DayResults$ModelLoser[i]}
      if(DayResults$ModelPick[i]==DayResults$ActualWinner[i]) {
        DayResults$Result[i]<-"Correct"
      } else if(DayResults$ActualWinner[i] == "Push") {
        DayResults$Result[i]<-"Push"
      } else {
        DayResults$Result[i]<-"InCorrect"
      }})
  }
  return(DayResults)
}
#season<-2021

GetPastSpreadHistory <- function(season) {
  DailyResults<-read.csv("./Basketball/DailyResults.csv")
  day<-max(DailyResults$DayNum)+1
  DailyResultsUpdate<-GetModelGapAnalysis(day, season)
  names(DailyResults)
  names(DailyResultsUpdate)
  DailyResults<-rbind(DailyResults, DailyResultsUpdate) %>% arrange(desc(DayNum, ModelGap))
  write.csv(DailyResults,"./Basketball/DailyResults.csv", row.names = FALSE)
  DailyResults<-DailyResults[DailyResults$DayNum>day-14,]
  
  names(DailyResults)
  
  PastMatchups<-kable(DailyResults[c(9, 1:3, 11, 4:6, 12, 7, 13, 15, 16, 18:20, 23)], format = "markdown")
  filename<-"DetailedMatchups.Rmd"
  formateddate<-format(Sys.time(), '%B %d, %Y')
  htmlformat<-"---\ntitle: Detailed Actualized Matchups\noutput: html_document\n---\n\n"
  link<-"[home](https://maprankings.000webhostapp.com) | [Today's Matchups](https://maprankings.000webhostapp.com/Matchups) | 
  [Past 14 Days](https://maprankings.000webhostapp.com/Past14Days)"
  gap<-"\n---\n"
  note<-"***Spread based on close"
  rowstyle<-"<style>.selected{
  background-color:green;
}
</style>\n\n"
rowformat<-"<script>
  $(function(){
  $('tr').each(function(){
    var col_val1 = $(this).find('td:eq(4)').text();
    var col_val2 = $(this).find('td:eq(8)').text();
    if (col_val1 >= .5 && col_val2 >= .5){
      $(this).addClass('selected');  //the selected class colors the row green//
    }
  });
});
  </script>\n\n"
  cat(c(htmlformat,link, gap, note, gap, PastMatchups, rowstyle, rowformat),
      sep="\n",file=filename)
  render(filename)
  
  ModelSpreadAccuracy<-as.data.frame(spread(DailyResults[DailyResults$DayNum>day-14,] %>%  group_by(DayNum, ModelGap, Result) %>% count(Result), key = "Result", value = "n")) 
  ModelSpreadAccuracy[is.na(ModelSpreadAccuracy)]<-0
  ModelSpreadAccuracy$Percent<-paste(round((ModelSpreadAccuracy$Correct/(ModelSpreadAccuracy$Correct+ModelSpreadAccuracy$InCorrect))*100, 2), '%', sep = "") 
  ModelGapAccuracy<-aggregate(ModelSpreadAccuracy[3:5], ModelSpreadAccuracy[2], FUN = sum)
  ModelDayAccuracy<-aggregate(ModelSpreadAccuracy[3:5], ModelSpreadAccuracy[1], FUN = sum)
  
  ModelGapAccuracy$Percent<-paste(round((ModelGapAccuracy$Correct/(ModelGapAccuracy$Correct+ModelGapAccuracy$InCorrect))*100, 0), '%', sep = "")
  ModelDayAccuracy$Percent<-paste(round((ModelDayAccuracy$Correct/(ModelDayAccuracy$Correct+ModelDayAccuracy$InCorrect))*100, 0), '%', sep = "")
  
  DailyResults$MinTeamCorrelation<-rep(NA, nrow(DailyResults))
  
  for (i in 1:nrow(DailyResults)) {
    DailyResults$MinTeamCorrelation[i]<-floor(as.numeric(min(DailyResults$HomeModelCorrelation[i], DailyResults$AwayModelCorrelation[i]))*10)/10
  }
  
  CorrelationAccuracy<-as.data.frame(spread(DailyResults[DailyResults$DayNum>day-14,] %>%  group_by(MinTeamCorrelation, Result) %>% count(Result), key = "Result", value = "n")) 
  CorrelationAccuracy[is.na(CorrelationAccuracy)]<-0
  CorrelationAccuracy$Percent<-paste(round((CorrelationAccuracy$Correct/(CorrelationAccuracy$Correct+CorrelationAccuracy$InCorrect))*100, 0), '%', sep = "")
  
  
  ModelGapAccuracyKable<-kable(ModelGapAccuracy, format = "markdown")
  ModelDayAccuracyKable<-kable(ModelDayAccuracy, format = "markdown")
  CorrelationAccuracyKable<-kable(CorrelationAccuracy, format = "markdown")
  filename<-"Past14Days"
  formateddate<-format(Sys.time(), '%B %d, %Y')
  htmlformat<-"---\ntitle: Past 14 Days Actualized Matchups\noutput: html_document\n---\n\n"
  link<-"[home](https://maprankings.000webhostapp.com) | [Today's Matchups](https://maprankings.000webhostapp.com/Matchups) | 
  [Detailed Matchups](https://maprankings.000webhostapp.com/DetailedMatchups)"
  gap<-"\n---\n"
  note<-"***Spread based on close"
  cat(c(htmlformat,link, gap, note, gap, CorrelationAccuracyKable, gap, ModelGapAccuracyKable, gap, ModelDayAccuracyKable),
      sep="\n",file=filename)
  render(filename)
  
}


############
#Bracket
############
fillactualbracket<- function(season) {
  Results<-NCAATourneyDetailedResults[NCAATourneyDetailedResults$Season==season,][c(1, 3:6)]
  ModelStats3<-MatchupStats(SeasonMatchups(season), season, TeamRankings2, FinalTeamStats)
  homenormalizedata3<-Team1.2(ModelStats3)
  awaynormalizedata3<-Team2.2(ModelStats3)
  Comp3<-ModelResults2(homenormalizedata3, awaynormalizedata3, ModelStats3, homelm2, awaylm2)
  TeamRankings3<-TeamRankings(Comp3) 
  
  if(nrow(Results)>0) {
    Seeds<-NCAATourneySeeds[NCAATourneySeeds$Season==season,][c(3, 2)]
    for (team in Results$WTeamID) {
      Results$TeamName1[Results$WTeamID==team]<-Teams[Teams$TeamID==team,]$TeamName
      Results$Seed1[Results$WTeamID==team]<-Seeds[Seeds$TeamID==team,]$Seed
    }
    for (team in Results$LTeamID) {
      Results$TeamName2[Results$LTeamID==team]<-Teams[Teams$TeamID==team,]$TeamName
      Results$Seed2[Results$LTeamID==team]<-Seeds[Seeds$TeamID==team,]$Seed
    }
    
    Results$Match1 <- paste0(Results$WTeamID, " : ", Results$LTeamID)
    Results$Match2 <- paste0(Results$LTeamID, " : ", Results$WTeamID)
    
    PlayInGames<-Seeds[str_count(Seeds$Seed)==4,]
    
    PlayInGames$Slot <- substr(PlayInGames$Seed, 1, 3)
    PlayInGames$Winner <- rep(NA, nrow(PlayInGames))
    PlayIn_games <- unique(PlayInGames$Slot)
    
    
    
    Comp3$Matchup<-paste0(Comp3$HomeTeamName, " : ", Comp3$AwayTeamName)
    
    for(game in PlayIn_games){
      teams <- PlayInGames$TeamID[PlayInGames$Slot==game]
      Team1 <- teams[1]
      Team2 <- teams[2]
      
      matchup1 <- paste0(Team1, " : ", Team2)
      matchup2 <- paste0(Team2, " : ", Team1)
      for(matchup in Results$Match1) {
        if(matchup==matchup1) {
          GameResult<-Results[Results$Match1==matchup,][c(2:8)]
          GameResult$Matchup1<-paste0(GameResult$TeamName1, " : ", GameResult$TeamName2)
          GameResult$Matchup2<-paste0(GameResult$TeamName2, " : ", GameResult$TeamName1)
          PlayInGames$Winner[PlayInGames$Slot==game]<-GameResult[1]
          
        }
        if(matchup==matchup2) {
          GameResult<-Results[Results$Match1==matchup2,][c(2:8)]
          GameResult$Matchup1<-paste0(GameResult$TeamName1, " : ", GameResult$TeamName2)
          GameResult$Matchup2<-paste0(GameResult$TeamName2, " : ", GameResult$TeamName1)
          PlayInGames$Winner[PlayInGames$Slot==game]<-GameResult[1]
        }
      }
    }
    
    SeedUpdate<-unique(PlayInGames[c(3:4)])
    SeedUpdate$Winner<-as.integer(unlist(SeedUpdate$Winner))
    names(SeedUpdate)[1:2]<-c("Seed", "TeamID")
    
    FinalSeeds<-left_join(
      rbind(SeedUpdate[c(2, 1)],Seeds),
      Teams[c(1:2)], by = c("TeamID"="TeamID"))
    
    
    bracket<-NCAATourneySlots[NCAATourneySlots$Season == season,]
    
    bracket$TeamID1<-rep(NA, nrow(bracket))
    bracket$Team1<-rep(NA, nrow(bracket))
    bracket$TeamID2<-rep(NA, nrow(bracket))
    bracket$Team2<-rep(NA, nrow(bracket))
    bracket$SpreadBucket<-rep(NA, nrow(bracket))
    bracket$Probability<-rep(NA, nrow(bracket))
    
    
    for(seed in FinalSeeds$Seed) {
      bracket$Team1[bracket$StrongSeed==seed]<-FinalSeeds$TeamName[FinalSeeds$Seed==seed]
      bracket$Team2[bracket$WeakSeed==seed]<-FinalSeeds$TeamName[FinalSeeds$Seed==seed]
      bracket$TeamID1[bracket$StrongSeed==seed]<-FinalSeeds$TeamID[FinalSeeds$Seed==seed]
      bracket$TeamID2[bracket$WeakSeed==seed]<-FinalSeeds$TeamID[FinalSeeds$Seed==seed]
    }
    
    
    populatebracket<- function(roundnumber) {
      if(roundnumber>1) {
        bracket<-updatedbracket
      }
      games<- bracket$Slot[substr(bracket$Slot, 2, 2)==roundnumber]
      for(game in games){
        Team1 <- bracket$TeamID1[bracket$Slot==game]
        Team2 <- bracket$TeamID2[bracket$Slot==game]
        
        matchup1 <- paste0(Team1, " : ", Team2)
        matchup2 <- paste0(Team2, " : ", Team1)
        
        for(matchup in Results$Match1) {
          if(matchup==matchup1) {
            GameResult<-Results[Results$Match1==matchup,][c(2:8)]
            GameResult$Matchup1<-paste0(GameResult$TeamName1, " : ", GameResult$TeamName2)
            GameResult$Matchup2<-paste0(GameResult$TeamName2, " : ", GameResult$TeamName1)
            bracket$Winner[bracket$Slot==game]<-GameResult[1]
            bracket$Winner<-as.integer(unlist(bracket$Winner))
            bracket$Team1Score[bracket$Slot==game]<- GameResult[2]
            bracket$Team2Score[bracket$Slot==game]<- GameResult[4]
            bracket$TeamID1[bracket$StrongSeed==game]<-bracket$Winner[bracket$Slot==game]
            bracket$TeamID2[bracket$WeakSeed==game]<-bracket$Winner[bracket$Slot==game]
            bracket$Team1[bracket$StrongSeed==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            bracket$WinnerTeam[bracket$Slot==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            bracket$Team2[bracket$WeakSeed==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            GameResult$Team1Score<-(Comp3$Team1Score[Comp3$Matchup==GameResult$Matchup1]+Comp3$Team2Score[Comp3$Matchup==GameResult$Matchup2])/2
            GameResult$Team2Score<-(Comp3$Team2Score[Comp3$Matchup==GameResult$Matchup1]+Comp3$Team1Score[Comp3$Matchup==GameResult$Matchup2])/2
            bracket$Team1ProjScore[bracket$Slot==game]<- round(as.numeric(GameResult[10]), 2)
            bracket$Team2ProjScore[bracket$Slot==game]<- round(as.numeric(GameResult[11]), 2)
            bracket$SpreadBucket[bracket$Slot==game]<-CalculateError2(GameResult)[15]
            
          }
          if(matchup==matchup2) {
            GameResult<-Results[Results$Match1==matchup2,][c(2:8)]
            GameResult$Matchup1<-paste0(GameResult$TeamName1, " : ", GameResult$TeamName2)
            GameResult$Matchup2<-paste0(GameResult$TeamName2, " : ", GameResult$TeamName1)
            bracket$Winner[bracket$Slot==game]<-GameResult[1]
            bracket$Winner<-as.integer(unlist(bracket$Winner))
            bracket$Team1Score[bracket$Slot==game]<- GameResult[4]
            bracket$Team2Score[bracket$Slot==game]<- GameResult[2]
            bracket$TeamID1[bracket$StrongSeed==game]<-bracket$Winner[bracket$Slot==game]
            bracket$TeamID2[bracket$WeakSeed==game]<-bracket$Winner[bracket$Slot==game]
            bracket$Team1[bracket$StrongSeed==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            bracket$Team2[bracket$WeakSeed==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            bracket$WinnerTeam[bracket$Slot==game]<-Teams$TeamName[Teams$TeamID==bracket$Winner[bracket$Slot==game]]
            GameResult$Team1Score<-(Comp3$Team1Score[Comp3$Matchup==GameResult$Matchup1]+Comp3$Team2Score[Comp3$Matchup==GameResult$Matchup2])/2
            GameResult$Team2Score<-(Comp3$Team2Score[Comp3$Matchup==GameResult$Matchup1]+Comp3$Team1Score[Comp3$Matchup==GameResult$Matchup2])/2
            bracket$Team1ProjScore[bracket$Slot==game]<- round(as.numeric(GameResult[11]), 2)
            bracket$Team2ProjScore[bracket$Slot==game]<- round(as.numeric(GameResult[10]), 2)
            bracket$SpreadBucket[bracket$Slot==game]<-CalculateError2(GameResult)[15]
          }
        }
      }
      if(roundnumber==6) {
        
      }
      return(bracket)
    }
    
    updatedbracket<-populatebracket(1)
    updatedbracket<-populatebracket(2)
    updatedbracket<-populatebracket(3)
    updatedbracket<-populatebracket(4)
    updatedbracket<-populatebracket(5)
    finalbracket<-populatebracket(6) %>% arrange(Slot)
    #finalbracket$Team1<-paste0(finalbracket$StrongSeed, " ", finalbracket$Team1)
    #finalbracket$Team2<-paste0(finalbracket$WeakSeed, " ", finalbracket$Team2)
    
    finalbracket$round<-substr(finalbracket[2]$Slot, 2,2)
    finalbracket$region<-substr(finalbracket[2]$Slot, 3,3)
    finalbracket$SpreadBucket<-as.character(unlist(finalbracket$SpreadBucket))
    
    
    for(bucket in ProbabilityChart$SpreadBucket) {
      finalbracket$Probability[finalbracket$SpreadBucket==bucket]<-round(ProbabilityChart$Probability[ProbabilityChart$SpreadBucket==bucket], 2)
    }
    
    Champion<-Teams$TeamName[Teams$TeamID==finalbracket$Winner[finalbracket$Slot=='R6CH']]
    
    return(finalbracket)
  }
}

publish_bracket <- function() {
  
  pdf("seedbracket.pdf",width=11,height=8.5)
  
  
  x<-seq(0,220,(221/67))
  y<-0:66
  
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
       axes=F, col="white")
  segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2))) 
  segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
  segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
  segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  segments(100,c(27,37),120,c(27,37))
  segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2))) 
  segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
  segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
  segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  
  
  #####################
  ###W REGION
  #####################
  text(60.8,49.5,"W REGION",cex=1)
  #Round1
  text(9.8,64.5,finalbracket$Team1[finalbracket$Slot=='R1W1'],cex=.4)
  text(16.5,63.8,finalbracket$Score[finalbracket$Slot=='R1W1'],cex=.4)
  text(16.5,63.0,finalbracket$ProjScore[finalbracket$Slot=='R1W1'],cex=.4)
  text(10.5,63.4,finalbracket$Probability[finalbracket$Slot=='R1W1'],cex=.4)
  text(9.8,62.5,finalbracket$Team2[finalbracket$Slot=='R1W1'],cex=.4)
  
  text(9.8,60.5,finalbracket$Team1[finalbracket$Slot=='R1W8'],cex=.4)
  text(16.5,59.8,finalbracket$Score[finalbracket$Slot=='R1W8'],cex=.4)
  text(16.5,59.0,finalbracket$ProjScore[finalbracket$Slot=='R1W8'],cex=.4)
  text(10.5,59.4,finalbracket$Probability[finalbracket$Slot=='R1W8'],cex=.4)
  text(9.8,58.5,finalbracket$Team2[finalbracket$Slot=='R1W8'],cex=.4)
  
  text(9.8,56.5,finalbracket$Team1[finalbracket$Slot=='R1W4'],cex=.4)
  text(16.5,55.8,finalbracket$Score[finalbracket$Slot=='R1W4'],cex=.4)
  text(16.5,55.0,finalbracket$ProjScore[finalbracket$Slot=='R1W4'],cex=.4)
  text(10.5,55.4,finalbracket$Probability[finalbracket$Slot=='R1W4'],cex=.4)
  text(9.8,54.5,finalbracket$Team2[finalbracket$Slot=='R1W4'],cex=.4)
  
  text(9.8,52.5,finalbracket$Team1[finalbracket$Slot=='R1W5'],cex=.4)
  text(16.5,51.8,finalbracket$Score[finalbracket$Slot=='R1W5'],cex=.4)
  text(16.5,51.0,finalbracket$ProjScore[finalbracket$Slot=='R1W5'],cex=.4)
  text(10.5,51.4,finalbracket$Probability[finalbracket$Slot=='R1W5'],cex=.4)
  text(9.8,50.5,finalbracket$Team2[finalbracket$Slot=='R1W5'],cex=.4)
  
  text(9.8,48.5,finalbracket$Team1[finalbracket$Slot=='R1W3'],cex=.4)
  text(16.5,47.8,finalbracket$Score[finalbracket$Slot=='R1W3'],cex=.4)
  text(16.5,47.0,finalbracket$ProjScore[finalbracket$Slot=='R1W3'],cex=.4)
  text(10.5,47.4,finalbracket$Probability[finalbracket$Slot=='R1W3'],cex=.4)
  text(9.8,46.5,finalbracket$Team2[finalbracket$Slot=='R1W3'],cex=.4)
  
  text(9.8,44.5,finalbracket$Team1[finalbracket$Slot=='R1W6'],cex=.4)
  text(16.5,43.8,finalbracket$Score[finalbracket$Slot=='R1W6'],cex=.4)
  text(16.5,43.0,finalbracket$ProjScore[finalbracket$Slot=='R1W6'],cex=.4)
  text(10.5,43.4,finalbracket$Probability[finalbracket$Slot=='R1W6'],cex=.4)
  text(9.8,42.5,finalbracket$Team2[finalbracket$Slot=='R1W6'],cex=.4)
  
  text(9.8,40.5,finalbracket$Team1[finalbracket$Slot=='R1W7'],cex=.4)
  text(16.5,39.8,finalbracket$Score[finalbracket$Slot=='R1W7'],cex=.4)
  text(16.5,39.0,finalbracket$ProjScore[finalbracket$Slot=='R1W7'],cex=.4)
  text(10.5,39.4,finalbracket$Probability[finalbracket$Slot=='R1W7'],cex=.4)
  text(9.8,38.5,finalbracket$Team2[finalbracket$Slot=='R1W7'],cex=.4)
  
  text(9.8,36.5,finalbracket$Team1[finalbracket$Slot=='R1W2'],cex=.4)
  text(16.5,35.8,finalbracket$Score[finalbracket$Slot=='R1W2'],cex=.4)
  text(16.5,35.0,finalbracket$ProjScore[finalbracket$Slot=='R1W2'],cex=.4)
  text(10.5,35.4,finalbracket$Probability[finalbracket$Slot=='R1W2'],cex=.4)
  text(9.8,34.5,finalbracket$Team2[finalbracket$Slot=='R1W2'],cex=.4)
  
  #Round2
  text(29.8,63.5,finalbracket$Team1[finalbracket$Slot=='R2W1'],cex=.4)
  text(36.5,61.8,finalbracket$Score[finalbracket$Slot=='R2W1'],cex=.4)
  text(36.5,61.0,finalbracket$ProjScore[finalbracket$Slot=='R2W1'],cex=.4)
  text(36.5,60.2,finalbracket$Probability[finalbracket$Slot=='R2W1'],cex=.4)
  text(29.8,59.5,finalbracket$Team2[finalbracket$Slot=='R2W1'],cex=.4)
  
  text(29.8,55.5,finalbracket$Team1[finalbracket$Slot=='R2W4'],cex=.4)
  text(36.5,53.8,finalbracket$Score[finalbracket$Slot=='R2W4'],cex=.4)
  text(36.5,53.0,finalbracket$ProjScore[finalbracket$Slot=='R2W4'],cex=.4)
  text(36.5,52.2,finalbracket$Probability[finalbracket$Slot=='R2W4'],cex=.4)
  text(29.8,51.5,finalbracket$Team2[finalbracket$Slot=='R2W4'],cex=.4)
  
  text(29.8,47.5,finalbracket$Team1[finalbracket$Slot=='R2W3'],cex=.4)
  text(36.5,45.8,finalbracket$Score[finalbracket$Slot=='R2W3'],cex=.4)
  text(36.5,45.0,finalbracket$ProjScore[finalbracket$Slot=='R2W3'],cex=.4)
  text(36.5,44.2,finalbracket$Probability[finalbracket$Slot=='R2W3'],cex=.4)
  text(29.8,43.5,finalbracket$Team2[finalbracket$Slot=='R2W3'],cex=.4)
  
  text(29.8,39.5,finalbracket$Team1[finalbracket$Slot=='R2W2'],cex=.4)
  text(36.5,37.8,finalbracket$Score[finalbracket$Slot=='R2W2'],cex=.4)
  text(36.5,37.0,finalbracket$ProjScore[finalbracket$Slot=='R1W2'],cex=.4)
  text(36.5,36.2,finalbracket$Probability[finalbracket$Slot=='R1W2'],cex=.4)
  text(29.8,35.5,finalbracket$Team2[finalbracket$Slot=='R2W2'],cex=.4)
  
  
  #Round3
  text(49.8,61.5,finalbracket$Team1[finalbracket$Slot=='R3W1'],cex=.4)
  text(56.5,57.8,finalbracket$Score[finalbracket$Slot=='R3W1'],cex=.4)
  text(56.5,57.0,finalbracket$ProjScore[finalbracket$Slot=='R3W1'],cex=.4)
  text(56.5,56.2,finalbracket$Probability[finalbracket$Slot=='R3W1'],cex=.4)
  text(49.8,53.5,finalbracket$Team2[finalbracket$Slot=='R3W1'],cex=.4)
  
  text(49.8,45.5,finalbracket$Team1[finalbracket$Slot=='R3W2'],cex=.4)
  text(56.5,41.8,finalbracket$Score[finalbracket$Slot=='R3W2'],cex=.4)
  text(56.5,41.0,finalbracket$ProjScore[finalbracket$Slot=='R3W2'],cex=.4)
  text(56.5,40.2,finalbracket$Probability[finalbracket$Slot=='R3W2'],cex=.4)
  text(49.8,37.5,finalbracket$Team2[finalbracket$Slot=='R3W2'],cex=.4)
  
  #Round4
  text(69.8,57.5,finalbracket$Team1[finalbracket$Slot=='R4W1'],cex=.4)
  text(76.5,49.8,finalbracket$Score[finalbracket$Slot=='R4W1'],cex=.4)
  text(76.5,49.0,finalbracket$ProjScore[finalbracket$Slot=='R4W1'],cex=.4)
  text(76.5,48.2,finalbracket$Probability[finalbracket$Slot=='R4W1'],cex=.4)
  text(69.8,41.5,finalbracket$Team2[finalbracket$Slot=='R4W1'],cex=.4)
  
  
  #####################
  ###X REGION
  #####################
  text(60.8,15.5,"X REGION",cex=1)
  
  #Round1
  text(9.8,30.5,finalbracket$Team1[finalbracket$Slot=='R1X1'],cex=.4)
  text(16.5,29.8,finalbracket$Score[finalbracket$Slot=='R1X1'],cex=.4)
  text(16.5,29.0,finalbracket$ProjScore[finalbracket$Slot=='R1W1'],cex=.4)
  text(10.5,29.4,finalbracket$Probability[finalbracket$Slot=='R1W1'],cex=.4)
  text(9.8,28.5,finalbracket$Team2[finalbracket$Slot=='R1X1'],cex=.4)
  
  text(9.8,26.5,finalbracket$Team1[finalbracket$Slot=='R1X8'],cex=.4)
  text(16.5,25.8,finalbracket$Score[finalbracket$Slot=='R1X8'],cex=.4)
  text(16.5,25.0,finalbracket$ProjScore[finalbracket$Slot=='R1X8'],cex=.4)
  text(10.5,25.4,finalbracket$Probability[finalbracket$Slot=='R1X8'],cex=.4)
  text(9.8,24.5,finalbracket$Team2[finalbracket$Slot=='R1X8'],cex=.4)
  
  text(9.8,22.5,finalbracket$Team1[finalbracket$Slot=='R1X4'],cex=.4)
  text(16.5,21.8,finalbracket$Score[finalbracket$Slot=='R1X4'],cex=.4)
  text(16.5,21.0,finalbracket$ProjScore[finalbracket$Slot=='R1X4'],cex=.4)
  text(10.5,21.4,finalbracket$Probability[finalbracket$Slot=='R1X4'],cex=.4)
  text(9.8,20.5,finalbracket$Team2[finalbracket$Slot=='R1X4'],cex=.4)
  
  text(9.8,18.5,finalbracket$Team1[finalbracket$Slot=='R1X5'],cex=.4)
  text(16.5,17.8,finalbracket$Score[finalbracket$Slot=='R1X5'],cex=.4)
  text(16.5,17.0,finalbracket$ProjScore[finalbracket$Slot=='R1X5'],cex=.4)
  text(10.5,17.4,finalbracket$Probability[finalbracket$Slot=='R1X5'],cex=.4)
  text(9.8,16.5,finalbracket$Team2[finalbracket$Slot=='R1X5'],cex=.4)
  
  text(9.8,14.5,finalbracket$Team1[finalbracket$Slot=='R1X3'],cex=.4)
  text(16.5,13.8,finalbracket$Score[finalbracket$Slot=='R1X3'],cex=.4)
  text(16.5,13.0,finalbracket$ProjScore[finalbracket$Slot=='R1X3'],cex=.4)
  text(10.5,13.4,finalbracket$Probability[finalbracket$Slot=='R1X3'],cex=.4)
  text(9.8,12.5,finalbracket$Team2[finalbracket$Slot=='R1X3'],cex=.4)
  
  text(9.8,10.5,finalbracket$Team1[finalbracket$Slot=='R1X6'],cex=.4)
  text(16.5,9.8,finalbracket$Score[finalbracket$Slot=='R1X6'],cex=.4)
  text(16.5,9.0,finalbracket$ProjScore[finalbracket$Slot=='R1X6'],cex=.4)
  text(10.5,9.4,finalbracket$Probability[finalbracket$Slot=='R1X6'],cex=.4)
  text(9.8,8.5,finalbracket$Team2[finalbracket$Slot=='R1X6'],cex=.4)
  
  text(9.8,6.5,finalbracket$Team1[finalbracket$Slot=='R1X7'],cex=.4)
  text(16.5,5.8,finalbracket$Score[finalbracket$Slot=='R1X7'],cex=.4)
  text(16.5,5.0,finalbracket$ProjScore[finalbracket$Slot=='R1X7'],cex=.4)
  text(10.5,5.4,finalbracket$Probability[finalbracket$Slot=='R1X7'],cex=.4)
  text(9.8,4.5,finalbracket$Team2[finalbracket$Slot=='R1X7'],cex=.4)
  
  text(9.8,2.5,finalbracket$Team1[finalbracket$Slot=='R1X2'],cex=.4)
  text(16.5,1.8,finalbracket$Score[finalbracket$Slot=='R1X2'],cex=.4)
  text(16.5,1.0,finalbracket$ProjScore[finalbracket$Slot=='R1X2'],cex=.4)
  text(10.5,1.4,finalbracket$Probability[finalbracket$Slot=='R1X2'],cex=.4)
  text(9.8,0.5,finalbracket$Team2[finalbracket$Slot=='R1X2'],cex=.4)
  
  #Round2
  text(29.8,29.5,finalbracket$Team1[finalbracket$Slot=='R2X1'],cex=.4)
  text(36.5,27.8,finalbracket$Score[finalbracket$Slot=='R2X1'],cex=.4)
  text(36.5,27.0,finalbracket$ProjScore[finalbracket$Slot=='R2X1'],cex=.4)
  text(36.5,26.2,finalbracket$Probability[finalbracket$Slot=='R2X1'],cex=.4)
  text(29.8,25.5,finalbracket$Team2[finalbracket$Slot=='R2X1'],cex=.4)
  
  text(29.8,21.5,finalbracket$Team1[finalbracket$Slot=='R2X4'],cex=.4)
  text(36.5,19.8,finalbracket$Score[finalbracket$Slot=='R2X4'],cex=.4)
  text(36.5,19.0,finalbracket$ProjScore[finalbracket$Slot=='R2X4'],cex=.4)
  text(36.5,18.2,finalbracket$Probability[finalbracket$Slot=='R2X4'],cex=.4)
  text(29.8,17.5,finalbracket$Team2[finalbracket$Slot=='R2X4'],cex=.4)
  
  text(29.8,13.5,finalbracket$Team1[finalbracket$Slot=='R2X3'],cex=.4)
  text(36.5,11.8,finalbracket$Score[finalbracket$Slot=='R2X3'],cex=.4)
  text(36.5,11.0,finalbracket$ProjScore[finalbracket$Slot=='R2X3'],cex=.4)
  text(36.5,10.2,finalbracket$Probability[finalbracket$Slot=='R2X3'],cex=.4)
  text(29.8,9.5,finalbracket$Team2[finalbracket$Slot=='R2X3'],cex=.4)
  
  text(29.8,5.5,finalbracket$Team1[finalbracket$Slot=='R2X2'],cex=.4)
  text(36.5,3.8,finalbracket$Score[finalbracket$Slot=='R2X2'],cex=.4)
  text(36.5,3.0,finalbracket$ProjScore[finalbracket$Slot=='R2X2'],cex=.4)
  text(36.5,2.2,finalbracket$Probability[finalbracket$Slot=='R2X2'],cex=.4)
  text(29.8,1.5,finalbracket$Team2[finalbracket$Slot=='R2X2'],cex=.4)
  
  
  #Round3
  text(49.8,27.5,finalbracket$Team1[finalbracket$Slot=='R3X1'],cex=.4)
  text(56.8,23.8,finalbracket$Score[finalbracket$Slot=='R3X1'],cex=.4)
  text(56.5,23.0,finalbracket$ProjScore[finalbracket$Slot=='R3X1'],cex=.4)
  text(56.5,22.2,finalbracket$Probability[finalbracket$Slot=='R3X1'],cex=.4)
  text(49.8,19.5,finalbracket$Team2[finalbracket$Slot=='R3X1'],cex=.4)
  
  text(49.8,11.5,finalbracket$Team1[finalbracket$Slot=='R3X2'],cex=.4)
  text(56.8,7.8,finalbracket$Score[finalbracket$Slot=='R3X2'],cex=.4)
  text(56.5,7.0,finalbracket$ProjScore[finalbracket$Slot=='R3X2'],cex=.4)
  text(56.5,6.2,finalbracket$Probability[finalbracket$Slot=='R3X2'],cex=.4)
  text(49.8,3.5,finalbracket$Team2[finalbracket$Slot=='R3X2'],cex=.4)
  
  #Round4
  text(69.8,23.5,finalbracket$Team1[finalbracket$Slot=='R4X1'],cex=.4)
  text(76.8,15.8,finalbracket$Score[finalbracket$Slot=='R4X1'],cex=.4)
  text(76.5,15.0,finalbracket$ProjScore[finalbracket$Slot=='R4X1'],cex=.4)
  text(76.5,14.2,finalbracket$Probability[finalbracket$Slot=='R4X1'],cex=.4)
  text(69.8,7.5,finalbracket$Team2[finalbracket$Slot=='R4X1'],cex=.4)
  
  #####################
  ###Y REGION
  #####################
  text(160.8,49.5,"Y REGION",cex=1)
  
  #Round1
  text(209.8,64.5,finalbracket$Team1[finalbracket$Slot=='R1Y1'],cex=.4)
  text(203.8,63.8,finalbracket$Score[finalbracket$Slot=='R1Y1'],cex=.4)
  text(203.5,63.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y1'],cex=.4)
  text(209.5,63.4,finalbracket$Probability[finalbracket$Slot=='R1Y1'],cex=.4)
  text(209.8,62.5,finalbracket$Team2[finalbracket$Slot=='R1Y1'],cex=.4)
  
  text(209.8,60.5,finalbracket$Team1[finalbracket$Slot=='R1Y8'],cex=.4)
  text(203.8,59.8,finalbracket$Score[finalbracket$Slot=='R1Y8'],cex=.4)
  text(203.5,59.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y8'],cex=.4)
  text(209.5,59.4,finalbracket$Probability[finalbracket$Slot=='R1Y8'],cex=.4)
  text(209.8,58.5,finalbracket$Team2[finalbracket$Slot=='R1Y8'],cex=.4)
  
  text(209.8,56.5,finalbracket$Team1[finalbracket$Slot=='R1Y4'],cex=.4)
  text(203.8,55.8,finalbracket$Score[finalbracket$Slot=='R1Y4'],cex=.4)
  text(203.5,55.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y4'],cex=.4)
  text(209.5,55.4,finalbracket$Probability[finalbracket$Slot=='R1Y4'],cex=.4)
  text(209.8,54.5,finalbracket$Team2[finalbracket$Slot=='R1Y4'],cex=.4)
  
  text(209.8,52.5,finalbracket$Team1[finalbracket$Slot=='R1Y5'],cex=.4)
  text(203.8,51.8,finalbracket$Score[finalbracket$Slot=='R1Y5'],cex=.4)
  text(203.5,51.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y5'],cex=.4)
  text(209.5,51.4,finalbracket$Probability[finalbracket$Slot=='R1Y5'],cex=.4)
  text(209.8,50.5,finalbracket$Team2[finalbracket$Slot=='R1Y5'],cex=.4)
  
  text(209.8,48.5,finalbracket$Team1[finalbracket$Slot=='R1Y3'],cex=.4)
  text(203.8,47.8,finalbracket$Score[finalbracket$Slot=='R1Y3'],cex=.4)
  text(203.5,47.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y3'],cex=.4)
  text(209.5,47.4,finalbracket$Probability[finalbracket$Slot=='R1Y3'],cex=.4)
  text(209.8,46.5,finalbracket$Team2[finalbracket$Slot=='R1Y3'],cex=.4)
  
  text(209.8,44.5,finalbracket$Team1[finalbracket$Slot=='R1Y6'],cex=.4)
  text(203.8,43.8,finalbracket$Score[finalbracket$Slot=='R1Y6'],cex=.4)
  text(203.5,43.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y6'],cex=.4)
  text(209.5,43.4,finalbracket$Probability[finalbracket$Slot=='R1Y6'],cex=.4)
  text(209.8,42.5,finalbracket$Team2[finalbracket$Slot=='R1Y6'],cex=.4)
  
  text(209.8,40.5,finalbracket$Team1[finalbracket$Slot=='R1Y7'],cex=.4)
  text(203.8,39.8,finalbracket$Score[finalbracket$Slot=='R1Y7'],cex=.4)
  text(203.5,39.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y7'],cex=.4)
  text(209.5,39.4,finalbracket$Probability[finalbracket$Slot=='R1Y7'],cex=.4)
  text(209.8,38.5,finalbracket$Team2[finalbracket$Slot=='R1Y7'],cex=.4)
  
  text(209.8,36.5,finalbracket$Team1[finalbracket$Slot=='R1Y2'],cex=.4)
  text(203.8,35.8,finalbracket$Score[finalbracket$Slot=='R1Y2'],cex=.4)
  text(203.5,35.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y2'],cex=.4)
  text(209.5,35.4,finalbracket$Probability[finalbracket$Slot=='R1Y2'],cex=.4)
  text(209.8,34.5,finalbracket$Team2[finalbracket$Slot=='R1Y2'],cex=.4)
  
  #Round2
  text(189.8,63.5,finalbracket$Team1[finalbracket$Slot=='R2Y1'],cex=.4)
  text(183.8,61.8,finalbracket$Score[finalbracket$Slot=='R2Y1'],cex=.4)
  text(183.8,61.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y1'],cex=.4)
  text(183.8,60.2,finalbracket$Probability[finalbracket$Slot=='R1Y1'],cex=.4)
  text(189.8,59.5,finalbracket$Team2[finalbracket$Slot=='R2Y1'],cex=.4)
  
  text(189.8,55.5,finalbracket$Team1[finalbracket$Slot=='R2Y4'],cex=.4)
  text(183.8,53.8,finalbracket$Score[finalbracket$Slot=='R2Y4'],cex=.4)
  text(183.8,53.0,finalbracket$ProjScore[finalbracket$Slot=='R1Y4'],cex=.4)
  text(183.8,52.2,finalbracket$Probability[finalbracket$Slot=='R1Y4'],cex=.4)
  text(189.8,51.5,finalbracket$Team2[finalbracket$Slot=='R2Y4'],cex=.4)
  
  text(189.8,47.5,finalbracket$Team1[finalbracket$Slot=='R2Y3'],cex=.4)
  text(183.8,45.8,finalbracket$Score[finalbracket$Slot=='R2Y3'],cex=.4)
  text(183.8,45.0,finalbracket$ProjScore[finalbracket$Slot=='R2Y3'],cex=.4)
  text(183.8,44.2,finalbracket$Probability[finalbracket$Slot=='R2Y3'],cex=.4)
  text(189.8,43.5,finalbracket$Team2[finalbracket$Slot=='R2Y3'],cex=.4)
  
  text(189.8,39.5,finalbracket$Team1[finalbracket$Slot=='R2Y2'],cex=.4)
  text(183.8,37.8,finalbracket$Score[finalbracket$Slot=='R2Y2'],cex=.4)
  text(183.8,37.0,finalbracket$ProjScore[finalbracket$Slot=='R2Y2'],cex=.4)
  text(183.8,36.2,finalbracket$Probability[finalbracket$Slot=='R2Y2'],cex=.4)
  text(189.8,35.5,finalbracket$Team2[finalbracket$Slot=='R2Y2'],cex=.4)
  
  
  #Round3
  text(169.8,61.5,finalbracket$Team1[finalbracket$Slot=='R3Y1'],cex=.4)
  text(163.8,57.8,finalbracket$Score[finalbracket$Slot=='R3Y1'],cex=.4)
  text(163.8,57.0,finalbracket$ProjScore[finalbracket$Slot=='R3Y1'],cex=.4)
  text(163.8,56.2,finalbracket$Probability[finalbracket$Slot=='R3Y1'],cex=.4)
  text(169.8,53.5,finalbracket$Team2[finalbracket$Slot=='R3Y1'],cex=.4)
  
  text(169.8,45.5,finalbracket$Team1[finalbracket$Slot=='R3Y2'],cex=.4)
  text(163.8,41.8,finalbracket$Score[finalbracket$Slot=='R3Y2'],cex=.4)
  text(163.8,41.0,finalbracket$ProjScore[finalbracket$Slot=='R3Y2'],cex=.4)
  text(163.8,40.2,finalbracket$Probability[finalbracket$Slot=='R3Y2'],cex=.4)
  text(169.8,37.5,finalbracket$Team2[finalbracket$Slot=='R3Y2'],cex=.4)
  
  #Round4
  text(149.8,57.5,finalbracket$Team1[finalbracket$Slot=='R4Y1'],cex=.4)
  text(143.8,49.8,finalbracket$Score[finalbracket$Slot=='R4Y1'],cex=.4)
  text(143.8,49.0,finalbracket$ProjScore[finalbracket$Slot=='R4Y1'],cex=.4)
  text(143.8,48.2,finalbracket$Probability[finalbracket$Slot=='R4Y1'],cex=.4)
  text(149.8,41.5,finalbracket$Team2[finalbracket$Slot=='R4Y1'],cex=.4)
  
  #####################
  ###Z REGION
  #####################
  text(160.8,15.5,"Z REGION",cex=1)
  
  #Round1
  text(209.8,30.5,finalbracket$Team1[finalbracket$Slot=='R1Z1'],cex=.4)
  text(203.8,29.8,finalbracket$Score[finalbracket$Slot=='R1Z1'],cex=.4)
  text(203.5,29.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z1'],cex=.4)
  text(209.5,29.4,finalbracket$Probability[finalbracket$Slot=='R1Z1'],cex=.4)
  text(209.8,28.5,finalbracket$Team2[finalbracket$Slot=='R1Z1'],cex=.4)
  
  text(209.8,26.5,finalbracket$Team1[finalbracket$Slot=='R1Z8'],cex=.4)
  text(203.8,25.8,finalbracket$Score[finalbracket$Slot=='R1Z8'],cex=.4)
  text(203.5,25.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z8'],cex=.4)
  text(209.5,25.4,finalbracket$Probability[finalbracket$Slot=='R1Z8'],cex=.4)
  text(209.8,24.5,finalbracket$Team2[finalbracket$Slot=='R1Z8'],cex=.4)
  
  text(209.8,22.5,finalbracket$Team1[finalbracket$Slot=='R1Z4'],cex=.4)
  text(203.8,21.8,finalbracket$Score[finalbracket$Slot=='R1Z4'],cex=.4)
  text(203.5,21.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z4'],cex=.4)
  text(209.5,21.4,finalbracket$Probability[finalbracket$Slot=='R1Z4'],cex=.4)
  text(209.8,20.5,finalbracket$Team2[finalbracket$Slot=='R1Z4'],cex=.4)
  
  text(209.8,18.5,finalbracket$Team1[finalbracket$Slot=='R1Z5'],cex=.4)
  text(203.8,17.8,finalbracket$Score[finalbracket$Slot=='R1Z5'],cex=.4)
  text(203.5,17.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z5'],cex=.4)
  text(209.5,17.4,finalbracket$Probability[finalbracket$Slot=='R1Z5'],cex=.4)
  text(209.8,16.5,finalbracket$Team2[finalbracket$Slot=='R1Z5'],cex=.4)
  
  text(209.8,14.5,finalbracket$Team1[finalbracket$Slot=='R1Z3'],cex=.4)
  text(203.8,13.8,finalbracket$Score[finalbracket$Slot=='R1Z3'],cex=.4)
  text(203.5,13.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z3'],cex=.4)
  text(209.5,13.4,finalbracket$Probability[finalbracket$Slot=='R1Z3'],cex=.4)
  text(209.8,12.5,finalbracket$Team2[finalbracket$Slot=='R1Z3'],cex=.4)
  
  text(209.8,10.5,finalbracket$Team1[finalbracket$Slot=='R1Z6'],cex=.4)
  text(203.8,9.8,finalbracket$Score[finalbracket$Slot=='R1Z6'],cex=.4)
  text(203.5,9.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z6'],cex=.4)
  text(209.5,9.4,finalbracket$Probability[finalbracket$Slot=='R1Z6'],cex=.4)
  text(209.8,8.5,finalbracket$Team2[finalbracket$Slot=='R1Z6'],cex=.4)
  
  text(209.8,6.5,finalbracket$Team1[finalbracket$Slot=='R1Z7'],cex=.4)
  text(203.8,5.8,finalbracket$Score[finalbracket$Slot=='R1Z7'],cex=.4)
  text(203.5,5.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z7'],cex=.4)
  text(209.5,5.4,finalbracket$Probability[finalbracket$Slot=='R1Z7'],cex=.4)
  text(209.8,4.5,finalbracket$Team2[finalbracket$Slot=='R1Z7'],cex=.4)
  
  text(209.8,2.5,finalbracket$Team1[finalbracket$Slot=='R1Z2'],cex=.4)
  text(203.8,1.8,finalbracket$Score[finalbracket$Slot=='R1Z2'],cex=.4)
  text(203.5,1.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z2'],cex=.4)
  text(209.5,1.4,finalbracket$Probability[finalbracket$Slot=='R1Z2'],cex=.4)
  text(209.8,0.5,finalbracket$Team2[finalbracket$Slot=='R1Z2'],cex=.4)
  
  #Round2
  text(189.8,29.5,finalbracket$Team1[finalbracket$Slot=='R2Z1'],cex=.4)
  text(183.8,27.8,finalbracket$Score[finalbracket$Slot=='R2Z1'],cex=.4)
  text(183.8,27.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z1'],cex=.4)
  text(183.8,26.2,finalbracket$Probability[finalbracket$Slot=='R1Z1'],cex=.4)
  text(189.8,25.5,finalbracket$Team2[finalbracket$Slot=='R2Z1'],cex=.4)
  
  text(189.8,21.5,finalbracket$Team1[finalbracket$Slot=='R2Z4'],cex=.4)
  text(183.8,19.8,finalbracket$Score[finalbracket$Slot=='R2Z4'],cex=.4)
  text(183.8,19.0,finalbracket$ProjScore[finalbracket$Slot=='R1Z4'],cex=.4)
  text(183.8,18.2,finalbracket$Probability[finalbracket$Slot=='R1Z4'],cex=.4)
  text(189.8,17.5,finalbracket$Team2[finalbracket$Slot=='R2Z4'],cex=.4)
  
  text(189.8,13.5,finalbracket$Team1[finalbracket$Slot=='R2Z3'],cex=.4)
  text(183.8,11.8,finalbracket$Score[finalbracket$Slot=='R2Z3'],cex=.4)
  text(183.8,11.0,finalbracket$ProjScore[finalbracket$Slot=='R2Z3'],cex=.4)
  text(183.8,10.2,finalbracket$Probability[finalbracket$Slot=='R2Z3'],cex=.4)
  text(189.8,9.5,finalbracket$Team2[finalbracket$Slot=='R2Z3'],cex=.4)
  
  text(189.8,5.5,finalbracket$Team1[finalbracket$Slot=='R2Z2'],cex=.4)
  text(183.8,3.8,finalbracket$Score[finalbracket$Slot=='R2Z2'],cex=.4)
  text(183.8,3.0,finalbracket$ProjScore[finalbracket$Slot=='R2Z2'],cex=.4)
  text(183.8,2.2,finalbracket$Probability[finalbracket$Slot=='R2Z2'],cex=.4)
  text(189.8,1.5,finalbracket$Team2[finalbracket$Slot=='R2Z2'],cex=.4)
  
  
  #Round3
  text(169.8,27.5,finalbracket$Team1[finalbracket$Slot=='R3Z1'],cex=.4)
  text(163.8,23.8,finalbracket$Score[finalbracket$Slot=='R3Z1'],cex=.4)
  text(163.8,23.0,finalbracket$ProjScore[finalbracket$Slot=='R3Z1'],cex=.4)
  text(163.8,22.2,finalbracket$Probability[finalbracket$Slot=='R3Z1'],cex=.4)
  text(169.8,19.5,finalbracket$Team2[finalbracket$Slot=='R3Z1'],cex=.4)
  
  text(169.8,11.5,finalbracket$Team1[finalbracket$Slot=='R3Z2'],cex=.4)
  text(163.8,7.8,finalbracket$Score[finalbracket$Slot=='R3Z2'],cex=.4)
  text(163.8,7.0,finalbracket$ProjScore[finalbracket$Slot=='R3Z2'],cex=.4)
  text(163.8,6.2,finalbracket$Probability[finalbracket$Slot=='R3Z2'],cex=.4)
  text(169.8,3.5,finalbracket$Team2[finalbracket$Slot=='R3Z2'],cex=.4)
  
  #Round4
  text(149.8,23.5,finalbracket$Team1[finalbracket$Slot=='R4Z1'],cex=.4)
  text(143.8,15.8,finalbracket$Score[finalbracket$Slot=='R4Z1'],cex=.4)
  text(143.8,15.0,finalbracket$ProjScore[finalbracket$Slot=='R4Z1'],cex=.4)
  text(143.8,14.2,finalbracket$Probability[finalbracket$Slot=='R4Z1'],cex=.4)
  text(149.8,7.5,finalbracket$Team2[finalbracket$Slot=='R4Z1'],cex=.4)
  
  
  #Final4
  text(89.8,49.5,finalbracket$Team1[finalbracket$Slot=='R5WX'],cex=.4)
  text(95.8,16.8,finalbracket$Score[finalbracket$Slot=='R5WX'],cex=.4)
  text(95.8,16.0,finalbracket$ProjScore[finalbracket$Slot=='R5WX'],cex=.4)
  text(95.8,15.2,finalbracket$Probability[finalbracket$Slot=='R5WX'],cex=.4)
  text(89.8,15.5,finalbracket$Team2[finalbracket$Slot=='R5WX'],cex=.4)
  
  text(129.8,49.5,finalbracket$Team1[finalbracket$Slot=='R5YZ'],cex=.4)
  text(123.8,48.8,finalbracket$Score[finalbracket$Slot=='R5YZ'],cex=.4)
  text(123.5,48.0,finalbracket$ProjScore[finalbracket$Slot=='R5YZ'],cex=.4)
  text(123.5,47.2,finalbracket$Probability[finalbracket$Slot=='R5YZ'],cex=.4)
  text(129.8,15.5,finalbracket$Team2[finalbracket$Slot=='R5YZ'],cex=.4)
  
  #Championship
  text(109.8,37.5,finalbracket$Team1[finalbracket$Slot=='R6CH'],cex=.4)
  
  text(109.8,27.5,finalbracket$Team2[finalbracket$Slot=='R6CH'],cex=.4)
  
  
  #Champion
  text(109.8,34.5,Champion,cex=2.5)
  text(109.8,31.5,finalbracket$Score[finalbracket$Slot=='R6CH'],cex=1)
  text(109.8,29.8,finalbracket$ProjScore[finalbracket$Slot=='R6CH'],cex=.4)
  text(109.8,28.8,finalbracket$Probability[finalbracket$Slot=='R6CH'],cex=.4)
  text(109.8,63.5,paste0(season, " NCAA TOURNAMENT"),cex=2)
  
  dev.off()
}


