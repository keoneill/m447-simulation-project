nba <- read.csv("nba.csv", header=TRUE, check.names =FALSE)
nba14 <- read.csv("2014-nba-stats.csv", header=TRUE, check.names =FALSE)
# nba[which(nba$Team.Place == "Warriors.Away"),] # gets row for warriors.away

###########################################################################
# Setting up linear model for predicting scores (based on 2014 nba stats) (using score2 in calculateScore function)
# PTS = points, AST = Assists, FGA = Field Goal Attempts, TPA = Three-point attempts, FTA = Free throw attempts
summary(m.nba <- lm(PTS ~ FGA + FGp + TPA + TPp + FTA + FTp , data= nba14))

# Call:
#     lm(formula = PTS ~ FGA + FGp + TPA + TPp + FTA + FTp, data = nba14)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.30861 -0.11833 -0.01633  0.10413  0.45459 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.025e+02  2.715e+00 -37.760  < 2e-16 ***
# FGA          9.099e-01  2.009e-02  45.295  < 2e-16 ***
# FGp          1.696e+02  4.107e+00  41.288  < 2e-16 ***
# TPA          3.446e-01  9.442e-03  36.501  < 2e-16 ***
# TPp          2.300e+01  2.706e+00   8.502 7.62e-09 ***
# FTA          7.642e-01  2.068e-02  36.946  < 2e-16 ***
# FTp          2.283e+01  1.467e+00  15.562 2.28e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1889 on 25 degrees of freedom
# Multiple R-squared:  0.9981,	Adjusted R-squared:  0.9976 
# F-statistic:  2168 on 6 and 25 DF,  p-value: < 2.2e-16

# rnorm(n, mean, sd)
# FGA - set sd at 10%
# 3PA - set sd at 15%
# FTA - set sd at 20%

calculateScore <- function(teamStats, fga_adj) {
    n <- 1 # number of random numbers to produce

    # using the best combination based on linear model
    AST <- rnorm(n, teamStats$AST, teamStats$AST*.2)
    FGA <- rnorm(n, teamStats$FGA, teamStats$FGA*.1) + fga_adj
    FGp <- rnorm(n, teamStats$FGp, .05)
    TPA <- rnorm(n, teamStats$TPA, teamStats$TPA*.15)
    TPp <- rnorm(n, teamStats$TPp, .09)    
    FTA <- rnorm(n, teamStats$FTA, teamStats$FTA*.2)
    FTp <- rnorm(n, teamStats$FTp, .1)    
    score <- FGA*.9099 + FGp*169.6 + TPA*.3446 + TPp*23 + FTA*.7642 + FTp*22.83 - 102.5

    return (score)
}

simulateGame <- function(awayTeam, homeTeam, stats.df, nruns) {
    homeTeamStats <- stats.df[which(stats.df$Team.Place == paste(homeTeam, ".Home", sep="")),]
    awayTeamStats <- stats.df[which(stats.df$Team.Place == paste(awayTeam, ".Away", sep="")),]
    homeWins <- 0
    awayWins <- 0
    homeAvgScore <- 0
    awayAvgScore <- 0
    scores <- data.frame("Home" = 0, "Away" = 0)
    # defensive adjustments to Field Goal Attempts
    fga_adj <- ((homeTeamStats$STL - awayTeamStats$STL) + (homeTeamStats$Oreb - awayTeamStats$Oreb) + 
                    (homeTeamStats$Dreb - awayTeamStats$Dreb) - (homeTeamStats$TO - awayTeamStats$TO))
    for(i in 1:nruns) {
        homeScore <- calculateScore(homeTeamStats, fga_adj)
        awayScore <- calculateScore(awayTeamStats, 0)
        newRow <- c(homeScore, awayScore )
        scores <- rbind(newRow, scores)
        ifelse(homeScore > awayScore, homeWins <- homeWins+1, awayWins <- awayWins+1)
    }
    homeAvgScore <- round(mean(scores$Home))
    awayAvgScore <- round(mean(scores$Away))
    if(homeAvgScore == awayAvgScore) { # use average assists per game to break ties
        ifelse(homeTeamStats$AST > awayTeamStats$AST, 
               homeAvgScore <- homeAvgScore+1, awayAvgScore <- awayAvgScore+1)
    }
    results <- list(AwayWins = awayWins, HomeWins = homeWins, 
                    AwayAvgScore = awayAvgScore, HomeAvgScore = homeAvgScore)
    return(results)
}

#function that returns a result of 7 game series
playSeries <- function(lowSeed, highSeed, stats.df) {
    highSeedWins <- 0
    lowSeedWins <- 0
    for (i in 1:7) 
    {
        if (highSeedWins == 4 | lowSeedWins == 4) {
            break
        }
        if (i %in% c(1,2,5,7)) {
            # Higher Seed is at Home
            homeTeam <- highSeed
            awayTeam <- lowSeed
            results <- simulateGame(awayTeam, homeTeam, stats.df, 7)
            if(results$HomeAvgScore > results$AwayAvgScore) { 
                   highSeedWins <- highSeedWins + 1 
                  # print(paste("Game ", i, ": ", homeTeam, " (", highSeedWins, ", ", lowSeedWins, ")", sep = ""))
            }
            else {
                lowSeedWins <- lowSeedWins + 1
                #print(paste("Game ", i, ": ", awayTeam, " (", highSeedWins, ", ", lowSeedWins, ")", sep = ""))
            }
        }
        if (i %in% c(3,4,6)) 
        {   # Higher Seed is on road
            homeTeam <- lowSeed
            awayTeam <- highSeed
            results <- simulateGame(awayTeam, homeTeam, stats.df, 7)
            if(results$HomeAvgScore > results$AwayAvgScore) { 
                lowSeedWins <- lowSeedWins + 1 
                #print(paste("Game ", i, ": ", homeTeam, " (", highSeedWins, ", ", lowSeedWins, ")", sep= ""))
            }
            else {
                highSeedWins <- highSeedWins + 1
                #print(paste("Game ", i, ": ", awayTeam, " (", highSeedWins, ", ", lowSeedWins, ")",sep= ""))
            }
        }
    }
    result <- c(highSeedWins,lowSeedWins)
    return(result)
} # end playSeries function

simulateSeries <- function(lowSeed, highSeed, stats.df) {
    iter <- 100
    lowSeedAvgWins <- 0
    highSeedAvgWins <- 0
    lowSeedSeriesWins <- 0
    highSeedSeriesWins <- 0
    for (i in 1:iter) {
        winVec <- playSeries(lowSeed, highSeed, stats.df)
        lowSeedAvgWins <- lowSeedAvgWins + winVec[2]
        highSeedAvgWins <- highSeedAvgWins + winVec[1]
        ifelse(winVec[1] > winVec[2], highSeedSeriesWins <- highSeedSeriesWins + 1, 
               lowSeedSeriesWins <- lowSeedSeriesWins + 1)
    }
    lowSeedAvgWins <- lowSeedAvgWins / iter
    highSeedAvgWins <- highSeedAvgWins / iter
    results <- c(highSeedSeriesWins, lowSeedSeriesWins, highSeedAvgWins, lowSeedAvgWins)
    print(paste(highSeed, "Avg wins:", highSeedAvgWins, "Series Wins:", highSeedSeriesWins))
    print(paste(lowSeed, "Avg wins:", lowSeedAvgWins, "Series Wins:", lowSeedSeriesWins)) 
#    return(results)
}

# simulateGame(AwayTeam, HomeTeam, dataFrame, numberRuns) # parameters
# simulateGame("Warriors", "Hornets", nba, 10000) # example
# Predictions using original model (using score in calculateScore function)
########################################################

# Prediction    |    Actual Score   (Home Team on bottom)

##########  12-02-2015  ################################

# Nuggets   91      Nuggets     90      Confidence: 60.1 %
# Bulls     94      Bulls       99 

# Warriors  108     Warriors    116     Confidence: 72.3 %
# Hornets   101     Hornets     99

# Pacers    97      Pacers      103     Confidence: 59.6 %
# Clippers  94      Clippers    91


##########  12-03-2015 #################################

# (11-7)Thunder   100     Thunder     95      Confidence: 72.87 %
# (10-6)Heat      93      Heat        97

# (6-13)Nuggets   91      Nuggets     106     Confidence: 85.19 %
# (12-7)Raptors   102     Raptors     105

# (10-8)Magic    100      Magic       103     Confidence: 82.60 %
# (8-8)Jazz      91      Jazz        94

# (15-4)Spurs     97      Spurs       103     Confidence: 49.33 %
# (11-8)Grizzlies 96      Grizzlies   83

# (12-5)Pacers        97  Pacers          111      Confidence: 58.23 %
# (7-12)Trailblazers  99  Trailblazers    123

# (10-8)Celtics   97      Celtics 114            Confidence: 60.85 % 
# (7-12)Kings     100     Kings   97

# prediction record 6-3



######################################################
# predictions with updated model

########### 12-07-2015  ##############################

# Pistons(12-9) 96              Pistons 84
# Hornets(11-8) 103     66 %    Hornets 104

# Spurs(17-4)   98     63 %     Spurs   119
# 76ers(1-20)   92              Sixers  68

# Mavericks(12-9)   103  68 %   Mavericks   104
# Knicks(10-11)     95          Knicks      97

# Lakers(3-17)     95   71 %    Lakers  93
# Raptors(12-9)    105          Raptors 102

# Wizards(8-10)     95 58 %     Wizards 114    
# Heat(12-6)        98          Heat    103

# Suns(8-13)        103 68 %    Suns    103    
# Bulls(11-6)       95          Bulls   101

# Celtics(11-9)     102 59 %    Celtics     111
# Pelicans(5-15)    106         Pelicans    93

# Trailblazers(9-12)    98  59 %    Trailblazers    88
# Bucks(8-13)           103         Bucks           90

# Clippers(11-9)        107     71 %    Clippers        110
# Timberwolves(8-11)    97              Timberwolves    106

# Prediction record 7-2

##################################################################
#Predictions using final model with defensive stats
#####  12-09-2015 ################

# Rockets  103  52 %   Rockets 109
# Wizards  104         Wizards 103

# Heat      88  91 %    Heat 81
# Hornets   112         Hornets 99

# Bulls     101 66 %    Bulls     100
# Celtics   108         Celtics   105

# Spurs     100 58 %    Spurs     94
# Raptors   104         Raptors   97

# Grizzlies 95  77 %    Grizzlies 93
# Pistons   108         Pistons   92

# Lakers        96  60 %    Lakers        122
# Timberwolves  101         Timberwolves  123

# Clippers  107 69 %    Clippers  109
# Bucks     99          Bucks     95

# Knicks    98  52 %    Knicks    85
# Jazz      99          Jazz      106

# Magic     98  76 %    Magic     104
# Suns      110         Suns      107

# Hawks     99  64 %    Hawks     98
# Mavericks 93          Mavericks 95

################################################################
###
# Prediction Record for dec 12th, 8-2
###
#---------------------------------------------
###
# Prediction Record for playoffs,  10-5
###
################################################################

#  2014 NBA Playoffs
# Actual Results

# 1 Warriors    4 
# 8 Pelicans    0       1 Warriors  4

# 4 Trailblazers 1      5 Grizzlies 2   
# 5 Grizzlies    4                      1 Warriors 4

# 3 Clippers    4                       2 Rockets   1
# 6 Spurs       3       3 Clippers  3

# 2 Rockets    4        2 Rockets   4
# 7 Mavericks  1                                    Warriors 4 

# 1 Hawks   4                                       Cavs     2    
# 8 Nets    2       1 Hawks     4

# 4 Raptors 0       5 Wizards   2
# 5 Wizards 4                           1 Hawks 0
                    
# 3 Bulls   4                           2 Cavs  4
# 6 Bucks   2       3 Bulls 2

# 2 Cavs    4       2 Cavs  4
# 7 Celtics  0

# Simulations
# First Round (4-4)
simulateSeries("Pelicans", "Warriors", nba14)       
# "Warriors Avg wins: 4 Series Wins: 100"
# "Pelicans Avg wins: 0.35 Series Wins: 0"
simulateSeries("Grizzlies", "Trailblazers", nba14)  
# "Trailblazers Avg wins: 4 Series Wins: 100"
# "Grizzlies Avg wins: 1.19 Series Wins: 0"
simulateSeries("Spurs", "Clippers", nba14)          
# "Clippers Avg wins: 3.94 Series Wins: 96"
# "Spurs Avg wins: 1.68 Series Wins: 4"
simulateSeries("Mavericks", "Rockets", nba14)
# "Rockets Avg wins: 2.19 Series Wins: 22"
# "Mavericks Avg wins: 3.51 Series Wins: 78"
simulateSeries("Nets", "Hawks", nba14)
# "Hawks Avg wins: 3.92 Series Wins: 95"
# "Nets Avg wins: 1.2 Series Wins: 5"
simulateSeries("Wizards", "Raptors", nba14)
# "Raptors Avg wins: 3.88 Series Wins: 94"
# "Wizards Avg wins: 1.34 Series Wins: 6"
simulateSeries("Bucks", "Bulls", nba14)
# "Bulls Avg wins: 3.84 Series Wins: 92"
# "Bucks Avg wins: 1.4 Series Wins: 8"
simulateSeries("Celtics", "Cavs", nba14)
# "Cavs Avg wins: 2.74 Series Wins: 42"
# "Celtics Avg wins: 3.22 Series Wins: 58"

# Second Round (3-1)
simulateSeries("Grizzlies", "Warriors", nba14)
# "Warriors Avg wins: 4 Series Wins: 100"
# "Grizzlies Avg wins: 0.21 Series Wins: 0"
simulateSeries("Clippers", "Rockets", nba14)
# "Rockets Avg wins: 1.22 Series Wins: 8"
# "Clippers Avg wins: 3.81 Series Wins: 92"
simulateSeries("Wizards", "Hawks", nba14)
# "Hawks Avg wins: 3.85 Series Wins: 92"
# "Wizards Avg wins: 2.19 Series Wins: 8"
simulateSeries("Bulls", "Cavs", nba14)
# "Cavs Avg wins: 3.38 Series Wins: 70"
# "Bulls Avg wins: 2.52 Series Wins: 30"

# Third Round (2-0)
simulateSeries("Rockets", "Warriors", nba14)
# "Warriors Avg wins: 4 Series Wins: 100"
# "Rockets Avg wins: 0.44 Series Wins: 0"
simulateSeries("Cavs", "Hawks", nba14)
# "Hawks Avg wins: 2.65 Series Wins: 41"
# "Cavs Avg wins: 3.17 Series Wins: 59"

# Finals (1-0)
simulateSeries("Cavs", "Warriors", nba14)
# "Warriors Avg wins: 4 Series Wins: 100"
# "Cavs Avg wins: 0.45 Series Wins: 0"
