## Correlation Matrix


install.packages("corrplot")

library(corrplot)


dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)
# View(dataset)

train <- dataset

train$seed <- train$team1_seed - train$team2_seed
train$fg2pct <- train$team1_fg2pct - train$team2_fg2pct
train$fg3pct <- train$team1_fg3pct - train$team2_fg3pct
train$ftpct <- train$team1_ftpct - train$team2_ftpct
train$blockpct <- train$team1_blockpct - train$team2_blockpct
train$oppfg2pct <- train$team1_oppfg2pct - train$team2_oppfg2pct
train$oppfg3pct <- train$team1_oppfg3pct - train$team2_oppfg3pct
train$oppftpct <- train$team1_oppftpct - train$team2_oppftpct
train$oppblockpct <- train$team1_oppblockpct - train$team2_oppblockpct
train$oppf3grate <- train$team1_oppf3grate - train$team2_oppf3grate
train$arate <- train$team1_arate - train$team2_arate
train$opparate <- train$team1_opparate - train$team2_opparate
train$stlrate <- train$team1_stlrate - train$team2_stlrate
train$oppstlrate <- train$team1_oppstlrate - train$team2_oppstlrate
train$school_s16 <- train$team1_pt_school_s16 - train$team2_pt_school_s16
train$school_ff <- train$team1_pt_school_ff - train$team2_pt_school_ff
train$season_wins <- train$team1_pt_team_season_wins - train$team2_pt_team_season_wins
train$adjoe <- train$team1_adjoe - train$team2_adjoe
train$adjde <- train$team1_adjde - train$team2_adjde
train$distance <- train$distance

train
# View(train)

data <- train[,101:120]
# View(data)

res <- cor(data)

round(res, 2)

par(oma=c(5,0,0,0))
corrplot(res, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)


