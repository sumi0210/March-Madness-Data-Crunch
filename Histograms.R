## code for histogram march madness variables

dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)

train <- dataset

train$seed_delta <- train$team1_seed - train$team2_seed
train$fg2pct_delta <- train$team1_fg2pct - train$team2_fg2pct
train$fg3pct_delta <- train$team1_fg3pct - train$team2_fg3pct
train$ftpct_delta <- train$team1_ftpct - train$team2_ftpct
train$blockpct_delta <- train$team1_blockpct - train$team2_blockpct
train$oppfg2pct_delta <- train$team1_oppfg2pct - train$team2_oppfg2pct
train$oppfg3pct_delta <- train$team1_oppfg3pct - train$team2_oppfg3pct
train$oppftpct_delta <- train$team1_oppftpct - train$team2_oppftpct
train$oppblockpct_delta <- train$team1_oppblockpct - train$team2_oppblockpct
train$oppf3grate_delta <- train$team1_oppf3grate - train$team2_oppf3grate
train$arate_delta <- train$team1_arate - train$team2_arate
train$opparate_delta <- train$team1_opparate - train$team2_opparate
train$stlrate_delta <- train$team1_stlrate - train$team2_stlrate
train$oppstlrate_delta <- train$team1_oppstlrate - train$team2_oppstlrate
train$school_s16_delta <- train$team1_pt_school_s16 - train$team2_pt_school_s16
train$school_ff_delta <- train$team1_pt_school_ff - train$team2_pt_school_ff
train$team_season_wins_delta <- train$team1_pt_team_season_wins - train$team2_pt_team_season_wins
train$adjoe_delta <- train$team1_adjoe - train$team2_adjoe
train$adjde_delta <- train$team1_adjde - train$team2_adjde

data <- train[,101:119]

par(mfrow=c(4,5))
colnames <- dimnames(data)[[2]]
for (i in 1:19) {
  hist(data[,i], main=colnames[i], probability=TRUE, col="blue4", border="red")
}