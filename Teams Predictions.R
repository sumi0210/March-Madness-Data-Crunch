library(ggplot2)
library(ggthemes)
library(pscl) # Logistic Regression
install.packages("ROCR")
library(ROCR) # ROCR Curve
install.packages("dplyr")
library(dplyr)

###########################################################
#       Model 1 - Selected variables (intuitively)        #
###########################################################

## accuracy: 72.717

dataset <- read.csv("NCAA_Tourney_2002-2016_rev1.csv", header = T, stringsAsFactors = F)
View(dataset)

da.dataset <- data.frame(dataset[,(1:6)], dataset[,(8:24)], dataset[,(26:42)])
# View(da.dataset)

train <- da.dataset[which(da.dataset$season < 2012),]
# View(train)
test <- da.dataset[which(da.dataset$season >= 2012),]
# View(test)

lapply(train, class)

lr_model <- glm(result ~. -season-team1_id-team1_score-team2_id-team2_score, family = "binomial", data = train)
lr_model
summary(lr_model)

test_prediction <- predict(lr_model, test, type = 'response')
test_prediction

ROCRpred <- prediction(test_prediction, test$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc


################################################################
#        Model 2 - Include 3 point field goal attempts         #
################################################################

## model became less accurate with the addition of field goal attempts
## accuracy: 71.869

dataset <- read.csv("NCAA_Tourney_2002-2016_field goal attempts.csv", header = T, stringsAsFactors = F)
View(dataset)

da.dataset <- data.frame(dataset[,(1:6)], dataset[,(8:26)], dataset[,(28:46)])
# View(da.dataset)

train <- da.dataset[which(da.dataset$season < 2012),]
# View(train)
test <- da.dataset[which(da.dataset$season >= 2012),]
# View(test)


lr_model <- glm(result ~. -season-team1_id-team1_score-team2_id-team2_score, family = "binomial", data = train)
lr_model
summary(lr_model)

test_prediction <- predict(lr_model, test, type = 'response')
test_prediction

ROCRpred <- prediction(test_prediction, test$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc


###################################################################
#                    Model 3 - team deltas                        #
###################################################################

dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)
View(dataset)

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
train$oe_delta <- train$team1_oe - train$team2_oe
train$de_delta <- train$team1_de - train$team2_de
train$pt_school_s16_delta <- train$team1_pt_school_s16 - train$team2_pt_school_s16
train$pt_school_ff_delta <- train$team1_pt_school_ff - train$team2_pt_school_ff
train$pt_team_season_wins__delta <- train$team1_pt_team_season_wins - train$team2_pt_team_season_wins

train
View(train)

################ Predict (deltas) using split 67.67% / 33.33% ################
## accuracy: 75.475

train_model <- train[which(train$season < 2012),]
train1 <- train_model[,100:119]
# View(train1)
test_model <- train[which(train$season >= 2012),]
test1 <- test_model[,100:119]
# View(test1)

lr_model <- glm(result ~., family = "binomial", data = train1)
lr_model
summary(lr_model)

test1$prob <- predict(lr_model, test1, type = 'response')
# View(test1)

test1 <- test1 %>% mutate(model_pred = 1*(prob >= .50) + 0, result_binary = 1*(result == 1) + 0)

test1 <- test1 %>% mutate(accurate = 1*(model_pred == result_binary))
sum(test1$accurate)/nrow(test1)

ROCRpred <- prediction(test_prediction, test1$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

############# Predict (deltas) using split 50.00% / 50.00% (random) ##############
## accuracy: 78.698.... 77.490.... 78.634.... 75.659.... 78.116

train1 <- train[,100:119]
# View(train1)

train_model <- train1[sample(nrow(train1), round(0.5 * nrow(train1)), replace = FALSE), ]
test_model <- train1[-as.numeric(rownames(train_model)),]

lr_model <- glm(result ~., family = "binomial", data = train_model)
lr_model
summary(lr_model)

test_prediction <- predict(lr_model, test_model, type = 'response')
test_prediction


ROCRpred <- prediction(test_prediction, test_model$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc


################################################################
#         Model 4 - Ewan's variables (reduced model)           #
################################################################

## accuracy: 76.633.... 79.545.... 77.800.... 79.883.... 77.647

dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)
# View(dataset)

train <- dataset[sample(nrow(dataset), round(0.5 * nrow(dataset)), replace = FALSE), ]
test <- dataset[-as.numeric(rownames(train)),]

# run this to calculate log loss; not run for building the model for 2017 prediction
train <- dataset[which(dataset$season < 2014),]
# View(train)
test <- dataset[which(dataset$season >= 2014),]
# View(test)

lr_model <- glm(result ~ team1_seed + team2_seed + team1_blockpct + team2_blockpct + team1_oppf3grate + team2_oppf3grate + team1_pt_team_season_wins + team2_pt_team_season_wins, family = "binomial", data = train)
lr_model
summary(lr_model)

test$prob <- predict(lr_model, test, type = 'response')
test
table <- data.frame(test)
# View(table)

test <- test %>% mutate(model_pred = 1*(prob >= .50) + 0, result_binary = 1*(result == 1) + 0)

test <- test %>% mutate(accurate = 1*(model_pred == result_binary))
sum(test$accurate)/nrow(test_model)

# write to csv to calculate the log loss for 2014-2016
write.csv(table, file = "2014-2016_Predictions.csv",row.names=FALSE)

ROCRpred <- prediction(test, test$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

################################################################
#    Model 5 - Ewan's variables (reduced model) using deltas   #
################################################################

## accuracy: 77.766.... 77.768.... 77.359.... 77.684.... 79.547

dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)
# View(dataset)

train <- dataset

train$seed_delta <- train$team1_seed - train$team2_seed
train$blockpct_delta <- train$team1_blockpct - train$team2_blockpct
train$oppf3grate_delta <- train$team1_oppf3grate - train$team2_oppf3grate
train$pt_team_season_wins__delta <- train$team1_pt_team_season_wins - train$team2_pt_team_season_wins

train1 <- train[,100:104]
# View(train1)

train_model <- train1[sample(nrow(train1), round(0.5 * nrow(train1)), replace = FALSE), ]
test_model <- train1[-as.numeric(rownames(train_model)),]

lr_model <- glm(result ~., family = "binomial", data = train_model)
lr_model
summary(lr_model)

test_model$prob <- predict(lr_model, test_model, type = "response")
# View(test_model)

test_model <- test_model %>% mutate(model_pred = 1*(prob >= .50) + 0, result_binary = 1*(result == 1) + 0)

test_model <- test_model %>% mutate(accurate = 1*(model_pred == result_binary))
sum(test_model$accurate)/nrow(test_model)


ROCRpred <- prediction(test_prediction, test_model$result)
acc <- performance(ROCRpred, measure = "auc")
acc <- acc@y.values[[1]]
acc

################################################################
#      Model 6 - Variables selected using Excel analysis       #
################################################################

dataset <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, stringsAsFactors = F)
# View(dataset)

train <- dataset[sample(nrow(dataset), round(0.5 * nrow(dataset)), replace = FALSE), ]
test <- dataset[-as.numeric(rownames(train)),]

# run this to calculate log loss; not run for building the model for 2017 prediction
train <- dataset[which(dataset$season < 2014),]
# View(train)
test <- dataset[which(dataset$season >= 2014),]
# View(test)

lr_model <- glm(result ~ team1_seed + team2_seed + team1_blockpct + team2_blockpct + team1_pt_school_ncaa + team2_pt_school_ncaa
                + team1_pt_overall_ncaa + team2_pt_overall_ncaa + team1_pt_school_s16 + team2_pt_school_s16 + team1_pt_overall_s16 
                + team2_pt_overall_s16 + team1_pt_school_ff + team2_pt_school_ff + team1_pt_overall_ff + team2_pt_overall_ff  
                + team1_pt_career_school_wins + team2_pt_career_school_wins + team1_pt_career_school_losses + team2_pt_career_school_losses  
                + team1_pt_career_overall_losses + team2_pt_career_overall_losses + team1_pt_team_season_losses  
                + team2_pt_team_season_losses + team1_pt_team_season_wins + team2_pt_team_season_wins + team1_oppblockpct + team2_oppblockpct 
                + team1_oppf3grate + team2_oppf3grate, family = "binomial", data = train)
lr_model
summary(lr_model)

test$prob <- predict(lr_model, test, type = 'response')
# View(test)

test <- test %>% mutate(model_pred = 1*(prob >= .50) + 0, result_binary = 1*(result == 1) + 0)

test <- test %>% mutate(accurate = 1*(model_pred == result_binary))
sum(test$accurate)/nrow(test)
