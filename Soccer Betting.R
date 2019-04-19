library("RSQLite")
library(rattle)
library(XML)
library(dplyr)
library(keras)
library(rpart)
library(xgboost)
library(ramify)
library(caret)

library(rpart)
library(rpart.plot)
library(randomForest)
library(kernlab)
library(ggbiplot)

setwd("C:\\Users\\Jiawei\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA")

##### connect to db #####
con <- dbConnect(drv=RSQLite::SQLite(), dbname="soccer\\database.sqlite")
tables <- dbListTables(con)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

match <- lDataFrames[[3]]
playerAttributes <- lDataFrames[[5]]

###### MATCH TABLE ANALYSIS #####
###### CONVERT XML DATA TO MEANINGFUL VALUES #####
match$goal_qty <- match$home_team_goal + match$away_team_goal
match$shot_on_qty <- NA
match$shot_off_qty <- NA
match$foul_qty <- NA
match$cross_qty <- NA
match$corner_qty <- NA
match$home_possesion_half <- NA
match$home_possesion_full <- NA
match$yellow_card_qty <- NA
match$red_card_qty <- NA

count = 1
while (count <= nrow(match))
{
  if(!(is.na(match$shoton[count])))
  {
    xml_data <- xmlToList(match$shoton[count])
    match$shot_on_qty[count] <- length(xml_data)
    
    xml_data <- xmlToList(match$shotoff[count])
    match$shot_off_qty[count] <- length(xml_data)
    
    xml_data <- xmlToList(match$foulcommit[count])
    match$foul_qty[count] <- length(xml_data)
    
    xml_data <- xmlToList(match$cross[count])
    match$cross_qty[count] <- length(xml_data)
    
    xml_data <- xmlToList(match$corner[count])
    match$corner_qty[count] <- length(xml_data)
    
    xml_data <- xmlToList(match$possession[count])
    if (length(xml_data) >= 1)
    {
      i = 1
      while (i <= length(xml_data))
      {
        if (try(as.numeric(xml_data[i]$value$elapsed) <= 47) && try(as.numeric(xml_data[i]$value$elapsed) >= 40))
        {
          try(match$home_possesion_half[count] <- xml_data[i]$value$homepos)
        }
        else if (try(as.numeric(xml_data[i]$value$elapsed) <= 90) && try(as.numeric(xml_data[i]$value$elapsed) >= 82))
        {
          try(match$home_possesion_full[count] <- xml_data[i]$value$homepos)
        }
        i = i + 1
      }
    }
    # to address scenarios which do not record time at 45 or 90
    if (length(xml_data) == 4)
    {
      try(match$home_possesion_half[count] <- xml_data[2]$value$homepos)
      try(match$home_possesion_full[count] <- xml_data[4]$value$homepos)
    }
    
    xml_data <- xmlToList(match$card[count])
    i = 1
    count_red = 0
    count_yellow = 0
    while (i <= length(xml_data))
    {
      if(xml_data[i]$value$comment == "y")
      {
        count_yellow = count_yellow + 1
      }
      else
      {
        count_red = count_red + 1
      }
      
      i = i + 1
    }
    match$yellow_card_qty[count] <- count_yellow
    match$red_card_qty[count] <- count_red
    
    print(count)
  }
  count = count + 1
}

backup <- match

# change zero to NA because the xml data seems to be wrong
match$shot_on_qty[match$shot_on_qty == 0] <- NA
match$shot_off_qty[match$shot_off_qty == 0] <- NA
match$foul_qty[match$foul_qty == 0] <- NA
match$cross_qty[match$cross_qty == 0] <- NA
match$corner_qty[match$corner_qty == 0] <- NA


##### PLAYER ATTRIBUTES #####
playerAttributes$player_api_id <- as.factor(playerAttributes$player_api_id)
playerAttributes_avg <- playerAttributes %>% group_by(player_api_id) %>% dplyr::summarise(overall_rating = mean(overall_rating, na.rm = TRUE),
                                                                          potential = mean(potential, na.rm = TRUE),
                                                                          crossing = mean(crossing, na.rm = TRUE),
                                                                          finishing = mean(finishing, na.rm = TRUE),
                                                                          heading_accuracy = mean(heading_accuracy, na.rm = TRUE),
                                                                          short_passing = mean(short_passing, na.rm = TRUE),
                                                                          volleys = mean(volleys, na.rm = TRUE),
                                                                          dribbling = mean(dribbling, na.rm = TRUE),
                                                                          curve = mean(curve, na.rm = TRUE),
                                                                          free_kick_accuracy = mean(free_kick_accuracy, na.rm = TRUE),
                                                                          long_passing = mean(long_passing, na.rm = TRUE),
                                                                          ball_control = mean(ball_control, na.rm = TRUE),
                                                                          acceleration = mean(acceleration, na.rm = TRUE),
                                                                          sprint_speed = mean(sprint_speed, na.rm = TRUE),
                                                                          agility = mean(agility, na.rm = TRUE),
                                                                          reactions = mean(reactions, na.rm = TRUE),
                                                                          balance = mean(balance, na.rm = TRUE),
                                                                          shot_power = mean(shot_power, na.rm = TRUE),
                                                                          jumping = mean(jumping, na.rm = TRUE),
                                                                          stamina = mean(stamina, na.rm = TRUE),
                                                                          strength = mean(strength, na.rm = TRUE),
                                                                          long_shots = mean(long_shots, na.rm = TRUE),
                                                                          aggression = mean(aggression, na.rm = TRUE),
                                                                          interceptions = mean(interceptions, na.rm = TRUE),
                                                                          positioning = mean(positioning, na.rm = TRUE),
                                                                          vision = mean(vision, na.rm = TRUE),
                                                                          penalties = mean(penalties, na.rm = TRUE),
                                                                          marking = mean(marking, na.rm = TRUE),
                                                                          standing_tackle = mean(standing_tackle, na.rm = TRUE),
                                                                          sliding_tackle = mean(sliding_tackle, na.rm = TRUE),
                                                                          gk_diving = mean(gk_diving, na.rm = TRUE),
                                                                          gk_handling = mean(gk_handling, na.rm = TRUE),
                                                                          gk_kicking = mean(gk_kicking, na.rm = TRUE),
                                                                          gk_positioning = mean(gk_positioning, na.rm = TRUE),
                                                                          gk_reflexes = mean(gk_reflexes, na.rm = TRUE))

##### MERGE AND RENAME THE COLUMNS #####

matchPlayer <- (merge(playerAttributes_avg, match, by.x = "player_api_id",  by.y = "home_player_1", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_2", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_3", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_4", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_5", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_6", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_7", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_8", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_9", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_10", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "home_player_11", all.y = TRUE))

cols <- colnames(matchPlayer)
i = 1
while (i < 37) 
{
  names(matchPlayer)[seq(i,ncol(matchPlayer),36)[1:11]] <- paste(cols[i], "home", 1:11, sep = "_")
  i = i + 1
}

matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id",  by.y = "away_player_1", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_2", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_3", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_4", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_5", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_6", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_7", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_8", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_9", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_10", all.y = TRUE))
matchPlayer <- (merge(playerAttributes_avg, matchPlayer, by.x = "player_api_id", by.y = "away_player_11", all.y = TRUE))

cols <- colnames(matchPlayer)
i = 1
while (i < 37) 
{
  names(matchPlayer)[seq(i,ncol(matchPlayer),36)[1:11]] <- paste(cols[i], "away", 1:11, sep = "_")
  i = i + 1
}


##### SUM UP RATINGS #####

# AWAY
cols <- colnames(matchPlayer)[2:36]
cols <- substr(cols, 1, nchar(cols)-1)
cols <- paste(cols, "Sum", sep="")

count = 0
while(count < 35)
{
  i=0
  temp <- 0
  while (i < 11)
  {
    temp = temp + matchPlayer[count+2+36*i]
    i = i + 1
  }
  count = count + 1
  matchPlayer[,cols[count]] <- temp
}

# HOME
cols <- colnames(matchPlayer)[398:432]
cols <- substr(cols, 1, nchar(cols)-1)
cols <- paste(cols, "Sum", sep="")

count = 0
while(count < 35)
{
  i=0
  temp <- 0
  while (i < 11)
  {
    temp = temp + matchPlayer[count+398+36*i]
    i = i + 1
  }
  count = count + 1
  matchPlayer[,cols[count]] <- temp
}

# DIFFERENCE
cols <- colnames(matchPlayer)[896:930]
cols <- substr(cols, 1, nchar(cols)-9)
cols <- paste(cols, "Diff", sep="_")

count = 0
while(count < 35)
{
  temp <- matchPlayer[,931+count] - matchPlayer[,896+count]
  count = count + 1
  matchPlayer[,cols[count]] <- temp
}


##### REMOVAL OF UNWANTED COLUMNS #####
matchPlayerReduced <- matchPlayer[, 793:ncol(matchPlayer)]
matchPlayerReduced <- matchPlayerReduced[, -grep(pattern = "^home_player", colnames(matchPlayerReduced))]
matchPlayerReduced <- matchPlayerReduced[,-grep(pattern = "^away_player", colnames(matchPlayerReduced))]

# REMOVE COLS WITH XML DATA SINCE THEY HAVE ALREADY BEEN CONVERTED
matchPlayerReduced <- matchPlayerReduced[, -(12:19)]
matchPlayerReduced$id <- NULL

# MODIFYING STRUCTURE OF COLUMNS
matchPlayerReduced$season <- substr(matchPlayerReduced$season, 1, 4)
matchPlayerReduced$country_id <- as.factor(matchPlayerReduced$country_id)
matchPlayerReduced$league_id <- as.factor(matchPlayerReduced$league_id)
matchPlayerReduced$home_team_api_id <- as.factor(matchPlayerReduced$home_team_api_id)
matchPlayerReduced$away_team_api_id <- as.factor(matchPlayerReduced$away_team_api_id)
matchPlayerReduced$season <- as.numeric(matchPlayerReduced$season)

?confusionMatrix()

##### HANDLING MISSING VALUES #####

# MANAGING BETTING ODDS
cols <- colnames(matchPlayerReduced)

full_bet_home_win <- data.frame(matchPlayerReduced$season)
full_bet_home_draw <- data.frame(matchPlayerReduced$season)
full_bet_home_lose <- data.frame(matchPlayerReduced$season)

count = 0
while(count < 10)
{
  full_bet_home_win[, cols[count*3 + 11]] <- matchPlayerReduced[, count*3 + 11]
  full_bet_home_draw[, cols[count*3 + 12]] <- matchPlayerReduced[, count*3 + 12]
  full_bet_home_lose[, cols[count*3 + 13]] <- matchPlayerReduced[, count*3 + 13]
  count = count + 1
}

avgBettingOdds_win <- rowMeans(full_bet_home_win[, 2:11], na.rm = TRUE)
avgBettingOdds_draw <- rowMeans(full_bet_home_draw[, 2:11], na.rm = TRUE)
avgBettingOdds_lose <- rowMeans(full_bet_home_lose[, 2:11], na.rm = TRUE)

matchPlayerReduced_NoMissing <- matchPlayerReduced

count = 0
while (count < 10)
{
  matchPlayerReduced_NoMissing[, count*3 + 11] <- ifelse(is.na(matchPlayerReduced[, count*3 + 11]), avgBettingOdds_win, matchPlayerReduced[, count*3 + 11])
  matchPlayerReduced_NoMissing[, count*3 + 12] <- ifelse(is.na(matchPlayerReduced[, count*3 + 12]), avgBettingOdds_win, matchPlayerReduced[, count*3 + 12])
  matchPlayerReduced_NoMissing[, count*3 + 13] <- ifelse(is.na(matchPlayerReduced[, count*3 + 13]), avgBettingOdds_win, matchPlayerReduced[, count*3 + 13])
  count = count + 1
}


# MANAGING TEAM SCORES
i = 0 
while (i < 35)
{
  count = 1
  while(count <= nrow(matchPlayerReduced_NoMissing))
  {
    # IF ONE IS MISSING, WE SET IT TO THE OTHER TEAM
    # IF BOTH ARE MISSING, WE SET BOTH TO ZERO
    if(is.na(matchPlayerReduced_NoMissing[count, i + 51]) + is.na(matchPlayerReduced_NoMissing[count, i + 86]) == 2) 
    {
      matchPlayerReduced_NoMissing[count, i + 51] <- 0 
      matchPlayerReduced_NoMissing[count, i + 86] <- 0
      matchPlayerReduced_NoMissing[count, i + 121] <- 0
    }
    else if (is.na(matchPlayerReduced_NoMissing[count, i + 51]) + is.na(matchPlayerReduced_NoMissing[count, i + 86]) == 1) 
    {
      if(is.na(matchPlayerReduced_NoMissing[count, i + 51]) == 1)
      {
        matchPlayerReduced_NoMissing[count, i + 51] <- matchPlayerReduced_NoMissing[count, i + 86]
        matchPlayerReduced_NoMissing[count, i + 121] <- 0
      }
      else
      {
        matchPlayerReduced_NoMissing[count, i + 86] <- matchPlayerReduced_NoMissing[count, i + 51]
        matchPlayerReduced_NoMissing[count, i + 121] <- 0
      }
    }
    count = count + 1
  }
  i = i + 1
  print(i)
}

# REMOVE UNAVOIDABLE NA
matchPlayerReduced_NoMissing <- matchPlayerReduced_NoMissing[!is.na(matchPlayerReduced_NoMissing$B365H),]

##### WIN/LOSE MODEL #####
modelWinLose <- matchPlayerReduced_NoMissing
modelWinLose$date <- NULL
modelWinLose <- modelWinLose[, -(40:49)]
modelWinLose$Home_win <- ifelse(modelWinLose$home_team_goal > modelWinLose$away_team_goal, "Win", ifelse(modelWinLose$home_team_goal < modelWinLose$away_team_goal, "Lose", "Draw"))
modelWinLose <- modelWinLose[, -(8:9)]
modelWinLose$match_api_id <- NULL
modelWinLose$Home_win <- as.factor(modelWinLose$Home_win)
modelWinLose <- modelWinLose[, -c(1,2,5,6)]

set.seed(5678)
split <- floor(nrow(modelWinLose)*0.75)
train_ind <- sample(seq_len(nrow(modelWinLose)), size = split)
modelWinLose_Train <- modelWinLose[train_ind,]
modelWinLose_Test <- modelWinLose[-train_ind,]

# PCA FOR NEURAL NETWORK ONLY
PCA_modelWinLose_Train <- modelWinLose_Train[, 1:137]
PCA_modelWinLose_Test <- modelWinLose_Test[, 1:137]

PCA_prin_comp <- prcomp(PCA_modelWinLose_Train, scale. = TRUE)
PCA_std_dev <- PCA_prin_comp$sdev
PCA_var <- PCA_std_dev ^ 2
PCA_proportion_var <- PCA_var / sum(PCA_var)
sum(PCA_proportion_var[1:25])

# scree plot
plot(PCA_proportion_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

# cumulative scree plot
plot(cumsum(PCA_proportion_var), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# biplot
g <- ggbiplot(PCA_prin_comp, obs.scale = 1, var.scale = 1, 
              groups = modelWinLose_Train$Home_win, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
ggsave(filename="Biplot.png", plot = last_plot(), path = "Plots\\", width = 100, height = 20, units = "cm", limitsize = FALSE, dpi = 800)


PCA_modelWinLose_Train <- data.frame(PCA_prin_comp$x)
PCA_modelWinLose_Train <- PCA_modelWinLose_Train[, 1:25]
PCA_modelWinLose_Train$Home_win <- modelWinLose_Train$Home_win

PCA_modelWinLose_Test <- predict(PCA_prin_comp, newdata = PCA_modelWinLose_Test)
PCA_modelWinLose_Test <- as.data.frame(PCA_modelWinLose_Test)
PCA_modelWinLose_Test <- PCA_modelWinLose_Test[, 1:25]
PCA_modelWinLose_Test$Home_win <- modelWinLose_Test$Home_win


nn_modelWinLose_Train_X <- PCA_modelWinLose_Train[1:25]
nn_modelWinLose_Train_Y <- PCA_modelWinLose_Train[26]
nn_modelWinLose_Test_X <- PCA_modelWinLose_Test[1:25]
nn_modelWinLose_Test_Y <- PCA_modelWinLose_Test[26]

nn_modelWinLose_Train_Y$Home_win <- ifelse(nn_modelWinLose_Train_Y$Home_win == "Win", 2, ifelse(nn_modelWinLose_Train_Y$Home_win == "Draw", 1, 0))
nn_modelWinLose_Test_Y$Home_win <- ifelse(nn_modelWinLose_Test_Y$Home_win == "Win", 2, ifelse(nn_modelWinLose_Test_Y$Home_win == "Draw", 1, 0))

nn_modelWinLose_Train_X <- as.matrix(nn_modelWinLose_Train_X)
nn_modelWinLose_Test_X <- as.matrix(nn_modelWinLose_Test_X)
nn_modelWinLose_Train_Y <- as.matrix(nn_modelWinLose_Train_Y)
nn_modelWinLose_Test_Y <- as.matrix(nn_modelWinLose_Test_Y)

dimnames(nn_modelWinLose_Train_X) <- NULL
dimnames(nn_modelWinLose_Test_X) <- NULL
dimnames(nn_modelWinLose_Train_Y) <- NULL
dimnames(nn_modelWinLose_Test_Y) <- NULL

nn_modelWinLose_Train_Y <- to_categorical(nn_modelWinLose_Train_Y)
nn_modelWinLose_Test_Y <- to_categorical(nn_modelWinLose_Test_Y)


##### BOOSTING #####
# ACCURACY = 0.5051
# ACCURACY_PCA = 0.5012
boost_modelWinLose_Train_Y <- modelWinLose_Train[138]
boost_modelWinLose_Test_Y <- modelWinLose_Test[138]
boost_modelWinLose_Train_Y$Home_win <- ifelse(boost_modelWinLose_Train_Y$Home_win == "Win", 2, ifelse(boost_modelWinLose_Train_Y$Home_win == "Draw", 1, 0))
boost_modelWinLose_Test_Y$Home_win <- ifelse(boost_modelWinLose_Test_Y$Home_win == "Win", 2, ifelse(boost_modelWinLose_Test_Y$Home_win == "Draw", 1, 0))
boost_modelWinLose_Train_Y <- as.numeric(boost_modelWinLose_Train_Y$Home_win)
boost_modelWinLose_Test_Y <- as.numeric(boost_modelWinLose_Test_Y$Home_win)

params <- list(booster = "gbtree", objective = "multi:softprob", eta=0.3, gamma=0, max_depth=3, 
               colsample_bytree=0.7, "num_class" = 3)

boost_winLose <- xgboost(param = params, data = nn_modelWinLose_Train_X, label = boost_modelWinLose_Train_Y, nrounds = 100)

pred_boosting <- predict(object=boost_winLose, nn_modelWinLose_Test_X)
pred_boosting <- matrix(pred_boosting, nrow = 5650, byrow = TRUE)
pred_boosting <- argmax(pred_boosting)
pred_boosting <- pred_boosting - 1

t_boosting <- table(boost_modelWinLose_Test_Y, pred_boosting)
confusionMatrix(t_boosting)

##### SINGLE DEICISON TREE MODEL #####
# ACCURACY = 0.5235
# ACCURACY_PCA = 0.5227
myTree <- rpart(Home_win ~., data = modelWinLose_Train, method = "class", 
                parms = list(split = "information"), 
                control = rpart.control(minsplit = 2, minbucket = 20, maxdepth = 10, 
                                        usesurrogate = 0, maxsurrogate = 0, cp = 0.01), model = TRUE)
rpart.plot(myTree)
fancyRpartPlot(myTree)

pred <- predict(object=myTree, modelWinLose_Test, type="class")
t <- table(modelWinLose_Test$Home_win,pred)
confusionMatrix(t)
print(myTree)

?rpart()

##### RANDOM FOREST MODEL #####
# ACCURACY = 0.5228
# ACCURACY_PCA = 0.5257

forest_winLose <- randomForest(Home_win ~ ., data = modelWinLose_Train, importance = TRUE)

pred_forest <- predict(object = forest_winLose, modelWinLose_Test, type = "class")
t_forest <- table(modelWinLose_Test$Home_win,pred_forest)
confusionMatrix(t_forest)

forest_winLose$confusion
forest_winLose$importance
forest_winLose$err.rate
forest_winLose$votes
forest_winLose$classes

##### NEURAL NETWORK MODEL #####
# ACCURACY = 0.4557
# ACCURACY_PCA_50_50 = 0.5077876
# ACCURACY_PCA_50_18 = 0.520708
# accuracy_PCA_25_50 = 0.5219469
# accuracy_PCA_25_18 = 0.520354
nn_model <- keras_model_sequential() 

nn_model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = c(25)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

nn_model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history_nn_model <- nn_model %>% fit(
  nn_modelWinLose_Train_X, nn_modelWinLose_Train_Y, 
  epochs = 50, batch_size = 5, 
  validation_split = 0.2
)

nn_model %>% evaluate(nn_modelWinLose_Test_X, nn_modelWinLose_Test_Y)
nn_training_records <- cbind(history_nn_model$metrics$val_loss, history_nn_model$metrics$val_acc, history_nn_model$metrics$loss, history_nn_model$metrics$acc)

pred_nn <- nn_model %>% predict_classes(nn_modelWinLose_Test_X)
pred_nn <- as.matrix(pred_nn)

t_nn <- table(boost_modelWinLose_Test_Y, pred_nn)
confusionMatrix(t_nn)

##### SUPPORT VECTOR MACHINES #####

# C = 1, sigma = 0.0001
start.time <- Sys.time()
svm_winLose_1_0.0001 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 1, prob.model = TRUE, kpar = list(sigma=0.0001))
pred_svm_rbf_1_0.0001 <- predict(object=svm_winLose_1_0.0001, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_1_0.0001)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 1, sigma = 0.001
start.time <- Sys.time()
svm_winLose_1_0.001 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 1, prob.model = TRUE, kpar = list(sigma=0.001))
pred_svm_rbf_1_0.001 <- predict(object=svm_winLose_1_0.001, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_1_0.001)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 1, sigma = 0.01
start.time <- Sys.time()
svm_winLose_1_0.01 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 1, prob.model = TRUE, kpar = list(sigma=0.01))
pred_svm_rbf_1_0.01 <- predict(object=svm_winLose_1_0.01, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_1_0.01)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 1, sigma = 0.1
start.time <- Sys.time()
svm_winLose_1_0.1 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 1, prob.model = TRUE, kpar = list(sigma=0.1))
pred_svm_rbf_1_0.1 <- predict(object=svm_winLose_1_0.1, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_1_0.1)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 1, sigma = 1
start.time <- Sys.time()
svm_winLose_1_1 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 1, prob.model = TRUE, kpar = list(sigma=1))
pred_svm_rbf_1_1 <- predict(object=svm_winLose_1_1, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_1_1)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 50, sigma = 0.01
start.time <- Sys.time()
svm_winLose_50_0.01 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 50, prob.model = TRUE, kpar = list(sigma=0.01))
pred_svm_rbf_50_0.01 <- predict(object=svm_winLose_50_0.01, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_50_0.01)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# C = 0.02, sigma = 0.01
start.time <- Sys.time()
svm_winLose_0.02_0.01 <- ksvm(Home_win ~., data = modelWinLose_Train, kernel = "rbfdot", C = 0.02 , prob.model = TRUE, kpar = list(sigma=0.01))
pred_svm_rbf_0.02_0.01 <- predict(object=svm_winLose_0.02_0.01, modelWinLose_Test)
t_svm_rbf <- table(modelWinLose_Test$Home_win, pred_svm_rbf_0.02_0.01)
confusionMatrix(t_svm_rbf)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


##### ENSEMBLE MODEL #####
# ACCURACY = 0.5271
ensemble_pred <- data.frame(pred, pred_forest, pred_svm_rbf_1_0.001, pred_boosting, pred_nn)
ensemble_pred$pred_boosting <- ifelse(ensemble_pred$pred_boosting == 2, "Win", ifelse(ensemble_pred$pred_boosting == 1, "Draw", "Lose"))
ensemble_pred$pred_nn <- ifelse(ensemble_pred$pred_nn == 2, "Win", ifelse(ensemble_pred$pred_nn == 1, "Draw", "Lose"))

count = 1
while(count <= ncol(ensemble_pred))
{
  ensemble_pred[,count] <- as.character(ensemble_pred[,count])
  count = count + 1
}

ensemble_pred$final <- NA

count = 1
while (count <= nrow(ensemble_pred))
{
  i = 1
  countWin = 0
  countDraw = 0
  countLose = 0
  while (i <= ncol(ensemble_pred)-1)
  {
    if(ensemble_pred[count, i] == "Win")
      countWin = countWin + 1
    else if (ensemble_pred[count, i] == "Draw")
      countDraw = countDraw + 1
    else
      countLose = countLose + 1
    i = i + 1
  }
  if((countWin > countDraw) & (countWin > countLose))
    ensemble_pred[count, ncol(ensemble_pred)] <- "Win"
  else if ((countDraw > countWin) & (countDraw > countLose))
    ensemble_pred[count, ncol(ensemble_pred)] <- "Draw"
  else if ((countLose > countWin) & (countLose > countDraw))
    ensemble_pred[count, ncol(ensemble_pred)] <- "Lose"
  else
    ensemble_pred[count, ncol(ensemble_pred)] <- ensemble_pred[count, 2]
  count = count + 1
}

pred_ensemble <- as.factor(ensemble_pred$final)
t_ensemble <- table(modelWinLose_Test$Home_win, pred_ensemble)
confusionMatrix(t_ensemble)

##### WINNINGS FROM MODEL PREDICTIONS #####

# IDENTIFY MOST "GENEROUS" BOOKMAKER
cols <- colnames(modelWinLose_Train)
bet_home_win <- data.frame(modelWinLose_Train$Home_win)
bet_home_draw <- data.frame(modelWinLose_Train$Home_win)
bet_home_lose <- data.frame(modelWinLose_Train$Home_win)

count = 0
while(count < 10)
{
  bet_home_win[, cols[count*3 + 3]] <- modelWinLose_Train[, count*3 + 3]
  bet_home_draw[, cols[count*3 + 4]] <- modelWinLose_Train[, count*3 + 4]
  bet_home_lose[, cols[count*3 + 5]] <- modelWinLose_Train[, count*3 + 5]
  count = count + 1
}

colnames(bet_home_win)[1] <- "result"
colnames(bet_home_draw)[1] <- "result"
colnames(bet_home_lose)[1] <- "result"

bet_home_win <- subset(bet_home_win, bet_home_win$result == "Win")
bet_home_draw <- subset(bet_home_draw, bet_home_draw$result == "Draw")
bet_home_lose <- subset(bet_home_lose, bet_home_lose$result == "Lose")

bet_home_win$result <- NULL
bet_home_draw$result <- NULL
bet_home_lose$result <- NULL

bet_home_win <- as.numeric(argmax(bet_home_win))
bet_home_draw <- as.numeric(argmax(bet_home_draw))
bet_home_lose <- as.numeric(argmax(bet_home_lose))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bet_home_win <- Mode(bet_home_win)
bet_home_draw <- Mode(bet_home_draw)
bet_home_lose <- Mode(bet_home_lose)

# MOST GENEROUS BOOKMAKERS
bet_home_win <- cols[(bet_home_win-1)*3 + 3]
bet_home_draw <- cols[(bet_home_draw-1)*3 + 4]
bet_home_lose <- cols[(bet_home_lose-1)*3 + 5]

# COMPUTE WINNINGS
modelWinLose_Test$prediction <- pred_ensemble
modelWinLose_Test$result <- ifelse(modelWinLose_Test$prediction == modelWinLose_Test$Home_win, 1, 0)
modelWinLose_Test$winnings <- 0

count = 1
while (count <= nrow(modelWinLose_Test))
{
  if(modelWinLose_Test[count, ncol(modelWinLose_Test)-1] == 1)
  {
    if(modelWinLose_Test[count, ncol(modelWinLose_Test)-2] == "Win")
      modelWinLose_Test[count, ncol(modelWinLose_Test)] = 100 * modelWinLose_Test[count, bet_home_win]
    else if(modelWinLose_Test[count, ncol(modelWinLose_Test)-2] == "Draw")
      modelWinLose_Test[count, ncol(modelWinLose_Test)] = 100 * modelWinLose_Test[count, bet_home_draw]
    else
      modelWinLose_Test[count, ncol(modelWinLose_Test)] = 100 * modelWinLose_Test[count, bet_home_lose]
  }
  else
    modelWinLose_Test[count, ncol(modelWinLose_Test)] = -100
  
  count = count + 1
}

sum(modelWinLose_Test$winnings)


##### OUTPUT FILES #####
# To generate the tables in csv for quicker analysis/manipulaation of data
write.csv(match, "Output\\match.csv")
write.csv(matchPlayer, "Output\\matchPlayer.csv")
write.csv(playerAttributes_avg, "Output\\playerAttributesAvg.csv")
write.csv(modelWinLose, "Output\\modelWinLose.csv")
write.csv(colnames(modelWinLose), "Output\\matchPlayerColnames.csv")
write.csv(modelWinLose_Train, "Output\\modelWinLoseTrain.csv")
write.csv(modelWinLose_Test, "Output\\modelWinLoseTest.csv")
write.csv(test, "Output\\test.csv")

write.csv(cols, "Output\\cols.csv")
write.csv(matchPlayerReduced, "Output\\matchPlayerReduced.csv")
write.csv(matchPlayerReduced_NoMissing, "Output\\matchPlayerReduced_noMissing.csv")

write.csv(ensemble_pred, "Output\\ensemble_pred.csv")
write.csv(modelWinLose_Test$Home_win, "Output\\test results.csv")
write.csv(nn_training_records, "Output\\nn_training_records.csv")

######

# Odds
# Odds data are collected before the game starts
# reference = https://www.kaggle.com/hugomathien/soccer/discussion/22508#latest-132023
# based on bet365 website, winnings = bet amount * odds
# therefore, probability should be computed as 1 / ( 1 + odds )
# reference = https://mybettingsites.co.uk/learn/betting-odds-explained/


