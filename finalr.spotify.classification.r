library(dlookr)
library(dplyr)
library(dbscan)
library(meanShiftR)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)

data.raw=read.csv("wranglingFinalData.csv")


data.raw %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

data.clean <- data.raw
data.tree <- data.clean
data <-  data.raw

hist(data.tree$danceability,main = "Histogram of Danceability")
hist(data.tree$track_popularity, main = "Histogram of Popularity")


col=c(4,12:24)
cordata=data[,col]
datacor=cor(cordata)
library(corrplot)
corrplot(datacor, type="upper")




precentile.cut <- .75
data.tree <- mutate(data.tree, dancebin = factor(case_when(danceability >= precentile.cut ~ "H",
                                                                      TRUE                ~ "M")))
index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.80)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

cut = 86
data.tree <- mutate(data.tree, popbin = factor(case_when(track_popularity >= cut ~ "H",
                                                         #track_popularity < cut ~ "M",
                                                           TRUE                ~ "M")))
# cut.hi <-  .70
# cut.trash <- .35
# data.tree.3cut <- mutate(data.tree, dancebin = factor(case_when(danceability >= cut.hi ~ "H",
#                                                                  danceability < cut.trash ~ "L",
#                                                                  #danceability < cut.trash ~ "L")))
 #                                                                TRUE                ~ "M")))
data.tree %>% count(popbin)


index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.80)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

data.tree.train %>% count(popbin)

# tree.beta.fit = rpart(parms=parms, control = 0.001, data=data.tree.train.popular, data.tree.train.popular$danceability ~   + energy + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo )
set.seed(13603433)
#key + loudness + acousticness	+ instrumentalness + liveness +
col=c(12:26)
data.tree.train.sub <- data.tree.train[,col] 
tree.spotify.1 <- rpart(danceability ~ energy +  danceability  + loudness + acousticness + key + track_popularity + liveness + valence + 	tempo 	,
                        data = data.tree.train.sub, method = "class", subset = index.collection)
cp_choose <- tree.spotify.1$cptable[,1][which.min(tree.spotify.1$cptable[,4])]
tree.pruned <- prune.rpart(tree.spotify.1, cp_choose)
tree.pred <- predict(tree.pruned, newdata = data.tree.test , type = "class")
confusion.matrix <- table(tree.pred, data.tree.test$popbin)
confusion.matrix
tree.pruned
tree.spotify.1

sum(diag(confusion.matrix)) / sum(confusion.matrix)  # the % accuracy on the test set. 
windows(20,20)
rpart.plot(tree.pruned, main="Popular Song define as track_popularity >= 86")
prp(tree.pruned,varlen=15)	



hist(data.tree.train.popular$pop)
















#Use with ctree
library(party)
cut <- .80
data.tree <- mutate(data.tree, dancebin = factor(case_when(danceability >= cut ~ "H",
                                                                       #danceability < cut ~ "M",
                                                                       TRUE                ~ "M")))

fit <- ctree(dancebin ~ energy +  danceability  + loudness + acousticness,    
             data=data.tree.train)
print(fit)
windows(20,20)
plot(fit, main="Popularity: Cutoff = 80%")
predictions <- predict(fit, data.tree.test)





set.seed(13603433)
index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.80)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]
#key + loudness + acousticness	+ instrumentalness + liveness +
tree.spotify.1 <- rpart(playlist_genre ~  energy +   valence + key + loudness + acousticness	+ instrumentalness + liveness +	tempo,
                        data = data.tree.train, method = "class", subset = index.collection)
cp_choose <- tree.spotify.1$cptable[,1][which.min(tree.spotify.1$cptable[,4])]
tree.pruned <- prune.rpart(tree.spotify.1, cp_choose)
tree.pred <- predict(tree.pruned, newdata = data.tree.test , type = "class")
confusion.matrix <- table(tree.pred, data.tree.test$isMale)
confusion.matrix

sum(diag(confusion.matrix)) / sum(confusion.matrix)  # the % accuracy on the test set. 
windows(20,20)
rpart.plot(tree.spotify.1)
prp(tree.spotify.1,varlen=15)	



#+ key + loudness + acousticness	+ instrumentalness + liveness +
fit <- ctree(dancebin ~    tempo + key,    
             data=data.tree.train)
print(fit)
windows(20,20)
plot(fit, main="Popbin")
predictions <- predict(fit, data.tree.test)
confusion.matrix <- table(predictions, data.tree.test$isMale)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix) 


































#########################################################################################################################
#########################################################################################################################
# #str(data.tree.train)
# tree.beta.fit = rpart(data=data.tree.train, data.tree.train$track_popularity ~ .)
# fancyRpartPlot(tree.beta.fit, caption = 'Popular')
# #tree.beta.g = rpart(data=data.tree.train, data.tree.train$playlist_genre ~ .)
# #tree.beta.s = rpart(data=data.tree.train, data.tree.train$playlist_subgenre ~ .)
# 
# data.tree.train %>% count(isMale)
# data.tree.train.popular <- data.tree.train
# 
# #data.tree.train.popular$popbin <- factor('H')
# cut <- 90
# data.tree.train.popular <- mutate(data.tree.train.popular, popbin = factor(case_when(track_popularity >= cut ~ "H",
#                                                                                      track_popularity < cut ~ "M",
#                                              TRUE                ~ "M")))
# 
# hist(data.tree.train.popular$danceability)
# cut <- .95
# data.tree.train.popular <- mutate(data.tree.train.popular, dancebin = factor(case_when(danceability >= cut ~ "H",
#                                                                                        danceability < cut ~ "M",
#                                                                                      TRUE                ~ "M")))
# 
# data.tree.train.popular %>% count(dancebin)
# #data.tree.train.popular$popbin[which(data.tree.train.popular$track_popularity >= 90)] <- factor('H')
# 
# # list <- which(data.tree.train.popular$track_popularity < 90)
# # list
# # data.tree.train.popular$popbin[1] #<- factor('M')
# # data.tree.train.popular$popbin[1] = factor('M')
# # data.tree.train.popular %>% count(popbin)
# # data.tree.train.popular %>% count(popbin)
# 
# #data.tree.train.popular$popbin[which(data.tree.train.popular$track_popularity < 40)] <- "Low"
# 
# 
# data.tree.train.popular %>% count(isMale)
# parms = list(loss = matrix(c(0, 1, 1, 0), ncol = 2))
# tree.beta.fit = rpart(parms=parms, data=data.tree.train.popular, data.tree.train.popular$isMale ~ loudness )
# prp(tree.beta.fit)					# Will plot the tree
# prp(tree.beta.fit,varlen=15)				# Shorten variable names
# windows(20,20)
# rpart.plot(tree.beta.fit)
# printcp(tree.beta.fit)
# plotcp(tree.beta.fit)
# tree.beta.fit.pruned <- prune(tree.beta.fit,.01)
# 
# prp(tree.beta.fit.pruned)					# Will plot the tree
# prp(tree.beta.fit.pruned,varlen=15)	
# 
# # Interatively prune the tree
# #new.tree.1 <- prp(tree.beta.fit,snip=TRUE)$obj # interactively trim the tree
# #prp(new.tree.1) # display the new tree
# # 
# # data.tree.train.duration <- data.tree.train
# # data.tree.train.duration$durbin <- data.tree.train.duration$duration_ms
# # data.tree.train.duration$durbin <- "High"
# # data.tree.train.duration$durbin[which(data.tree.train.duration$duration_ms < 300000)] <- "Mid"
# # data.tree.train.duration %>% count(durbin)
# 
# tree.beta.fit = rpart(parms=parms, control = 0.001, data=data.tree.train.popular, data.tree.train.popular$danceability ~   + energy + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo )
# 
# printcp(tree.beta.fit)
# plotcp(tree.beta.fit)
# tree.beta.fit.pruned <- prune(tree.beta.fit,.01)
# 
# prp(tree.beta.fit.pruned)					# Will plot the tree
# prp(tree.beta.fit.pruned,varlen=15)				# Shorten variable names
# 
# 
# tree.beta.fit = rpart(parms=parms, control = 0.001, data=data.tree.train.popular, data.tree.train.popular$energy ~   +  danceability  + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo )
# printcp(tree.beta.fit)
# plotcp(tree.beta.fit)
# tree.beta.fit.pruned <- prune(tree.beta.fit,.011215)
# 
# prp(tree.beta.fit.pruned)					# Will plot the tree
# prp(tree.beta.fit.pruned,varlen=15)				# Shorten variable names
# 
# 
# data.tree.train.popular <- data.tree.train
# cut <-  90
# data.tree.train.popular <- mutate(data.tree.train.popular, popbin = factor(case_when(track_popularity >= cut ~ "H",
#                                                                                      track_popularity < cut ~ "M",                                                                                   
#                                                                                      TRUE                ~ "M")))
# data.tree.train.popular %>% count(popbin)
# 
# rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
#   #Calculates rmse for a regression decision tree
#   #Arguments:
#   # testing - test data set
#   # target  - target variable (length 1 character vector)
#   yhat <- predict(model_obj, newdata = testing)
#   actual <- testing[[target]]
#   sqrt(mean((yhat-actual)^2))
# }
# 
# 
# library(party)
# #+ key + loudness + acousticness
# fit <- ctree(danceability ~ energy + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo    ,   data=data.tree.train.popular)
# windows(20,20)
# plot(fit, main="Popbin")
# mse <- rmse_reg(fit,data.tree.test, "danceability")
# 
# insamp <- predict(fit)
# outsamp<- predict(fit,data.tree.test)
# 
# mspe.in <- rmse_reg(fit, data.tree.train.popular, "danceability")
# mspe.out <- rmse_reg(fit, data.tree.test, "danceability")
# 
# mspe.in
# mspe.out
# 
# library(randomForest)
# fit <- randomForest(popbin ~ energy +  danceability  +  loudness,   data=data.tree.train.popular)
# print(fit) # view results
# importance(fit) # importance of each predictor
# plot(fit, main="Conditional Inference Tree for Popularity Bin")
# 

# 
# ####################################################################################################
# # train a model using our training data
# model <- xgboost(data = as.matrix(data.tree.train.popular[,12:13]),
#                  label=data.tree.train.popular$track_popularity,
#                  nround = 2 # max number of boosting iterations
#                   # the objective function
# )
# 
# dtrain <- xgb.DMatrix(data = as.matrix(data.tree.train.popular[,12:13]), label= data.tree.train.popular$track_popularity)
# model_tuned <- xgboost(data = dtrain, # the data           
#                        max.depth = 3, # the maximum depth of each decision tree
#                        nround = 10, # number of boosting rounds
#                        early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
#                       # objective = "binary:logistic", # the objective function
#                        #scale_pos_weight = negative_cases/postive_cases, # control for imbalanced classes
#                        gamma = 1) # add a regularization term
# 
# # generate predictions for our held-out testing data
# pred <- predict(model, as.matrix(data.tree.train[,12:13]))
# 
# # get & print the classification error
# err <- mean(as.numeric(pred > 0.5) != data.tree.train$danceability)
# print(paste("test-error=", err))
# 
# xgb.plot.tree(model = model)
# xgb.plot.
# colnames(data.tree.train[,12:15])
