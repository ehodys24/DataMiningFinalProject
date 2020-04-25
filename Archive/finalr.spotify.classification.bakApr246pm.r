library(dlookr)
library(dplyr)
library(dbscan)
library(meanShiftR)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions

data.raw=read.csv("wranglingFinalData.csv")

doRpartTree <- function(x, response) {
  tree.beta.fit = rpart(data=x, response ~  . )                      
  prp(tree.beta.fit)					# Will plot the tree
  prp(tree.beta.fit,varlen=15)				# Shorten variable names
  
}


data.clean <- data.raw
data.tree <- data.clean

index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.80)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

#data.transformed1 <- data.tree[-index.collection,]

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#str(data.tree.train)
tree.beta.fit = rpart(data=data.tree.train, data.tree.train$track_popularity ~ .)
fancyRpartPlot(tree.beta.fit, caption = 'Popular')
#tree.beta.g = rpart(data=data.tree.train, data.tree.train$playlist_genre ~ .)
#tree.beta.s = rpart(data=data.tree.train, data.tree.train$playlist_subgenre ~ .)

data.tree.train %>% count(isMale)
data.tree.train.popular <- data.tree.train

#data.tree.train.popular$popbin <- factor('H')
cut <- 90
data.tree.train.popular <- mutate(data.tree.train.popular, popbin = factor(case_when(track_popularity >= cut ~ "H",
                                                                                     track_popularity < cut ~ "M",
                                             TRUE                ~ "M")))

hist(data.tree.train.popular$danceability)
cut <- .95
data.tree.train.popular <- mutate(data.tree.train.popular, dancebin = factor(case_when(danceability >= cut ~ "H",
                                                                                       danceability < cut ~ "M",
                                                                                     TRUE                ~ "M")))

data.tree.train.popular %>% count(dancebin)
#data.tree.train.popular$popbin[which(data.tree.train.popular$track_popularity >= 90)] <- factor('H')

# list <- which(data.tree.train.popular$track_popularity < 90)
# list
# data.tree.train.popular$popbin[1] #<- factor('M')
# data.tree.train.popular$popbin[1] = factor('M')
# data.tree.train.popular %>% count(popbin)
# data.tree.train.popular %>% count(popbin)

#data.tree.train.popular$popbin[which(data.tree.train.popular$track_popularity < 40)] <- "Low"


data.tree.train.popular %>% count(isMale)
parms = list(loss = matrix(c(0, 1, 1, 0), ncol = 2))
tree.beta.fit = rpart(parms=parms, data=data.tree.train.popular, data.tree.train.popular$isMale ~ loudness )
prp(tree.beta.fit)					# Will plot the tree
prp(tree.beta.fit,varlen=15)				# Shorten variable names
windows(20,20)
rpart.plot(tree.beta.fit)
printcp(tree.beta.fit)
plotcp(tree.beta.fit)
tree.beta.fit.pruned <- prune(tree.beta.fit,.01)

prp(tree.beta.fit.pruned)					# Will plot the tree
prp(tree.beta.fit.pruned,varlen=15)	

# Interatively prune the tree
#new.tree.1 <- prp(tree.beta.fit,snip=TRUE)$obj # interactively trim the tree
#prp(new.tree.1) # display the new tree
# 
# data.tree.train.duration <- data.tree.train
# data.tree.train.duration$durbin <- data.tree.train.duration$duration_ms
# data.tree.train.duration$durbin <- "High"
# data.tree.train.duration$durbin[which(data.tree.train.duration$duration_ms < 300000)] <- "Mid"
# data.tree.train.duration %>% count(durbin)

tree.beta.fit = rpart(parms=parms, control = 0.001, data=data.tree.train.popular, data.tree.train.popular$danceability ~   + energy + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo )

printcp(tree.beta.fit)
plotcp(tree.beta.fit)
tree.beta.fit.pruned <- prune(tree.beta.fit,.01)

prp(tree.beta.fit.pruned)					# Will plot the tree
prp(tree.beta.fit.pruned,varlen=15)				# Shorten variable names


tree.beta.fit = rpart(parms=parms, control = 0.001, data=data.tree.train.popular, data.tree.train.popular$energy ~   +  danceability  + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo )
printcp(tree.beta.fit)
plotcp(tree.beta.fit)
tree.beta.fit.pruned <- prune(tree.beta.fit,.011215)

prp(tree.beta.fit.pruned)					# Will plot the tree
prp(tree.beta.fit.pruned,varlen=15)				# Shorten variable names


data.tree.train.popular <- data.tree.train
cut <-  90
data.tree.train.popular <- mutate(data.tree.train.popular, popbin = factor(case_when(track_popularity >= cut ~ "H",
                                                                                     track_popularity < cut ~ "M",                                                                                   
                                                                                     TRUE                ~ "M")))
data.tree.train.popular %>% count(popbin)

rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}


library(party)
#+ key + loudness + acousticness
fit <- ctree(danceability ~ energy + key + loudness + acousticness	+ instrumentalness + liveness + valence + 	tempo    ,   data=data.tree.train.popular)
windows(20,20)
plot(fit, main="Popbin")
mse <- rmse_reg(fit,data.tree.test, "danceability")

insamp <- predict(fit)
outsamp<- predict(fit,data.tree.test)

mspe.in <- rmse_reg(fit, data.tree.train.popular, "danceability")
mspe.out <- rmse_reg(fit, data.tree.test, "danceability")

mspe.in
mspe.out

library(randomForest)
fit <- randomForest(popbin ~ energy +  danceability  +  loudness,   data=data.tree.train.popular)
print(fit) # view results
importance(fit) # importance of each predictor
plot(fit, main="Conditional Inference Tree for Popularity Bin")


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
