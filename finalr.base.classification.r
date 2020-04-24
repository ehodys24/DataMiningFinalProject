library(dlookr)
library(dplyr)

data.raw=read.csv("cleandata_final.csv")

data.raw %>% count(listens < 0)

data.clean <- data.raw
data.clean$listens[data.clean$listens < 0] <- 0
data.clean %>% count(listens < 0)

data.transformed <- data.clean
data.transformed$listensBin <- 0 
#data.transformed$listensBin 
data.transformed$listensBin[which(data.transformed$listens < 10000)]  <-  1
data.transformed$listensBin[which(data.transformed$listens < 20000 & data.transformed$listens > 10000)]  <-  2
data.transformed$listensBin[which(data.transformed$listens < 30000 & data.transformed$listens > 20000)]  <-  3
data.transformed$listensBin[which(data.transformed$listens < 40000 & data.transformed$listens > 30000)]  <-  4
data.transformed$listensBin[which(data.transformed$listens < 50000 & data.transformed$listens > 40000)]  <-  5
data.transformed$listensBin[which(data.transformed$listens < 60000 & data.transformed$listens > 50000)]  <-  6

data.transformed$listensBinCat[which(data.transformed$listens < 10000)]  <-  'Low'
data.transformed$listensBinCat[which(data.transformed$listens < 20000 & data.transformed$listens > 10000)]  <-  'Low-Mid'
data.transformed$listensBinCat[which(data.transformed$listens < 30000 & data.transformed$listens > 20000)]  <-  'Mid'
data.transformed$listensBinCat[which(data.transformed$listens < 40000 & data.transformed$listens > 30000)]  <-  'Mid-High'
data.transformed$listensBinCat[which(data.transformed$listens < 50000 & data.transformed$listens > 40000)]  <-  'High'
data.transformed$listensBinCat[which(data.transformed$listens < 60000 & data.transformed$listens > 50000)]  <-  'Ultra'

hist(data.transformed$listensBinCat)
#test <-  data.transformed[, c("listensBin","audio_features1")]

str(data.transformed)
which(colnames(data.transformed)=="genre_top")
which(colnames(data.transformed)=="duration")
which(colnames(data.transformed)=="listens")
which(colnames(data.transformed)=="listensBin")
#cols <- c(21,44:84, 787)
#cols <- c(7,44:700)

cols <- c(1:784)
data.tree = data.transformed[cols]


#data.tree <- data.transformed
index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.70)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

#data.transformed1 <- data.tree[-index.collection,]

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#str(data.tree.train)
tree.beta.l = rpart(data=data.tree.train, data.tree.train$listens ~ .)
tree.beta.d = rpart(data=data.tree.train, data.tree.train$duration ~ .)

tree.beta.loc = rpart(data=data.tree.train, data.tree.train$location ~ .)
tree.beta.i = rpart(data=data.tree.train, data.tree.train$interest ~ .)
tree.beta.f = rpart(data=data.tree.train, data.tree.train$favorites2 ~ .)
tree.beta.y = rpart(data=data.tree.train, data.tree.train$yearCreated ~ .)


#tree.beta.g = rpart(data=data.tree.train, data.tree.train$genre_top ~ .)

# plot mytree
fancyRpartPlot(tree.beta.l, caption = NULL)
fancyRpartPlot(tree.beta.d, caption = NULL)
#fancyRpartPlot(tree.beta.g, caption = NULL)

typeof(data.tree.train$date_released)
data.tree.train$datefactor <-  as.factor(data.tree.train$date_released) 

strsplit(data.tree.train$datefactor,'/')

 
