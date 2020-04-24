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

hist(data.transformed$listensBin)

test <-  data.transformed[, c("listensBin","audio_features1")]

str(data.transformed)
which(colnames(data.transformed)=="listensBin")
cols <- c(7,44:400, 787)
data.tree = data.transformed[,cols]
index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.70)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

data.transformed1 <- data.tree[-index.collection,]




str(data.tree.train)
tree.beta = rpart(data=data.transformed1, data.transformed1$gen ~ .)
plot(tree.beta)
colnames(data.tree)





 
