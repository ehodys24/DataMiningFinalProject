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

data.transformed$listens[which(data.transformed$listens < 20,000 & data.transformed$listens > 10,000)]

data.clean %>% count(listens > 0 & listens < 10000)
data.transformed %>% count(listens > 0 & listens < 10000)

data.transformed %>% count(listensBin == 1)
data.transformed %>% count(listensBin == 2)


data.transformed %>% count(listensBin == 0)
data.transformed %>% count(listensBin == 1)
data.transformed %>% count(listensBin == 2)
data.transformed %>% count(listensBin == 3)
data.transformed %>% count(listensBin == 4)
data.transformed %>% count(listensBin == 5)
data.transformed %>% count(listensBin == 6)

hist(data.transformed$listensBin)

test=data.transformed[,44:198]
wss <- (nrow(test)-1)*sum(apply(test,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(test,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares", main="WSS vs Cluster number")

library(factoextra)
fit_2 <- kmeans(test, 2)
cluster2 = fviz_cluster(fit_2, data = test)

fit_3 <- kmeans(test, 3)
cluster3 = fviz_cluster(fit_3, data = test)

fit_4 <- kmeans(test, 4)
cluster4 = fviz_cluster(fit_4, data = test)

fit_5 <- kmeans(test, 5)
cluster5 = fviz_cluster(fit_5, data = test)

fit_6 <- kmeans(test, 6)
cluster6 = fviz_cluster(fit_6, data = test)

cluster2
cluster3
cluster4
cluster5
cluster6

test.dist=dist(test)
test.hclust=hclust(test.dist, method="ward")
plot(test.hclust)
