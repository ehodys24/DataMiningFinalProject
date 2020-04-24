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




 
