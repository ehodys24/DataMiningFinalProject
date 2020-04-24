library(dlookr)
library(dplyr)

data.raw=read.csv("wranglingFinalData.csv")

data.clean <- data.raw
data.tree <- data.clean

index.collection <- sample(nrow(data.tree ),nrow(data.tree )*0.80)
data.tree.train <- data.tree[index.collection,]
data.tree.test <- data.tree[-index.collection,]

cols <- c(4,12)
which(colnames(data.tree.train)=="danceability")
test=data.tree.train[,cols]
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

library(fpc)
library(dbscan)
library(meanShiftR)
#Mean-Shift Clustering

## an example using the iris dataset
## help( iris )

## prepare data matrix (a subset of the iris dataset)
set.seed( 2 )
indices <- sample( 1:nrow( iris ), 80 )
iris.data <- t( iris[indices,c( "Sepal.Length", "Sepal.Width" )] )

## run mean shift algorithm
clustering <- msClustering( iris.data, h=0.8 )
print( clustering )

## plot the clusters
## Not run: 
plot( iris.data[1,], iris.data[2,], col=clustering$labels+2, cex=0.8,
      pch=16, xlab="Sepal.Length", ylab="Sepal.Width" )
points( clustering$components[1,], clustering$components[2,],
        col=2+( 1:ncol( clustering$components ) ), cex=1.8, pch=16 )
## End(Not run)



## using multiple cores (2)
## Not run: 
options( mc.cores=2 )
clustering.mc <- msClustering( iris.data, multi.core=TRUE )
## End(Not run)

#(DBSCAN)


