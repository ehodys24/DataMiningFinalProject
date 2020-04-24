data.seed = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', header=F)
data.seed = data.seed[,1:7]
colnames(data.seed) = c("area", "perimeter","campactness", "length", "width", "asymmetry", "groovelength")

data.seed <- scale(data.seed) 

library(factoextra)
distance <- get_dist(data.seed)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fit <- kmeans(data.seed, 2) 

table(fit$cluster)

fit

fviz_cluster(fit, data = data.seed)

fviz_cluster(fit, data = data.seed)

k3 <- kmeans(data.seed, centers = 3, nstart = 25)
k4 <- kmeans(data.seed, centers = 4, nstart = 25)
k5 <- kmeans(data.seed, centers = 5, nstart = 25)


p1 <- fviz_cluster(fit, geom = "point", data = data.seed) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data.seed) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data.seed) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data.seed) + ggtitle("k = 5")


library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

install.packages("fpc")

library(fpc)
plotcluster(data.seed, fit$cluster)

data.seed[fit$cluster==1,]

aggregate(data.seed,by=list(fit$cluster),FUN=mean)

fit$centers

wss <- (nrow(data.seed)-1)*sum(apply(data.seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(data.seed,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

prediction.strength(data.seed, Gmin=2, Gmax=15, M=10,cutoff=0.8)

d = dist(data.seed, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(data.seed, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

data.seed.dist=dist(data.seed)

data.seed.hclust=hclust(data.seed.dist, method="ward")
plot(data.seed.hclust)

data.seed.3clust = cutree(data.seed.hclust,k=3)

data.seed[data.seed.3clust==3,]

plotcluster(data.seed, data.seed.3clust)

library(arules)
data("Groceries")
summary(Groceries)

x = Groceries[size(Groceries) > 30]
inspect(x)

itemFrequencyPlot(Groceries, support = 0.1, cex.names=0.8)

basket_rules <- apriori(Groceries,parameter = list(sup = 0.003, conf = 0.5,target="rules"))

summary(basket_rules)

inspect(head(basket_rules))
inspect(subset(basket_rules, size(basket_rules)>4))

inspect(subset(basket_rules, lift>5))

yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt" & lift>3.5)

inspect(yogurt.rhs)

meat.lhs <- subset(basket_rules, subset = lhs %in% "meat" & lift>1.5)

inspect(meat.lhs)

install.packages('arulesViz')

library('arulesViz')

plot(basket_rules)

plot(basket_rules, interactive=TRUE)

plot(head(sort(basket_rules, by="lift"), 10), method = "graph")

plot(basket_rules, method="grouped")
