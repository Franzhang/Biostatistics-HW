yf <- read.delim("E:/OneDrive/BIOSTAT_MPH/Intelligent Data Analysis/Homework/PS4-Data.txt")
plot(yf)
# Determine number of clusters
wss <- (nrow(yf)-1)*sum(apply(yf,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(yf, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
fit2 <- kmeans(yf, 2)
fit3 <- kmeans(yf, 3)
fit4 <- kmeans(yf, 4)
fit5 <- kmeans(yf, 5)

# plot fit2
level1 <- which(fit2$cluster == 1)
level2 <- which(fit2$cluster == 2)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "K-mean Clustering", sub = "K = 2")
points(yf[level2, ], col = "blue", pch = 16)

# plot fit3
level1 <- which(fit3$cluster == 1)
level2 <- which(fit3$cluster == 2)
level3 <- which(fit3$cluster == 3)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "K-mean Clustering", sub = "K = 3")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)

# plot fit4
level1 <- which(fit4$cluster == 1)
level2 <- which(fit4$cluster == 2)
level3 <- which(fit4$cluster == 3)
level4 <- which(fit4$cluster == 4)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "K-mean Clustering", sub = "K = 4")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)

# plot fit5
level1 <- which(fit5$cluster == 1)
level2 <- which(fit5$cluster == 2)
level3 <- which(fit5$cluster == 3)
level4 <- which(fit5$cluster == 4)
level5 <- which(fit5$cluster == 5)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "K-mean Clustering", sub = "K = 5")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)
points(yf[level5, ], col = "magenta", pch = 16)

# find the SSE for each value of k
fit2$tot.withinss
fit3$tot.withinss
fit4$tot.withinss
fit5$tot.withinss

# hierarchical clustering: MIN
fit <- hclust(dist(yf), method = "single")
plot(fit)
# cut tree into 2 clusters
rect.hclust(fit, k = 2, border = "red")
level1 <- which(cutree(fit, k = 2) == 1)
level2 <- which(cutree(fit, k = 2) == 2)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MIN", sub = "K = 2")
points(yf[level2, ], col = "blue", pch = 16)

# cut tree into 3 clusters
rect.hclust(fit, k = 3, border = "red")
level1 <- which(cutree(fit, k = 3) == 1)
level2 <- which(cutree(fit, k = 3) == 2)
level3 <- which(cutree(fit, k = 3) == 3)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MIN", sub = "K = 3")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)

# cut tree into 4 clusters
rect.hclust(fit, k = 4, border = "red")
level1 <- which(cutree(fit, k = 4) == 1)
level2 <- which(cutree(fit, k = 4) == 2)
level3 <- which(cutree(fit, k = 4) == 3)
level4 <- which(cutree(fit, k = 4) == 4)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MIN", sub = "K = 4")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)

# cut tree into 5 clusters
plot(fit)
rect.hclust(fit, k = 5, border = "red")
level1 <- which(cutree(fit, k = 5) == 1)
level2 <- which(cutree(fit, k = 5) == 2)
level3 <- which(cutree(fit, k = 5) == 3)
level4 <- which(cutree(fit, k = 5) == 4)
level5 <- which(cutree(fit, k = 5) == 5)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MIN", sub = "K = 5")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)
points(yf[level5, ], col = "magenta", pch = 16)

# hierarchical clustering: MAX
fit <- hclust(dist(yf), method = "complete")
plot(fit)
# cut tree into 2 clusters
rect.hclust(fit, k = 2, border = "red")
level1 <- which(cutree(fit, k = 2) == 1)
level2 <- which(cutree(fit, k = 2) == 2)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MAX", sub = "K = 2")
points(yf[level2, ], col = "blue", pch = 16)

# cut tree into 3 clusters
rect.hclust(fit, k = 3, border = "red")
level1 <- which(cutree(fit, k = 3) == 1)
level2 <- which(cutree(fit, k = 3) == 2)
level3 <- which(cutree(fit, k = 3) == 3)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MAX", sub = "K = 3")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)

# cut tree into 4 clusters
rect.hclust(fit, k = 4, border = "red")
level1 <- which(cutree(fit, k = 4) == 1)
level2 <- which(cutree(fit, k = 4) == 2)
level3 <- which(cutree(fit, k = 4) == 3)
level4 <- which(cutree(fit, k = 4) == 4)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MAX", sub = "K = 4")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)

# cut tree into 5 clusters
plot(fit)
rect.hclust(fit, k = 5, border = "red")
level1 <- which(cutree(fit, k = 5) == 1)
level2 <- which(cutree(fit, k = 5) == 2)
level3 <- which(cutree(fit, k = 5) == 3)
level4 <- which(cutree(fit, k = 5) == 4)
level5 <- which(cutree(fit, k = 5) == 5)
plot(yf[level1,], col = "red", pch = 16, xlim = c(-45, 45), ylim = c(0, 65),
main = "Hierarchical Clustering: MAX", sub = "K = 5")
points(yf[level2, ], col = "blue", pch = 16)
points(yf[level3, ], col = "black", pch = 16)
points(yf[level4, ], col = "orange", pch = 16)
points(yf[level5, ], col = "magenta", pch = 16)

# Rand Index for 2 clusters

fit2 <- kmeans(yf, 2)
fit <- hclust(dist(yf), method = "complete")
cutree(fit, k = 2)
cluster2 <- data.frame(yf, Kmeans = fit2$cluster, Hclust = cutree(fit, k = 2))
library(mclust)
adjustedRandIndex(fit2$cluster, cutree(fit, k = 2))
library(phyclust)
RRand(fit2$cluster, cutree(fit, k = 2)) # RandIndex = 0.49807

# Rand Index for 3 clusters
RRand(fit3$cluster, cutree(fit, k = 3)) # RandIndex = 0.8512

# Rand Index for 4 clusters
RRand(fit4$cluster, cutree(fit, k = 4)) # RandIndex = 0.7783

# Rand Index for 5 clusters
RRand(fit5$cluster, cutree(fit, k = 5)) # RandIndex = 0.8642 

# DBSCAN
library(fpc)
yf <- matrix(c(5,8,10,8,11,8,6,7,10,7,12,7,13,7,5,6,10,6,13,6,6,5,9,4,11,5,14,6,15,5,2,4,3,4,5,4,6,4,7,4,15,4,3,3,7,3,8,2),
nrow = 24, byrow = TRUE)
plot(yf)
dbscan(yf, eps = 2, MinPts = 3, showplot = TRUE)



