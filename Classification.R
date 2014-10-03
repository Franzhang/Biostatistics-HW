# Using information gain
# purity threshod set at 95%
# no other pruning.

E <- -70/210*log2(70/210) * 3
e1 <- -(68/69)*log2(68/69)-(1/69)*log2(1/69)
e2 <- -(55/57)*log2(55/57)-(2/57)*log2(2/57)
e3 <- 0
e4 <- -(20/21)*log2(20/21)-(1/21)*log2(1/21)
e5 <- 0
e6 <- 0
e7 <- 0
W.E <- (69*e1 + 57*e2 + 21*e4)/210 
(69*e1 + 57*e2 + 49*e3 + 21*e4 + 4*e5 + 1*e6 + 9*e7)/210
IG <- E - W.E

# Using information gain
# purity threshod set at 80%
# no other pruning

rm(list=ls(all=TRUE))
E <- -70/210*log2(70/210) * 3
e1 <- -(68/69)*log2(68/69)-(1/69)*log2(1/69)
e2 <- -(70/84)*log2(70/84)-(14/84)*log2(14/84)
e3 <- -(55/57)*log2(55/57)-(2/57)*log2(2/57)
W.E <-(69*e1 + 84*e2 + 57*e3)/210
IG <- E - W.E




# Using Gini Index to construct classification tree
Gini <- 1- ((70/210)^2 * 3)
g1 <- 1 - ((68/69)^2 + (1/69)^2)
g2 <- 1 - ((55/57)^2 + (2/57)^2)
g3 <- 1 - (6/6)^2
g4 <- 1 - (1/1)^2
g5 <- 1 - (1/1)^2
g6 <- 1 - ((60/61)^2 + (1/61)^2)
g7 <- 1 - (8/8)^2
g8 <- 1 - (6/6)^2
g9 <- 1 - (1/1)^2
W.G <- (69*g1 + 57*g2 + 6*g3 + 1*g4 + 1*g5 + 61*g6 + 8*g7 + 6*g8 + 1*g9)/210

# Read zoo.csv into R
zoo <- read.csv("E:\\OneDrive\\BIOSTAT_MPH\\Intelligent Data Analysis\\Homework\\zoo.csv")
head(zoo)
summary(zoo$c.type)
# First, let's try manually.
Entropy <- -(4/101)*log2(4/101)-(20/101)*log2(20/101)-(13/101)*log2(13/101)-(8/101)*log2(8/101)-(10/101)*log2(10/101)-(41/101)*log2(41/101)-(5/101)*log2(5/101)
# Split by hair
# CART method
library(rpart)
temp <- rpart(c.type ~ hair, data = zoo, method = "class")
# leaf 1 (58 obs), leaf 2 (43 obs)
e1 <- -(4/58)*log2(4/58)-(20/58)*log2(20/58)-(13/58)*log2(13/58)-(4/58)*log2(4/58)-(10/58)*log2(10/58)-(2/58)*log2(2/58)-(5/58)*log2(5/58)
e2 <- -(4/43)*log2(4/43)-(39/43)*log2(39/43)


WeightedEntropy.hair <- (58*e1 + 43*e2)/101
InfoGain.hair <- Entropy - WeightedEntropy.hair
SplitInfo.hair <- -(58/101)*log2(58/101)-(43/101)*log2(43/101)
GainRatio.hair <- InfoGain.hair/SplitInfo.hair


install.packages("entropy")
entropy(c(4,20,13,4,10,2,5), unit = "log2")
entropy(c(4,39), unit = "log2")
# Compare with e1 and e2, we got the same result.

# work on other Attribute
temp <- rpart(c.type ~ catsize, data = zoo, method = "class")
# Warning:
# When classified by some variables, such as "domestic", 
# the algorithm won't generate any son node 
# because domestic is not an efficient splitting variable.
# Manual calculation show IG = 0.05066878,
# which is too small to let rpart do the classification
summary(temp)
# obtain the counts for seven types in son node 1
counts1 <- temp$frame[,9][2,2:8]
counts1 <- as.integer(counts1)
# obtain the counts for seven types in son node 2
counts2 <- temp$frame[,9][3,2:8]
counts2 <- as.integer(counts2)
e1 <- entropy(counts1, unit = "log2")
e2 <- entropy(counts2, unit = "log2")
WeightedEntropy <- ((temp$frame[2,"n"])*e1 + (temp$frame[3,"n"])*e2)/(temp$frame[1,"n"])
InfoGain <- Entropy - WeightedEntropy
SplitInfo <- entropy(as.integer(table(zoo$catsize)), unit = "log2")
GainRatio <- InfoGain/SplitInfo


# While working on CART package I came up with an easier method.
# exclude “legs” because it’s not a binary variable
# exclude “type” and “name”
for (i in names(zoo)[c(-13,-17,-18)]){
newtable <- table(zoo$c.type, zoo[,i])
e1 <- entropy(newtable[,1], unit = "log2")
e2 <- entropy(newtable[,2], unit = "log2")
WeightedEntropy <- (sum(newtable[,1])*e1 + sum(newtable[,2])*e2)/101
InfoGain <- Entropy - WeightedEntropy
SplitInfo <- entropy(as.integer(table(zoo[,i])), unit = "log2")
GainRatio <- InfoGain/SplitInfo
print(data.frame(InfoGain,SplitInfo,GainRatio))
}

# dealing with variable "legs" separately
newtable <- table(zoo$c.type, zoo$legs)
e1 <- entropy(newtable[,1], unit = "log2")
e2 <- entropy(newtable[,2], unit = "log2")
e3 <- entropy(newtable[,3], unit = "log2")
e4 <- entropy(newtable[,4], unit = "log2")
e5 <- entropy(newtable[,5], unit = "log2")
e6 <- entropy(newtable[,6], unit = "log2")
WeightedEntropy <- (sum(newtable[,1])*e1 + sum(newtable[,2])*e2 + 
sum(newtable[,3])*e3 + sum(newtable[,4])*e4 + sum(newtable[,5])*e5 + 
sum(newtable[,6])*e6)/101
InfoGain <- Entropy - WeightedEntropy
SplitInfo <- entropy(as.integer(table(zoo$legs)), unit = "log2")
GainRatio <- InfoGain/SplitInfo

# Q3
rm(list = ls(all = TRUE))
seeds <- read.table("E:\\OneDrive\\BIOSTAT_MPH\\Intelligent Data Analysis\\Homework\\seeds_dataset2.txt", header = FALSE)
data <- seeds[order(seeds$V1),][,c(1,8)]
head(table(data))
group1 <- data[data$V1 <=12.70,]
group2 <- data[data$V1 > 12.70, ]
# class 1 type in group1
sum(table(group1)[,1])
# class 3 type in group1
sum(table(group1)[,2])
library(entropy)
# Group1 entropy
e1 <- entropy(c(sum(table(group1)[,1]),sum(table(group1)[,2])),unit = "log2")
# Group2 entropy
e2 <- entropy(c(sum(table(group2)[,1]),sum(table(group2)[,2]),sum(table(group2)[,3])),unit = "log2")
weighted <- (e1*sum(table(group1)) + e2 * sum(table(group2)))/210
Entropy <- -70/210*log2(70/210) * 3
IG <- Entropy - weighted
IG

# examine split point for 15.36
group1 <- data[data$V1 <=15.36,]
group2 <- data[data$V1 > 15.36, ]
# class 1 type in group1
sum(table(group1)[,1])
# class 3 type in group1
sum(table(group1)[,2])
library(entropy)
# Group1 entropy
e1 <- entropy(c(sum(table(group1)[,1]),sum(table(group1)[,2])),unit = "log2")
# Group2 entropy
e2 <- entropy(c(sum(table(group2)[,1]),sum(table(group2)[,2])),unit = "log2")
weighted <- (e1*sum(table(group1)) + e2 * sum(table(group2)))/210
Entropy <- -70/210*log2(70/210) * 3
IG <- Entropy - weighted
IG

# Q5
x <- matrix(c(5,7,3,9,1,6,9,12,7,1,2,-2,3,4,1,-3,4,4,0,1,7,5,3,5,1,4,1,0,3,1),
nrow = 6, byrow = TRUE)
class <- c(1,1,0,0,1,0)
w <- c(0,0,0,0,0)

for(i in 1:6){
xv <- ifelse(w%*%x[i,] > 0, 1, 0)
j <- x[i,]
if(xv > class[i]) j <- -j
if(xv != class[i]) w <- w+j
print(w)
}

# add the records (3 1 3 -2 1) and (5 8 6 9 0) to the dataset 
x <- matrix(c(5,7,3,9,1,6,9,12,7,1,2,-2,3,4,1,-3,4,4,0,1,7,5,3,5,1,4,1,0,3,1,3,1,3,-2,1,5,8,6,9,1),
nrow = 8, byrow = TRUE)
class <- c(1,1,0,0,1,0,1,0)
w <- c(0,0,0,0,0)

for(i in 1:8){
xv <- ifelse(w%*%x[i,] > 0, 1, 0)
j <- x[i,]
if(xv > class[i]) j <- -j
if(xv != class[i]) w <- w+j
print(w)
}
