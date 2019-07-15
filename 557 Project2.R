# Stat 557 Project 2
# Load & check data
dat_original <- read.csv("~/Documents/Works/2018 Spring/557/Projects/Project 2/Anuran Calls (MFCCs)/Frogs_MFCCs.csv")
dim(dat_original)
# View(dat_original)
dat1 <- dat_original[which(dat_original$Species == "AdenomeraAndre"),]
dat2 <- dat_original[which(dat_original$Species == "AdenomeraHylaedactylus"),]
dat3 <- dat_original[which(dat_original$Species == "Ameeregatrivittata"),]
dat4 <- dat_original[which(dat_original$Species == "HypsiboasCinerascens"),]
dat5 <- dat_original[which(dat_original$Species == "HypsiboasCordobae"),]
dat_new <- rbind(dat1, dat2, dat3, dat4, dat5)
dim(dat_new)
# View(dat_new)
daty <- dat_new[, 23]
# daty
# as.factor(daty)
# levels(daty)
table(daty)
datx <- dat_new[, -23]
# datx

#########################################################################

# PCA
pca <- princomp(datx)
# Screeplot
screeplot(pca)
# Summary
summary(pca)

pc.comp1 <- pca$scores[, 1]
pc.comp2 <- pca$scores[, 2]

# Original class = 5 
plot(pc.comp1, pc.comp2,col = daty)

#########################################################################

# 80% of the sample size
smp_size <- floor(0.80 * nrow(datx))

## set the seed to make the partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(datx)), size = smp_size)
train <- datx[train_ind, ]
test <- datx[-train_ind, ]

#########################################################################

library(clue)

#########################################################################

# Apply K-Means Algorithm
# k = 1
# Training data
train1 <- kmeans(train, centers = 1, nstart = 25)
table(daty[train_ind], train1$cluster)
# Testing data
test1 <- cl_predict(train1, newdata = test)
table(daty[-train_ind], test1)

#########################################################################

# k = 5
# Training data
train5 <- kmeans(train, centers = 5, nstart = 25)
table(daty[train_ind], train5$cluster)
# Testing data
test5 <- cl_predict(train5, newdata = test)
table(daty[-train_ind], test5)

#########################################################################

# k = 10
# Training data
train10 <- kmeans(train, centers = 10, nstart = 25)
table(daty[train_ind], train10$cluster)
# Testing data
test10 <- cl_predict(train10, newdata = test)
table(daty[-train_ind], test10)

#########################################################################

# k = 15
# Training data
train15 <- kmeans(train, centers = 15, nstart = 25)
table(daty[train_ind], train15$cluster)
# Testing data
test15 <- cl_predict(train15, newdata = test)
table(daty[-train_ind], test15)

#########################################################################

# k = 20
# Training data
train20 <- kmeans(train, centers = 20, nstart = 25)
table(daty[train_ind], train20$cluster)
# Testing data
test20 <- cl_predict(train20, newdata = test)
table(daty[-train_ind], test20)

#########################################################################

# PCA
X <- cbind(pc.comp1, pc.comp2)
# X

#########################################################################

# 80% of the sample size
smp <- floor(0.80 * nrow(X))

## set the seed to make your partition reproductible
set.seed(456)
indx <- sample(seq_len(nrow(X)), size = smp)
trainx <- X[indx, ]
testx <- X[-indx, ]

#########################################################################

plot(X, col = daty)

#########################################################################

# K-means
# k = 1
# Training data
trainx1 <- kmeans(trainx, centers = 1, nstart = 25)
table(daty[indx], trainx1$cluster)
# Testing data
testx1 <- cl_predict(trainx1, newdata = test)
table(daty[-indx], testx1)

# Plot
cl1 <- kmeans(X, 1)
plot(pc.comp1, pc.comp2,col = cl1$cluster)
points(cl1$centers, pch = 16)

#########################################################################

# k = 5
# Training data
trainx5 <- kmeans(trainx, centers = 5, nstart = 25)
table(daty[indx], trainx5$cluster)
# Testing data
testx5 <- cl_predict(trainx5, newdata = test)
table(daty[-indx], testx5)

# Plot
cl5 <- kmeans(X, 5)
plot(pc.comp1, pc.comp2,col = cl5$cluster)
points(cl5$centers, pch = 16)

#########################################################################

# k = 10
# Training data
trainx10 <- kmeans(trainx, centers = 10, nstart = 25)
table(daty[indx], trainx10$cluster)
# Testing data
testx10 <- cl_predict(trainx10, newdata = test)
table(daty[-indx], testx10)

# Plot
cl10 <- kmeans(X, 10)
plot(pc.comp1, pc.comp2,col = cl10$cluster)
points(cl10$centers, pch = 16)

#########################################################################

# k = 15
# Training data
trainx15 <- kmeans(trainx, centers = 15, nstart = 25)
table(daty[indx], trainx15$cluster)
# Testing data
testx15 <- cl_predict(trainx15, newdata = test)
table(daty[-indx], testx15)

# Plot
cl15 <- kmeans(X, 15)
plot(pc.comp1, pc.comp2,col = cl15$cluster)
points(cl10$centers, pch = 16)

#########################################################################

# k = 20
# Training data
trainx20 <- kmeans(trainx, centers = 20, nstart = 25)
table(daty[indx], trainx20$cluster)
# Testing data
testx20 <- cl_predict(trainx20, newdata = test)
table(daty[-indx], testx20)

# Plot
cl20 <- kmeans(X, 20)
plot(pc.comp1, pc.comp2,col = cl20$cluster)
points(cl20$centers, pch = 16)

#########################################################################

# K-center

# Greedy algorithm for k-center
kcenter <- function(x, K){
  centers <- list()
  n <- length(x[,1])
  # step 1
  centers[[1]] <- x[sample(1:n, 1),1:2]
  # step 2
  distances <- vector("numeric", n)
  cluster <- vector("integer", n)
  for (j in 1:n){
    distances[j] <- dist(rbind(centers[[1]], x[j,1:2]))
    cluster[j] <- 1
  }
  # step 3
  for(i in 2:K){
    centers[[i]] <- x[which.max(distances),1:2]
    for(j in 1:n){
      if(dist(rbind(centers[[i]], x[j,1:2])) <= distances[j]){
        distances[j] <- dist(rbind(centers[[i]], x[j,1:2]))
        cluster[j] <- i
      }
    }
  }
  cluster
}     

# k = 1
kc1 <- kcenter(X, 1)

# Plot
plot(pc.comp1, pc.comp2,col = kc1$clustering)
points(kc1$medoids, pch = 10, col='white',lwd='2')

kc5 <- (X, 5)

# Plot
plot(pc.comp1, pc.comp2,col = kc5$clustering)
points(kc5$medoids, pch = 10, col='white',lwd='2')

kc10 <- pam(X, 10)

# Plot
plot(pc.comp1, pc.comp2,col = kc5$clustering)
points(kc10$medoids, pch = 10, col='white',lwd='2')

kc15 <- pam(X, 15)

# Plot
plot(pc.comp1, pc.comp2,col = kc15$clustering)
points(kc15$medoids, pch = 10, col='white',lwd='2')

kc20 <- pam(X, 20)

# Plot
plot(pc.comp1, pc.comp2,col = kc20$clustering)
points(kc20$medoids, pch = 10, col='white',lwd='2')

#########################################################################

# KNN
# 5-fold cross-validation
#Randomly shuffle the data
datx <- datx[sample(nrow(datx)), ]
#Create 5 equally size folds
folds <- cut(seq(1, nrow(datx)), breaks = 5, labels = FALSE)

#Perform 5 fold cross validation
for(i in 1:5){
  #Segement the data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- datx[testIndexes, ]
  trainData <- datx[-testIndexes, ]
}

set.seed(123) 
test <- testIndexes
train.datx <- datx[-test,]
test.datx <- datx[test,]

train.def <- daty[-test]
test.def <- daty[test]

#########################################################################

library(class)

knn.1 <-  knn(train.datx, test.datx, train.def, k = 1)
knn.5 <-  knn(train.datx, test.datx, train.def, k = 5)
knn.10 <- knn(train.datx, test.datx, train.def, k = 10)
knn.15 <- knn(train.datx, test.datx, train.def, k = 15)
knn.20 <- knn(train.datx, test.datx, train.def, k = 20)

#########################################################################

## For the error rates within the training set

100 * sum(train.def == knn.1)/24000  # For knn = 1
100 * sum(train.def == knn.5)/24000  # For knn = 5
100 * sum(train.def == knn.10)/24000  # For knn = 10
100 * sum(train.def == knn.15)/24000  # For knn = 15
100 * sum(train.def == knn.20)/24000 # For knn = 20

#########################################################################

## For the error rates within the testing set

100 * sum(test.def == knn.1)/6000  # For knn = 1
100 * sum(test.def == knn.5)/6000  # For knn = 5
100 * sum(test.def == knn.10)/6000  # For knn = 10
100 * sum(test.def == knn.15)/6000  # For knn = 15
100 * sum(test.def == knn.20)/6000 # For knn = 20

#########################################################################

# use sum of squares to decide k
########################################################################

# Training data
########################################################################
wss <- function(k) {
  kmeans(trainData, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15


wss_values <- map_dbl(k.values, wss)
k.values=1:10
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# Testing data
########################################################################
wss <- function(k) {
  kmeans(testData, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:10


wss_values <- map_dbl(k.values, wss)
k.values=1:10
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

########################################################################