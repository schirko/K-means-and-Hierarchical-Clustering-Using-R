library(NbClust)
library(corrplot)
library(cluster) 
library(factoextra)

## MSDS680 - Machine Learning
## Week 6 Assignment - K-Means & HCA
## Scott Schirkofsky
## June 14, 2020

rm(list=ls()) #remove base objects to clear workspace

##Collecting data/load CSV
custs <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"), header = TRUE, sep = ",")
head(data,5)

##Data Exploration
str(custs) 
summary(custs)
head(custs)

corrmatrix <- cor(custs)
corrplot(corrmatrix, method = 'number')

## Data preperation
custs <- na.omit(custs)
custs$Channel <- NULL
custs$Region <-NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df <- scale(custs)
head(df,5)

## Question 3 - Train the model
km <- kmeans(custs, 3)
km$cluster
km$centers
km$size
custs$cluster <- km$cluster
km$tot.withinss

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# Gap statistic
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

## Question 2
plot(custs[c("Grocery", "Delicassen")], col=km$cluster)
points(km$centers[,c("Grocery", "Delicassen")], col=1:5, pch=8, cex=2)

## Part 2 - HCA optimal clusters
# Complete linkage
hc = hclust(dist(custs, method="euclidean"), method="ward.D2")
hc
plot(hc, hang = -0.01, cex = 0.7)
fviz_nbclust(nb)
plot(hc, hang = -0.01, cex = 0.7)

fit <- cutree(hc, k = 3)
table(fit)
fit

plot(hc)
rect.hclust(hc, k = 3, border = "red")

# Single Linkage
nb2 <- NbClust(custs, distance = "euclidean", min.nc = 2,
               max.nc = 10, method = "single")
fviz_nbclust(nb2)
