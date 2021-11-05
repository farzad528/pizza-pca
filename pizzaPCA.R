# load packages
install.packages("ggplot2")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("e1071")
install.packages("lattice")
install.packages("cluster")
install.packages("MASS")
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(e1071)
library(lattice)
library(caret)
library(cluster)
library(MASS)


#############################################################
## Data Visualization
#############################################################

# load dataset
df <- read.csv("Pizza.csv")

brand*id <-paste(df$brand, df$id)
brandid <- paste(df$brand, df$id)
brandid <- paste(df$brand, df$id, sep="")
df <- cbind(df, brandid)
row.names(df) <- df$brandid 

# summarize data
summary(df)
head(df)
dim(df)
sapply(df, class)
sapply(df[,3:9], sd)
skew <- apply(df[,3:9], 2, skewness)
print(skew)
correlations <- cor(df[,3:9])
print(correlations)

### data visualizations 
# histograms
par(mfrow=c(3,3))
for(i in 3:9) {
  hist(df[,i], main=names(df)[i])
}


# density plots
par(mfrow=c(3,3))
for(i in 3:9) {
  plot(density(df[,i]), main=names(df)[i])
}

# box and whisker plots
par(mfrow=c(3,3))
for(i in 3:9) {
  boxplot(df[,i], main=names(df)[i])
}

# bar plots for categorical 
par(mfrow=c(1,1))
for(i in 1:1) {
  counts <- table(df[,i])
  name <- names(df)[i]
  barplot(counts, main=name)
}

# multivariate, lets try correlation plot
corrplot(correlations, method="circle")

# need to convert classes to categorical factors 
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)

# scatter plot matrix and w/class
pairs(df)
pairs(brand~., data=df, col=df$brand)

# density plot w/class
x <- df[,3:9]
y <- df[,1]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# box and whisker plot w/class
x <- df[,3:9]
y <- df[,1]
featurePlot(x=x, y=y, plot="box")

# Notice, that they are multiple rows with the same brand name. This could be problematic. Let's take a look at the unique values.
classes <- unique(df$brand, incomparables = FALSE)
uids <- unique(df$id, incomparables = FALSE)
print(classes)
length(uids)
# get the count of the unique values for pizza brand
table(df$brand)

# drop id bc we don't need it for PCA
df <- df[c(-2)]

###### perhaps new dataframe with joining factor and id as row.names as shown in example

#############################################################
## Principal Component Analysis 
#############################################################

df <- read.csv("Pizza.csv")

# drop id and brand
df.min <- df[c(-2)]
df.min <- df.min[c(-1)]
cor(df.min) #conduct PCA on correlation matrix to create variables on commensurate scale
pizza.pca <-PCA(df.min, scale.unit=TRUE, graph=TRUE)

pizza.pca$eig #print eigenvalues
fviz_eig(pizza.pca) #Scree plot

pizza.pca$var #prints coord, cor, cos2 and contrib-eigenvectors are coord/sqrt(eigenvalue)
pizza.pca$ind$coord #individuals expressed by PCAs
fviz_pca_var(pizza.pca, col.var="cos2") #correlation circle with cos2
fviz_pca_biplot(pizza.pca, col.var="cos2") #coordinates of individuals in PCA space and correlations between PCA and variables


#############################################################
## Hierarchical Clustering
#############################################################



### agglomerative 
# load dataset
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)

# drop id and brand
df <- df[c(-1, -2)]

#Normalize data
normalized.data <- scale(df)

#Change type to dataframe
newdf<-data.frame(normalized.data)

#Calculate the distance
dist.pizza<-get_dist(newdf, method="euclidean")

#Visualize to find the outlier
fviz_dist(dist.pizza)

# carry out hierarchical clustering
hc <- hclust(dist.pizza)
# Compute with agnes
hc2 <- agnes(newdf, method="complete")
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
# function to compute coefficient
ac <- function(x) {
  agnes(newdf, method = x)$ac
}
map_dbl(m, ac)

# visualize cluster dendrogram
fviz_dend(hc)

hc$merge
hc$height

# visualize the cutree
# first: plot
plot(hc)
# then draw superimpoes rectangular compartments for each cluster on the tree
rect.hclust(hc, k=4, border = 2:6)

# can draw th cut line 
abline(h=3, col='yellow')

# cut the tree by number of cluster: k
result_cluster_k <- cutree(hc, k=4)
# cut the tree by height: h 
result_cluster <- cutree(hc, h =4)
result_cluster
length(unique(result_cluster))

# optimum number of clusters
fviz_nbclust(newdf, FUN = hcut, method = "wss")

# dunn index
install.packages('clValid', dependencies = TRUE)
suppressPackageStartupMessages(library(clValid))
result_cluster <- cutree(hc, k=2)
dunn(get_dist(normalized.data, method = "euclidean"), result_cluster)

# examine and analyze clusters
#plot clusters
plot(newdf, col=result_cluster_k)
# distances from the dendrogram
pizza.coph <- cophenetic(hc)
# visualize dengrogram distances with a heatmap
fviz_dist(pizza.coph)
# correlation between dendrogram distances and euclidean distances
cor(get_dist(newdf, method="euclidean"),pizza.coph)

### divisive
# load dataset
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)

# drop id and brand
df <- df[c(-1, -2)]

#Normalize data
normalized.data <- scale(df)

#Change type to dataframe
newdf<-data.frame(normalized.data)

#Calculate the distance
dist.pizza<-get_dist(newdf, method="euclidean")

#Visualize to find the outlier
fviz_dist(dist.pizza)

# carry out hierarchical clustering
dc <- diana(dist.pizza)
# visualize cluster dendrogram
fviz_dend(dc)

dc$merge
dc$height

# visualize the cutree
# first: plot
plot(dc)
# then draw superimpoes rectangular compartments for each cluster on the tree
rect.hclust(dc, k=4, border = 2:6)

# can draw th cut line 
abline(h=3, col='yellow')

# cut the tree by number of cluster: k
result_cluster_k <- cutree(dc, k=4)
# cut the tree by height: h 
result_cluster <- cutree(dc, h =4)
result_cluster
length(unique(result_cluster))

# optimum number of clusters
fviz_nbclust(newdf, FUN = hcut, method = "wss")

# dunn index
install.packages('clValid', dependencies = TRUE)
suppressPackageStartupMessages(library(clValid))
result_cluster <- cutree(hc, k=2)
dunn(get_dist(normalized.data, method = "euclidean"), result_cluster)

# examine and analyze clusters
#plot clusters
plot(newdf, col=result_cluster_k)
# distances from the dendrogram
pizza.coph <- cophenetic(hc)
# visualize dengrogram distances with a heatmap
fviz_dist(pizza.coph)
# correlation between dendrogram distances and euclidean distances
cor(get_dist(newdf, method="euclidean"),pizza.coph)

#############################################################
## Kmeans Clustering
#############################################################
# load dataset
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)

# drop id and brand
df <- df[c(-1, -2)]

#Normalize data
normalized.data <- scale(df)

#Change type to dataframe
newdf<-data.frame(normalized.data)
# Calculate distances
dist.pizza<-get_dist(newdf, method="euclidean")

#look at the data as a matrix
as.matrix(dist.pizza)

# look at the heatmap of the data as a matrix
fviz_dist(dist.pizza) 

# set seed to guarantee reproducibility
set.seed(99) 

# centers is the number of clusters, nstart is the number of times you want the algorithm to run. 
PizzaCluster<-kmeans(newdf,centers=3,nstart=1)

#examine your results
str(PizzaCluster) 

#plot your results
plot(newdf,col=PizzaCluster$cluster) 

# add the centers to your plot
points(PizzaCluster$centers, col=1:2,pch=8) 

###Add cluster identifier to dataframe to see where the clusters fall
##Reload Dataset to see brands again
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)
kmeans_pizza_df <- data.frame(Cluster = PizzaCluster$cluster, df)
head(kmeans_pizza_df)


