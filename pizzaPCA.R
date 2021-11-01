# load packages
install.packages("ggplot2")
install.packages("factoextra")
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(e1071)
library(caret)

# load dataset
df <- read.csv("Pizza.csv")

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
par(mfrow=c(3,3))
for(i in 1:1) {
  counts <- table(df[,i])
  name <- names(df)[i]
  barplot(counts, main=name)
}

# multivariate, lets try correlation plot
corrplot(correlations, method="circle")

# scatter plot matrix and w/class
pairs(df)
pairs(brand~., data=df, col=df$brand)

# density plot w/class
x <- df[,3:9]
y <- df[,1]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# box and whiskey plot w/class
x <- df[,3:9]
y <- df[,1]
featurePlot(x=x, y=y, plot="box")

# Notice, that they are multiple rows with the same brand name. This could be problematic. Let's take a look at the unique values.
classes <- unique(df$brand, incomparables = FALSE)
uids <- unique(df$id, incomparables = FALSE)

# get the count of the unique values for pizza brand
table(df$brand)
table(df$uids)

# drop id 
df <- df[c(-2)]

# need to convert classes to categorical factors 
df <- read.csv("Pizza.csv", stringsAsFactors = TRUE)


###### perhaps new dataframe with joining factor and id as row.names as shown in example


##### PCA Method 1 using caret
# pre-processing PCA using caret
preprocessParams <- preProcess(df, method=c("center", "scale", "pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, df)
# summarize the transformed dataset
summary(transformed)


##### PCA Method 2
# drop id and brand
df <- df[c(-1, -2)]
pizza.pca <-PCA(df, scale.unit=TRUE, graph=TRUE)





