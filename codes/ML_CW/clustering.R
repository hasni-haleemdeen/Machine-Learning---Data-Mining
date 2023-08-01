####task 1####
##subtask 1##

#Loading the required packages
library("readxl")
library("tidyverse")
library("cluster")
library(dplyr)
#Reading the xlsx file data into a variable
#Package installation to read xlsx file
install.packages("readxl")
Data <- readxl::read_excel("C:/Users/DELL/Desktop/codes(ml)/ML_CW/vehicles.xlsx")

#Removing the column -> Samples
Altered_Data <- subset(Data, select = -c(Samples))
head(Altered_Data)
names(Altered_Data)

#Checking the data type of the data in columns
glimpse(Altered_Data)

#The classes available
unique(Altered_Data$Class)

#Removing the class label "y" and keeping only the x labels in the first 18 columns
Labelled_X <- Altered_Data[,-19]

#Dimensions of x label
dim(Labelled_X)

#Checking for any null value in any position in the dataset
which(is.na.data.frame(Labelled_X))
names(Labelled_X)

#Creating boxplots and summary statistics for x label
boxplot(Labelled_X)
summary(Labelled_X)

#Defining functions to detect outliers from multiple columns in a dataframe
detect_outlier <- function(x) {
  
  #Calculate first quantile
  firstQuantile <- quantile(x, probs=.25)
  
  #Calculate third quantile
  thirdQuantile <- quantile(x, probs=.75)
  
  #Calculate interquartile range
  IQR = thirdQuantile - firstQuantile
  
  #Return true or false
  x > thirdQuantile + (IQR*1.5) | x < firstQuantile - (IQR*1.5)
}

##Defining functions to remove outliers from multiple columns in a dataframe
remove_outlier <- function(dataframe, columns) {
  
  #Traverse through columns vector using a for loop
  for (col in columns) {
    # Remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]),]
  }
  return(dataframe)
}

#Removing outliers from x label data
dataframe <- remove_outlier(Labelled_X, names(Labelled_X))
boxplot(dataframe)

#Removing final outlier
alterData <- remove_outlier(dataframe, names(dataframe))
boxplot(alterData)

dim(alterData)

#Normalizing the x labels using Z-Score standardization as it is useful for finding outliers
alterDataNorm <- as.data.frame(scale(alterData[1:18]))
boxplot(alterDataNorm)

#Installing NbCluster package
install.packages("NbClust")
library("NbClust")

#Setting seed for reproducibility
set.seed(3)

#Using NbClust to find the optimal number of clusters
noOfClusters <- NbClust(alterDataNorm, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
noOfClusters <- NbClust(alterDataNorm, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
noOfClusters <- NbClust(alterDataNorm, distance = "maximum", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Load the factoextra package for clustering analysis visualization
library("factoextra")

# Implement Elbow method to find optimal k using WSS (within-cluster sum of squares)
fviz_nbclust(alterDataNorm, kmeans, method = 'wss')

# Implement Gap Statistic method to find optimal k
fviz_nbclust(alterDataNorm, kmeans, method = 'gap_stat')

# Implement Silhouette method to find optimal k
fviz_nbclust(alterDataNorm, kmeans, method = 'silhouette')

# Apply K-means clustering with k = 3
kmeanfinalOutcome <- kmeans(alterDataNorm, 3)

# Assign cluster label for each data point
kmeanfinalOutcome$cluster

# Compute and show the cluster centers
kmeanfinalOutcome$centers

# Compute the within-cluster sum of squares
wss = kmeanfinalOutcome$tot.withinss
wss

# Compute the between-cluster sum of squares
bss = kmeanfinalOutcome$betweenss
bss

# Compute the ratio of between-cluster sum of squares and total sum of squares
bss/kmeanfinalOutcome$totss

# Compute silhouette for the k-means clustering
sil <- silhouette(kmeanfinalOutcome$cluster, dist(alterDataNorm))
fviz_silhouette(sil)

# Load the fpc package for cluster analysis
install.packages("fpc")
library(fpc)

# Visualize the clustering result
fviz_cluster(kmeanfinalOutcome, data = alterDataNorm)


##subtask 2##

#Load required libraries
library("readxl")
library("tidyverse")
library("NbClust")
library("gridExtra")
library("ggplot2")
library("factoextra")
library("fpc")

#Reading the xlsx file data into a variable
Data <- readxl::read_excel("C:/Users/DELL/Desktop/codes(ml)/ML_CW/vehicles.xlsx")

#Remove the column named "Samples"
Altered_Data <- subset(Data, select = -c(Samples))

#Print the first few rows of the modified dataset
head(Altered_Data)

#Print the column names of the modified dataset
names(Altered_Data)

#Print the data type of each column in the modified dataset
glimpse(Altered_Data)

#Print the unique classes available in the "Class" column of the modified dataset
unique(Altered_Data$Class)

#Remove the "Class" label and keep the first 18 columns as the x labels
Labelled_X <- Altered_Data[,-19]

#Print the dimensions of the modified dataset
dim(Labelled_X)

#Check for any null values in the modified dataset
which(is.na.data.frame(Labelled_X))

#Print the column names of the modified dataset
names(Labelled_X)

#Calculate the variances of each column in the modified dataset
apply(Labelled_X,2,var)

#Print the first few rows of the modified dataset
head(Labelled_X)

#Create a boxplot to visualize the distribution of each column in the modified dataset
boxplot(Labelled_X)

#Normalize the modified dataset
Labelled_X_scaled <-scale(Labelled_X)

#Print the first few rows of the normalized dataset
head(Labelled_X_scaled)

#Calculate the correlation matrix of the normalized dataset
cor(Labelled_X_scaled)

#Calculate the mean correlation of the normalized dataset
mean(cor(Labelled_X_scaled))

#Calculate the covariance matrix of the normalized dataset
Labelled_X_cov <- cov(Labelled_X_scaled)

#Print the first few rows of the covariance matrix
head(Labelled_X_cov)

#Calculate the eigenvalues and eigenvectors of the covariance matrix
Labelled_X_eigen <- eigen(Labelled_X_cov)

#Print the eigenvalues and eigenvectors
Labelled_X_eigen
str(Labelled_X_eigen)
Labelled_X_eigen$vectors

#Calculate the proportion of variance explained by each principal component
PVE <- Labelled_X_eigen$values / sum(Labelled_X_eigen$values)
round(PVE, 2)

#visualize a scree plot, proportion of variance explained by each principal component
PVEplot <- qplot(c(1:18), PVE) +
  geom_line() +
  xlab("Principal Component") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#visualize a cumulative plot, cumulative proportion of variance explained by each principal component
cumPVE <- qplot(c(1:18), cumsum(PVE)) +
  geom_line() +
  xlab("Principal Component") +
  ylab(NULL) +
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

#Display both the scree plot and the cumulative scree plot side by side
grid.arrange(PVEplot, cumPVE, ncol = 2)

# Convert PVE to percentage and create a barplot to visualize the variance explained by each PC
varPercent <- PVE*100
barplot(varPercent, xlab='PC', ylab='Percent Variance', 
        names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')

# Add a horizontal line representing the percentage of variance explained by a random variable
abline(h=1/ncol(Labelled_X)*100, col='red')

# Extract the first 6 eigenvectors from Labelled_X_eigen and store them in phi
phi <- Labelled_X_eigen$vectors[,1:6]

# Invert the signs of the eigenvectors in phi
phi <- -phi     

# Extract the first principal component (PC1) of Labelled_X_scaled
PC1 <- as.matrix(Labelled_X_scaled) %*% phi[,1]

# Extract the second principal component (PC2) of Labelled_X_scaled
PC2 <- as.matrix(Labelled_X_scaled) %*% phi[,2]

# Extract the third principal component (PC3) of Labelled_X_scaled
PC3 <- as.matrix(Labelled_X_scaled) %*% phi[,3]

# Extract the fourth principal component (PC4) of Labelled_X_scaled
PC4 <- as.matrix(Labelled_X_scaled) %*% phi[,4]

# Extract the fifth principal component (PC5) of Labelled_X_scaled
PC5 <- as.matrix(Labelled_X_scaled) %*% phi[,5]

# Extract the sixth principal component (PC6) of Labelled_X_scaled
PC6 <- as.matrix(Labelled_X_scaled) %*% phi[,6]

# Combine the principal components into a new dataset
dataset <- cbind(PC1,PC2,PC3,PC4,PC5,PC6)

# Create a boxplot of the dataset
boxplot(dataset)

# Display the first few rows of the dataset
head(dataset)

# Set the random seed to 3 for reproducibility
set.seed(3)

# Use the NbClust function to determine the optimal number of clusters using different distance metrics and clustering methods
noOfClusters = NbClust(dataset,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
noOfClusters = NbClust(dataset,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")
noOfClusters = NbClust(dataset,distance="maximum", min.nc=2,max.nc=10,method="kmeans",index="all")

#visualize the results of the NbClust function using the within-cluster sum of squares method
fviz_nbclust(dataset, kmeans, method = 'wss')

#visualize the results of the NbClust function using the gap statistic method
fviz_nbclust(dataset, kmeans, method = 'gap_stat')

#visualize the results of the NbClust function using the silhouette method
fviz_nbclust(dataset, kmeans, method = 'silhouette')

# Use the kmeans function to perform k-means clustering on the dataset with k=2
kmeanfinalOutcome <- kmeans(dataset,2)

# Display the cluster assignments for each data point
kmeanfinalOutcome$cluster

# Display the center coordinates of each cluster
kmeanfinalOutcome$centers

# Calculate the within-cluster sum of squares
wss = kmeanfinalOutcome$tot.withinss
wss

#Calculate the between-cluster sum of square
bss = kmeanfinalOutcome$betweenss
bss

#Calculate the ratio between the between-cluster sum of square and the total sum of square
bss/kmeanfinalOutcome$totss

#Calculate the silhouette index
sil <- silhouette(kmeanfinalOutcome$cluster, dist(dataset))
fviz_silhouette(sil)

#Calculate the Calinski-Harabasz index
calinhara(dataset,kmeanfinalOutcome$cluster,cn=max(kmeanfinalOutcome$cluster))

#Visualize the clustering results using fviz_cluster
fviz_cluster(kmeanfinalOutcome, data = dataset)