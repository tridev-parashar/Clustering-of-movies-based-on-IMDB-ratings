# Loading necessary libraries
library(dplyr)
library(factoextra)
library(caret)
library(cluster)

# Importing the data
movies <- read.csv("IMDB-Movies.csv")
head(movies)

#removing rows with NA values
movies <- na.omit(movies)

# select only the numeric columns
num_cols <- c("Runtime..Minutes.", "Rating", "Votes", "Revenue..Millions.")

numeric_vars <- movies[,num_cols]
head(numeric_vars)

# apply PCA and scale the data: default mean 0
pca_res <- prcomp(numeric_vars, scale = TRUE)
pca_res$x
pca_res

# summary gives the Standard deviation and proportion of variance explained by PCs
summary(pca_res)

# Plotting the Variance explained by PCs
# check the variance explained by each PC
pca_res$sdev
pr.var = pca_res$sdev ^2

# proportion of variance explained
pve = pr.var/sum(pr.var)

plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0.4,1),
     type = "b")

#Plotting with PC1 and PC2
biplot(pca_res, scale = 0,cex=0.5)

# For better interpretation
# Reversing the sign of the rotation and x-axis coordinates of the PCA output
pca_res$rotation = -pca_res$rotation
pca_res$x = -pca_res$x

# Plotting the PC1 and PC2 with reversed signs.
biplot(pca_res, scale = 0,cex=0.5)
############################################
############################################

# Scaling the data and finding the distance.
sd.data = scale(numeric_vars)
data.dist = dist(sd.data)


######################################
#####Hierarchical clustering
#######################################

# Plotting with different types of linkages
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", labels=movies$Title,
     main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
     main = "Average Linkage",labels=movies$Title,
     xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),labels=movies$Title,
     main = "Single Linkage",
     xlab = "", sub = "", ylab = "")

# finding the optimal number of clusters - by default taking complete linkage
fviz_nbclust(sd.data, hcut, method = "silhouette")

#cut the dendrogram
hc.out <- hclust(data.dist)
# taking 7 clusters to visualize as 2 clusters become high level clustering,
# hence, choosing the next best no.of clusters from the silhoutte graph
k <- 7
cutree <- cutree(hc.out, k)

# Calculate the silhouette width for each observation for hierarchical clustering
sil <- silhouette(cutree, dist(sd.data))

# Plot the silhouette plot
fviz_silhouette(sil, label = TRUE)+
  ggtitle("Hierarchical Clustering Results")


# Color labels
col_labels <- cutree
col_labels
# Plotting the dendrogram
fviz_dend(hc.out, k = k, cex = 0.6,
          color_labels_by_k = TRUE,
          color_labels = col_labels)


##############################
##########kmeans##############
##############################

# Finding Optimal number of clusters for k-means
k_means=fviz_nbclust(sd.data, kmeans, method = "silhouette")
k_means

# Fetching the optimal k for k-means
k <- as.numeric(as.character(k_means$data$clusters[which.max(k_means$data$y)]))

# Plotting the silhouette for k-means
res = kmeans(sd.data,k, nstart = 20)
sil = silhouette(res$cluster, dist(sd.data)^2)
fviz_silhouette(sil, label = TRUE)+
  ggtitle("K-Means Clustering Results")
