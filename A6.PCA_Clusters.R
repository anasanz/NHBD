


#############PCA removing buildings variable#############

rm(list=ls())
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="extracted_values",header=TRUE)
e<-e[,-c(1)]
e<-e[,c(1:4,6:9,5,10:25)]
e<-e[ ,-24] #REMOVE BUILDINGS
which(is.na(e[9:24])) #No NA

#Scale variables
sd_e<- as.data.frame(scale(e[9:24]))

#PCA
pc <- prcomp(sd_e)
#Eigenvalues
eig <- (pc$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.active <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)
head(eig.active)
# Screeplot eigenvalues
barplot(eig.active[, 2], names.arg=1:nrow(eig.active), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eig.active), 
      eig.active[, 2], 
      type="b", pch=19, col = "red")


#Number of clusters
wss <- (nrow(sd_e)-1)*sum(apply(sd_e,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(sd_e,
                                     centers=i)$withinss)
plot( 1:15, wss, type="o", xlab="Number of Clusters",
      ylab="Within groups sum of squares", pch = 16)


#As K-means analysis does not provide a criterion of choice for the optimal number of clusters, 
#we determined that number from the DD-weighted gap method (Basille) - Github, Seasonality package#
#For a better biological interpretation of the clusters, we limited the number of cluster selecion 
#to a range of 1-6, since with more than 6 clusters there was a high overlap of some clusters with 
#many axes of the PCA#
v<-gap(e[,9:ncol(e)], from = 1, to = 7, nsim = 100,
       ref.dist = c("unif"), clust.method = "k-means",
       dist.method = "euclidean", weighted = TRUE, tol = 1,
       seed = 1)
plot.gap(v)
#4C#
pc <- prcomp(sd_e)
comp <- data.frame(pc$x[,1:5])
k4 <- kmeans(comp, 4, nstart=25, iter.max=1000)
e$Clusters <- k4$cluster
#Plot 3D
library(rgl)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k4$cluster)
#Save 4c
#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
#write.csv(e,file="4Clusters")



#With 4 clusters, not enough heterogeneity of the landscape (only natal territories for 2/4 clusters)
library(clValid)

val<-clValid(sd_e, 3:10, clMethods = c("hierarchical","kmeans"), 
             validation = "stability", metric = "euclidean", method = "ward",
             verbose=FALSE)
optimalScores(val)
#     Score      Method Clusters
#APN 0.07946399 kmeans        3
#AD  3.60162037 kmeans        6
#ADM 0.39516589 kmeans        3
#FOM 0.75631380 kmeans        6
val2<-clValid(sd_e, 3:6, clMethods = c("hierarchical","kmeans"), 
              validation = "internal", metric = "euclidean", method = "ward",
              verbose=FALSE)
optimalScores(val2)
#                    Score       Method Clusters
#Connectivity 240.70595238 hierarchical        3
#Dunn           0.01959638 hierarchical        3
#Silhouette     0.23548808       kmeans        3


#6C (3 Clusters is to few to test my hypothesis) #
pc <- prcomp(sd_e)
comp <- data.frame(pc$x[,1:5])
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
e$Clusters <- k6$cluster
#Save 6c
#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
#write.csv(e,file="6Clusters")
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="6Clusters",header=TRUE)

library(rgl)
library(RColorBrewer)
library(scales)

palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(comp$PC1, comp$PC2, comp$PC3, col=e$Clusters)

text3d(pc$rotation[,1:3], texts=rownames(pc$rotation), col="black")

coords <- NULL
for (i in 1:nrow(pc$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$rotation[i,1:3]))
}
coords<-coords*10
lines3d(coords, col="red", lwd=4)
?lines3d


#Biplot
library(factoextra)
library(digest)
library(FactoMineR)

fviz_pca_biplot(pc, label="var", habillage=as.factor(e$Clusters),col.var = "black")+
  ggtitle("K6") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_ind(pc, geom="point")
fviz_pca_var(pc)

fviz_pca_biplot(pc, axes = c(3,4), label="var", habillage=as.factor(e$Clusters),col.var = "black") +
  labs(color=NULL) + ggtitle("K6") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(2,3), label="var", habillage=as.factor(e$Clusters),col.var = "black") +
  labs(color=NULL) + ggtitle("K6") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(3,4), label="var", habillage=as.factor(e$Clusters),col.var = "black") +
  labs(color=NULL) + ggtitle("K6") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(4,5), label="var", habillage=as.factor(e$Clusters),col.var = "black") +
  labs(color=NULL) + ggtitle("K6") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

rm(list=ls())
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="6Clusters",header=TRUE)
e<-e[,-c(1)]

fviz_pca_biplot(pc, label="var", habillage=as.factor(e$Clusters),col.var = "black") + 
 labs(color="Clusters") + ggtitle("PCA of wolf territories") + guides (shape = FALSE) +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

##########################################################
# Save many cluster possibilities to test the hypothesis
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="6Clusters",header=TRUE)
sd_e<- as.data.frame(scale(e[11:26]))
pc <- prcomp(sd_e)
comp <- data.frame(pc$x[,1:5])
#3c
k4 <- kmeans(comp, 3, nstart=25, iter.max=1000)
e$Clusters_3 <- k4$cluster
#4C
k4 <- kmeans(comp, 4, nstart=25, iter.max=1000)
e$Clusters_4 <- k4$cluster
#5C
k5 <- kmeans(comp, 5, nstart=25, iter.max=1000)
e$Clusters_5 <- k5$cluster
#7C
k7 <- kmeans(comp, 7, nstart=25, iter.max=1000)
e$Clusters_7 <- k7$cluster
#8C
k8 <- kmeans(comp, 8, nstart=25, iter.max=1000)
e$Clusters_8 <- k8$cluster
#9C
k9 <- kmeans(comp, 9, nstart=25, iter.max=1000)
e$Clusters_9 <- k9$cluster

#10C
k10 <- kmeans(comp, 10, nstart=25, iter.max=1000)
e$Clusters_10 <- k10$cluster


#Save
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
write.csv(e,file="ManyClusters")
#################################################################


#################################################################
#Try to divide in 6 clusters by using different clustering methods

#K-MEANS

pc <- prcomp(sd_e)
comp <- data.frame(pc$x[,1:5])
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
e$Clusters_kmeans <- k6$cluster

#HIERARCHICAL

d <- dist(sd_e, method = "euclidean") 
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups6 <- cutree(fit, k=6)
e$Clusters_hier <- groups6

#PAM (Partition Around Medoids)

#[k-medoid] is more robust to noise and outliers as compared to k-means because it minimizes 
#a sum of pairwise dissimilarities instead of a sum of squared Euclidean distances." k-means will 
#place the center of the cluster towards the outliers, whereas k-medoid will select one of the more 
#clustered members (the medoid) as the center.

library (fpc)
pam1<-pamk(sd_e,krange = 6,criterion = "asw",usepam = TRUE,scaling = FALSE)
e$Clusters_pam <- pam1$pamobject$clustering

#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
#write.csv(e,file="ManyClusterMethods")

##########################################################################





#Average and SD of variables for each cluster (APPENDIX)

ag1<-aggregate(. ~ Clusters, e, function(x) c(mean = mean(x)))
ag2<-aggregate(. ~ Clusters, e, function(x) c(sd = sd(x)))

library(tidyr)
ag1<-as.data.frame(t(ag1))
ag1<-ag1[-c(1:12), ]
ag1<-round(ag1,digits = 2)
ag2<-as.data.frame(t(ag2))
ag2<-ag2[-c(1:12),]
ag2<-round(ag2,digits = 2)

tab<-matrix(ncol = dim(ag1)[2], nrow = dim(ag1)[1])
for (i in 1:nrow(ag1)){
pas<-paste(ag1[i, ], " (", ag2[i, ], ")", sep="")
tab[i,]<-pas
}

tab<-as.data.frame(tab)
colnames(tab)<-c("Rough terrain (1)", "Intermediate human (2)", "High human (3)", 
                 "Intermediate rough (4)", "Bear (5)", "Moose (6)")
rownames(tab)<-c( "Human density", "Human-dominated", "Agriculture",
                  "Forest","Mire","Water","Mountain","Secondary road", "Main road",
                  "Bear","Altitude","Slope","Roughness","Remoteness","Moose")

write.csv(tab,file="Average_values")
