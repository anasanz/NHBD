

# NHBD graph showing estimate for:
  # - Each sex (Male, Female)
  # - Each dispersing length category (Short, Medium, Long)
  # - Each habitat definition method: dist, clust methods (Kmeans 6C, PAM 6C, hier 6C), Cluster div.(4 - 10 C)


rm(list=ls())

library(survival)

# ---- LOAD DATA ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e<-read.csv("Data_NHBD_id_wolf_density.csv")
e<-e[,-c(1,2)]
  
#Add dispersal distances
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")
disperB<-disper[ ,c("X_birth","Y_birth")]
disperE<-disper[ ,c("X_Established","Y_Established")]
x1<-as.matrix(disperB)
x2<-as.matrix(disperE)
  
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
dist <- NULL
for(i in 1:nrow(x1)) dist[i] <- euc.dist(x1[i,],x2[i,])
dist #Vector with distances between points
d<-as.data.frame(dist)
disper$dist <- d$dist
colnames(disper)[2] <- "ID_individual"
disper <- disper[ ,c("ID_individual","dist")]
  
library(dplyr)
e <- left_join(e,disper)

#Save file with dispersal distances
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
write.csv(e,"Data_NHBD_id_wolf_density_distances.csv")


# Different cluster divisions
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
clu <- read.csv("ManyClusters")
  
  
# Different clustering methods
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
m <- read.csv("ManyClusterMethods")

# ---- MALES ----

  # ---- A. Short ----
    # ---- 1. Distance metric ----

e <- subset(e,dist <= 40000 & Sex == "M")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
            + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
            strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,dist <= 40000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,dist <= 40000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,dist <= 40000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

  # ---- B. Medium ----
    # ---- 1. Distance metric ----

e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
              + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
              + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
              strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

  # ---- C. Long ----
    # ---- 1. Distance metric ----

e <- subset(e,dist > 200000 & Sex == "M")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
              + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
              + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
              strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,dist > 200000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,dist > 200000 & Sex == "M")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,dist > 200000 & Sex == "M")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

# ---- FEMALES ----

  # ---- A. Short ----
    # ---- 1. Distance metric ----

e <- subset(e,dist <= 40000 & Sex == "F")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
              + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
              + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
              strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,dist <= 40000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,dist <= 40000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,dist <= 40000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

  # ---- B. Medium ----
    # ---- 1. Distance metric ----

e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
              + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
              + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
              strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,40000 < dist & dist <= 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

  # ---- C. Long ----
    # ---- 1. Distance metric ----

e <- subset(e,dist > 200000 & Sex == "F")

e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens + 
              + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)


    # ---- 2. KMEANS 6C ----

e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)


    # ---- 3. PAM 6C ----

e$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Select variables
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E + wolf_density * E  + 
              + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens +
              strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

    # ---- 4. HIER 6C ----

e$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 5. 4C KMEANS ----

e$Clusters <- clu$Clusters_4
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)


#JOIN DATASETS
clr <- rbind(b1,b2,b3,b4)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)


    # ---- 6. 5C KMEANS ----

e$Clusters <- clu$Clusters_5
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)



#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5)

c <- clogit(Category ~ E  + wolf_density * E  
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 
            + strata(ID_individual), clr)

summary(c)
confint(c,level = 0.95)

    # ---- 7. 7C KMEANS ----

e$Clusters <- clu$Clusters_7
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6, b7)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)

    # ---- 8. 8C KMEANS ----

e$Clusters <- clu$Clusters_8 
e <- subset(e,dist > 200000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 9. 9C KMEANS ----

e$Clusters <- clu$Clusters_9
e <- subset(e,dist > 200000 & Sex == "F")


#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				

summary(c)
confint(c,level = 0.95)

    # ---- 10. 10C KMEANS ----

e$Clusters <- p$Clusters_10
e <- subset(e,dist > 200000 & Sex == "F")

#CLUSTER 1
cluster.1<-e[e$Clusters == 1, ]
length(which(cluster.1$Category == "Natal"))
natal<-cluster.1[cluster.1$Category == "Natal", ]
#Select only the IDS that are born in this cluster (natal territories + their available and established)
cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b1$Category <- as.character(b1$Category)
b1<-b1[!b1$Category == "Natal", ]
b1$Category[b1$Category == "Established"] <- 1
b1$Category[b1$Category == "Available"] <- 0
b1$Category<-as.numeric(b1$Category)
#Cluster as factor
b1$Clusters <- as.factor(b1$Clusters)
#Create E
b1$E<-b1$Category
b1$E <- 0
b1$E[b1$Clusters == 1] <- 1


#CLUSTER 2
cluster.2<-e[e$Clusters == 2, ]
length(which(cluster.2$Category == "Natal"))
natal<-cluster.2[cluster.2$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b2$Category <- as.character(b2$Category)
b2<-b2[!b2$Category == "Natal", ]
b2$Category[b2$Category == "Established"] <- 1
b2$Category[b2$Category == "Available"] <- 0
b2$Category<-as.numeric(b2$Category)
#Cluster as factor
b2$Clusters <- as.factor(b2$Clusters)
#Create E
b2$E<-b2$Category
b2$E <- 0
b2$E[b2$Clusters == 2] <- 1
unique(b2$ID_individual)


#CLUSTER 3

cluster.3<-e[e$Clusters == 3, ]
length(which(cluster.3$Category == "Natal"))
natal<-cluster.3[cluster.3$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b3$Category <- as.character(b3$Category)
b3<-b3[!b3$Category == "Natal", ]
b3$Category[b3$Category == "Established"] <- 1
b3$Category[b3$Category == "Available"] <- 0
b3$Category<-as.numeric(b3$Category)
#Cluster as factor
b3$Clusters <- as.factor(b3$Clusters)
#Create E
b3$E<-b3$Category
b3$E <- 0
b3$E[b3$Clusters == 3] <- 1
unique(b3$ID_individual)


#CLUSTER 4
cluster.4<-e[e$Clusters == 4, ]
length(which(cluster.4$Category == "Natal"))
natal<-cluster.4[cluster.4$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b4$Category <- as.character(b4$Category)
b4<-b4[!b4$Category == "Natal", ]
b4$Category[b4$Category == "Established"] <- 1
b4$Category[b4$Category == "Available"] <- 0
b4$Category<-as.numeric(b4$Category)
#Cluster as factor
b4$Clusters <- as.factor(b4$Clusters)
#Create E
b4$E<-b4$Category
b4$E <- 0
b4$E[b4$Clusters == 4] <- 1
unique(b4$ID_individual)

#CLUSTER 5

cluster.5<-e[e$Clusters == 5, ]
length(which(cluster.5$Category == "Natal"))
natal<-cluster.5[cluster.5$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b5$Category <- as.character(b5$Category)
b5<-b5[!b5$Category == "Natal", ]
b5$Category[b5$Category == "Established"] <- 1
b5$Category[b5$Category == "Available"] <- 0
b5$Category<-as.numeric(b5$Category)
#Cluster as factor
b5$Clusters <- as.factor(b5$Clusters)
#Create E
b5$E<-b5$Category
b5$E <- 0
b5$E[b5$Clusters == 5] <- 1
unique(b5$ID_individual)


#CLUSTER 6

cluster.6<-e[e$Clusters == 6, ]
length(which(cluster.6$Category == "Natal"))
natal<-cluster.6[cluster.6$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b6$Category <- as.character(b6$Category)
b6<-b6[!b6$Category == "Natal", ]
b6$Category[b6$Category == "Established"] <- 1
b6$Category[b6$Category == "Available"] <- 0
b6$Category<-as.numeric(b6$Category)
#Cluster as factor
b6$Clusters <- as.factor(b6$Clusters)
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)

#CLUSTER 7

cluster.7<-e[e$Clusters == 7, ]
length(which(cluster.7$Category == "Natal"))
natal<-cluster.7[cluster.7$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl7 <- cluster.7[which(cluster.7$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b7<-e[which(e$ID_individual %in% cl7$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b7$Category <- as.character(b7$Category)
b7<-b7[!b7$Category == "Natal", ]
b7$Category[b7$Category == "Established"] <- 1
b7$Category[b7$Category == "Available"] <- 0
b7$Category<-as.numeric(b7$Category)
#Cluster as factor
b7$Clusters <- as.factor(b7$Clusters)
#Create E
b7$E<-b7$Category
b7$E <- 0
b7$E[b7$Clusters == 7] <- 1
unique(b7$ID_individual)

#CLUSTER 8

cluster.8<-e[e$Clusters == 8, ]
length(which(cluster.8$Category == "Natal"))
natal<-cluster.8[cluster.8$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl8 <- cluster.8[which(cluster.8$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b8<-e[which(e$ID_individual %in% cl8$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b8$Category <- as.character(b8$Category)
b8<-b8[!b8$Category == "Natal", ]
b8$Category[b8$Category == "Established"] <- 1
b8$Category[b8$Category == "Available"] <- 0
b8$Category<-as.numeric(b8$Category)
#Cluster as factor
b8$Clusters <- as.factor(b8$Clusters)
#Create E
b8$E<-b8$Category
b8$E <- 0
b8$E[b8$Clusters == 8] <- 1
unique(b8$ID_individual)

#CLUSTER 9

cluster.9<-e[e$Clusters == 9, ]
length(which(cluster.9$Category == "Natal"))
natal<-cluster.9[cluster.9$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl9 <- cluster.9[which(cluster.9$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b9<-e[which(e$ID_individual %in% cl9$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b9$Category <- as.character(b9$Category)
b9<-b9[!b9$Category == "Natal", ]
b9$Category[b9$Category == "Established"] <- 1
b9$Category[b9$Category == "Available"] <- 0
b9$Category<-as.numeric(b9$Category)
#Cluster as factor
b9$Clusters <- as.factor(b9$Clusters)
#Create E
b9$E<-b9$Category
b9$E <- 0
b9$E[b9$Clusters == 9] <- 1
unique(b9$ID_individual)

#CLUSTER 10

cluster.10<-e[e$Clusters == 10, ]
length(which(cluster.10$Category == "Natal"))
natal<-cluster.10[cluster.10$Category == "Natal", ]
#Select only the IDS that are born in this cluster
cl10 <- cluster.10[which(cluster.10$ID_individual %in% natal$ID_individual), ]
#For the IDs born in cl, where did they establish and what did they have available?
b10<-e[which(e$ID_individual %in% cl10$ID_individual), ]

#Transform category to binary variable: CASE-CONTROL
b10$Category <- as.character(b10$Category)
b10<-b10[!b10$Category == "Natal", ]
b10$Category[b10$Category == "Established"] <- 1
b10$Category[b10$Category == "Available"] <- 0
b10$Category<-as.numeric(b10$Category)
#Cluster as factor
b10$Clusters <- as.factor(b10$Clusters)
#Create E
b10$E<-b10$Category
b10$E <- 0
b10$E[b10$Clusters == 10] <- 1
unique(b10$ID_individual)

#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)				
summary(c)
confint(c,level = 0.95)


# ---- RESULTS GRAPH ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
r <- read.csv("Results_NHBD.csv", sep = ";")

# Males
r <- r[c(1:30),c(2:6)]
#rescale dist 
r[r$Method=="Dist" ,3:5] <- r[r$Method=="Dist",3:5] *100*-1
r[r$Trajectory=="Long" & r$Method=="8C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="9C",3:5] <- NA

r <- data.frame( rbind(head(r, 10), NA, tail(r, -10)), row.names = NULL)
r <- data.frame( rbind(head(r, 21), NA, tail(r, -21)), row.names = NULL)

x <- seq_along(r$NHBD)

cc <- palette()
c <- palette(c(cc,"purple","brown"))
c[c=="yellow"] <- "orange"
# rescale dist

plot(-30,ylim = c(-10,22), xlim=c(0,34),
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5) , ylab = "NHBD", xlab = " ", axes = FALSE, main = "Males")
axis(1,lwd.tick=0, labels = FALSE)
axis(1,at=c(11,22),labels=c("",""))

axis(1,at=c(6,16,29),labels=c("Short","Medium","Long"),tick = 0)

axis(2)
arrows(x,r$X2.50.,x, r$X97.50., code=3, angle=90, length=0.04,col=c[r$Method])
points(r$NHBD,
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5))

abline(h=0, lty=2)

legend(x=23,y=20, legend = unique(r$Method)[1:5], col = c[r$Method][1:5],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9) )
legend(x=29,y=20, legend = unique(r$Method)[6:10], col = c[r$Method][6:10],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9)  )

# For 8C and 9C cluster divisions, there is no individuals that establishes in the
# same cluster for long dispersing males (There is no E = 1 in the Category 1)

# Females

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
r <- read.csv("Results_NHBD.csv", sep = ";")
r <- r[c(31:60),c(2:6)]

#rescale dist 
r[r$Method=="Dist" ,3:5] <- r[r$Method=="Dist",3:5] *100*-1
r[r$Trajectory=="Long" & r$Method=="8C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="9C",3:5] <- NA

r <- data.frame( rbind(head(r, 10), NA, tail(r, -10)), row.names = NULL)
r <- data.frame( rbind(head(r, 21), NA, tail(r, -21)), row.names = NULL)

x <- seq_along(r$NHBD)

cc <- palette()
c <- palette(c(cc,"purple","brown"))
c[c=="yellow"] <- "orange"
# rescale dist

plot(-30,ylim = c(-7,15), xlim=c(0,34),
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5) , ylab = "NHBD", xlab = " ", axes = FALSE, main = "Females")
axis(1,lwd.tick=0, labels = FALSE)
axis(1,at=c(11,22),labels=c("",""))

axis(1,at=c(6,16,29),labels=c("Short","Medium","Long"),tick = 0)

axis(2)
arrows(x,r$X2.50.,x, r$X97.50., code=3, angle=90, length=0.04,col=c[r$Method])
points(r$NHBD,
       pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5))

abline(h=0, lty=2)

legend(x=23,y=15, legend = unique(r$Method)[1:5], col = c[r$Method][1:5],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9) )
legend(x=29,y=15, legend = unique(r$Method)[6:10], col = c[r$Method][6:10],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9)  )