

# 1. NHBD graph showing estimate for:
  # - Each sex (Male, Female)
  # - Each dispersing length category (Short, Medium, Long)
  # - Each habitat definition method: dist, clust methods (Kmeans 6C, PAM 6C, hier 6C), Cluster div.(4 - 10 C)

# 2. Graph % of Natal habitats available in 6C kMEANS for each sex-traj

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
e1 <- left_join(e,disper)

#Include distance metric

e2 <- as.data.frame(e1[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e2) <- e1$X

d <- dist(e2,method = "euclidean")
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]


ID <- unique(e1$ID_individual)
e1$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e1$ID_individual==ID[i])
  
  e1$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  
}

#Save file with dispersal distances
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
write.csv(e1,"Data_NHBD_id_wolf_density_distances.csv") # Short (<40000), Medium (<200000), Long (>200000)


# Different cluster divisions
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
clu <- read.csv("ManyClusters")
  
  
# Different clustering methods
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
m <- read.csv("ManyClusterMethods")

# --- check correlations 
cor(e1[ ,c(10:24,26)])
e1 <- e1[ ,-which(colnames(e1) %in% c("humanlands_1","agri_1", "dem_1", "roughness_1", "mires_1", "roadens_sec1"))]
names(e1)


# prepare the table


df <-  data.frame(Sex=c(rep("M",30),rep("F",30)),
                  Trajectory= rep(c(rep("Short",10), rep("Medium",10), rep("Long", 10)),2),
                  Method= rep(c("Dist","Kmeans_6C", "Pam_6C", "Hier_6C","4C","5C","7C","8C","9C","10C"),6),
                  NHBD= rep(NA,60),		  
                  CI.2.5= rep(NA,60),
                  CI.97.5= rep(NA,60),       
                  Z=rep(NA,60),       
                  p=rep(NA,60))


# ---- MALES ----

  # ---- A. Short ----
    # ---- 1. Distance metric ----

e <- subset(e1,dist <= 40000 & Sex == "M")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 2. KMEANS 6C ----

e <- subset(e1,dist <= 40000 & Sex == "M")

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
names(clr)



c <- clogit(Category ~ E + wolf_density * E + Clusters + strata(ID_individual), clr)
c1 <- clogit(Category ~ E + wolf_density * E + strata(ID_individual), clr)

c2 <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
            + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
            + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	

c3 <- clogit(Category ~ E  + wolf_density * E  				
             + human_1 + forest_1 + water_1				
             + mountains_1 + mainroad_1 + bear_1 				
             + slope_1 + roadbuild_1 + moose_dens 				
             + strata(ID_individual), clr)	


summary(c3)
confint(c3,level = 0.95)

# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 48.42 % of available where natal


summary.c1<-  summary(c3)
conf.c1 <- confint(c3,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1,dist <= 40000 & Sex == "M")

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

c1 <- clogit(Category ~ E  + wolf_density * E  				
             + human_1 + forest_1 + water_1				
             + mountains_1 + mainroad_1 + bear_1 				
             + slope_1 + roadbuild_1 + moose_dens 				
             + strata(ID_individual), clr)	


summary(c1)
confint(c1,level = 0.95)


summary.c1<-  summary(c1)
conf.c1 <- confint(c1,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])

    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1,dist <= 40000 & Sex == "M")

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

c <- clogit(Category ~ E + wolf_density * E + Clusters + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1,dist <= 40000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1,dist <= 40000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1,dist <= 40000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1,dist <= 40000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1,dist <= 40000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1,dist <= 40000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Short" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])



  # ---- B. Medium ----
    # ---- 1. Distance metric ----

e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])

    # ---- 2. KMEANS 6C ----

e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])



# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 36.83 % of available where natal

    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])

    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Medium" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])


  # ---- C. Long ----
    # ---- 1. Distance metric ----

e <- subset(e1, dist > 200000 & Sex == "M")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 2. KMEANS 6C ----

e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])


# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 26.57 % of available where natal

    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1, dist > 200000 & Sex == "M")

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

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])




    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1, dist > 200000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1, dist > 200000 & Sex == "M")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1, dist > 200000 & Sex == "M")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="M" & df$Trajectory=="Long" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])


# ---- FEMALES ----

  # ---- A. Short ----
    # ---- 1. Distance metric ----

e <- subset(e1, dist <= 40000 & Sex == "F")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 2. KMEANS 6C ----

e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])



# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 47.42 % of available where natal

    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1, dist <= 40000 & Sex == "F")

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

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])




    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1, dist <= 40000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1, dist <= 40000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1, dist <= 40000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Short" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])



  # ---- B. Medium ----
    # ---- 1. Distance metric ----

e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 2. KMEANS 6C ----

e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])



# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 45.11 % of available where natal


    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])




    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1, 40000 < dist & dist <= 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Medium" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])


  # ---- C. Long ----
    # ---- 1. Distance metric ----

e <- subset(e1, dist > 200000 & Sex == "F")

e$Category <- as.character(e$Category)

cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + distance * wolf_density 				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), cd)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["distance",c(1,4,5)]
ci <- conf.c1["distance",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="Dist",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 2. KMEANS 6C ----

e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="Kmeans_6C",c(4:8)] <- c(values[1], ci, values[2:3])

which(clr$Category == 1) #It doesnt converge. Only 4 individuals!



# Explore % of available natal habitats
av <- clr[clr$Category == 0, ]
sum(av$E)/nrow(av)*100 # 34.09 % of available where natal

    # ---- 3. PAM 6C ----

e1$Clusters <- m$Clusters_pam # First run LOAD DATA
e <- subset(e1, dist > 200000 & Sex == "F")

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

c <- clogit(Category ~ E  + wolf_density * E  				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="Pam_6C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 4. HIER 6C ----

e1$Clusters <- m$Clusters_hier # First run LOAD DATA
e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="Hier_6C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 5. 4C KMEANS ----

e1$Clusters <- clu$Clusters_4
e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="4C",c(4:8)] <- c(values[1], ci, values[2:3])



    # ---- 6. 5C KMEANS ----

e1$Clusters <- clu$Clusters_5
e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="5C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 7. 7C KMEANS ----

e1$Clusters <- clu$Clusters_7
e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="7C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 8. 8C KMEANS ----

e1$Clusters <- clu$Clusters_8 
e <- subset(e1, dist > 200000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="8C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 9. 9C KMEANS ----

e1$Clusters <- clu$Clusters_9
e <- subset(e1, dist > 200000 & Sex == "F")


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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="9C",c(4:8)] <- c(values[1], ci, values[2:3])


    # ---- 10. 10C KMEANS ----

e1$Clusters <- p$Clusters_10
e <- subset(e1, dist > 200000 & Sex == "F")

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
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)

summary.c1<-  summary(c)
conf.c1 <- confint(c,level = 0.95)
values <- summary.c1$coefficients["E",c(1,4,5)]
ci <- conf.c1["E",]
df[df$Sex=="F" & df$Trajectory=="Long" & df$Method=="10C",c(4:8)] <- c(values[1], ci, values[2:3])





# 1---- RESULTS GRAPH ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
write.csv(df, "Results_NHBD.csv")

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
r <- read.csv("Results_NHBD.csv", sep = ",")

# Males
r <- r[c(1:30),c(3:7)]
#rescale dist 
r[r$Method=="Dist" ,3:5] <- r[r$Method=="Dist",3:5] *100*-1
#Remove methods that dont converge
r[r$Trajectory=="Short" & r$Method=="Hier_6C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="8C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="9C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="10C",3:5] <- NA
r[r$Trajectory=="Long" & r$Method=="Kmeans_6C",3:5] <- NA

r <- data.frame( rbind(head(r, 10), NA, tail(r, -10)), row.names = NULL)
r <- data.frame( rbind(head(r, 21), NA, tail(r, -21)), row.names = NULL)

x <- seq_along(r$NHBD)

cc <- palette()
c <- palette(c(cc,"purple","brown"))
c[c=="yellow"] <- "orange"
# rescale dist
par(mfrow=c(1,2))
plot(-30,ylim = c(-7,10), xlim=c(0,34),
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5) , ylab = "NHBD", xlab = " ", axes = FALSE, main = "Males")
axis(1,lwd.tick=0, labels = FALSE)
axis(1,at=c(11,22),labels=c("",""))

axis(1,at=c(6,16,29),labels=c("Short","Medium","Long"),tick = 0)

axis(2)
arrows(x,r$CI.2.5, x, r$CI.97.5, code=3, angle=90, length=0.04,col=c[r$Method])
points(r$NHBD,
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5))

abline(h=0, lty=2)


# For 8C and 9C cluster divisions, there is no individuals that establishes in the
# same cluster for long dispersing males (There is no E = 1 in the Category 1)

# Females

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
r <- read.csv("Results_NHBD.csv", sep = ",")
r <- r[c(31:60),c(3:7)]

#rescale dist 
r[r$Method=="Dist" ,3:5] <- r[r$Method=="Dist",3:5] *100*-1
#r[r$Trajectory=="Long" & r$Method=="8C",3:5] <- NA
#r[r$Trajectory=="Long" & r$Method=="9C",3:5] <- NA

r <- data.frame( rbind(head(r, 10), NA, tail(r, -10)), row.names = NULL)
r <- data.frame( rbind(head(r, 21), NA, tail(r, -21)), row.names = NULL)

x <- seq_along(r$NHBD)

cc <- palette()
c <- palette(c(cc,"purple","brown"))
c[c=="yellow"] <- "orange"
# rescale dist

plot(-30,ylim = c(-7,10), xlim=c(0,34),
     pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5) , ylab = "NHBD", xlab = " ", axes = FALSE, main = "Females")
axis(1,lwd.tick=0, labels = FALSE)
axis(1,at=c(11,22),labels=c("",""))

axis(1,at=c(6,16,29),labels=c("Short","Medium","Long"),tick = 0)

axis(2)
arrows(x, r$CI.2.5, x, r$CI.97.5, code=3, angle=90, length=0.04,col=c[r$Method])
points(r$NHBD,
       pch = 21, col = c[r$Method],bg=adjustcolor(c[r$Method],alpha.f = 0.5))

abline(h=0, lty=2)

legend(x=19,y=10, legend = unique(r$Method)[1:5], col = c[r$Method][1:5],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9) )
legend(x=30,y=10, legend = unique(r$Method)[6:10], col = c[r$Method][6:10],
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9)  )



# 2---- NATAL AV. GRAPH ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 3")
av <- read.csv("Natal_av.csv", sep = ";")

avm <- av[av$Sex == "Male",]
avf <- av[av$Sex == "Female",]

plot(avm$Av_natal, pch = 16, ylim = c(0,80), xlim = c(0.9,3.3),
     ylab = "% natal habitat available", xlab = " ", col = "blue", axes = FALSE)
lines(avm$Av_natal, col = "blue")

points(avf$Av_natal, col = "red", pch = 16)
lines(avf$Av_natal, col = "red")
axis(1,lwd.tick=0, labels = FALSE)
axis(1,at=c(11,22),labels=c("",""))
axis(1,at=c(1,2,3),labels=c("Short","Medium","Long"))
axis(2)

legend(x=2.5,y=80, legend = unique(av$Sex), col = c("blue","red"),
       pch = 18, cex = 0.85,bty="o",bg=grey(0.9),box.col=grey(0.9) )


# 3---- General results whole population(Coefficients env covariates and wolf density effect)----
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e <- read.csv("Data_NHBD_id_wolf_density_distances.csv")
e <- e[ ,-which(colnames(e) %in% c("humanlands_1","agri_1", "dem_1", "roughness_1", "mires_1", "roadens_sec1"))]

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

c <- clogit(Category ~ E  + wolf_density * E  + Sex * E				
            + human_1 + forest_1 + water_1				
            + mountains_1 + mainroad_1 + bear_1 				
            + slope_1 + roadbuild_1 + moose_dens 				
            + strata(ID_individual), clr)	
summary(c)
confint(c,level = 0.95)



# 4---- Export data for submission (6 K-means and distance metric)----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e1 <- read.csv("Data_NHBD_id_wolf_density_distances.csv") # File with dispersal distances
e1$ID_individual <- as.numeric(e1$ID_individual)
colnames(e1)[26] <- "Kmeans_6C"

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
clu <- read.csv("ManyClusters") # Different cluster divisions
e1$C4 <- clu$Clusters_4
e1$C5 <- clu$Clusters_5
e1$C7 <- clu$Clusters_7
e1$C8 <- clu$Clusters_8
e1$C9 <- clu$Clusters_9
e1$C10 <- clu$Clusters_10

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
m <- read.csv("ManyClusterMethods") # Different clustering methods
e1$Pam_6C <- m$Clusters_pam
e1$Hier_6C <- m$Clusters_hier

e1$dispersal_distance <- NA
e1$dispersal_distance <- as.character(e1$dispersal_distance)

for (i in 1:nrow(e1)) {
  if (e1$dist[i] <= 40000) {e1$dispersal_distance[i] = "Short"}
  if (e1$dist[i] > 40000 & e1$dist[i] <= 200000) {e1$dispersal_distance[i] = "Medium"}
  if (e1$dist[i] > 200000) {e1$dispersal_distance[i] = "Long"}
}

library(dplyr)
e1 <- arrange(e1, Sex, desc(dispersal_distance), ID_individual)

e1 <- e1[, which(colnames(e1) %in% c("ID_individual", "Sex", "Category",
                                   "human_1", "forest_1", "water_1", "mountains_1", "mainroad_1", "bear_1", 
                                   "slope_1" ,"roadbuild_1", "moose_dens", "wolf_density",
                                   "distance", "Kmeans_6C", "Pam_6C", "Hier_6C",
                                   "C4", "C5", "C7", "C8", "C9", "C10",
                                   "dispersal_distance"))]

write.csv(e1, "Data_resubmission2.csv")





 

