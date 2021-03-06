


rm(list=ls())
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e<-read.csv(file="extracted_values_study_area1.csv",header=TRUE)

e<-e[,-c(1,2)]
e<-e[,c(1:3,5:8,4,9:25)]
e<-e[ ,-23] #REMOVE BUILDINGS

e <- e[complete.cases(e[8:23]), ]
which(is.na(e[8:23]))

#PCA and CLusters
sd_e<- as.data.frame(scale(e[8:23]))
pc <- prcomp(sd_e)
comp <- data.frame(pc$x[,1:5])
#6 KMEANS
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
e$Clusters <- k6$cluster
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
write.csv(e,file="clusters_study_area.csv")


#CLR

#CLR 6 CLUSTER DIVISION

library(survival)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e<-read.csv("clusters_study_area.csv")

library(survival)
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
#Select variables
b1<-b1[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
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
#Select variables
b2<-b2[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
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
#Select variables
b3<-b3[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
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
#Select variables
b4<-b4[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
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
#Select variables
b5<-b5[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
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
b6<-b6[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
#Create E
b6$E<-b6$Category
b6$E <- 0
b6$E[b6$Clusters == 6] <- 1
unique(b6$ID_individual)


#JOIN DATASETS
clr <- rbind( b1,b2,b3,b4,b5,b6)

c <- clogit(Category ~ Clusters + E + wolf_density + wolf_density * E  + Sex * E + strata(ID_individual), clr)
summary(c)
confint(c,level = 0.95)




