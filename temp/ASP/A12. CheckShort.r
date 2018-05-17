
library(raster)
library(sp)
library("rgdal", lib.loc="~/R/win-library/3.1")

#Check strange trajectories

#Natal coordinates
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")

shortB<-disper[ which(disper$ID %in% unique(short.strange$ID_individual)),c("ID","X_birth","Y_birth")]
coordinates(shortB) <- c("X_birth","Y_birth")
shortB_xy<- coordinates(shortB)

shortE<-disper[ which(disper$ID %in% unique(short.strange$ID_individual)),c("ID","X_Established","Y_Established")]
coordinates(shortE) <- c("X_Established","Y_Established")
shortE_xy<- coordinates(shortE)

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("short.RData")
load("Short_ID.RData")#ID and year

short$ID2 <- 1:nrow(short)
ss <- short[which(short$ID %in% short.strange$ID_individual), ]

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS")
study_area <- readOGR(".", "hand_study_area1")
study_area <- study_area[which(study_area$KUSTX_ %in% c(35)),]##keep only sweden and norway  and small lakes.
plot(study_area)

strange <- list(h[[7]], h[[8]], h[[12]], h[[19]], h[[40]], h[[47]])

 for (i in 1:length(strange)){
   
   for (j in 1:11){
     
     plot(study_area)
     plot(strange[[i]][[j]][[1]], add = TRUE, cex = 0.1) 
     points(shortB[i, ], col = "red", cex = 2)
     points(shortE[i, ], col = "red", cex = 2)
   }
   print(i)
   
 }

# Medium

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("h1.RData")
load("Medium_ID.RData")#ID and year

mediumB<-disper[ which(disper$ID %in% unique(medium$ID)),c("ID","X_birth","Y_birth")]
coordinates(mediumB) <- c("X_birth","Y_birth")
mediumB_xy<- coordinates(mediumB)

mediumE<-disper[ which(disper$ID %in% unique(medium$ID)),c("ID","X_Established","Y_Established")]
coordinates(mediumE) <- c("X_Established","Y_Established")
mediumE_xy<- coordinates(mediumE)


for (i in 1:length(h)){
  
  for (j in 1:11){
    
    plot(study_area)
    plot(h[[i]][[j]], add = TRUE, cex = 0.1) 
    points(mediumB[i, ], col = "red", cex = 2)
    points(mediumE[i, ], col = "red", cex = 2)
  }
  print(i)
  
}