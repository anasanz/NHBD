## LOAD NECESSARY PACKAGES 
rm(list = ls())
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(survival)
library(dplyr)
library(snow)
library(splitstackshape)
library(stringr)

## SET WORKING DIRECTORY 
# setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data")
# setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
#setwd("~/Documents/NHBD/Data")

## ---- I. LOAD NECESSARY OBJECTS ----
## ====  1. load the dispersal distances ====
setwd("C:/My_documents/ana/nhbd/NHBD/Data")
distances <- read.csv("Data_NHBD_id_wolf_density_distances.csv", header = T)
  # KEEP ONLY NECESSARY COLUMNS 
distances <- distances[,c("ID_individual","Year", "dist","x", "y","Category")]

## ====  2. LOAD THE WOLF DENSITY ====
pack<-read.table("packs_and_pairs12.txt",dec = ".", head = T,fill=TRUE)


######        REMOVING PACKS           
#    I removed the unknown territory with unknown ID:
#   -1998/99  scent_marking_pair  Bracke
#   -1998/99	pack	                Dals-Ed-Halden
#   -1999/00	pack	                Dals_Ed-Halden
#   -2000/01	pack	                Dals_Ed-Halden
#   -2010/11	scent_marking_pair	Eidskog
#   -2003/04	scent_marking_pair	Liljendal
#   -2007/08	scent_marking_pair	Mangen
#   -2011/12	scent_marking_pair	Varaldskog
#   -2001/02	scent_marking_pair	Tisjon
#   -2002/03	scent_marking_pair	Tisjon
#   -1998/99	scent_marking_pair	Stollet-Malung    
#####Create a column with winter_terrtiory to delete them
pack$winter_territory<-paste(pack$winter,pack$territory,sep="_")

###### Delete the territories where i don't know the ID###
list_ter<-c("1998/99_Bracke",
            "1998/99_Dals-Ed-Halden",
            "1999/00_Dals_Ed-Halden",
            "2000/01_Dals_Ed-Halden",
            "2004/05_Julussa",
            "2003/04_Liljendal",
            "2007/08_Mangen",
            "2004/05_Romskog",
            "2005/06_Romskog",
            "1998/99_Stollet-Malung",
            "2011/12_Varaldskog")

for (i in 1:length(list_ter)){
  pack <- pack[-which(pack$winter_territory==list_ter[i]), ]
}

## Remove all territories before winter 1998/1999
year <- str_split_fixed(pack$winter, "/", 2)
pack$year <- as.numeric(year[,1])
pack <- pack[-which(pack$year < 1998), ]

#count the nb of couples years
hey <- paste(pack$ID_F, pack$ID_M, sep="_")

######          PACK DENSITY         


####FIND THE NBR OF WOLF IN A RADIUS OF 40000km and for each year 
pack$density <- 0
pack$territory <- as.character(pack$territory)
pack$winter <- as.character(pack$winter)
pack$X <- as.numeric(pack$X)
pack$Y <- as.numeric(pack$Y)

## ---- II. LOAD ALL BUFFER SIZES IN LOOPS ----
## ====  1. GET LOOP READY ====
setwd("C:/My_documents/ana/nhbd/NHBD/Data/NHBD_BUFFER")
buffer.size <- seq(25000, 300000, by=5000)
nhbd_short_M<- nhbd_short_F<- nhbd_medium_M<- nhbd_medium_F<- nhbd_long_M<- nhbd_long_F<- 0
nhbd_short_M_hier<- nhbd_short_F_hier<- nhbd_medium_M_hier<- nhbd_medium_F_hier<- nhbd_long_M_hier<- nhbd_long_F_hier<- 0
nhbd_short_M_pam<- nhbd_short_F_pam<- nhbd_medium_M_pam<- nhbd_medium_F_pam<- nhbd_long_M_pam<- nhbd_long_F_pam<- 0

nhbd_short_M_Kmeans <- nhbd_short_F_Kmeans <- nhbd_medium_M_Kmeans <- list()
nhbd_medium_F_Kmeans <- nhbd_long_M_Kmeans <- nhbd_long_F_Kmeans <- list()
for(i in 1:8){
nhbd_short_M_Kmeans[[i]] <- nhbd_short_F_Kmeans[[i]] <- nhbd_medium_M_Kmeans[[i]] <- 0
nhbd_medium_F_Kmeans[[i]] <- nhbd_long_M_Kmeans[[i]] <- nhbd_long_F_Kmeans[[i]] <- 0
}
xxx=10
for(xxx in 1:length(buffer.size)){

## ====  2. LOAD e OBJECT WITH AVAIALBLE POINTS DRAWN FROM BUFFER SIZE ====
  load(paste("e",buffer.size[xxx] ,".RData", sep=""))
  colnames(e)[7:8] <-  c("coords.x1","coords.x2")
## ====  3. UPDATE THE DISPERSAL DISTANCES AND X Y FOR NATAL AND ESTABLISHED TERRITORY ====
  e$disp_distance <- 0
  ID <- unique(distances$ID_individual)
  for(i in 1:length(ID)){
    e[e$ID_individual==ID[i],]$disp_distance  <- distances[distances$ID_individual== ID[i],]$dist
    e[e$ID_individual==ID[i] & e$Category=="Established",]$coords.x1  <- distances[distances$ID_individual== ID[i] & distances$Category=="Established",]$x
    e[e$ID_individual==ID[i] & e$Category=="Established" ,]$coords.x2  <- distances[distances$ID_individual== ID[i] & distances$Category=="Established",]$y
    e[e$ID_individual==ID[i] & e$Category=="Natal" ,]$coords.x1  <- distances[distances$ID_individual== ID[i] & distances$Category=="Natal",]$x
    e[e$ID_individual==ID[i] & e$Category=="Natal" ,]$coords.x2  <- distances[distances$ID_individual== ID[i] & distances$Category=="Natal",]$y
  }
  
  
## ====  4. UPDATE THE WOLF DENSITY ====
  
  e$wolf_density1 <- 0
  
  
  for (i in 1:length(e$wolf_density)){
    tmp <- pack[pack$year == e$Year[i],]
    
    for (j in 1:nrow(tmp)){
      
      ###FOrmula to get the distances between points#
      if (sqrt((tmp$X[j]-e$coords.x2[i])*(tmp$X[j]-e$coords.x2[i])+(tmp$Y[j]-e$coords.x1[i])*(tmp$Y[j]-e$coords.x1[i]))< 40000){
        e$wolf_density1[i] <- e$wolf_density1[i]+ 1} 
      else{e$wolf_density1[i] <- e$wolf_density1[i]+ 0}
    }
  }
  
  ## JUST CHECK IF IT IS OKAY #
  # e$wolf_density1==e$wolf_density
  
## ====  5. SUBSET TO NECESSARY COLUMNS ====
  
  e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  row.names(e1) <- e$X
  d <- dist(e1,method = "euclidean")
  d1 <- as.matrix(d)
  ## ====  6. DISTANCE METRIC ====
  
  ID <- unique(e$ID_individual)
  e$distance <- 0
  for(i in 1:length(ID)){
    IDD <- which(e$ID_individual==ID[i])
    e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
  }
  
  # PREPAPRE THE DATA SET FOR CONDITIONNAL LOGISTIC REGRESSION  
  e$Category <- as.character(e$Category)
  cd <- e[e$Category!="Natal",]
  
  cd$Category[cd$Category == "Established"] <- 1
  cd$Category[cd$Category == "Available"] <- 0
  cd$Category <- as.numeric(cd$Category)
  
  cd$Sex <- as.character(cd$Sex)
  short <- subset(cd, disp_distance <= 40000)
  short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  medium <- subset(cd,40000 < disp_distance & disp_distance <= 200000)
  medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long <- subset(cd, disp_distance > 200000)
  long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long_M <- subset(long, Sex== "M")
  long_F <- subset(long, Sex== "F")
  short_M <- subset(short, Sex== "M")
  short_F <- subset(short, Sex== "F")
  medium_M <- subset(medium, Sex== "M")
  medium_F <- subset(medium, Sex== "F")
  
  c_short_M <- summary(clogit(Category ~ distance  		
                    + strata(ID_individual), short_M))
  c_short_F <- summary(clogit(Category ~ distance  		
                    + strata(ID_individual), short_F))	
  
  c_medium_M <- summary(clogit(Category ~ distance  	
                     + strata(ID_individual), medium_M))	
  c_medium_F <- summary(clogit(Category ~ distance  	
                       + strata(ID_individual), medium_F))

  c_long_M <- summary(clogit(Category ~ distance  				
                   + strata(ID_individual), long_M))
  c_long_F <- summary(clogit(Category ~ distance  				
                     + strata(ID_individual), long_F))
  
  
  # c_short_F <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #             + strata(ID_individual), short_F))
  # c_short_M <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #                   + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #                   + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #                   + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #                   + strata(ID_individual), short_M))
  # 
  # c_medium_M <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #                      + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #                      + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #                      + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #                      + strata(ID_individual), medium_M))
  # c_medium_F <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #             + strata(ID_individual), medium_F))
  # 
  # 
  # c_long_F <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #             + strata(ID_individual), long_F))
  # c_long_M <- summary(clogit(Category ~ distance  + wolf_density1 * distance
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens
  #             + strata(ID_individual), long_M))
  
  
  nhbd_short_M[xxx]<- (c_short_M$coefficients)[1]
  nhbd_short_F[xxx]<- (c_short_F$coefficients)[1]
  
  nhbd_medium_M[xxx]<- (c_medium_M$coefficients)[1]
  nhbd_medium_F[xxx]<- (c_medium_F$coefficients)[1]
  
  nhbd_long_M[xxx]<- (c_long_M$coefficients)[1]
  nhbd_long_F[xxx]<- (c_long_F$coefficients)[1]
  
  ## ====  7. kMEANS ====
    if(sum(is.na(e$roadbuild_1))>0){
  e1 <- e[- which(is.na(e$roadbuild_1)),]
  }else{
    e1 <- e
  }
  sd_e <- as.data.frame(scale(e1[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                               "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                              "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")]))
  
  sd_e <- na.omit(sd_e)
  pc <- prcomp(sd_e)
  comp <- data.frame(pc$x[,1:5])
  
  #RUN THE CLUSTERS 
  CLUSTERS <- c(3:10)
  for(kk in 1:length(CLUSTERS)){
  k <- kmeans(comp, CLUSTERS[kk], nstart=25, iter.max=1000)
  sd_e$Clusters <- k$cluster
  sd_e$ID_individual <- e1$ID_individual
  sd_e$disp_distance <- e1$disp_distance
  sd_e$Sex <- e1$Sex
  
  ### make the NHBD variable 
  sd_e$Category <- as.character(e1$Category)
  sd_e$NHBD <- 0
  IDDD <- unique(sd_e$ID_individual)
  for( ii in 1:length(IDDD)){
    tmp <- sd_e[sd_e$ID_individual==IDDD[ii],]
    sd_e[sd_e$ID_individual==IDDD[ii],]$NHBD <- as.numeric(tmp$Clusters == tmp[tmp$Category=="Natal",]$Clusters)
  }
  
  
  
  sd_e1 <- sd_e[sd_e$Category!="Natal",]
  
  sd_e1$Category[sd_e1$Category == "Established"] <- 1
  sd_e1$Category[sd_e1$Category == "Available"] <- 0
  sd_e1$Category <- as.numeric(sd_e1$Category)
  
  sd_e1$Sex <- as.character(sd_e1$Sex)
  short <- subset(sd_e1, disp_distance <= 40000)
  short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  medium <- subset(sd_e1,40000 < disp_distance & disp_distance <= 200000)
  medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long <- subset(sd_e1, disp_distance > 200000)
  long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                        "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                        "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long_M <- subset(long, Sex== "M")
  long_F <- subset(long, Sex== "F")
  short_M <- subset(short, Sex== "M")
  short_F <- subset(short, Sex== "F")
  medium_M <- subset(medium, Sex== "M")
  medium_F <- subset(medium, Sex== "F")
  
  c_short_M <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_M))
  c_short_F <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_F))	
  
  c_medium_M <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_M))	
  c_medium_F <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_F))
  
  c_long_M <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_M))
  c_long_F <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_F))
  
  
  nhbd_short_M_Kmeans[[kk]][xxx]<- (c_short_M$coefficients)[1]
  nhbd_short_F_Kmeans[[kk]][xxx]<- (c_short_F$coefficients)[1]
  
  nhbd_medium_M_Kmeans[[kk]][xxx]<- (c_medium_M$coefficients)[1]
  nhbd_medium_F_Kmeans[[kk]][xxx]<- (c_medium_F$coefficients)[1]
  
  nhbd_long_M_Kmeans[[kk]][xxx]<- (c_long_M$coefficients)[1]
  nhbd_long_F_Kmeans[[kk]][xxx]<- (c_long_F$coefficients)[1]
  
  
  }
  
  ## ====  8. HIERARCHICAL CLUSTERING 6 CLUSTERS====
  
  sd_e2 <- sd_e[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
    "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
    "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")]
  d <- dist(sd_e2, method = "euclidean") 
  fit <- hclust(d, method="ward.D") 
  plot(fit) # display dendogram
  groups6 <- cutree(fit, k=6)
  sd_e$Clusters <- groups6
  sd_e$ID_individual <- e1$ID_individual
  sd_e$disp_distance <- e1$disp_distance
  sd_e$Sex <- e1$Sex
  
  ### make the NHBD variable 
  sd_e$Category <- as.character(e1$Category)
  sd_e$NHBD <- 0
  IDDD <- unique(sd_e$ID_individual)
  for( ii in 1:length(IDDD)){
    tmp <- sd_e[sd_e$ID_individual==IDDD[ii],]
    sd_e[sd_e$ID_individual==IDDD[ii],]$NHBD <- as.numeric(tmp$Clusters == tmp[tmp$Category=="Natal",]$Clusters)
  }
  
  
  
  sd_e1 <- sd_e[sd_e$Category!="Natal",]
  
  sd_e1$Category[sd_e1$Category == "Established"] <- 1
  sd_e1$Category[sd_e1$Category == "Available"] <- 0
  sd_e1$Category <- as.numeric(sd_e1$Category)
  
  sd_e1$Sex <- as.character(sd_e1$Sex)
  short <- subset(sd_e1, disp_distance <= 40000)
  short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  medium <- subset(sd_e1,40000 < disp_distance & disp_distance <= 200000)
  medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long <- subset(sd_e1, disp_distance > 200000)
  long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                        "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                        "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long_M <- subset(long, Sex== "M")
  long_F <- subset(long, Sex== "F")
  short_M <- subset(short, Sex== "M")
  short_F <- subset(short, Sex== "F")
  medium_M <- subset(medium, Sex== "M")
  medium_F <- subset(medium, Sex== "F")
  
  c_short_M <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_M))
  c_short_F <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_F))	
  
  c_medium_M <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_M))	
  c_medium_F <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_F))
  
  c_long_M <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_M))
  c_long_F <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_F))
  
  
  nhbd_short_M_hier[xxx]<- (c_short_M$coefficients)[1]
  nhbd_short_F_hier[xxx]<- (c_short_F$coefficients)[1]
  
  nhbd_medium_M_hier[xxx]<- (c_medium_M$coefficients)[1]
  nhbd_medium_F_hier[xxx]<- (c_medium_F$coefficients)[1]
  
  nhbd_long_M_hier[xxx]<- (c_long_M$coefficients)[1]
  nhbd_long_F_hier[xxx]<- (c_long_F$coefficients)[1]
  
  
  ## ====  9. PAM CLUSTERING 6 CLUSTERS====
  library(fpc)
  pam1 <- pamk(sd_e2, krange = 6, criterion = "asw", usepam = TRUE, scaling = FALSE)
  sd_e$Clusters <- pam1$pamobject$clustering
  sd_e$ID_individual <- e1$ID_individual
  sd_e$disp_distance <- e1$disp_distance
  sd_e$Sex <- e1$Sex
  
  ### make the NHBD variable 
  sd_e$Category <- as.character(e1$Category)
  sd_e$NHBD <- 0
  IDDD <- unique(sd_e$ID_individual)
  for( ii in 1:length(IDDD)){
    tmp <- sd_e[sd_e$ID_individual==IDDD[ii],]
    sd_e[sd_e$ID_individual==IDDD[ii],]$NHBD <- as.numeric(tmp$Clusters == tmp[tmp$Category=="Natal",]$Clusters)
  }
  
  
  
  sd_e1 <- sd_e[sd_e$Category!="Natal",]
  
  sd_e1$Category[sd_e1$Category == "Established"] <- 1
  sd_e1$Category[sd_e1$Category == "Available"] <- 0
  sd_e1$Category <- as.numeric(sd_e1$Category)
  
  sd_e1$Sex <- as.character(sd_e1$Sex)
  short <- subset(sd_e1, disp_distance <= 40000)
  short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
           "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(short[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  medium <- subset(sd_e1,40000 < disp_distance & disp_distance <= 200000)
  medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(medium[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                            "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                            "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long <- subset(sd_e1, disp_distance > 200000)
  long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
          "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
          "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")] <- scale(long[,c("human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                                                                                        "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                                                                                        "roughness_1", "roadbuild_1", "moose_dens","wolf_density1")])
  
  long_M <- subset(long, Sex== "M")
  long_F <- subset(long, Sex== "F")
  short_M <- subset(short, Sex== "M")
  short_F <- subset(short, Sex== "F")
  medium_M <- subset(medium, Sex== "M")
  medium_F <- subset(medium, Sex== "F")
  
  c_short_M <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_M))
  c_short_F <- summary(clogit(Category ~ NHBD  		
                              + strata(ID_individual), short_F))	
  
  c_medium_M <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_M))	
  c_medium_F <- summary(clogit(Category ~ NHBD  	
                               + strata(ID_individual), medium_F))
  
  c_long_M <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_M))
  c_long_F <- summary(clogit(Category ~ NHBD  				
                             + strata(ID_individual), long_F))
  
  
  nhbd_short_M_pam[xxx]<- (c_short_M$coefficients)[1]
  nhbd_short_F_pam[xxx]<- (c_short_F$coefficients)[1]
  
  nhbd_medium_M_pam[xxx]<- (c_medium_M$coefficients)[1]
  nhbd_medium_F_pam[xxx]<- (c_medium_F$coefficients)[1]
  
  nhbd_long_M_pam[xxx]<- (c_long_M$coefficients)[1]
  nhbd_long_F_pam[xxx]<- (c_long_F$coefficients)[1]
  
  
    print(xxx)
}  


pdf(paste("buffer_Distance",".pdf",sep=""))
par(mfrow=c(1,3))
plot(nhbd_short_M*-1~buffer.size, pch=16, ylab= "NHBD coeff",main="SHORT", ylim=c( -0,0.04))
points(nhbd_short_F*-1~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_short_M*-1, y0= nhbd_short_F*-1)

abline(h=0)
plot(nhbd_medium_M*-1~buffer.size, pch=16, ylab= "NHBD coeff",main="MEDIUM", ylim=c( -0.0080,0.010))
points(nhbd_medium_F*-1~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_medium_M*-1, y0= nhbd_medium_F*-1)


plot(nhbd_long_M*-1~buffer.size, pch=16, ylab= "NHBD coeff",main="LONG", ylim=c( -0.009,0.001))
points(nhbd_long_F*-1~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_long_F*-1, y0= nhbd_long_M*-1)
dev.off()


######
setwd("C:/My_documents/ana/nhbd/NHBD/temp/CM")
for(i in 1:8){
pdf(paste("buffer_Kmeans",CLUSTERS[i],".pdf",sep=""))
par(mfrow=c(1,3))
plot(nhbd_short_M_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",main="SHORT", ylim=c( -1,4))
points(nhbd_short_F_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_short_M_Kmeans[[i]], y0= nhbd_short_F_Kmeans[[i]])
abline(h=0)

plot(nhbd_medium_M_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",main="MEDIUM", ylim=c( -1.5,2.5))
points(nhbd_medium_F_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_medium_M_Kmeans[[i]], y0= nhbd_medium_F_Kmeans[[i]])


plot(nhbd_long_M_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",main="LONG", ylim=c( -3.5,1.5))
points(nhbd_long_F_Kmeans[[i]]~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_long_F_Kmeans[[i]], y0= nhbd_long_M_Kmeans[[i]])
dev.off()
}


###PAM
pdf(paste("buffer_PAM_6",".pdf",sep=""))
par(mfrow=c(1,3))
plot(nhbd_short_M_pam~buffer.size, pch=16, ylab= "NHBD coeff",main="SHORT", ylim=c( -1,4))
points(nhbd_short_F_pam~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_short_M_pam, y0= nhbd_short_F_pam)
abline(h=0)

plot(nhbd_medium_M_pam~buffer.size, pch=16, ylab= "NHBD coeff",main="MEDIUM", ylim=c( -1.5,2.5))
points(nhbd_medium_F_pam~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_medium_M_pam, y0= nhbd_medium_F_pam)


plot(nhbd_long_M_pam~buffer.size, pch=16, ylab= "NHBD coeff",main="LONG", ylim=c( -3.5,1.5))
points(nhbd_long_F_pam~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_long_F_pam, y0= nhbd_long_M_pam)
dev.off()

##
pdf(paste("buffer_hier_6",".pdf",sep=""))
par(mfrow=c(1,3))
plot(nhbd_short_M_hier~buffer.size, pch=16, ylab= "NHBD coeff",main="SHORT", ylim=c( -1,4))
points(nhbd_short_F_hier~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_short_M_hier, y0= nhbd_short_F_hier)
abline(h=0)

plot(nhbd_medium_M_hier~buffer.size, pch=16, ylab= "NHBD coeff",main="MEDIUM", ylim=c( -1.5,2.5))
points(nhbd_medium_F_hier~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_medium_M_hier, y0= nhbd_medium_F_hier)


plot(nhbd_long_M_hier~buffer.size, pch=16, ylab= "NHBD coeff",main="LONG", ylim=c( -3.5,1.5))
points(nhbd_long_F_hier~buffer.size, pch=16, ylab= "NHBD coeff",col="red")
abline(h=0)
segments(x0 = buffer.size,x1=buffer.size, y1=nhbd_long_F_hier, y0= nhbd_long_M_hier)
dev.off()


## check 
 setwd("C:/My_documents/ana/nhbd/NHBD/Data")

COUNTRIES <- readOGR("countries.removed.islands.shp")       ## Detailed Map of Scandinavia (including Finland & parts of Russia)
COUNTRIES <- COUNTRIES[which(COUNTRIES$ISO %in% c("NOR","SWE")),]              ## Just take Sweden and Norway 

short <- subset(e, disp_distance <= 40000)
medium <- subset(e, 40000 < disp_distance & disp_distance <= 200000)
long <- subset(e, disp_distance > 200000)


tmp <- long[long$ID_individual == unique(long$ID_individual)[10],]
plot(COUNTRIES,ylim=c(6500000, 7000000) )
axis(2)
points(tmp$coords.x2 ~ tmp$coords.x1, col=as.factor(tmp$Category),pch=16)

tmp <- medium[medium$ID_individual == unique(medium$ID_individual)[10],]
plot(COUNTRIES,ylim=c(6500000, 7000000) )
axis(2)
points(tmp$coords.x2 ~ tmp$coords.x1, col=as.factor(tmp$Category),pch=16)

tmp <- short[short$ID_individual == unique(short$ID_individual)[10],]
plot(COUNTRIES,ylim=c(6500000, 7000000))
axis(2)
points(tmp$coords.x2 ~ tmp$coords.x1, col=as.factor(tmp$Category),pch=16)




plot(nhbd~buffer.size, pch=16, ylab= "NHBD coeff")
abline(h=0, lty=2)
setwd("C:/My_documents/ana/nhbd/NHBD/Data")

load("buf_dist_good.RData")
md1 <- as.numeric(unlist(lapply(md, function(x) x[[1]])))
col <- unlist(lapply(md, function(x) x[[2]]))
col[col=="short"] <- "blue"
col[col=="Short"] <- "blue"
col[col=="Medium"] <- "red"
col[col=="Medium and Long"] <- "black"
abline(v=unlist(md1), col=col, lty=2)
legend("topright", fill=c("blue","red", "black"), legend=c("Short","Medium","Black"))


####
confint(c,level = 0.95)

c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), sd_e1)
summary(c)
confint(c,level = 0.95)
