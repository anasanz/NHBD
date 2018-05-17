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

###################################################
######        REMOVING PACKS           ############
###################################################
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
#################################################

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

##############################################################
##############################################################
######          PACK DENSITY         #########################
##############################################################
##############################################################

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

xxx=1
for(xxx in 1:length(buffer.size)){
## ====  2. LOAD e OBJECT WITH AVAIALBLE POINTS DRAWN FROM BUFFER SIZE ====
  load(paste("e",buffer.size[xxx] ,".RData", sep=""))
  
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
  
  # ASSIGN DISTANCE TO EACH INDIVIDUAL   
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
  
  c_long_F
  c_long_M
  c_short_F
  c_short_M
  c_medium_M
  c_medium_F
  
  # c_short <- clogit(Category ~ distance  + wolf_density1 * distance  				
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
  #             + strata(ID_individual), short_F)	
  # c_medium <- clogit(Category ~ distance  + wolf_density1 * distance  				
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
  #             + strata(ID_individual), medium)	
  # c_long <- clogit(Category ~ distance  + wolf_density1 * distance  				
  #             + human_1 + humanlands_1 + agri_1 + forest_1 + mires_1 + water_1				
  #             + mountains_1 + roadens_sec1 + mainroad_1 + bear_1 + dem_1 				
  #             + slope_1 + roughness_1 + roadbuild_1 + moose_dens 				
  #             + strata(ID_individual), long)	
  
  nhbd_short_M[xxx]<- (c_short_M$coefficients)[1]
  nhbd_short_F[xxx]<- (c_short_F$coefficients)[1]
  
  nhbd_medium_M[xxx]<- (c_medium_M$coefficients)[1]
  nhbd_medium_F[xxx]<- (c_medium_F$coefficients)[1]
  
  nhbd_long_M[xxx]<- (c_long_M$coefficients)[1]
  nhbd_long_F[xxx]<- (c_long_F$coefficients)[1]
  
  
    print(xxx)
}  





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

c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)
