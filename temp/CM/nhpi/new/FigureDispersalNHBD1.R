rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(rgeos)
library(RgoogleMaps)
library(ggmap)
library(raster)
library(gridExtra)
# ==== I. GET THE SATELLITE IMAGE OF SCANDINAVIA 
# ext <- c(9.756424,19.497841, 58.261077,61.847504)
# 
# ext[c(1,3)] <- ext[c(1,3)]+1
# ext[c(2,4)] <- ext[c(2,4)]-1
# bbox <- make_bbox(lon=ext[1:2], lat=ext[3:4] )
# dc <- qmap(location = bbox, maptype = "hybrid") 

dc <- qmap(location = c( 13.960256,60.178051), zoom = 6, maptype = "hybrid") 
dc

# ==== II. LOAD GPS DATA AND COMPUTE MCP ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/revisionOpenRoyal/MinorRevisions")
gps_birth <- gps_established <-  read.csv("dispersal_p.csv", header = TRUE)

# CREATE SP FILE
coordinates(gps_birth) <- cbind(gps_birth$X_birth, gps_birth$Y_birth)
proj4string(gps_birth) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
gps_birth <- spTransform(gps_birth, CRS("+proj=longlat +datum=WGS84"))
gps_birth$X_birth <- coordinates(gps_birth)[,1]
gps_birth$Y_birth <- coordinates(gps_birth)[,2]

coordinates(gps_established) <- cbind(gps_established$X_Established, gps_established$Y_Established)
proj4string(gps_established) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
gps_established <- spTransform(gps_established, CRS("+proj=longlat +datum=WGS84"))
gps_established$X_Established <- coordinates(gps_established)[,1]
gps_established$Y_Established <- coordinates(gps_established)[,2]


#---- 1. FEMALE ----
gps_birthF <- gps_birth[gps_birth$MALES=="F",]
gps_establishedF <- gps_established[gps_established$MALES=="F",]

#create adf with start and end arrows
female.arraows <- data.frame(matrix(NA,nrow=nrow(gps_birthF),ncol = 4))
for(i in 1:nrow(gps_birthF)){
  #start (birth)
  female.arraows[i,1] <- gps_birthF$X_birth[i]
  female.arraows[i,2] <- gps_birthF$Y_birth[i]
  
  #end (setted)
  female.arraows[i,3] <- gps_establishedF$X_Established[i]
  female.arraows[i,4] <- gps_establishedF$Y_Established[i]
  
}



color <- rainbow(nrow(female.arraows))
female.arraows$color <- color
bm <- dc + geom_segment(data=female.arraows, aes(x=female.arraows[,1],
                                                 y=female.arraows[,2], 
                                                 xend=female.arraows[,3],
                                                 yend=female.arraows[,4]), 
                        arrow=arrow(ends="last",length=unit(0.30,"cm")), colour=color, size=0.4)

bm <- bm+annotate("text", x = 9.2146, y = 63.1563, label = "Female",size=10, colour="white") 
bm

# bm <- bm + geom_point( data = female.arraows, aes(x=female.arraows[,1], y=female.arraows[,2]),
#                        color = "black",fill="red",  shape=21,
#                        size=3, alpha=1)
# bm <- bm + geom_point( data = female.arraows, aes(x=female.arraows[,3], y=female.arraows[,4]),
#                        color = "black",fill="red",  shape=22,
#                        size=3, alpha=1)

bm
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/revisionOpenRoyal/MinorRevisions")
pdf("Female_dispersal_map.pdf", width = 6, height =6)
print(bm)
dev.off()

#---- 2. MALE ----
gps_birthM <- gps_birth[gps_birth$MALES=="M",]
gps_establishedM <- gps_established[gps_established$MALES=="M",]

#create adf with start and end arrows
male.arraows <- data.frame(matrix(NA,nrow=nrow(gps_birthM),ncol = 4))
for(i in 1:nrow(gps_birthM)){
  #start (birth)
  male.arraows[i,1] <- gps_birthM$X_birth[i]
  male.arraows[i,2] <- gps_birthM$Y_birth[i]
  
  #end (setted)
  male.arraows[i,3] <- gps_establishedM$X_Established[i]
  male.arraows[i,4] <- gps_establishedM$Y_Established[i]
  
}



color <- rainbow(nrow(male.arraows))
male.arraows$color <- color
bmM <- dc + geom_segment(data=male.arraows, aes(x=male.arraows[,1],
                                                 y=male.arraows[,2], 
                                                 xend=male.arraows[,3],
                                                 yend=male.arraows[,4]), 
                        arrow=arrow(ends="last",length=unit(0.30,"cm")), colour=color, size=0.4)

bmM <- bmM+annotate("text", x = 9.0146, y = 63.1563, label = "Male",size=10, colour="white") 
bmM
# bm <- bm + geom_point( data = female.arraows, aes(x=female.arraows[,1], y=female.arraows[,2]),
#                        color = "black",fill="red",  shape=21,
#                        size=3, alpha=1)
# bm <- bm + geom_point( data = female.arraows, aes(x=female.arraows[,3], y=female.arraows[,4]),
#                        color = "black",fill="red",  shape=22,
#                        size=3, alpha=1)

bmM
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/revisionOpenRoyal/MinorRevisions")
pdf("FemaleMale_dispersal_map.pdf", width = 8, height =6)
grid.arrange(bm, bmM, nrow = 1)
dev.off()
