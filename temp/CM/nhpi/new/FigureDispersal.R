rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(rgeos)
library(RgoogleMaps)
library(ggmap)
library(raster)

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
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new")
gps <- read.csv("gps.dataCM.csv", header = TRUE)

# CREATE SP FILE
coordinates(gps) <- cbind(gps$X, gps$Y)
proj4string(gps) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

# ---- 1. MCP 100% ----
gps$territory <- unlist(lapply(strsplit(as.character(gps$Study_year) ,"_"), function(x) x[1]))
mcp_100 <- mcp(gps[,"territory"], percent = 100) # Create MCP for each territory
mcp_100 <- spTransform(mcp_100, CRS("+proj=longlat +datum=WGS84"))

# PREPARE THE MCP FOR THE GGMAP
mcp_100_rtp <- fortify(mcp_100, data = mcp_100@data)
idd <- unique(mcp_100_rtp$group)
set.seed(1)
col <- rainbow(length(mcp_100$id))[sample(length(mcp_100$id))]
colo <-0
for(i in 1:nrow(mcp_100_rtp)){
  colo[i] <- col[which(idd ==mcp_100_rtp$group[i])]
}


bm <- dc + geom_polygon(data = mcp_100_rtp ,
                       aes(long, lat, group = group),
                       fill = colo, colour = colo, alpha = rep(0.4, length(colo)))
bm

# ==== III. LOAD NATAL TERRITORY LOCATION AND MAKE ARROWS ====
# setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/nhbd/NHBD/temp/ASP/NHBD_Humans/Data")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/nhbd/NHBD/temp/CM/nhpi/new")
hum <- read.csv("data_pairs_human_complete.csv")
split <- strsplit(as.character(hum$Territory_antonio), "_")
hum$Territory_antonio <- sapply(split, function(x) x[1])
hum <- hum[hum$Territory_antonio!="Forshyttan",]

#---- 1. FEMALE ----
Female <- hum[,c("Territory_antonio","X_birth_F","Y_birth_F")]
coordinates(Female) <- hum[,c("X_birth_F","Y_birth_F")]
proj4string(Female) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
Female <- spTransform(Female, CRS("+proj=longlat +datum=WGS84"))

Female$LAT <- coordinates(Female)[,2]
Female$LONG <- coordinates(Female)[,1]

centroids <- as.data.frame(gCentroid(mcp_100, byid=TRUE))

#create adf with start and end arrows
female.arraows <- data.frame(matrix(NA,nrow=nrow(Female),ncol = 5))
for(i in 1:nrow(Female)){
  centr.id <- which(rownames(centroids)== Female$Territory_antonio[i])
  #start (birth)
  female.arraows[i,1] <- Female$X_birth_F[i]
  female.arraows[i,2] <- Female$Y_birth_F[i]
  
  #end (setted)
  female.arraows[i,3] <- centroids$x[centr.id]
  female.arraows[i,4] <- centroids$y[centr.id]
  
}


plot(mcp_100,col=col)
for(i in 10){
segments(x1=female.arraows[i,1],
         y1=female.arraows[i,2], 
         x0=female.arraows[i,3],
         y0=female.arraows[i,4])
}
plot(mcp_100[12,],col="red",add=T)



bm <- dc + geom_segment(data=female.arraows, aes(x=female.arraows[,1],
                                                 y=female.arraows[,2], 
                                                 xend=female.arraows[,3],
                                                 yend=female.arraows[,4]), 
                        arrow=arrow(ends="last",length=unit(0.30,"cm")), colour="white", size=0.4)
bm <- bm + geom_polygon(data = mcp_100_rtp ,
                        aes(long, lat, group = group),
                        fill = colo, colour = colo, alpha = rep(0.4, length(colo)))


bm <- bm + geom_point( data = female.arraows, aes(x=female.arraows[,1], y=female.arraows[,2]),
                       color = "black",fill="red",  shape=21,
                       size=3, alpha=1)
bm

setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf("Female_dispersal_map.pdf", width = 6, height =6)
print(bm)
dev.off()

#---- 2. MALE ----
Male <- hum[,c("Territory_antonio","X_birth_M","Y_birth_M")]
Male <- Male[!is.na(Male$X_birth_M),]
coordinates(Male) <- Male[,c("X_birth_M","Y_birth_M")]
proj4string(Male) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
Male <- spTransform(Male, CRS("+proj=longlat +datum=WGS84"))

Male$LAT <- coordinates(Male)[,2]
Male$LONG <- coordinates(Male)[,1]

centroids <- as.data.frame(gCentroid(mcp_100, byid=TRUE))

#create adf with start and end arrows
Male.arraows <- data.frame(matrix(NA,nrow=nrow(Male),ncol = 5))
for(i in 1:nrow(Male)){
  centr.id <- which(rownames(centroids)== Male$Territory_antonio[i])
  #start (birth)
  Male.arraows[i,1] <- Male$X_birth_M[i]
  Male.arraows[i,2] <- Male$Y_birth_M[i]
  
  #end (setted)
  Male.arraows[i,3] <- centroids$x[centr.id]
  Male.arraows[i,4] <- centroids$y[centr.id]
  
}




bm <- dc + geom_segment(data=Male.arraows, aes(x=Male.arraows[,1],
                                                 y=Male.arraows[,2], 
                                                 xend=Male.arraows[,3],
                                                 yend=Male.arraows[,4]), 
                        arrow=arrow(ends="last",length=unit(0.30,"cm")), colour="white", size=0.4)
bm <- bm + geom_polygon(data = mcp_100_rtp ,
                        aes(long, lat, group = group),
                        fill = colo, colour = colo, alpha = rep(0.4, length(colo)))


bm <- bm + geom_point( data = Male.arraows, aes(x=Male.arraows[,1], y=Male.arraows[,2]),
                       color = "black",fill="red",  shape=21,
                       size=3, alpha=1)
bm

setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf("Male_dispersal_map.pdf", width = 6, height =6)
print(bm)
dev.off()