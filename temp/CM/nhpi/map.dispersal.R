rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(rgeos)

# ---- 1. Check data with MCP 90 % ----
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/nhbd/NHBD/temp/ASP/NHBD_Humans/Data")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #USED GPS positions only

scand <- readOGR("C:/Personal_Cloud/OneDrive/Work/Phd_Cyril/Pack_dynamic/GIS/Scandinavia_border_33N.shp")

ID <- unique(d$territory_)
# ---- 2. MCP 100% ----
coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

split <- strsplit(as.character(c$territory_), "_")
c$territory_ <- sapply(split, function(x) x[1])


mcp_100 <- mcp(c[,1], percent = 100) # Create MCP for each territory
par(mar=c(1,1,1,1))
plot(scand, xlim= c(440000,480000), ylim=c(6500000,7000000) )
plot(mcp_100, col = adjustcolor(as.numeric(as.factor(mcp_100$id)), alpha=0.5), add=T)

## plot natal territory#
hum <- read.csv("data_pairs_human_complete.csv", sep = ";")
split <- strsplit(as.character(hum$Territory_antonio), "_")
hum$Territory_antonio <- sapply(split, function(x) x[1])

Female <- hum[,c("Territory_antonio","X_birth_F","Y_birth_F")]
#coordinates(Female) <- hum[,c("X_birth_F","Y_birth_F")]

points(Female$Y_birth_F ~ Female$X_birth_F, pch=16, col="red")
centroids <- as.data.frame(gCentroid(mcp_100,byid=TRUE))
for(i in 1:nrow(Female)){
centr.id <- which(rownames(centroids)== Female$Territory_antonio[i])
arrows(x0=Female$X_birth_F[i], y0= Female$Y_birth_F[i],
           x1=centroids$x[centr.id], y1=centroids$y[centr.id],length = 0.1 )
}
title("Female")


par(mar=c(1,1,1,1))
plot(scand, xlim= c(440000,480000), ylim=c(6500000,7000000) )
plot(mcp_100, col = adjustcolor(as.numeric(as.factor(mcp_100$id)), alpha=0.5), add=T)

male <- hum[,c("Territory_antonio","X_birth_M","Y_birth_M")]
#coordinates(Female) <- hum[,c("X_birth_F","Y_birth_F")]

points(male$Y_birth_M ~ male$X_birth_M, pch=16, col="red")
centroids <- as.data.frame(gCentroid(mcp_100,byid=TRUE))
for(i in 1:nrow(male)){
  centr.id <- which(rownames(centroids)== male$Territory_antonio[i])
  arrows(x0=male$X_birth_M[i], y0= male$Y_birth_M[i],
           x1=centroids$x[centr.id], y1=centroids$y[centr.id],length = 0.1 )
}
title("Male")



