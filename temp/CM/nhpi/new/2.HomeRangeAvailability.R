rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

# ==== I. LOAD DATA ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new")
gps <- read.csv("gps.dataCM.csv", header = TRUE)
scand <- readOGR("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/Scandinavia_border_33N.shp")

# CREATE SP FILE
coordinates(gps) <- cbind(gps$X, gps$Y)
proj4string(gps) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame


# ==== II. DEFINE AVAILABILITY ====
# ---- 1. MCP 100% ----
ID <- unique(gps$Study_year)

# CREATE MCP 100
mcp_100 <- mcp(gps[,"Study_year"], percent = 100) # Create MCP for each territory
plot(mcp_100, col = mcp_100$id)
plot(mcp_100[1, ])

# ---- 2. KERNEL 99% ----

# CREATE KERNEL 99
kern <- kernelUD(gps[,"Study_year"], h = 2206.224)#"href")#4291.715)#"href")#3835)#"href") # use the max "href"
kern_99 <- getverticeshr(kern, 99)
plot(kern_99, col = kern_99$id)


h <- 0
for (i in 1:length(ID)){
  h[i] <- kern[[i]]@h$h
  plot(kern_99[kern_99$id==ID[i],])
  points(gps[gps$Study_year==ID[i],], col="red", pch=16)
}
max(h)
mean(h)


# ---- 3.1 CREATE RANDOM POINTS ----
tmp.mcp.df <- tmp.kern.df <-  list()

for (i in 1:length(ID)){
  mcpid <- mcp_100[mcp_100$id==ID[i], ]
  kernid <- kern_99[kern_99$id==ID[i], ]
  
  tmp <- gps[gps$Study_year==ID[i],]
  n.rdm.pts <- nrow(tmp)
  
  set.seed(i)
  #draw random points
  rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
  set.seed(i+5)#picked a value randomly
  rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
  
  plot(mcpid)
  points(rdm.mcp.sp)
  points(gps[gps$Study_year==ID[i],],col="red", pch=16)
  
  plot(kernid)
  points(rdm.kern.sp)
  points(gps[gps$Study_year==ID[i],], col="red", pch=16)
  
  tmp.mcp.df[[i]] <- data.frame( Study_year = ID[i] 
                                , X = coordinates(rdm.mcp.sp)[,1]
                                , Y = coordinates(rdm.mcp.sp)[,2])
  
  
  tmp.kern.df[[i]] <- data.frame( Study_year = ID[i] 
                                 , X = coordinates(rdm.kern.sp)[,1]
                                 , Y = coordinates(rdm.kern.sp)[,2])
}

df.mcp <- do.call(rbind, tmp.mcp.df)
df.kern <- do.call(rbind, tmp.kern.df)
df.mcp$used <- 0 # Used 0: Random
df.kern$used <- 0 # Used 0: Random

########### for the moving locations 
i=1
tmp.mcp.move.df <- tmp.kern.move.df <-  list()

for (i in 1:length(ID)){
  mcpid <- mcp_100[mcp_100$id==ID[i], ]
  kernid <- kern_99[kern_99$id==ID[i], ]
  
  tmp <- gps[gps$Study_year==ID[i] & gps$move==1,]
  n.rdm.pts <- nrow(tmp)
  
  set.seed(i)
  #draw random points
  rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
  set.seed(i+5)#picked a value randomly
  rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
  
  plot(mcpid)
  points(rdm.mcp.sp)
  points(gps[gps$Study_year==ID[i] & gps$move==1,], col="red", pch=16)
  
  plot(kernid)
  points(rdm.kern.sp)
  points(gps[gps$Study_year==ID[i] & gps$move==1,], col="red", pch=16)
  
  tmp.mcp.move.df[[i]] <- data.frame( Study_year = ID[i] 
                                 , X = coordinates(rdm.mcp.sp)[,1]
                                 , Y = coordinates(rdm.mcp.sp)[,2])
  
  
  tmp.kern.move.df[[i]] <- data.frame( Study_year = ID[i] 
                                  , X = coordinates(rdm.kern.sp)[,1]
                                  , Y = coordinates(rdm.kern.sp)[,2])
  
  
}

df.mcp.move <- do.call(rbind, tmp.mcp.move.df)
df.kern.move  <- do.call(rbind, tmp.kern.move.df)
df.mcp.move$used <- 0 # Used 0: Random
df.kern.move$used <- 0 # Used 0: Random


# ---- 2.2. JOIN RANDOM AND GPS LOCATIONS ----
# ---- 2.2.1 MOVE ----

## MOVE 
gps.move <- gps[gps$move==1,]
gps.move$used <- 1 # Used 1: GPS points

coordinates(df.mcp.move) <- df.mcp.move[,c("X","Y")]
colnames(df.mcp.move@data)[1] <- c("Study_year")
coordinates(df.kern.move) <- df.kern.move[,c("X","Y")]
colnames(df.kern.move@data)[1] <- c("Study_year")

proj4string(df.mcp.move) <- CRS(proj4string(gps.move))
proj4string(df.kern.move) <- CRS(proj4string(gps.move))

data.move.mcp <- rbind(gps.move[,c("Study_year","X", "Y","used")],
                       df.mcp.move[,c("Study_year","X", "Y","used")] ) # Join
data.move.kern <- rbind(gps.move[,c("Study_year","X", "Y","used")],
                        df.kern.move[,c("Study_year","X", "Y","used")] ) # Join

# PLOT TO CHECK #
data1 <- data.move.kern[data.move.kern$Study_year=="Fulufjallet_2010_W",]
plot(data1, col=as.factor(data1$used),pch=16)

data1 <- data.move.mcp[data.move.mcp$Study_year=="Fulufjallet_2010_W",]
plot(data1, col=as.factor(data1$used),pch=16)

# ---- 2.2.2 NOT MOVE ----
gps$used <- 1 # Used 1: GPS points
coordinates(df.mcp) <- df.mcp[,c("X","Y")]
colnames(df.mcp@data)[1] <- c("Study_year")
coordinates(df.kern) <- df.kern[,c("X","Y")]
colnames(df.kern@data)[1] <- c("Study_year")

proj4string(df.mcp) <- CRS(proj4string(gps))
proj4string(df.kern) <- CRS(proj4string(gps))


data.mcp <- rbind(gps[,c("Study_year","X", "Y","used")],
              df.mcp[,c("Study_year","X", "Y","used")] ) # Join

data.kern <- rbind(gps[,c("Study_year","X", "Y","used")],
              df.kern[,c("Study_year","X", "Y","used")] ) # Join

# PLOT TO CHECK #
data1 <- data.mcp[data.mcp$Study_year=="Fulufjallet_2010_W",]
plot(data1, col=as.factor(data1$used),pch=16)

data1 <- data.kern[data.kern$Study_year=="Fulufjallet_2010_W",]
plot(data1, col=as.factor(data1$used),pch=16)


# ==== III. EXTRACT COVARIATES AT THE HOME RANGE LEVEL ==== 
# ---- 1. MOOSE ----
moose <- readOGR("C:/Personal_Cloud/OneDrive/Work/Phd_Cyril/GIS_LAYERS/Hunting/Moose/moose_update2/moose_final_1997_2017.shp")

# ---- 1.1 RASTERIZE ----
#rasterise, use the sf package
moose <- st_as_sf(moose)
r <- raster(extent(moose))
res(r) <- 1000
r[] <- 1

# rasterize moose density
library(fasterize)
year <- paste("D", c(2000:2017), sep="")
moose.r <- stack(fasterize(moose, r, field = year[1]))

for(i in 2:length(year)){
  moose.r[[i]] <- fasterize(moose, r, field = year[i])
  # plot(moose.r[[i]])
}
names(moose.r) <- year

# ---- 1.2 EXTRACT MCP VALUES ----
# Extract mcp values 
centroids <- data.frame(getSpPPolygonsLabptSlots(mcp_100))
coordinates(centroids) <- centroids
year_terr <- as.numeric(unlist(lapply(strsplit(row.names(mcp_100),"_"),function(x) x[2])))

# Create a buffer of the size the wolf territory
buff <- gBuffer(centroids, width=17841.24, byid = T)

moose_dens <- 0
for(i in 1:length(buff)){
  moose_dens[i] <- mean(extract(moose.r[[paste("D", (year_terr[i]+1), sep="")]] ,buff[i,])[[1]], na.rm=T)
}

# ---- 1.3 LINK TO GPS DATA ----
id_terr <- row.names(mcp_100)
data.mcp$moose <- 0
data.kern$moose <- 0
data.move.mcp$moose <- 0
data.move.kern$moose <- 0

for(i in 1:length(id_terr)){
  data.mcp$moose[data.mcp$Study_year==id_terr[i]] <- moose_dens[i]
  data.kern$moose[data.kern$Study_year==id_terr[i]] <- moose_dens[i]
  data.move.mcp$moose[data.move.mcp$Study_year==id_terr[i]] <- moose_dens[i]
  data.move.kern$moose[data.move.kern$Study_year==id_terr[i]] <- moose_dens[i]
  
}


# ==== IV. WRITE FILE ==== 
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new")
write.csv(data.mcp, "all_points.not.moved_MCP.csv")
write.csv(data.kern, "all_points.not.moved_KERN.csv")


setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new")
write.csv(data.move.mcp, "all_points.move_MCP.csv")
write.csv(data.move.kern, "all_points.move_KERN.csv")




# ==== V. CREATE A DISPERSAL MAP ==== 




