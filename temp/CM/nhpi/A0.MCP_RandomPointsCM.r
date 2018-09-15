
rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

# ==== I. LOAD DATA ====
#setwd("~/Norway/NHBD_humans/Antonio")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
scand <- readOGR("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/Scandinavia_border_33N.shp")
ID <- unique(d$territory_)


# ==== II. DEFINE AVAILABILITY ====
# ---- 1. ANTONIO DEFINTION OF AVAILABILITY ----
# pdf(file="MCP.pdf")
for (i in 1:length(ID)){
  fc <- d[which(d$territory_ == ID[i] & d$id != "random"), ]
  fr <- d[which(d$territory_ == ID[i] & d$id == "random"), ]
  plot(scand, axes=TRUE, border="black", xlim = c(min(fc$x_UTM),max(fc$x_UTM)), ylim = c(min(fc$y_UTM),max(fc$y_UTM)))
  points(fc$x_UTM, fc$y_UTM, pch = 16, col = "red")
  points(fr$x_UTM, fr$y_UTM, pch = 16, col = "blue")
  
  print(nrow(fc)/nrow(fr))
}
#dev.off()


# ---- 2. MCP 100%  ----
# REMOVE THE RANDOM POINTS FROM ANTONIO 
used <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM","id","date")) ] #USED GPS positions only

## UPLOAD THE TIME 
time <- read.csv("dataset_09022018_territories_fixed.csv", header = TRUE, sep = ",")
# convert to date_time
date_time <- paste(time$date, time$time, sep=" ")
time$date_time <- as.POSIXct(strptime(date_time, "%d/%m/%Y %H:%M"),tz="GMT")

#Link the two files with this id 
time$id <- as.character(time$id)
used$id <- as.character(used$id)

##GET THE TIME HERE 
merge <- merge(x =used, y= time, by="id")
used.sp <- merge[,c("territory_", "x_UTM.x", "y_UTM.x","id","date_time","date.x")]



## REMOVE SOME WEIRD DUPLICATES.. SAME TIME SAME COORDINATES. 
nrow(used.sp)
length1 <- tapply(used.sp$territory_, used.sp$territory_,length)
used.sp$id[which(duplicated(paste(used.sp[,c("y_UTM.x")], used.sp[,c("y_UTM.x")], used.sp[,c("date_time")])))]

used.sp <- used.sp[!duplicated(paste(used.sp[,c("y_UTM.x")], used.sp[,c("y_UTM.x")], used.sp[,c("date_time")])),]

# CREATE SP FILE
coordinates(used.sp) <- cbind(used.sp$x_UTM, used.sp$y_UTM)
proj4string(used.sp) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame





date.summary <- matrix(NA, nrow=length(ID), ncol=10)
## check the time of the predation study
used.sp$date.x <- as.Date(strptime(used.sp$date.x, "%d/%m/%Y"))
# d$date <- as.POSIXct(strptime(d$date, "%d/%m/%Y"))
for (i in 1:length(ID)){
  fc <- used.sp[which(used.sp$territory_ == ID[i]), ]
  date.summary[i,1] <- as.character(ID[i])
  date.summary[i,2] <- as.character(as.Date(min(fc$date.x)))
  date.summary[i,3] <- as.character(as.Date(max(fc$date.x)))
  date.summary[i,4] <- as.character(diff(range(fc$date.x)))
  date.summary[i,5] <- length(fc$date.x)
  date.summary[i,6] <- as.character(fc$territory_[1])
  date.summary[i,7] <- NA#ifelse(fc$season[1]==1,"S","W")
  date.summary[i,8] <- NA#as.character(fc$study_star[1])
  date.summary[i,9] <- NA#as.character(fc$study_end[1])
  date.summary[i,10] <- length(fc$territory_)/ as.numeric(diff(range(fc$date.x)))
  
}

colnames(date.summary) <- c("Territory_year", "Start", "End","Range","Nblocations", "Territory", "Season","Start", "End","locations per day" )
write.csv(date.summary,file="summary_GPS.csv")


# CREATE MCP 100
mcp_100 <- mcp(used.sp[,1], percent = 100) # Create MCP for each territory
plot(mcp_100, col = mcp_100$id)
plot(mcp_100[1, ])
points(used.sp[used.sp$territory_==row.names(mcp_100[1, ]@data),], col="red")

# CREATE KERNEL 99
kern <- kernelUD(used.sp[,1], h = 3835)#"href") # use the max "href"
kern_99 <- getverticeshr(kern, 99)
plot(kern_99, col = kern_99$id)


h <- 0
ID <- unique(used.sp$territory_)
for (i in 1:length(ID)){
  h[i] <- kern[[i]]@h$h
  plot(kern_99[kern_99$id==ID[i],])
  points(used.sp[used.sp$territory_==ID[i],], col="red", pch=16)
}
max(h)


# ----  2.1 CONSIDER ONLY MOVING LOCATIONS 
BufferWidth <- 100
moving_used <- list()
for (i in 1:length(ID)){
  tmp <- used.sp[used.sp$territory_==ID[i],]
  tmp.mcp <- mcp_100[which(row.names(mcp_100[,1])==ID[i]),]
  plot(tmp.mcp)
  points(used.sp[used.sp$territory_==row.names(tmp.mcp[1, ]@data),], col="red")
  buffer <- gBuffer(tmp, width = BufferWidth, byid =F)
  buffer <- disaggregate(buffer)
  tmp$buffer <- over(tmp, buffer)
  buffer1 <- which(table(tmp$buffer)==1)
  tmp <- tmp[tmp$buffer %in% buffer1,]
  moving_used[[i]] <- tmp
  plot(buffer, add=T)  
  points(tmp)  
}  

moving_used <- do.call(rbind, moving_used)
tapply(moving_used$territory_ ,moving_used$territory_, length)

# ----  2.1 CREATE RANDOM POINTS ----
ID <- unique(used.sp$territory_)
tmp.mcp.df <- tmp.kern.df <-  list()
i=1

for (i in 1:length(ID)){
  mcpid <- mcp_100[mcp_100$id==ID[i], ]
  kernid <- kern_99[kern_99$id==ID[i], ]
  
  tmp <- used.sp[used.sp$territory_==ID[i],]
  n.rdm.pts <- nrow(tmp)
  
  set.seed(i)
  #draw random points
  rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
  set.seed(i)
  rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
  
  plot(mcpid)
  points(rdm.mcp.sp)
  points(used.sp[used.sp$territory_==ID[i],],col="red", pch=16)
  
  plot(kernid)
  points(rdm.kern.sp)
  points(used.sp[used.sp$territory_==ID[i],], col="red", pch=16)
  
  tmp.mcp.df[[i]] <- data.frame( territory_ = ID[i] 
                                , x_UTM = coordinates(rdm.mcp.sp)[,1]
                                , y_UTM = coordinates(rdm.mcp.sp)[,2])
  
  
  tmp.kern.df[[i]] <- data.frame( territory_ = ID[i] 
                                 , x_UTM = coordinates(rdm.kern.sp)[,1]
                                 , y_UTM = coordinates(rdm.kern.sp)[,2])
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
  
  tmp <- moving_used[moving_used$territory_==ID[i],]
  n.rdm.pts <- nrow(tmp)
  
  set.seed(i)
  #draw random points
  rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
  set.seed(i)
  rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
  
  plot(mcpid)
  points(rdm.mcp.sp)
  points(moving_used[moving_used$territory_==ID[i],],col="red", pch=16)
  
  plot(kernid)
  points(rdm.kern.sp)
  points(moving_used[moving_used$territory_==ID[i],], col="red", pch=16)
  
  tmp.mcp.move.df[[i]] <- data.frame( territory_ = ID[i] 
                                 , x_UTM = coordinates(rdm.mcp.sp)[,1]
                                 , y_UTM = coordinates(rdm.mcp.sp)[,2])
  
  
  tmp.kern.move.df[[i]] <- data.frame( territory_ = ID[i] 
                                  , x_UTM = coordinates(rdm.kern.sp)[,1]
                                  , y_UTM = coordinates(rdm.kern.sp)[,2])
  
  
}

df.mcp.move <- do.call(rbind, tmp.mcp.move.df)
df.kern.move  <- do.call(rbind, tmp.kern.move.df)
df.mcp.move$used <- 0 # Used 0: Random
df.kern.move$used <- 0 # Used 0: Random


# ---- 2.2. JOIN RANDOM AND GPS LOCATIONS ----
## MOVE 
moving_used$used <- 1 # Used 1: GPS points
coordinates(df.mcp.move) <- df.mcp.move[,c("x_UTM","y_UTM")]
colnames(df.mcp.move@data)[1] <- c("territory")
coordinates(df.kern.move) <- df.kern.move[,c("x_UTM","y_UTM")]
colnames(df.kern.move@data)[1] <- c("territory")

colnames(moving_used@data)[1:3] <- c("territory","x_UTM", "y_UTM")
proj4string(df.mcp.move) <- CRS(proj4string(used.sp))
proj4string(df.kern.move) <- CRS(proj4string(used.sp))

data.move.mcp <- rbind(moving_used[,c("territory","x_UTM", "y_UTM","used")],
                       df.mcp.move[,c("territory","x_UTM", "y_UTM","used")] ) # Join
data.move.kern <- rbind(moving_used[,c("territory","x_UTM", "y_UTM","used")],
                        df.kern.move[,c("territory","x_UTM", "y_UTM","used")] ) # Join

# PLOT TO CHECK #
data1 <- data.move.kern[data.move.kern$territory=="Fulufjallet_2010_w",]
plot(data1, col=as.factor(data1$used),pch=16)

data1 <- data.move.mcp[data.move.mcp$territory=="Fulufjallet_2010_w",]
plot(data1, col=as.factor(data1$used),pch=16)

## NOT MOVE 
used.sp$used <- 1 # Used 1: GPS points
coordinates(df.mcp) <- df.mcp[,c("x_UTM","y_UTM")]
colnames(df.mcp@data)[1] <- c("territory")
coordinates(df.kern) <- df.kern[,c("x_UTM","y_UTM")]
colnames(df.kern@data)[1] <- c("territory")

colnames(used.sp@data)[1:3] <- c("territory","x_UTM", "y_UTM")
proj4string(df.mcp) <- CRS(proj4string(used.sp))
proj4string(df.kern) <- CRS(proj4string(used.sp))


data.mcp <- rbind(used.sp[,c("territory","x_UTM", "y_UTM","used")],
              df.mcp[,c("territory","x_UTM", "y_UTM","used")] ) # Join

data.kern <- rbind(used.sp[,c("territory","x_UTM", "y_UTM","used")],
              df.kern[,c("territory","x_UTM", "y_UTM","used")] ) # Join

# PLOT TO CHECK #
data1 <- data.mcp[data.mcp$territory=="Fulufjallet_2010_w",]
plot(data1, col=as.factor(data1$used),pch=16)

data1 <- data.kern[data.kern$territory=="Fulufjallet_2010_w",]
plot(data1, col=as.factor(data1$used),pch=16)


# ---- 2.3. WRITE FILE ----
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
write.csv(data.mcp, "all_points.not.moved_MCP.csv")
write.csv(data.kern, "all_points.not.moved_KERN.csv")


setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
write.csv(data.move.mcp, "all_points.move_MCP.csv")
write.csv(data.move.kern, "all_points.move_KERN.csv")


# 
# 
# # ---- 3. KERNEL 99%  ----
# # Predation studies cover short time span. With a 90 % MCP the area where availability is defined leaves out places 
# # within the home range where wolves could have also been.
# 
# ## LOAD THE DATA 
# #setwd("~/Norway/NHBD_humans/Antonio")
# setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
# d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
# c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #USED GPS positions only
# coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
# proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
# 
# # KERNEL 99%
# kern <- kernelUD(c[,1], h = 3835)#"href") # use the max "href"
# kern_99 <- getverticeshr(kern, 99)
# plot(kern_99,col = kern_99$id)
# 
# 
# h <- 0
# ID <- unique(c$territory_)
# for (i in 1:length(ID)){
# h[i] <- kern[[i]]@h$h
# plot(kern_99[kern_99$id==ID[i],])
# points(c[c$territory_==ID[i],],col="red", pch=16)
# }
# max(h)
# 
# # ---- 3.1 CREATE RANDOM POINTS ----
# 
# ID <- unique(c$territory_)
# tmp.df <- list()
# i=1
# 
# for (i in 1:length(ID)){
#   mcpid <- kern_99[kern_99$id==ID[i], ]
#   tmp <- c[c$territory_==ID[i],]
#   n.rdm.pts <- nrow(tmp)
#   set.seed(i)
#   rdm.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
#   plot(mcpid)
#   points(rdm.sp)
#   points(c[c$territory_==ID[i],],col="red", pch=16)
#   
#   tmp.df[[i]] <- data.frame( territory_=ID[i] 
#                              , x_UTM=coordinates(rdm.sp)[,1]
#                              , y_UTM=coordinates(rdm.sp)[,2])
# }
# 
# df <- do.call(rbind, tmp.df)
# df$used <- 0 # Used 0: Random
# 
# 
# # ---- 3.2. JOIN RANDOM AND GPS LOCATIONS ----
# d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
# c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ]
# c$Used <- 1 # Used 1: GPS points
# colnames(c) <- colnames(df)
# 
# data.kern <- rbind(c,df) # Join
# 
# # PLOT TO CHECK #
# data.kern1 <- data.kern[data.kern$territory=="Fulufjallet_2010_w",]
# plot(data.kern1$x~data.kern1$y, col=as.factor(data.kern1$used),pch=16)
# 
# # ---- 3.3. WRITE FILE ----
# setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
# write.csv(data.kern, "all_points.kern.csv")
# 
# 
# 
# 
