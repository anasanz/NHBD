
rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(raster)



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
# Predation studies cover short time span. With a 90 % MCP the area where availability is defined leaves out places 
# within the home range where wolves could have also been.

## LOAD THE DATA 
#setwd("~/Norway/NHBD_humans/Antonio")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #USED GPS positions only


coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

# CREATE MCP 100
mcp_100 <- mcp(c[,1], percent = 100) # Create MCP for each territory
plot(mcp_100,col = mcp_100$id)
plot(mcp_100[1, ])



# ---- 2.1 CREATE RANDOM POINTS ----

ID <- unique(c$territory_)
tmp.df <- list()
i=1

for (i in 1:length(ID)){
mcpid <- mcp_100[mcp_100$id==ID[i], ]
tmp <- c[c$territory_==ID[i],]
n.rdm.pts <- nrow(tmp)
set.seed(i)
rdm.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
plot(mcpid)
points(rdm.sp)
points(c[c$territory_==ID[i],],col="red", pch=16)

tmp.df[[i]] <- data.frame( territory_=ID[i] 
                      , x_UTM=coordinates(rdm.sp)[,1]
                      , y_UTM=coordinates(rdm.sp)[,2])
}

df <- do.call(rbind, tmp.df)
df$used <- 0 # Used 0: Random


# ---- 2.2. JOIN RANDOM AND GPS LOCATIONS ----
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM","id")) ]
c$Used <- 1 # Used 1: GPS points
colnames(c) <- colnames(df)

### KEEP ONLY THE MOVING GPS LOCATIONS 
### update the time 
## upload a file to get the time
dtime <- read.csv("dataset_09022018_territories_fixed.csv", header = TRUE, sep = ",")
# convert to date_time
date_time <- paste(dtime$date, dtime$time, sep=" ")
dtime$date_time <- as.POSIXct(strptime(date_time, "%d/%m/%Y %H:%M"),tz="GMT")

#Link the two files with this id 
dtime$id <- as.character(dtime$id)
c$id <- as.character(c$id)

##GET THE TIME HERE 
merge <- merge(x =c, y= dtime, by="id")
c <- merge[,c("territory_", "x_UTM.x", "y_UTM.x","id","date_time","date.x")]

coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame


### 
buffer=200 
ID <- unique(c$territory_)

i=1
for (i in 1:length(ID)){
  
}


data <- rbind(c, df) # Join

# PLOT TO CHECK #
data1 <- data[data$territory=="Fulufjallet_2010_w",]
plot(data1$x~data1$y, col=as.factor(data1$used),pch=16)

# ---- 2.3. WRITE FILE ----
# setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
# write.csv(data, "all_points.csv")





# ---- 3. KERNEL 99%  ----
# Predation studies cover short time span. With a 90 % MCP the area where availability is defined leaves out places 
# within the home range where wolves could have also been.

## LOAD THE DATA 
#setwd("~/Norway/NHBD_humans/Antonio")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #USED GPS positions only
coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

# KERNEL 99%
kern <- kernelUD(c[,1], h = 3835)#"href") # use the max "href"
kern_99 <- getverticeshr(kern, 99)
plot(kern_99,col = kern_99$id)


h <- 0
ID <- unique(c$territory_)
for (i in 1:length(ID)){
h[i] <- kern[[i]]@h$h
plot(kern_99[kern_99$id==ID[i],])
points(c[c$territory_==ID[i],],col="red", pch=16)
}
max(h)

# ---- 3.1 CREATE RANDOM POINTS ----

ID <- unique(c$territory_)
tmp.df <- list()
i=1

for (i in 1:length(ID)){
  mcpid <- kern_99[kern_99$id==ID[i], ]
  tmp <- c[c$territory_==ID[i],]
  n.rdm.pts <- nrow(tmp)
  set.seed(i)
  rdm.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
  plot(mcpid)
  points(rdm.sp)
  points(c[c$territory_==ID[i],],col="red", pch=16)
  
  tmp.df[[i]] <- data.frame( territory_=ID[i] 
                             , x_UTM=coordinates(rdm.sp)[,1]
                             , y_UTM=coordinates(rdm.sp)[,2])
}

df <- do.call(rbind, tmp.df)
df$used <- 0 # Used 0: Random


# ---- 3.2. JOIN RANDOM AND GPS LOCATIONS ----
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ]
c$Used <- 1 # Used 1: GPS points
colnames(c) <- colnames(df)

data.kern <- rbind(c,df) # Join

# PLOT TO CHECK #
data.kern1 <- data.kern[data.kern$territory=="Fulufjallet_2010_w",]
plot(data.kern1$x~data.kern1$y, col=as.factor(data.kern1$used),pch=16)

# ---- 3.3. WRITE FILE ----
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
write.csv(data.kern, "all_points.kern.csv")




