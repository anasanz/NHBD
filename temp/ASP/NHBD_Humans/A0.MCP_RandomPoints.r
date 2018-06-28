
rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)

# ---- 1. Check data with MCP 90 % ----

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")

scand<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS", "Scandinavia_border_33N")

ID <- unique(d$territory_)
setwd("~/Norway/NHBD_humans/Antonio")

pdf(file="MCP.pdf")
for (i in 1:length(ID)){
  fc <- d[which(d$territory_ == ID[i] & d$id != "random"), ]
  fr <- d[which(d$territory_ == ID[i] & d$id == "random"), ]
  plot(scand, axes=TRUE, border="black", xlim = c(min(fc$x_UTM),max(fc$x_UTM)), ylim = c(min(fc$y_UTM),max(fc$y_UTM)))
  points(fc$x_UTM, fc$y_UTM, pch = 16, col = "red")
  points(fr$x_UTM, fr$y_UTM, pch = 16, col = "blue")
  
  print(nrow(fc)/nrow(fr))
}
dev.off()

# Predation studies cover short time span. With a 90 % MCP the area where availability is defined leaves out places 
# within the home range where wolves could have also been.

# ---- 2. MCP 100% ----

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #USED GPS positions only


coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

mcp_100 <- mcp(c[,1], percent = 100) # Create MCP for each territory
plot(mcp_100,col = mcp_100$id)
plot(mcp_100[1, ])


# ---- 3. Create random points ----

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

tmp.df[[i]] <- data.frame( territory_=ID[i] 
                      , x_UTM=coordinates(rdm.sp)[,1]
                      , y_UTM=coordinates(rdm.sp)[,2])
}

df <- do.call(rbind, tmp.df)

df$used <- 0 # Used 0: Random


# ---- 4. Join created random points with used GPS locations ----

d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ]
c$Used <- 1 # Used 1: GPS points
colnames(c) <- colnames(df)

data <- rbind(c,df) # Join

data1 <- data[data$territory=="Fulufjallet_2010_w",]
plot(data1$x~data1$y, col=as.factor(data1$used))



setwd("~/Norway/NHBD_humans")
write.csv(data, "all_points.csv")




