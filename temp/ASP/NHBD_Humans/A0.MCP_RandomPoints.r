rm(list=ls())
# MCP 100%

library(adehabitatHR)
library(sp)

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ] #GPS positions only


coordinates(c) <- cbind(c$x_UTM,c$y_UTM)
proj4string(c) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

mcp_100 <- mcp(c[,1], percent = 100)
plot(mcp_100,col = mcp_100$id)
plot(mcp_100[1, ])


# Random points

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

# MAL
# for (i in 1:length(ID)){
#   rp <- spsample(mcp_100@polygons[[i]], nrow(c[which(c$territory_ == ID[i]), ]), type="random", iter = 10)
#   df[which(df$territory == ID[i]), ] <- data.frame(territory = ID[i], rp@coords)
#   }
# 
# for (i in 1:length(ID)){
#   plot(mcp_100[mcp_100$id==ID[i], ])
#   points(df[which(df$territory == ID[i]),c(2,3)])
# }
df$used <- 0


##### JOIN 
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$id != "random"), which(colnames(d) %in% c("territory_", "x_UTM", "y_UTM")) ]
c$Used <- 1
colnames(c) <- colnames(df)

data <- rbind(c,df)

data1 <- data[data$territory=="Fulufjallet_2010_w",]
plot(data1$x~data1$y, col=as.factor(data1$used))



setwd("~/Norway/NHBD_humans")
write.csv(data, "all_points.csv")




