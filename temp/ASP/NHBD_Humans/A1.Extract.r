
library(rgdal)
library(raster)

# ---- Check the data ANTONIO ----

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

# With a 95 % MCP the area where availability is defined is too small
# Extract values from random points created in 100 % MCP (created file all_points.csv)

# ---- Extract from 100% MCP ----

#  A. ---- LAYERS ANTONIO ----

# 1. --- Vegetation ---

setwd("C:/Users/Ana/Documents/Norway/NHBD_humans/Antonio/GIS/vegetation")
veg <- raster('veg_25.tif') # 25 x 25

#Make one layer for each class
bveg<-layerize(veg,classes=NULL,bylayer=TRUE,suffix='numbers')

humanlands<-raster(bveg,layer=1)
writeRaster(humanlands,filename='humanlands',format='GTiff')
agri<-raster(bveg,layer=2)
writeRaster(agri,filename='agrilands',format='GTiff')
forest<-raster(bveg,layer=3)
writeRaster(forest,filename='forestlands',format='GTiff')
mires<-raster(bveg,layer=4)
writeRaster(mires,filename='mireslands',format='GTiff')
water<-raster(bveg,layer=5)
writeRaster(water,filename='waterlands',format='GTiff')
mountains<-raster(bveg,layer=6)
writeRaster(mountains,filename='mountainlands',format='GTiff')

humanlands <- raster("humanlands.tif")
agri <- raster("agrilands.tif")
forest <- raster("forestlands.tif")
mires <- raster("mireslands.tif")
water <- raster("waterlands.tif")
mountains <- raster("mountainlands.tif")


#Moving window vegetation:
#Calculate the proportion of each habitat in a 5x5 moving window
# In one cell, proportion of that habitat in the 25 neighbouring cells

prop.hab<-function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  gri1  <- gri*gri
  func <- function(x) (sum(x)/(gri1) )
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}

setwd("C:/Users/Ana/Documents/Norway/NHBD_humans/Antonio/GIS/Analysis") # Saved in folder Analysis

humland_pro <- prop.hab(humanlands, gri=5)
writeRaster(humland_pro,filename='humland_pro',format='GTiff')
agri <- prop.hab(agri, gri=5)
writeRaster(agri,filename='agri_pro',format='GTiff')
forest <- prop.hab(forest, gri=5)
writeRaster(forest,filename='forest_pro',format='GTiff')
mires_pro <- prop.hab(mires, gri=5)
writeRaster(mires_pro,filename='mires_pro',format='GTiff',overwrite=TRUE)
water_pro <- prop.hab(water, gri=5)
writeRaster(water_pro,filename='water_pro',format='GTiff',overwrite=TRUE)
mountains_pro <- prop.hab(mountains, gri=5)
writeRaster(mountains_pro,filename='mountains_pro',format='GTiff')

# 2. --- TRI ---

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Clips")
dem <- raster('clip_dem.tif')

tri <- function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  
  func <- function(x) (sum(abs((x[(foc)]- x[(c(1:(foc-1),(foc+1):(gri*gri)))])))) 
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}

tri5 <- tri(dem, gri=5)

setwd("C:/Users/Ana/Documents/Norway/NHBD_humans/Antonio/GIS/Analysis") 
writeRaster(tri5, "tri5", format = "GTiff") # Saved in folder Analysis


#  B. ---- COORDINATES ----

setwd("~/Norway/NHBD_humans")
d <- read.csv("all_points.csv", header = TRUE)
data <- d[d$territory=="Fulufjallet_2010_w",]
plot(data$x~data$y)
#  C. ---- EXTRACT ----

setwd("~/Norway/NHBD_humans/Antonio/GIS/Analysis")

humanlands <- raster("humland_pro.tif")
agri <- raster("agri_pro.tif")
forest <- raster("forest_pro.tif")
mires <- raster("mires_pro.tif")
water <- raster("water_pro.tif")
mountains <- raster("mountains_pro.tif")
stack_veg <- stack(humanlands, agri, forest, mires, water, mountains, RAT=TRUE)

tri <- raster("tri5.tif")
dem <- raster('clip_dem.tif')
stack_dem <- stack (tri,dem)

main <- raster("main25m.tif")
sec <- raster("2nd25m.tif")
stack_roads <- stack(main, sec)

coord <- d[ ,c("x_UTM","y_UTM")]

cells <- cellFromXY(stack_veg, coord) # 1. Tells the number of the cells where the coord. fall
cov_veg <- stack_veg[cells]           # 2. Retreats the value of those cells in the stack

cells <- cellFromXY(stack_dem, coord) 
cov_dem <- stack_dem[cells]  

cells <- cellFromXY(stack_roads, coord) 
cov_roads <- stack_roads[cells] 

df <- data.frame(d, cov_veg, cov_dem, cov_roads)

df$Season <- 0 # Include season
df$Season[grep("_s", df$territory, ignore.case = TRUE)] <- 1
df$Season[grep("_w", df$territory, ignore.case = TRUE)] <- 2

setwd("~/Norway/NHBD_humans/Antonio")
write.csv (df, "covariates_Antonio.csv")






