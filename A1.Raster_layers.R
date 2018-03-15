
#SCRIPT: Data exploration, rasterize and clip of layers 

library("rgdal", lib.loc="~/R/win-library/3.1")
library(sp)
library(raster)

disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal.txt")
View(disper)
coordinates(disper)<-c("X_birth","Y_birth","X_Established","Y_Established")
class(disper)

#Plot Scandinavian map
ogrInfo("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS", "Scandinavia_border_33N")
#read shp file (readOGR("data source name", "layer")). Create a spatial object called scand
scand<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS", "Scandinavia_border_33N")
#read projection CRS
print(proj4string(scand))
plot(scand, axes=TRUE, border="black") # Defines the color of the border line.
summary(scand)
#Check the attribute table of the layer (@). $ symbol would be to refer to a specific attribute
head(scand@data) 
#Plot points in the map
points(disper$X_birth, disper$Y_birth, pch=19, col="red", cex=0.5)
points(disper$X_Established, disper$Y_Established, pch=19, col="red", cex=0.5)

#Rasterize shp file MOOSE DENSITY
ogrInfo("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Moose density", "moose_N_S_density_km2_lakes_2ha_urban_1ha_with_full_area")
moose<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Moose density", "moose_N_S_density_km2_lakes_2ha_urban_1ha_with_full_area")
head(moose)
print(proj4string(moose))
plot(moose, axes=TRUE, border="black")
points(disper$X_birth, disper$Y_birth, pch=19, col="red", cex=0.5)
summary(moose)
extent(moose)
####RASTER
r1 <- raster(resolution=1000, extent(moose))
#One raster layer for each year#

ras.moos2011 = rasterize(moose,r1,field="D2011")
plot(ras.moos2011,main="Moose density 2011",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2010 = rasterize(moose,r1,field="D2010")
plot(ras.moos2010,main="Moose density 2010",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2009 = rasterize(moose,r1,field="D2009")
plot(ras.moos2009,main="Moose density 2009",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2008 = rasterize(moose,r1,field="D2008")
plot(ras.moos2008,main="Moose density 2008",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2007 = rasterize(moose,r1,field="D2007")
plot(ras.moos2007,main="Moose density 2007",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2006 = rasterize(moose,r1,field="D2006")
plot(ras.moos2006,main="Moose density 2006",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2005 = rasterize(moose,r1,field="D2005")
plot(ras.moos2005,main="Moose density 2005",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2004 = rasterize(moose,r1,field="D2004")
plot(ras.moos2004,main="Moose density 2004",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2003 = rasterize(moose,r1,field="D2003")
plot(ras.moos2003,main="Moose density 2003",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2002 = rasterize(moose,r1,field="D2002")
plot(ras.moos2002,main="Moose density 2002",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2001 = rasterize(moose,r1,field="D2001")
plot(ras.moos2001,main="Moose density 2001",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos2000 = rasterize(moose,r1,field="D2000")
plot(ras.moos2000,main="Moose density 2000",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

ras.moos1999 = rasterize(moose,r1,field="D1999")
plot(ras.moos1999,main="Moose density 1999",xlim=c(-75755,1115157),ylim=c(6133503,7940399))

writeRaster(ras.moos2011,filename='moose_2011',format='GTiff')
writeRaster(ras.moos2010,filename='moose_2010',format='GTiff')
writeRaster(ras.moos2009,filename='moose_2009',format='GTiff')
writeRaster(ras.moos2008,filename='moose_2008',format='GTiff')
writeRaster(ras.moos2007,filename='moose_2007',format='GTiff')
writeRaster(ras.moos2006,filename='moose_2006',format='GTiff')
writeRaster(ras.moos2005,filename='moose_2005',format='GTiff')
writeRaster(ras.moos2004,filename='moose_2004',format='GTiff')
writeRaster(ras.moos2003,filename='moose_2003',format='GTiff')
writeRaster(ras.moos2002,filename='moose_2002',format='GTiff')
writeRaster(ras.moos2001,filename='moose_2001',format='GTiff')
writeRaster(ras.moos2000,filename='moose_2000',format='GTiff')
writeRaster(ras.moos1999,filename='moose_1999',format='GTiff')


# Get moose data of 2011 (File with data of 2012 needed,
#since the data of one year is actually the hunting record of the next year)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Moose density")
#Rasterize shp file MOOSE DENSITY
moose<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Moose density", "moose_final_1997_2014")
head(moose)
print(proj4string(moose))
extent(moose)
####RASTER
r1 <- raster(resolution=1000, extent(moose))
#One raster layer for each year#
ras.moos2012 = rasterize(moose,r1,field="D2012")
plot(ras.moos2012,main="Moose density 2012",xlim=c(-75755,1115157),ylim=c(6133503,7940399))
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Moose density")
writeRaster(ras.moos2012,filename='moose_2012',format='GTiff')

#HUMAN DENSITY
human<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Human density", "Human_density")
summary(human)
print(proj4string(human))
h<-spTransform(human,CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
print(proj4string(h))
plot(h, axes=TRUE, border="black")
points(disper$X_birth, disper$Y_birth, pch=19, col="red", cex=0.5)

#Check if density varies among years in each municipality. If it varies,
#I cannot use an average of all years.
human<-as.data.frame(human)
View(human)
x<-c(2000,2005,2010,2012)
y<-c(human[70,5],human[70,6],human[70,7],human[70,8])
ggplot(human,aes(x,y))+geom_smooth()
#There is not a clear general increase in the scandinavian population, so 
#I take the average.

####RASTER
r2 <- raster(resolution=1000,extent(moose))
ras.hum = rasterize(h,r2,field='mean')
###To change the legend: breakponts<-c(0.0,...) and add in plot breaks=breakpoints###
plot(ras.hum,main="Human density",xlim=c(-75754.88,1115150),ylim=c(6133894,7940399))
writeRaster(ras.hum,filename='human_2005',format='GTiff',overwrite=TRUE)

#SECONDARY ROADS
roadsec<-raster('roadens_sec1.tif')

#VEGETATION
veg<-raster('veg_reclass200.tif')
print(proj4string(veg))
extent(veg)
res(veg)

#Make one layer for each class
bveg<-layerize(veg,classes=NULL,bylayer=TRUE,suffix='numbers')
print(proj4string(bveg))
summary(bveg)

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

#BEAR DENSITY. Not used, I used the file bear_dens at the end
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Bear")
bear<-raster('bear.tif')
res(bear)
plot(bear)
print(proj4string(bear))
bear<-spTransform(as(bear,"SpatialGridDataFrame"),CRS('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bear_dens<-projectRaster(bear,crs='+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
writeRaster(bear_dens,filename='bear_proj',format='GTiff')# Bear layer with right projection

#DEMOGRAPHY NORWAY AND SWEDEN
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Dem2")
np<-raster('dem_norway_p.tif')
print(proj4string(np))
res(sp)
sp<-raster('dem_sweden_proj.tif')
print(proj4string(sp))

#MERGE NORWAY AND SWEDEN
dem_mosaic<-mosaic(np,sp,fun=mean,tolerance=0.4,filename='dem_mosaic2')
plot(dem_mosaic)
writeRaster(dem_mosaic,filename='dem_scand2',format='GTiff')

#BUILDINGS
build<-raster('builddens.tif')
plot(build)
res(build)
print(proj4string(build))

#ROADBUILD
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/roadbuild")
roadbuild<-raster('road_build.tiff')
res(roadbuild)
plot(roadbuild)
print(proj4string(roadbuild))


moose1999<-raster('moose_1999.tif')
moose2000<-raster('moose_2000.tif')
moose2001<-raster('moose_2001.tif')
moose2002<-raster('moose_2002.tif')
moose2003<-raster('moose_2003.tif')
moose2004<-raster('moose_2004.tif')
moose2005<-raster('moose_2005.tif')
moose2006<-raster('moose_2006.tif')
moose2007<-raster('moose_2007.tif')
moose2008<-raster('moose_2008.tif')
moose2009<-raster('moose_2009.tif')
moose2010<-raster('moose_2010.tif')
moose2011<-raster('moose_2011.tif')
moose2012<-raster('moose_2012.tif')

human2005<-raster('human_2005.tif')


veg<-raster('veg_reclass200.tif')
veg
plot(veg)

bear<-raster('bear_dens.tif')
bear
plot(bear)

dem<-raster('dem_scand2.tif')

roadsec<-raster('roadens_sec1.tif')
roadsec
plot(roadsec)

mainroad<-raster('roadens_main1.tif')
mainroad
plot(mainroad)

#Clip all layers with the extent of the roads clip layer

clip_moose1999 <- crop(moose1999, extent(roadsec), snap="out")
clip_moose2000 <- crop(moose2000, extent(roadsec), snap="out")
clip_moose2001 <- crop(moose2001, extent(roadsec), snap="out")
clip_moose2002 <- crop(moose2002, extent(roadsec), snap="out")
clip_moose2003 <- crop(moose2003, extent(roadsec), snap="out")
clip_moose2004 <- crop(moose2004, extent(roadsec), snap="out")
clip_moose2005 <- crop(moose2005, extent(roadsec), snap="out")
clip_moose2006 <- crop(moose2006, extent(roadsec), snap="out")
clip_moose2007 <- crop(moose2007, extent(roadsec), snap="out")
clip_moose2008 <- crop(moose2008, extent(roadsec), snap="out")
clip_moose2009 <- crop(moose2009, extent(roadsec), snap="out")
clip_moose2010 <- crop(moose2010, extent(roadsec), snap="out")
clip_moose2011 <- crop(moose2011, extent(roadsec), snap="out")
clip_moose2012 <- crop(moose2012, extent(roadsec), snap="out")

writeRaster(clip_moose1999,filename='clip_moose1999',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2000,filename='clip_moose2000',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2001,filename='clip_moose2001',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2002,filename='clip_moose2002',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2003,filename='clip_moose2003',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2004,filename='clip_moose2004',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2005,filename='clip_moose2005',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2006,filename='clip_moose2006',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2007,filename='clip_moose2007',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2008,filename='clip_moose2008',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2009,filename='clip_moose2009',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2010,filename='clip_moose2010',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2011,filename='clip_moose2011',format='GTiff',overwrite=TRUE)
writeRaster(clip_moose2012,filename='clip_moose2012',format='GTiff',overwrite=TRUE)

clip_hum <- crop(human2005, extent(roadsec), snap="out")
xmin(mainroad)<- 200167.2
xmax(mainroad)<- 751167.2
ymin(mainroad)<- 6376791
ymax(mainroad)<- 7061791
writeRaster(clip_hum,filename='clip_human',format='GTiff')

clip_veg <- crop(veg, extent(roadsec), snap="out")
plot(clip_veg)
clip_veg
writeRaster(clip_veg,filename='clip_veg',format='GTiff',overwrite=TRUE)
clip_humlands<-crop(humanlands, extent(roadsec), snap="out")
writeRaster(clip_humlands,filename='clip_humlands',format='GTiff',overwrite=TRUE)
clip_agri<-crop(agri, extent(roadsec), snap="out")
writeRaster(clip_agri,filename='clip_agri',format='GTiff',overwrite=TRUE)
clip_forest<-crop(forest, extent(roadsec), snap="out")
writeRaster(clip_forest,filename='clip_forest',format='GTiff',overwrite=TRUE)
clip_mires<-crop(mires, extent(roadsec), snap="out")
writeRaster(clip_mires,filename='clip_mires',format='GTiff',overwrite=TRUE)
clip_water<-crop(water, extent(roadsec), snap="out")
writeRaster(clip_water,filename='clip_water',format='GTiff',overwrite=TRUE)
clip_mountains<-crop(mountains, extent(roadsec), snap="out")
writeRaster(clip_mountains,filename='clip_mountains',format='GTiff',overwrite=TRUE)


clip_bear <- crop(bear_dens, extent(roadsec), snap="out")
plot(clip_bear,main='Bear density clip')
clip_bear
writeRaster(clip_bear,filename='clip_bear',format='GTiff')

writeRaster(mainroad,filename='mainroad',format='GTiff')

clip_dem <- crop(dem, extent(roadsec), snap="out")
plot(clip_dem,main='Dem clip')
writeRaster(clip_dem,filename='clip_dem',format='GTiff',overwrite=TRUE)

clip_roadbuild<-crop(roadbuild, extent(roadsec), snap="out")
plot(clip_roadbuild,main='Remotedness')
writeRaster(clip_roadbuild,filename='clip_roadbuild',format='GTiff')

clip_build<-crop(build, extent(roadsec), snap="out")
plot(clip_build,main='Remotedness')
writeRaster(clip_build,filename='clip_build',format='GTiff')

#SLOPE#
dem<-raster('clip_dem.tif')
res(dem)
slope<-terrain(dem,'slope',unit='degrees',neighbors=8,filename='slope',overwrite=TRUE)
plot(slope)
writeRaster(slope,filename='clip_slope',format='GTiff',overwrite=TRUE)

#ROUGHNESS
roughness<-terrain(dem,'roughness',neighbors=8,filename='roughness',overwrite=TRUE)
res(roughness)
writeRaster(roughness,filename='clip_roughness',format='GTiff',overwrite=TRUE)

# Moving window slope and roughness
prop.hab<-function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  gri1  <- gri*gri
  func <- function(x) (sum(x)/(gri1) )
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}
roughness_pro <- prop.hab(roughness, gri=5)
writeRaster(roughness_pro,filename='roughness_pro',format='GTiff',overwrite=TRUE)
slope_pro <- prop.hab(slope, gri=5)
writeRaster(slope_pro,filename='slope_pro',format='GTiff',overwrite=TRUE)
