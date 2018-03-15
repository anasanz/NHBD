
#SCRIPT: Match resolutions, layer stack, extract natal and established values
library("rgdal", lib.loc="~/R/win-library/3.1")
library(sp)
library(raster)

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Clips")

moose1999<-raster('clip_moose1999.tif')
moose2000<-raster('clip_moose2000.tif')  
moose2001<-raster('clip_moose2001.tif')                  
moose2002<-raster('clip_moose2002.tif')
moose2003<-raster('clip_moose2003.tif')  
moose2004<-raster('clip_moose2004.tif')  
moose2005<-raster('clip_moose2005.tif')  
moose2006<-raster('clip_moose2006.tif')  
moose2007<-raster('clip_moose2007.tif')  
moose2008<-raster('clip_moose2008.tif')  
moose2009<-raster('clip_moose2009.tif')  
moose2010<-raster('clip_moose2010.tif')  
moose2011<-raster('clip_moose2011.tif')
moose2012<-raster('clip_moose2012.tif')  
human<-raster('clip_human.tif')
humanlands<-raster('clip_humlands.tif')
agri<-raster('clip_agri.tif')
forest<-raster('clip_forest.tif')
mires<-raster('clip_mires.tif')
water<-raster('clip_water.tif')
mountains<-raster('clip_mountains.tif')
roadsec<-raster('roadens_sec1.tif')
mainroad<-raster('roadens_main1.tif')
bear<-raster('clip_bear.tif')
roadbuild<-raster('clip_roadbuild.tif')
build<-raster('clip_build.tif')
dem<-raster('clip_dem.tif')
slope<-raster('slope_pro.tif')
roughness<-raster('roughness_pro.tif')
res(roughness)
extent(roadsec)

#Moving window vegetation:
#Calculate the proportion of each habitat in 1000 x 1000 with 200 resolution
prop.hab<-function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  gri1  <- gri*gri
  func <- function(x) (sum(x)/(gri1) )
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}
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
summary(mountains)


#Match resolutions: resampling method, which transfers values between non matching rasters.
#Nearest neighbour is better for cathegorical data, and bilinear interpolation is better
#for continuous because it makes a weighted average between the center of the four nearests cells
#All layers with the same extent (...) as roadsec

#Record of moose in year t is the density in t-1, so I take this into account here
moose1999<-resample(moose1999,roadsec,method="bilinear",filename="moose1998_dens",format='GTiff',overwrite=TRUE)
moose2000<-resample(moose2000,roadsec,method="bilinear",filename="moose1999_dens",format='GTiff',overwrite=TRUE)
moose2001<-resample(moose2001,roadsec,method="bilinear",filename="moose2000_dens",format='GTiff',overwrite=TRUE)
moose2002<-resample(moose2002,roadsec,method="bilinear",filename="moose2001_dens",format='GTiff',overwrite=TRUE)
moose2003<-resample(moose2003,roadsec,method="bilinear",filename="moose2002_dens",format='GTiff',overwrite=TRUE)
moose2004<-resample(moose2004,roadsec,method="bilinear",filename="moose2003_dens",format='GTiff',overwrite=TRUE)
moose2005<-resample(moose2005,roadsec,method="bilinear",filename="moose2004_dens",format='GTiff',overwrite=TRUE)
moose2006<-resample(moose2006,roadsec,method="bilinear",filename="moose2005_dens",format='GTiff',overwrite=TRUE)
moose2007<-resample(moose2007,roadsec,method="bilinear",filename="moose2006_dens",format='GTiff',overwrite=TRUE)
moose2008<-resample(moose2008,roadsec,method="bilinear",filename="moose2007_dens",format='GTiff',overwrite=TRUE)
moose2009<-resample(moose2009,roadsec,method="bilinear",filename="moose2008_dens",format='GTiff',overwrite=TRUE)
moose2010<-resample(moose2010,roadsec,method="bilinear",filename="moose2009_dens",format='GTiff',overwrite=TRUE)
moose2011<-resample(moose2011,roadsec,method="bilinear",filename="moose2010_dens",format='GTiff',overwrite=TRUE)
moose2012<-resample(moose2012,roadsec,method="bilinear",filename="moose2011_dens",format='GTiff',overwrite=TRUE)

human<-resample(human,roadsec,method="bilinear",filename="human_1",format='GTiff',overwrite=TRUE)

humanlands<-resample(humland_pro,roadsec,method="bilinear",filename="humanlands_1",format='GTiff',overwrite=TRUE)
agri<-resample(agri,roadsec,method="bilinear",filename="agri_1",format='GTiff',overwrite=TRUE)
forest<-resample(forest,roadsec,method="bilinear",filename="forest_1",format='GTiff',overwrite=TRUE)
mires<-resample(mires_pro,roadsec,method="bilinear",filename="mires_1",format='GTiff',overwrite=TRUE)
water<-resample(water_pro,roadsec,method="bilinear",filename="water_1",format='GTiff',overwrite=TRUE)
mountains<-resample(mountains_pro,roadsec,method="bilinear",filename="mountains_1",format='GTiff',overwrite=TRUE)

mainroad<-resample(mainroad,roadsec,method="bilinear",filename="mainroad_1",format='GTiff',overwrite=TRUE)

bear<-resample(bear,roadsec,method="bilinear",filename="bear_1",format='GTiff',overwrite=TRUE)

roadbuild<-resample(roadbuild,roadsec,method="bilinear",filename="roadbuild_1",format='GTiff',overwrite=TRUE)

build<-resample(build,roadsec,method="bilinear",filename="build_1",format='GTiff',overwrite=TRUE)

dem<-resample(dem,roadsec,method="bilinear",filename="dem_1",format='GTiff',overwrite=TRUE)
slope<-resample(slope,roadsec,method="bilinear",filename="slope_1",format='GTiff',overwrite=TRUE)
roughness<-resample(roughness,roadsec,method="bilinear",filename="roughness_1",format='GTiff',overwrite=TRUE)

#Layer stack

moose1998<-raster('moose1998_dens.tif')
moose1999<-raster('moose1999_dens.tif')
moose2000<-raster('moose2000_dens.tif')  
moose2001<-raster('moose2001_dens.tif')                  
moose2002<-raster('moose2002_dens.tif')
moose2003<-raster('moose2003_dens.tif')  
moose2004<-raster('moose2004_dens.tif')  
moose2005<-raster('moose2005_dens.tif')  
moose2006<-raster('moose2006_dens.tif')  
moose2007<-raster('moose2007_dens.tif')  
moose2008<-raster('moose2008_dens.tif')  
moose2009<-raster('moose2009_dens.tif')  
moose2010<-raster('moose2010_dens.tif')  
moose2011<-raster('moose2011_dens.tif')  
human<-raster('human_1.tif')
humanlands<-raster('humanlands_1.tif')
agri<-raster('agri_1.tif')
forest<-raster('forest_1.tif')
mires<-raster('mires_1.tif')
water<-raster('water_1.tif')
mountains<-raster('mountains_1.tif')
roadsec<-raster('roadens_sec1.tif')
mainroad<-raster('mainroad_1.tif')
bear<-raster('bear_1.tif')
roadbuild<-raster('roadbuild_1.tif')
build<-raster('build_1.tif')
dem<-raster('dem_1.tif')
slope<-raster('slope_1.tif')
roughness<-raster('roughness_1.tif')


compareRaster(moose1998,moose1999,moose2000,moose2001,moose2002,moose2003,moose2004,
              moose2005,moose2006,moose2007,moose2008,moose2009,moose2010,moose2011,
              human,humanlands,agri,forest,mires,water,mountains,roadsec,mainroad,
              bear,dem,slope,roughness,roadbuild,build,
              extent=TRUE,rowcol=TRUE,res=TRUE,orig=TRUE)

stack<-stack(moose1998,moose1999,moose2000,moose2001,moose2002,moose2003,moose2004,
             moose2005,moose2006,moose2007,moose2008,moose2009,moose2010,moose2011,
             human,humanlands,agri,forest,mires,water,mountains,roadsec,mainroad,bear,
             dem,slope,roughness,roadbuild,build,RAT=TRUE)
save(stack,file = "stack.RData")

#Extract values of the coordinates buffered (NATAL AND ESTABLISHED)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")
disper<-disper[ ,c("X_birth","Y_birth")]
View(disper)

natal_values1<-extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
write.csv(natal_values1,file="natal_values_final")
View(natal_values1)

disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")
disper<-disper[ ,c("X_Established","Y_Established")]

established_values1<-extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
write.csv(established_values1,file="established_values_final")
View(established_values1)


