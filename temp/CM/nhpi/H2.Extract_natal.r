
library(dplyr)
library(rgdal)
library(raster)

#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Clips")
setwd("~/Norway/MASTER THESIS/Data/GIS/Clips")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/data.extraction")


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
load("stack.RData") # Load layers

# setwd("~/Norway/NHBD_humans")
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/nhbd/NHBD/temp/CM/nhpi/new")

d <- read.csv("data_pairs_human_complete.csv", header = TRUE) #Load territory coordinates

# Extract female birth territory values
disper<-d[ ,c("X_birth_F","Y_birth_F")] 
natal_F <- extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
natal_FID <- bind_cols(d[ , which(colnames(d) %in% c("ID_F", "Sex_GPS", "Territory_antonio"))], natal_F) #Join IDs and whether if was the female
                                                                                    #the one with the GPS collar
colnames(natal_FID)[4:33] <- paste("F",colnames(natal_FID)[4:33], sep = "_")
natal_FID <- natal_FID[ ,which(colnames(natal_FID) %in% c("Territory_antonio","Sex_GPS","ID_F", "F_human_1", "F_humanlands_1", "F_agri_1", "F_forest_1",
                                                        "F_roadens_sec1", "F_mainroad_1", "F_bear_1", "F_roadbuild_1",
                                                        "F_build_1"))] # Select human & interesting variables

# Extract male birth territory values
disper<-d[ ,c("X_birth_M","Y_birth_M")] 

# I remove the NA from the inmigrant individuals (Y)
disper <- disper[-which(is.na(disper)), ]
natal_M <- extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
# Add the rows I removed with NA (To make it fit when I join it with the females)
topdata <- natal_M[1:33, ]
bottomdata <- natal_M[34:43, ]
nadata <- as.data.frame(matrix(nrow = 1, ncol = 30))
colnames(nadata) <- colnames(natal_M)
natal_M <- rbind(topdata,nadata,bottomdata,nadata,nadata,nadata)

# Continue shaping the data
natal_MID <- bind_cols(d[ , which(colnames(d) %in% c("ID_M", "Sex_GPS"))], natal_M)
colnames(natal_MID)[4:32] <- paste("M",colnames(natal_MID)[4:32], sep = "_")
natal_MID <- natal_MID[ ,which(colnames(natal_MID) %in% c("ID_M", "M_human_1", "M_humanlands_1", "M_agri_1", "M_forest_1",
                                                          "M_roadens_sec1", "M_mainroad_1", "M_bear_1", "M_roadbuild_1",
                                                          "M_build_1"))] # Select human & interesting variables


values <- bind_cols(natal_FID,natal_MID)

# setwd("~/Norway/NHBD_humans")

write.csv(values,file="natal_values_complete.csv")


