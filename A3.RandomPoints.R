
library(raster)
library(sp)
library("rgdal", lib.loc="~/R/win-library/3.1")


#Buffer on territories (1997 included)
d<- readOGR("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks", "data_occupiedn")
df<-as.data.frame(d)
df<-df[ ,c(1,2,3,11,12)]
View(df)

u<-list()
for (i in 1:259){
  b<- buffer(d[i,c(12,11)],width = 17841)
  u[i]<-b
}
m <- do.call(bind, u)#Join the buffers of the list
class(df)
b<-SpatialPolygonsDataFrame(m,df)
View(b)
plot(b[1,])
save(b,file = "Buffer.RData")

#Data frame ID-Year

#Random points SHORT TRAJ
library(adehabitatLT)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("short.RData")#Random walks
load("Short_ID.RData")#ID and year
load("Buffer.RData")

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS")

study_area <- readOGR(".", "hand_study_area1")
study_area <- study_area[which(study_area$KUSTX_ %in% c(35)),]##keep only sweden and norway  and small lakes.
plot(study_area)
proj4string(b) <- CRS(proj4string(study_area))

g<-list()
for (j in 1:53){
  o<- list()
  for (i in 1:11){
    repeat{
    r <- ltraj2sldf(h[[j]][[i]]) #as lines df
    proj4string(r) <- CRS(proj4string(study_area)) #same crs
    rdm_sp <- spsample(r, 1,type="random") # sample 1 random point in line
    ovr <- is.na(over(rdm_sp, b[b$year==(short$Year.establishment[j]-1), 1])) 
            #1. Select buffers of the year before the year of establishment (dispering)
            #2. Over: NA -> Doesnt overlap
    ovr_sa <- is.na(over(rdm_sp,study_area))[,1]
            #Overlaps with study area
     if (ovr==TRUE & ovr_sa==FALSE) break
    }
    o[[i]] <- rdm_sp
  }
  print(j)
  g[[j]]<-o
}
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
save(g,file = "short_points.RData")
#If over is NA --> TRUE
#b[b$year== (df$year[j]-1), 1]...If overlaps with a buffer of the 
#same year of establishment (y-1 and y)


#Repeat for ID 34 and trajectory 3 (because it falls out of the raster
#stack and the values can not be extracted)
j = 34
i = 3
points(rdm_sp)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("stack.RData")
plot(study_area)
plot(stack[[1]], ylim = c(5000000, 8000000), add = TRUE)
points(rdm_sp)

#Random points MEDIUM+LONG TRAJ (Spatial points)

#Join lists in order (MEDIUM AND LONG, because SHORT is LTRAJ)
#MEDIUM
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("h1.RData")
h1<-h
load("h55.RData")
h55<-h[55:99]
load("h100.RData")
h100<-h[100:166]
medium<-c(h1,h55,h100)
length(medium)
#LONG
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("l1.RData")
l1<-h
load("l18.RData")
l18<-h[18:34]
load("l35.RData")
l35<-h[35:52]
long<-c(l1,l18,l35)
length(long)
#Join all of them
traj<-c(medium,long)
length(traj) #218
save(traj,file="traj.RData")

#Join medium and long data frames (ID and year)
load("Medium_ID.RData")
med<-medium
load("long_ID.RData")
lon<-long
library(dplyr)
ml<-bind_rows(med,lon)
str(ml)
save(ml,file="medlong_ID.RData")

g<-list()
for (j in 1:218){

  o<- list()
  for (i in 1:11){
    repeat{
      r <-ltraj2sldf(as.ltraj(coordinates(traj[[j]][[i]]), traj[[j]][[i]]$date
                             ,id="A" ))
      rdm_sp <- spsample(r, 1,type="random",bb= c(200167.2,751167.2,6376791,7061791))
      ovr <- is.na(over( rdm_sp, b[b$year==(ml$Year.establishment[j]-1), 1]))
      if (ovr==TRUE) break 
      
    }
    o[[i]] <- rdm_sp
  }
  print(j)
  g[[j]]<-o
}

#With time constraint:If it doesn't find a point within 3 minuts, it will look
#for another point on another trajectory (1:11 chosen randomly)

library(adehabitatLT)

load("medlong_ID.RData")
load("traj.RData")
load("Buffer.RData")

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS")
study_area <- readOGR(".", "hand_study_area1")
study_area <- study_area[which(study_area$KUSTX_ %in% c(35)),]##keep only sweden and norway  and small lakes.
plot(study_area)
proj4string(b) <- CRS(proj4string(study_area))

g<-list()
for (j in 1:218){
  
  o <- list()
  for (i in 1:11){
    strt<-Sys.time() 
    repeat{
      r <-ltraj2sldf(as.ltraj(coordinates(traj[[j]][[i]]), traj[[j]][[i]]$date
                              ,id="A" ))
      proj4string(r) <- CRS(proj4string(study_area))
      rdm_sp <- spsample(r, 1,type="random",bb= c(200167.2,751167.2,6376791,7061791))
      ovr <- is.na(over( rdm_sp, b[b$year==(ml$Year.establishment[j]-1), 1]))
      ovr_sa <- is.na(over(rdm_sp,study_area))[,1]
      
      if( as.numeric(Sys.time()- strt, unit="mins") > 3 ){
        a <- sample(11,1)
        r <-ltraj2sldf(as.ltraj(coordinates(traj[[j]][[a]]), traj[[j]][[a]]$date
                                ,id="A" ))
        proj4string(r) <- CRS(proj4string(study_area))
        rdm_sp <- spsample(r, 1,type="random")
        ovr <- is.na(over( rdm_sp, b[b$year==(ml$Year.establishment[j]-1), 1]))
        ovr_sa <- is.na(over(rdm_sp,study_area))[,1]
      }
      if (ovr==TRUE & ovr_sa==FALSE) break
      
    }
    o[[i]] <- rdm_sp
    print(paste(j,i,sep=" "))
  }
  g[[j]]<-o
}

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
save(g,file = "medlong_points.RData")

