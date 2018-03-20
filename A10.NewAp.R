
#New Approach to define availability
  # 1 - Straight line from natal to established
  # 2 - Buffer of different categories (50 km - 100 km - 150 km)
  # 3 - Create random points within the buffer
  # 4 - Extract habitat characteristics
  # 5 - Clustering 6C and/OR Distance metric
  # 6 - CLR
rm(list = ls())
library(sp)
library(rgdal)
library(rgeos)
library(raster)

# --------------- 1 Straight line from natal to established -------------------------- #

setwd("~/MASTER THESIS/Data")

disper <- read.delim("~/MASTER THESIS/Data/dispersal_p.txt")

nat<-disper[ ,c("X_birth","Y_birth")]
est<-disper[ ,c("X_Established","Y_Established")]

colnames(nat)[1] <- "X"
colnames(nat)[2] <- "Y"
colnames(est)[1] <- "X"
colnames(est)[2] <- "Y"

l <- vector("list", nrow(nat))

for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(nat[i, ], est[i,]))), as.character(i))
}

    # Plot 

    setwd("~/MASTER THESIS/Data/GIS")
    study_area <- readOGR(".", "hand_study_area1")
    proj4string(study_area)
    plot(study_area)
    
    spl <- SpatialLines(l[1])
    summary(spl)
    plot(spl, add = TRUE)
    points(nat[1, ])
    points(est[1, ])
    
    spl <- SpatialLines(l[2])
    summary(spl)
    plot(spl, add = TRUE)
    points(nat[2, ])
    points(est[2, ])

spl <- SpatialLines(l)


# --------------- 2 Buffer of different categories (50 km - 100 km - 150 km) -------------------------- #

buf <- gBuffer(spl,byid = TRUE, width = 25000)
proj4string(buf) <- proj4string(study_area)

  plot(study_area)
  plot(spl[1], add = TRUE)
  plot(buf[1], add = TRUE)
  plot(g[[1]][[1]], add = TRUE)
  
  plot(study_area)
  
  for (i in seq_along(spl)) {
    plot(spl[i], add = TRUE)
    plot(buf[i], add = TRUE)
  }

# --------------- 3 Create random points within the buffer -------------------------- #
  

  setwd("~/MASTER THESIS/Data/Random walks")
  load("Buffer.RData") #b: Data buffers occupied
  proj4string(b) <- proj4string(study_area)
  
  g<-list()
  for (j in 1:271){
    o<- list()
    for (i in 1:11){
      repeat{
        rdm_sp <- spsample(buf[j], 1,type="random", iter = 10) # sample 1 random point in line
        ovr <- is.na(over(rdm_sp, b[b$year==(disper$Year.establishment[j]-1), 1])) 
        #1. Select buffers of the year before the year of establishment (dispering)
        #2. Over: NA -> Doesnt overlap (TRUE)
        if (ovr==TRUE) break
      }
      o[[i]] <- rdm_sp
    }
    print(j)
    g[[j]]<-o
  }
  
  setwd("~/MASTER THESIS/Publication/Datos")
  save(g,file = "availablePointsBuffer.RData")
  
  # --------------- 4 Extract habitat characteristics -------------------------- #
  
  setwd("~/MASTER THESIS/Data/Random walks")
  load("stack.RData")
  proj4string(stack)
  extent(stack)
  proj4string(g[[1]][[1]])
  setwd("~/MASTER THESIS/Publication/Datos")
  load("availablePointsBuffer.RData")
  
  u<-list()
  for(j in 1:271){
    o<-list()
    for (i in 1:11){
      a_v<-extract(stack,g[[j]][[i]],method='simple',buffer=17841,
                   small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,
                   sp=TRUE)
      o[[i]]<-a_v
    }
    print(j)
    u[[j]]<-o
  }
  
  #?????????  Error in .local(.Object, ...) : 
 # 14.
 # .local(.Object, ...) 
 # 13.
 # initialize(value, ...) 
 # 12.
 # initialize(value, ...) 
 # 11.
 # new("GDALReadOnlyDataset", filename, silent = silent, allowedDrivers = allowedDrivers, 
 #     options = options) 
 # 10.
 # rgdal::GDAL.open(object@file@name, silent = TRUE) 
 # 9.
 # .readRasterLayerValues(xx, row, nrows, col, ncols) 
 # 8.
 # .local(x, ...) 
 # 7.
 # getValuesBlock(object, rn[i], rx[i] - rn[i] + 1, cn[i], cx[i] - 
 #                  cn[i] + 1) 
 # 6.
 # getValuesBlock(object, rn[i], rx[i] - rn[i] + 1, cn[i], cx[i] - 
 #                  cn[i] + 1) 
 # 5.
 # .xyvBuf(object, xy, buffer, fun, na.rm, layer = layer, nl = nl, 
 #         cellnumbers = cellnumbers, small = small) 
 # 4.
 # .xyValues(x, coordinates(y), ..., df = TRUE) 
 # 3.
 # .local(x, y, ...) 
 # 2.
 # extract(stack, g[[j]][[i]], method = "simple", buffer = 17841, 
 #         small = TRUE, fun = mean, na.rm = TRUE, df = TRUE, factors = TRUE, 
 #         sp = TRUE) 
 # 1.
 # extract(stack, g[[j]][[i]], method = "simple", buffer = 17841, 
 #         small = TRUE, fun = mean, na.rm = TRUE, df = TRUE, factors = TRUE, 
 #         sp = TRUE) #