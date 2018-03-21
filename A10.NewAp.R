
#New Approach to define availability
  # 1 - Straight line from natal to established
  # 2 - Buffer of different categories (50 km - 100 km - 150 km)
  # 3 - Create random points within the buffer
  # 4 - Extract habitat characteristics
  # 5 - Sort natal-established-available
  # 6 - Clustering 6C and/OR Distance metric
  # 7 - CLR
rm(list = ls())
library(sp)
library(rgdal)
library(rgeos)
library(raster)

# --------------- 1 Straight line from natal to established -------------------------- #

setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data")

disper <- read.delim("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/dispersal_p.txt")

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

    setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GIS")
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
  

  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/Random walks")
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
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Publication/Datos")
  save(g,file = "availablePointsBuffer.RData")
  
  # --------------- 4 Extract habitat characteristics -------------------------- #
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/Random walks")
  load("stack.RData")
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Publication/Datos")
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
  
  save(u,file = "extracted_values_available.RData") 
  
  # --------------- 5. Sort extracted -------------------------- #
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Publication/Datos") # Extracted available to data frame
  load("extracted_values_available.RData")
  
  hey <- u[[1]][[1]] 
  hey<- as.data.frame((hey))
  hey[1,] <- rep(NA, ncol(hey))
  hey$ID <- 0
  hey$ID_rand <- 0
  
  row.names(hey)<- 1
  
  for ( i in 1:length(u)){
    
    for ( j in 1:length(u[[i]])){
      tmp <- as.data.frame(u[[i]][[j]])[1,]
      row.names(tmp)  <- as.numeric(row.names(hey)[nrow(hey)])+1
      hey[(nrow(hey)+1) , 1: (ncol(hey)-2) ]  <- tmp
      
      hey$ID[(nrow(hey))] <- i
      hey$ID_rand[(nrow(hey))] <- j
    }
  }
  hey<-hey[-c(1),]
  Category <- vector(mode='numeric', length=271)
  hey <- data.frame(hey, Category)
  hey$Category<-factor(hey$Category,labels = "Available")
  
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data") # Join to natal/established
  e<-read.csv(file="extracted_values_NatEst",header=TRUE)
  
  ext<-bind_rows(hey,e)
  ext<-arrange(ext,ID, Category)

    
  
  