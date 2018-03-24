
#New Approach to define availability
  # 1 - Straight line from natal to established
  # 2 - Buffer of different categories (50 km - 100 km - 150 km)
  # 3 - Create random points within the buffer
  # 4 - Extract habitat characteristics
  # 5 - Sort natal-established-available
  # 6 - Distance metric and CLR
  # 7 - 6 clusters kmeans and CLR

rm(list = ls())
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

# --------------- 1 Straight line from natal to established -------------------------- #

setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data")
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")


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
  setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
  load("Buffer.RData") #b: Data buffers occupied territories the year before establishment
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
  setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
  
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
  
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data") 
  setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
  
  e<-read.csv(file="extracted_values_NatEst",header=TRUE)
  
  library(splitstackshape)
  id <- e[e$Category == "Natal", c(33:38)] # Get IDs in data frame with available territories
  id<-expandRows(id,11,count.is.col = FALSE)
  hey$ID_individual<-id$ID_individual
  hey$Year<-id$Year
  hey$Sex<-id$Sex
  
  ext<-bind_rows(hey,e) # Join to natal/established
  ext<-arrange(ext,ID, Category)
  
  ext <- ext[ ,c(32:37,30,31,1:29)]

  
  moose<-ext[ ,c(9:22)] #Make a column with all moose densities by year
  length((moose))
  
  ext$moose_dens<-0
  
  YEAR <- c(1998:2011)
  
  for (i in 1:3523){
    
    ext$moose_dens[i]<- moose[i, which(YEAR==ext$Year[i])]
  }
  
  View(ext)
  ext<-ext[,-c(9:22)]
  str(ext)
  
  setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Publication/Datos") #Join to wolf density
  setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
  
  w<-read.csv("Data_NHBD_id_wolf_density.csv")
  w <- w[,c(4,6,10,11,28)]
  
  e <- left_join(ext,w)
  e <- as.data.frame(e)
  e$moose_dens <- unlist(e$moose_dens)
  setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
  write.csv(e,file="all_extracted_buffer")
  
  # --------------- 6. Distance metric and CLR -------------------------- #
    
  library(survival)
  
  head(e)
  e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                           "roughness_1", "roadbuild_1", "moose_dens")])
  
  row.names(e1) <- e$X
  d <- dist(e1,method = "euclidean")
  class(d)
  d1 <- as.matrix(d)
  d1[1:10,1:10]
  
  
  ID <- unique(e$ID_individual)
  e$distance <- 0
  for(i in 1:length(ID)){
    IDD <- which(e$ID_individual==ID[i])
    
    e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
    
  }
  
  e$Category <- as.character(e$Category)
  
  cd <- e[e$Category!="Natal",]
  
  cd$Category[cd$Category == "Established"] <- 1
  cd$Category[cd$Category == "Available"] <- 0
  cd$Category <-as.numeric(cd$Category)
  
  
  c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), cd)
  summary(c)
  confint(c,level = 0.95)
  
  #When the distance increases (i.e., less similar territories), 
  #the probability of having the event increases: NO NHBD WITH THIS BUFFER SIZE
  
  # --------------- 7. 6 clusters and CLR ?-------------------------- #
  
  e <- e[ ,-c(23)] #Remove buildings
  e <- e[ ,c(1:4,6:8,5,9:24)]
  
  sd_e<- as.data.frame(scale(e[ ,c(8:23)]))
  pc <- prcomp(sd_e)
  comp <- data.frame(pc$x[,1:5])
  #6 KMEANS
  k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
  e$Clusters <- k6$cluster  

  
  library(survival)
  #CLUSTER 1
  cluster.1<-e[e$Clusters == 1, ]
  length(which(cluster.1$Category == "Natal"))
  natal<-cluster.1[cluster.1$Category == "Natal", ]
  #Select only the IDS that are born in this cluster (natal territories + their available and established)
  cl1 <- cluster.1[which(cluster.1$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b1<-e[which(e$ID_individual %in% cl1$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b1$Category <- as.character(b1$Category)
  b1<-b1[!b1$Category == "Natal", ]
  b1$Category[b1$Category == "Established"] <- 1
  b1$Category[b1$Category == "Available"] <- 0
  b1$Category<-as.numeric(b1$Category)
  #Cluster as factor
  b1$Clusters <- as.factor(b1$Clusters)
  #Select variables
  b1<-b1[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b1$E<-b1$Category
  b1$E <- 0
  b1$E[b1$Clusters == 1] <- 1
  
  
  #CLUSTER 2
  cluster.2<-e[e$Clusters == 2, ]
  length(which(cluster.2$Category == "Natal"))
  natal<-cluster.2[cluster.2$Category == "Natal", ]
  #Select only the IDS that are born in this cluster
  cl2 <- cluster.2[which(cluster.2$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b2<-e[which(e$ID_individual %in% cl2$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b2$Category <- as.character(b2$Category)
  b2<-b2[!b2$Category == "Natal", ]
  b2$Category[b2$Category == "Established"] <- 1
  b2$Category[b2$Category == "Available"] <- 0
  b2$Category<-as.numeric(b2$Category)
  #Cluster as factor
  b2$Clusters <- as.factor(b2$Clusters)
  #Select variables
  b2<-b2[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b2$E<-b2$Category
  b2$E <- 0
  b2$E[b2$Clusters == 2] <- 1
  unique(b2$ID_individual)
  
  
  #CLUSTER 3
  
  cluster.3<-e[e$Clusters == 3, ]
  length(which(cluster.3$Category == "Natal"))
  natal<-cluster.3[cluster.3$Category == "Natal", ]
  #Select only the IDS that are born in this cluster
  cl3 <- cluster.3[which(cluster.3$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b3<-e[which(e$ID_individual %in% cl3$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b3$Category <- as.character(b3$Category)
  b3<-b3[!b3$Category == "Natal", ]
  b3$Category[b3$Category == "Established"] <- 1
  b3$Category[b3$Category == "Available"] <- 0
  b3$Category<-as.numeric(b3$Category)
  #Cluster as factor
  b3$Clusters <- as.factor(b3$Clusters)
  #Select variables
  b3<-b3[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b3$E<-b3$Category
  b3$E <- 0
  b3$E[b3$Clusters == 3] <- 1
  unique(b3$ID_individual)
  
  
  #CLUSTER 4
  cluster.4<-e[e$Clusters == 4, ]
  length(which(cluster.4$Category == "Natal"))
  natal<-cluster.4[cluster.4$Category == "Natal", ]
  #Select only the IDS that are born in this cluster
  cl4 <- cluster.4[which(cluster.4$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b4<-e[which(e$ID_individual %in% cl4$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b4$Category <- as.character(b4$Category)
  b4<-b4[!b4$Category == "Natal", ]
  b4$Category[b4$Category == "Established"] <- 1
  b4$Category[b4$Category == "Available"] <- 0
  b4$Category<-as.numeric(b4$Category)
  #Cluster as factor
  b4$Clusters <- as.factor(b4$Clusters)
  #Select variables
  b4<-b4[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b4$E<-b4$Category
  b4$E <- 0
  b4$E[b4$Clusters == 4] <- 1
  unique(b4$ID_individual)
  
  #CLUSTER 5
  
  cluster.5<-e[e$Clusters == 5, ]
  length(which(cluster.5$Category == "Natal"))
  natal<-cluster.5[cluster.5$Category == "Natal", ]
  #Select only the IDS that are born in this cluster
  cl5 <- cluster.5[which(cluster.5$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b5<-e[which(e$ID_individual %in% cl5$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b5$Category <- as.character(b5$Category)
  b5<-b5[!b5$Category == "Natal", ]
  b5$Category[b5$Category == "Established"] <- 1
  b5$Category[b5$Category == "Available"] <- 0
  b5$Category<-as.numeric(b5$Category)
  #Cluster as factor
  b5$Clusters <- as.factor(b5$Clusters)
  #Select variables
  b5<-b5[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b5$E<-b5$Category
  b5$E <- 0
  b5$E[b5$Clusters == 5] <- 1
  unique(b5$ID_individual)
  
  
  #CLUSTER 6
  
  cluster.6<-e[e$Clusters == 6, ]
  length(which(cluster.6$Category == "Natal"))
  natal<-cluster.6[cluster.6$Category == "Natal", ]
  #Select only the IDS that are born in this cluster
  cl6 <- cluster.6[which(cluster.6$ID_individual %in% natal$ID_individual), ]
  #For the IDs born in cl, where did they establish and what did they have available?
  b6<-e[which(e$ID_individual %in% cl6$ID_individual), ]
  
  #Transform category to binary variable: CASE-CONTROL
  b6$Category <- as.character(b6$Category)
  b6<-b6[!b6$Category == "Natal", ]
  b6$Category[b6$Category == "Established"] <- 1
  b6$Category[b6$Category == "Available"] <- 0
  b6$Category<-as.numeric(b6$Category)
  #Cluster as factor
  b6$Clusters <- as.factor(b6$Clusters)
  #Select variables
  b6<-b6[ , c("ID_individual","Sex", "Category", "wolf_density", "Clusters")]
  #Create E
  b6$E<-b6$Category
  b6$E <- 0
  b6$E[b6$Clusters == 6] <- 1
  unique(b6$ID_individual)
  
  
  #JOIN DATASETS
  clr <- rbind( b1,b2,b3,b4,b5,b6)
  
  c <- clogit(Category ~ Clusters + E + wolf_density + wolf_density * E  + Sex * E + strata(ID_individual), clr)
  summary(c) # NO NHBD
  confint(c,level = 0.95)
  