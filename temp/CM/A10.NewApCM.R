## ====  ====
# New Approach to define availability
  # 1 - Straight line from natal to established
  # 2 - Buffer of different categories (50 km - 100 km - 150 km)
  # 3 - Create random points within the buffer
  # 4 - Extract habitat characteristics
  # 5 - Sort natal-established-available
  # 6.1 - Distance metric and CLR
  # 6.2 - 6 clusters kmeans and CLR

## LOAD NECESSARY PACKAGES 
rm(list = ls())
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(survival)
library(dplyr)
library(snow)
library(splitstackshape)

## SET WORKING DIRECTORY 
# setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data")
# setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
setwd("C:/My_documents/ana/nhbd/NHBD/Data")

buffer.size <- c(25000, 75000, 125000, 175000, 250000)
for(xxx in 1:length(buffer.size)){
## ==== I. LOAD NECESSARY DATA ====

# THE STUDY AREA 
study_area <- readOGR(".", "hand_study_area1")

# LOAD SWEDEN AND NORWAY 
COUNTRIES <- readOGR("countries.removed.islands.shp")       ## Detailed Map of Scandinavia (including Finland & parts of Russia)
COUNTRIES <- COUNTRIES[which(COUNTRIES$ISO %in% c("NOR","SWE")),]              ## Just take Sweden and Norway 


# LOAD THE BUFFER OF OCCUPIED TERRITORIES THE YEAR BEFORE ESTABLISHMENT 
load("Buffer.RData") #b: Data buffers occupied territories the year before establishment
proj4string(b) <- proj4string(study_area)

# LOAD EXTRACTED FROM NATAL AND ESTABLISHED 
e <- read.csv(file="extracted_values_NatEst.csv", header=TRUE) 

# LOAD WOLF DENSITY
w <- read.csv("Data_NHBD_id_wolf_density.csv")


# LOAD THE VEGETATION DATA 
load("stack.RData")
names.stack <- names(stack)
stack <- stack("stack.tif")
names(stack) <- names.stack

## ---- 1. CREATE A STRAIGHT LINE FROM NATAL TO ESTABLISHED ---- 
# LOAD THE DISPERSAL DATA
disper <- read.delim("dispersal_p.txt")

# GET ESTABLISHED 
nat <- disper[ ,c("X_birth","Y_birth")]
colnames(nat)[1] <- "X"
colnames(nat)[2] <- "Y"

est <- disper[ ,c("X_Established","Y_Established")]
colnames(est)[1] <- "X"
colnames(est)[2] <- "Y"

# CREATE THE SPLINES 
l <- vector("list", nrow(nat))

for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(nat[i, ], est[i,]))), as.character(i))
}
spl <- SpatialLines(l)


# PLOT TO CHECK 
plot(study_area)
# jus select a few of IDS
ID <- sample(1:length(spl), 20)
col <- rainbow(length(ID))

for(i in 1:length(ID)){
  plot(spl[ID[i]], add = TRUE, col=col[i])
  points(nat[ID[i], ], col=col[i])
  points(est[ID[i], ], col=col[i])
}


## ---- 2. CREATE BUFFER AROUND LINES ----

  
buf <- gBuffer(spl, byid = TRUE, width = buffer.size[xxx])
proj4string(buf) <- proj4string(study_area)

# PLOT TO CHECK 
plot(study_area)
# jus select a few of IDS
ID <- sample(1:length(spl), 20)
col <- rainbow(length(ID))

for(i in 1:length(ID)){
  plot(spl[ID[i]], add = TRUE, col=col[i])
  points(nat[ID[i], ], col=col[i])
  points(est[ID[i], ], col=col[i])
  plot(buf[ID[i], ], border=col[i], add=T)
}


## ---- 3. DRAW RANDOM POINTS WITHIN THE BUFFER  ----
# CLIP THE BUFFER SO RANDOM PTS ARE WITHIN THE STUDY AREA
clip_buffer <- gIntersection(study_area, buf, byid = T)
plot(study_area)


  g <- list()
  for (j in 1:271){
    o <- list()
    for (i in 1:11){
      repeat{
        rdm_sp <- spsample(clip_buffer[j], 1,type="random", iter = 10) # sample 1 random point in line
        #1. Select buffers of the year before the year of establishment (dispering)
        ovr <- is.na(over(rdm_sp, b[b$year==(disper$Year.establishment[j]-1), 1])) 
        #2. Over: NA -> Doesnt overlap (TRUE)
        if (ovr==TRUE) break
    
      }
      o[[i]] <- rdm_sp
    # plot 
    }
    plot(study_area)
    points(do.call(rbind,o), col="red", pch=16)
    plot(buf[j], border="red", add=T)
    
    print(j)
    g[[j]] <- o
  }
 
## ---- 4. EXTRACT HABITAT CHARACTERISTICS  ----
  ## PARRALLEL THE LOOP 
  # DEFINE NUMBER OF CORES 
  N.CORES = 6 
  # MAKE WRAPPER FUNCTION FOR THE CLUSTER APPLY
  extract.wrapper <- function(x){
    a_v <- raster::extract(stack, g[[j]][[x]], method='simple', buffer=17841,
            small=TRUE, fun=mean, na.rm=TRUE, df=TRUE, factors=TRUE,
            sp=TRUE)
    return(a_v)
  }
  
  # START THE CLUSTER AND SEND DATA AND LIBRARIES 
  cl <- makeCluster(N.CORES, "SOCK")
  clusterExport(cl, c("stack","g", "j"), envir = environment(NULL))  
  clusterEvalQ(cl, library(raster))

  # START THE LOOP 
u <- list()
  for(j in 1:271){
    o <- list()
    # for (i in 1:11){
    #   time <-proc.time()
    #   a_v <- extract(stack, g[[j]][[i]], method='simple', buffer=17841,
    #                small=TRUE, fun=mean, na.rm=TRUE, df=TRUE, factors=TRUE,
    #                sp=TRUE)
    # #   # alternative 
    #   time1 <- proc.time()-time

      # time <-proc.time()
      # bu <- gBuffer(g[[j]][[i]], width = 17841 )
      # r <- stack[[1]]
      # #plot(r)
      # r.crop <- mask(r ,bu )
      # cell.id <- which(!is.na(r.crop[]))
      # time <-proc.time()
      # 
      # a_v <- colMeans(stack[cell.id], na.rm = T)
      # time2 <-proc.time()-time
    #   
    #   o[[i]] <- a_v
    # }
    o <- clusterApply(cl, 1:11, extract.wrapper)
    
    print(j)
    
    u[[j]] <- o
  }
stopCluster(cl)


## ---- 5. SORT EXTRACTED AND MERGE AVAILABLE AND OBSERVED TERRITORIES ---- 
availables <- list()
for ( i in 1:length(u)){
  availables[[i]] <- as.data.frame(do.call(rbind, u[[i]]))
  availables[[i]]$ID_rand <- c(1:nrow(availables[[i]]))
  availables[[i]]$ID <- i
}
hey <- do.call(rbind, availables)
hey$Category <- "Available"
## ---- 5.1  MERGE AVAILABLE AND OBSERVED TERRITORIES ---- 
# PREPARE THE OBSERVED TERRITORIES 
  id <- e[e$Category == "Natal", c(33:38)] # Get IDs in data frame with available territories
  id <- expandRows(id, 11, count.is.col = FALSE) # 11 values of the same id to join to available (hey)
# ADD INDIVIDUAL INFORMATION TO AVAILABLE TERRITORIES 
  hey$ID_individual <- id$ID_individual
  hey$Year <- id$Year
  hey$Sex <- id$Sex
  hey$Category <- as.character(hey$Category)
# MERGE NATAL AND ESTABLISHED
  ext <- bind_rows(hey, e) # Join to natal/established
  ext <- arrange(ext, ID, Category)
  ext <- ext[ ,c(32:37,30,31,1:29)]

## ---- 5.2  GET THE MOOSE DENSITY IN ONE COLUMN ----
  moose <- ext[ ,c(9:22)] #Make a column with all moose densities by year

  ext$moose_dens<-0
  YEAR <- c(1998:2011)
  
  for (i in 1:3523){
    ext$moose_dens[i]<- moose[i, which(YEAR==ext$Year[i])]
  }
  
  # REMOVE THE USELESS MOOSE DENSITY COLUMNS
  ext <- ext[,-c(9:22)]
  
## ---- 5.3  DEAL WITH THE WOLF DENSITY ----
  w <- w[ ,c(4,6,10,11,28)]
  e <- left_join(ext, w)
  e <- as.data.frame(e)
  e$moose_dens <- unlist(e$moose_dens)
 
save(e, file=paste("e",buffer.size[xxx] ,".RData", sep=""))   
}


## ---- 6. EUCLIDEAN DISTANCE METRIC BETWEEN TERRITORIES ---- 
for(xxx in 1:length(buffer.size)){
  load(paste("e",buffer.size[xxx] ,".RData", sep=""))
  
e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                           "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                           "roughness_1", "roadbuild_1", "moose_dens")])
  
row.names(e1) <- e$X
d <- dist(e1,method = "euclidean")
d1 <- as.matrix(d)

# ASSIGN DISTANCE TO EACH INDIVIDUAL   
ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
    IDD <- which(e$ID_individual==ID[i])
    e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])
}

# PREPAPRE THE DATA SET FOR CONDITIONNAL LOGISTIC REGRESSION  
  e$Category <- as.character(e$Category)
  cd <- e[e$Category!="Natal",]
  
  cd$Category[cd$Category == "Established"] <- 1
  cd$Category[cd$Category == "Available"] <- 0
  cd$Category <-as.numeric(cd$Category)
  c <- clogit(Category ~ distance  + strata(ID_individual), cd)
  
  c1 <- summary(c)
 print(c1$coefficients)
}  
  
  ####
  confint(c,level = 0.95)
  
  c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), cd)
  summary(c)
  confint(c,level = 0.95)
  
  #When the distance increases (i.e., less similar territories), 
  #the probability of having the event increases: NO NHBD WITH THIS BUFFER SIZE
  
  
  
  # --------------- 6.2. 6 clusters and CLR -------------------------- #
  
  e <- e[ ,-c(23)] #Remove buildings
  e <- e[ ,c(1:4,6:8,5,9:24)]
  
  sd_e<- as.data.frame(scale(e[ ,c(8:23)]))
  pc <- prcomp(sd_e)
  comp <- data.frame(pc$x[,1:5])
  
  # 6 K-MEANS
  k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
  e$Clusters <- k6$cluster  
  

  # Prepare data for CLR (one data frame for individuals born in each cluster)
  
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
  
  # CLR #
  
  c <- clogit(Category ~ Clusters + E + wolf_density + wolf_density * E  + Sex * E + strata(ID_individual), clr)
  summary(c) # NO NHBD
  confint(c,level = 0.95)
  