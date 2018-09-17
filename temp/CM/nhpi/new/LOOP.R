rm(list=ls())
library(adehabitatHR)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(sf)

## CREATE IN FILES 
# setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/InFiles/")
# for(i in 1:100){
#   loop <- i
#   save(loop, file=paste("InLoop",i,".RData",sep=""))
# }


AIC_TAB <- list(KERN = list(closest = list(),
                            closest2 = list()),
                MCP = list(closest = list(),
                           closest2 = list()))
COEFF_TAB <- list(KERN = list(closest = list(),
                              closest2 = list()),
                  MCP = list(closest = list(),
                             closest2 = list()))

filesList <- list.files("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/InFiles/")

while(length(filesList)>0){
  set <- sample(filesList, 1)
  load(paste("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/InFiles/", set, sep=""))
  file.rename(from = paste("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/InFiles/",set,sep=""),
              to = paste("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/ProcessFiles/",set,sep=""))
  
  # ====----------------------- ====
  # ==== I. DEFINE AVAILABILITY ====
  # ====----------------------- ====
  # ==== I. LOAD DATA ====
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New")
  gps <- read.csv("gps.dataCM.csv", header = TRUE)
  scand <- readOGR("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/Scandinavia_border_33N.shp")
  
  # CREATE SP FILE
  coordinates(gps) <- cbind(gps$X, gps$Y)
  proj4string(gps) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame
  
  
  # ==== II. DEFINE AVAILABILITY ====
  # ---- 1. MCP 100% ----
  ID <- unique(gps$Study_year)
  
  # CREATE MCP 100
  mcp_100 <- mcp(gps[,"Study_year"], percent = 100) # Create MCP for each territory
  # plot(mcp_100, col = mcp_100$id)
  # plot(mcp_100[1, ])
  
  # ---- 2. KERNEL 99% ----
  
  # CREATE KERNEL 99
  kern <- kernelUD(gps[,"Study_year"], h ="href")# 2206.224)#"href")#4291.715)#"href")#3835)#"href") # use the max "href"
  kern_99 <- getverticeshr(kern, 99)
  # plot(kern_99, col = kern_99$id)
  
  
  h <- 0
  for (i in 1:length(ID)){
    h[i] <- kern[[i]]@h$h
    # plot(kern_99[kern_99$id==ID[i],])
    # points(gps[gps$Study_year==ID[i],], col="red", pch=16)
  }
  max(h)
  mean(h)
  
  
  # ---- 3.1 CREATE RANDOM POINTS ----
  tmp.mcp.df <- tmp.kern.df <-  list()
  
  for (i in 1:length(ID)){
    mcpid <- mcp_100[mcp_100$id==ID[i], ]
    kernid <- kern_99[kern_99$id==ID[i], ]
    
    tmp <- gps[gps$Study_year==ID[i],]
    n.rdm.pts <- nrow(tmp)
    
    set.seed(loop)
    #draw random points
    rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
    set.seed(loop)#picked a value randomly
    rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
    
    # plot(mcpid)
    # points(rdm.mcp.sp)
    # points(gps[gps$Study_year==ID[i],],col="red", pch=16)
    
    # plot(kernid)
    # points(rdm.kern.sp)
    # points(gps[gps$Study_year==ID[i],], col="red", pch=16)
    
    tmp.mcp.df[[i]] <- data.frame( Study_year = ID[i] 
                                   , X = coordinates(rdm.mcp.sp)[,1]
                                   , Y = coordinates(rdm.mcp.sp)[,2])
    
    
    tmp.kern.df[[i]] <- data.frame( Study_year = ID[i] 
                                    , X = coordinates(rdm.kern.sp)[,1]
                                    , Y = coordinates(rdm.kern.sp)[,2])
  }
  
  df.mcp <- do.call(rbind, tmp.mcp.df)
  df.kern <- do.call(rbind, tmp.kern.df)
  df.mcp$used <- 0 # Used 0: Random
  df.kern$used <- 0 # Used 0: Random
  
  ########### for the moving locations 
  i=1
  tmp.mcp.move.df <- tmp.kern.move.df <-  list()
  
  for (i in 1:length(ID)){
    mcpid <- mcp_100[mcp_100$id==ID[i], ]
    kernid <- kern_99[kern_99$id==ID[i], ]
    
    tmp <- gps[gps$Study_year==ID[i] & gps$move==1,]
    n.rdm.pts <- nrow(tmp)
    
    set.seed(loop)
    #draw random points
    rdm.mcp.sp <- spsample(mcpid, n.rdm.pts, type="random", iter = 10)
    set.seed(loop)#picked a value randomly
    rdm.kern.sp <- spsample(kernid, n.rdm.pts, type="random", iter = 10)
    
    # plot(mcpid)
    # points(rdm.mcp.sp)
    # points(gps[gps$Study_year==ID[i] & gps$move==1,], col="red", pch=16)
    
    # plot(kernid)
    # points(rdm.kern.sp)
    # points(gps[gps$Study_year==ID[i] & gps$move==1,], col="red", pch=16)
    
    tmp.mcp.move.df[[i]] <- data.frame( Study_year = ID[i] 
                                        , X = coordinates(rdm.mcp.sp)[,1]
                                        , Y = coordinates(rdm.mcp.sp)[,2])
    
    
    tmp.kern.move.df[[i]] <- data.frame( Study_year = ID[i] 
                                         , X = coordinates(rdm.kern.sp)[,1]
                                         , Y = coordinates(rdm.kern.sp)[,2])
    
    
  }
  
  df.mcp.move <- do.call(rbind, tmp.mcp.move.df)
  df.kern.move  <- do.call(rbind, tmp.kern.move.df)
  df.mcp.move$used <- 0 # Used 0: Random
  df.kern.move$used <- 0 # Used 0: Random
  
  
  # ---- 2.2. JOIN RANDOM AND GPS LOCATIONS ----
  # ---- 2.2.1 MOVE ----
  
  ## MOVE 
  gps.move <- gps[gps$move==1,]
  gps.move$used <- 1 # Used 1: GPS points
  
  coordinates(df.mcp.move) <- df.mcp.move[,c("X","Y")]
  colnames(df.mcp.move@data)[1] <- c("Study_year")
  coordinates(df.kern.move) <- df.kern.move[,c("X","Y")]
  colnames(df.kern.move@data)[1] <- c("Study_year")
  
  proj4string(df.mcp.move) <- CRS(proj4string(gps.move))
  proj4string(df.kern.move) <- CRS(proj4string(gps.move))
  
  data.move.mcp <- rbind(gps.move[,c("Study_year","X", "Y","used")],
                         df.mcp.move[,c("Study_year","X", "Y","used")] ) # Join
  data.move.kern <- rbind(gps.move[,c("Study_year","X", "Y","used")],
                          df.kern.move[,c("Study_year","X", "Y","used")] ) # Join
  
  # PLOT TO CHECK #
  data1 <- data.move.kern[data.move.kern$Study_year=="Fulufjallet_2010_W",]
  # plot(data1, col=as.factor(data1$used),pch=16)
  
  data1 <- data.move.mcp[data.move.mcp$Study_year=="Fulufjallet_2010_W",]
  # plot(data1, col=as.factor(data1$used),pch=16)
  
  # ---- 2.2.2 NOT MOVE ----
  gps$used <- 1 # Used 1: GPS points
  coordinates(df.mcp) <- df.mcp[,c("X","Y")]
  colnames(df.mcp@data)[1] <- c("Study_year")
  coordinates(df.kern) <- df.kern[,c("X","Y")]
  colnames(df.kern@data)[1] <- c("Study_year")
  
  proj4string(df.mcp) <- CRS(proj4string(gps))
  proj4string(df.kern) <- CRS(proj4string(gps))
  
  
  data.mcp <- rbind(gps[,c("Study_year","X", "Y","used")],
                    df.mcp[,c("Study_year","X", "Y","used")] ) # Join
  
  data.kern <- rbind(gps[,c("Study_year","X", "Y","used")],
                     df.kern[,c("Study_year","X", "Y","used")] ) # Join
  
  # PLOT TO CHECK #
  data1 <- data.mcp[data.mcp$Study_year=="Fulufjallet_2010_W",]
  # plot(data1, col=as.factor(data1$used),pch=16)
  
  data1 <- data.kern[data.kern$Study_year=="Fulufjallet_2010_W",]
  # plot(data1, col=as.factor(data1$used),pch=16)
  
  
  # ==== III. EXTRACT COVARIATES AT THE HOME RANGE LEVEL ==== 
  # ---- 1. MOOSE ----
  moose <- readOGR("C:/Personal_Cloud/OneDrive/Work/Phd_Cyril/GIS_LAYERS/Hunting/Moose/moose_update2/moose_final_1997_2017.shp")
  
  # ---- 1.1 RASTERIZE ----
  #rasterise, use the sf package
  moose <- st_as_sf(moose)
  r <- raster(extent(moose))
  res(r) <- 1000
  r[] <- 1
  
  # rasterize moose density
  library(fasterize)
  year <- paste("D", c(2000:2017), sep="")
  moose.r <- stack(fasterize(moose, r, field = year[1]))
  
  for(i in 2:length(year)){
    moose.r[[i]] <- fasterize(moose, r, field = year[i])
    # plot(moose.r[[i]])
  }
  names(moose.r) <- year
  
  # ---- 1.2 EXTRACT MCP VALUES ----
  # Extract mcp values 
  centroids <- data.frame(getSpPPolygonsLabptSlots(mcp_100))
  coordinates(centroids) <- centroids
  year_terr <- as.numeric(unlist(lapply(strsplit(row.names(mcp_100),"_"),function(x) x[2])))
  
  # Create a buffer of the size the wolf territory
  buff <- gBuffer(centroids, width=17841.24, byid = T)
  
  moose_dens <- 0
  for(i in 1:length(buff)){
    moose_dens[i] <- mean(extract(moose.r[[paste("D", (year_terr[i]+1), sep="")]] ,buff[i,])[[1]], na.rm=T)
  }
  
  # ---- 1.3 LINK TO GPS DATA ----
  id_terr <- row.names(mcp_100)
  data.mcp$moose <- 0
  data.kern$moose <- 0
  data.move.mcp$moose <- 0
  data.move.kern$moose <- 0
  
  for(i in 1:length(id_terr)){
    data.mcp$moose[data.mcp$Study_year==id_terr[i]] <- moose_dens[i]
    data.kern$moose[data.kern$Study_year==id_terr[i]] <- moose_dens[i]
    data.move.mcp$moose[data.move.mcp$Study_year==id_terr[i]] <- moose_dens[i]
    data.move.kern$moose[data.move.kern$Study_year==id_terr[i]] <- moose_dens[i]
    
  }
  
  
  # ==== IV. WRITE FILE ==== 
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/loop")
  save(data.mcp, file= paste("all_points.not.moved_MCP",loop,".RData",sep=""))
  save(data.kern, file=paste("all_points.not.moved_KERN",loop,".RData",sep=""))
  
  
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new/loop")
  save(data.move.mcp, file=paste("all_points.move_MCP",loop,".RData",sep=""))
  save(data.move.kern,file= paste("all_points.move_KERN",loop,".RData",sep=""))
  
  
  
  ####
  
  
  # ====----------------------- ====
  # ==== II. EXTRACT COVARIATES ====
  # ====----------------------- ====
  # ==== I. LOAD DATA ====
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/gis")
  #setwd("C:/Users/Ana/Documents/Norway/NHBD_humans/Antonio/GIS/vegetation")
  veg <- raster('veg_25.tif') # 25 x 25
  
  
  
  
  # ====  III. COORDINATES ====
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/loop")
  
  files <- c("all_points.not.moved_MCP", "all_points.not.moved_KERN",
             "all_points.move_MCP", "all_points.move_KERN")
  
  for(f in 1:length(files)){
    # setwd("~/Norway/NHBD_humans")
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new/loop")
    
    d <- get(load(paste(files[f], loop,".RData", sep="")))
    
    coord <- d[ ,c("X","Y")] # Coordinates used and random
    
    
    # ====  IV EXTRACT ----
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/gis")
    # setwd("~/Norway/NHBD_humans/GIS/Analysis")
    
    # Load layers
    ## VEGETATION
    humanlands <- raster("humland_pro.tif")
    agri <- raster("agri_pro.tif")
    forest <- raster("forest_pro.tif")
    mires <- raster("mires_pro.tif")
    water <- raster("water_pro.tif")
    mountains <- raster("mountains_pro.tif")
    stack_veg <- stack(humanlands, agri, forest, mires, water, mountains, RAT=TRUE)
    
    ## ELEVATION
    tri <- raster("tri5.tif")
    dem <- raster('clip_dem.tif')
    stack_dem <- stack (tri,dem)
    
    ## ROADS
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/gis/roads")
    main <- raster("main25m.tif")
    sec <- raster("2nd25m.tif")
    stack_roads <- stack(main, sec)
    
    # CLOSEST HUMAN THING
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/gis/buildings")
    build <- raster("dist_build25.tif")
    
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/gis/")
    # closest distance to main roads/2nd roads/buildings
    # done in arcmap with cells statitics tool.
    # min value from raster main roads/2nd roads and buildings
    closestcosa <- raster("closestcosa.tif")
    
    
    # closest distance to main roads/buildings
    # done in arcmap with cells statitics tool.
    # min value from raster main roads and buildings
    closestcosita <- raster("d_rd_build.tif")
    
    # plot(forest)
    # d<-d[d$territory_=="Grafjell_2003_w",]
    # points(d[d$territory_=="Grafjell_2003_w","y_UTM"]~d[d$territory_=="Grafjell_2003_w","x_UTM"],pch=16)
    # coord <- d[ ,c("x_UTM","y_UTM")] # Coordinates used and random
    
    # Extract values
    cells <- cellFromXY(stack_veg, coord) # 1. Tells the number of the cells where the coord. fall
    cov_veg <- stack_veg[cells]           # 2. Returns the value of those cells in the stack
    # na <- d[which(is.na(cov_veg[,1])),]
    # 
    # plot(na[na$territory_=="Grafjell_2003_w","y_UTM"]~na[na$territory_=="Grafjell_2003_w","x_UTM"],pch=16)
    # plot(forest,add=T)
    
    
    cells <- cellFromXY(stack_dem, coord) 
    cov_dem <- stack_dem[cells]  
    
    cells <- cellFromXY(stack_roads, coord) 
    cov_roads <- stack_roads[cells] 
    
    cells <- cellFromXY(build, coord) 
    cov_build <- build[cells] 
    
    cells <- cellFromXY(closestcosa, coord) 
    closest <- closestcosa[cells] 
    
    cells <- cellFromXY(closestcosita, coord) 
    closest2 <- closestcosita[cells] 
    
    df <- data.frame(d, cov_veg, cov_dem, cov_roads, cov_build, closest, closest2) # Join coordinates with extracted values
    
    #setwd("~/Norway/NHBD_humans/Antonio")
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new/loop")
    save(df, file= paste("covariates_",files[f], loop, ".RData", sep=""))
    
    print(f)
  }
  
  
  # ====----------------------- ====
  # ==== IV. GLM AND LINK ESTABLISHED ====
  # ====----------------------- ====
  library(dplyr) 
  library(lme4) 
  ### ==== I. CHECK CORRELATION BETWEEN VARIABLES ====
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/loop")
  files <- c("all_points.not.moved_MCP", "all_points.not.moved_KERN",
             "all_points.move_MCP", "all_points.move_KERN")
  
  # for(f in 1:length(files)){
  #   d1 <- read.csv(paste("covariates_",files[f], ".csv", sep=""), sep = ",")
  #   d1[is.na(d1)] <- 0
  #   d2 <- d1[ ,which(colnames(d1) %in% c("forest_pro", "clip_dem","mires_pro","clip_dem",
  #                                        "tri5", "main25m", "X2nd25m", "cov_build","closest","closest2"))]
  #   print(cor(d2))
  #   
  # }#f
  
  #forest_pro vs mires_pro == -0.64
  #X2nd25m vs closest == 0.8302396
  #closest2 vs cov_build ==0.86309757
  
  ### ==== II. PERFORM THE ANALYSIS ====
  Model <- list(closest = c("Study_year", "used", "forest_pro", "clip_dem",
                            "tri5", "main25m", "X2nd25m","closest2"),
                closest_all = c("Study_year", "used", "forest_pro", "clip_dem",
                                "tri5", "main25m", "cov_build","closest"))
  Variable <- c("closest2_M","closest_M")
  
  
  HR <- c("MCP.not.moved","KERN.not.moved","MCP.moved","KERN.moved")
  VARIABLE <- c("closest","closest2")
  
  thresholdNBGPS <- c(250,250,50,55)
  # thresholdNBGPS <- c(0,0,0,0)
  
  for(FF in 1:length(files)){
    setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/loop/")
    d1 <- get(load(paste("covariates_", files[FF], loop, ".RData", sep="")))
    
    ## modify territory names
    levels(unique(d1$Study_year))
    # d1$territory <- as.character(d1$territory)
    # d1$territory[which(d1$territory == "Forshyttan_2005s")] <- "Forshyttan_2005_s"
    # d1$territory[which(d1$territory == "Kloten_2010w")] <- "Kloten_2010_w"
    # d1$territory[which(d1$territory == "Glaskogen_2002_S")] <- "Glaskogen_2002_s2"
    # d1$territory[which(d1$territory == "Glaskogen_2002_S")] <- "Glaskogen_2002_s3"
    # 
    # d1$territory[which(d1$territory == "Jangen_2004_W")] <- "Jangen_2004_W"
    # d1$territory[which(d1$territory == "Kloten_2008_W")] <- "Kloten_2008_w"
    # d1$territory[which(d1$territory == "Kukumaki_2014_S")] <- "Kukumaki_2014_s"
    # d1$territory[which(d1$territory == "Nyskoga_2003_S")] <- "Nyskoga_2003_s"
    # d1$territory[which(d1$territory == "Nyskoga_2004_W")] <- "Nyskoga_2004_w"
    # d1$territory[which(d1$territory == "Riala_2010_W")] <- "Riala_2010_w"
    # d1$territory[which(d1$territory == "Stadra_2003_W")] <- "Stadra_2003_w"
    # d1$territory[which(d1$territory == "Tandsjon_2014_S")] <- "Tandsjon_2014_s"
    # d1$territory[which(d1$territory == "Tandsjon_2014_W")] <- "Tandsjon_2014_w"
    # d1$territory[which(d1$territory == "Tenskog_2010_W")] <- "Tenskog_2010_w"
    # d1$territory[which(d1$territory == "Ulriksberg_2006_W")] <- "Ulriksberg_2006_w"
    # d1$territory[which(d1$territory == "Kukumaki_2014_W")] <- "Kukumaki_2014_w"
    # d1$territory[which(d1$territory == "Bograngen_2003_S")] <- "Bograngen_2003_s"
    # d1$territory[which(d1$territory == "Fulufjallet_2009_W")] <- "Fulufjallet_2009_w"
    # 
    # d1$territory[which(d1$territory == "Glaskogen_2002_S")] <- "Glaskogen_2002_s1"
    # 
    # d1$territory[which(d1$territory == "Kukumaki_2015w")] <- "Kukumaki_2015_w"
    # d1$territory[which(d1$territory == "Kukumaki_2015s")] <- "Kukumaki_2015_s"
    
    ### ---- 1. SELECT THE VARIABLES FOR THE MODEL ====
    M=1
    for(M in 1:length(Model)){
      
      d <- d1[ , which(colnames(d1) %in% Model[[M]])]
      
      #keep moose here 
      d$moose <- d1$moose
      
      ### ---- 2. SCALE BY TERRYTORY ====
      terr <- unique(d$Study_year) 
      for(i in 1:length(terr)){
        d[d$Study_year==terr[i],c(3:8)] <- scale(d[d$Study_year==terr[i],c(3:8)])
      }
      
      ### ---- 3. RUN SEPARATE GLM PER TERRITORY ====
      # CREATE NECESSARY OBJECT 
      IDD <- unique(d$Study_year)
      m <- matrix(NA, ncol=9, nrow=length(unique(d$Study_year)))
      m <- data.frame(m)
      colnames(m) <- c("Study_year","(Intercept)",Model[[M]][3:8],"moose")
      rownames(m) <- IDD
      glm.list <- list()
      
      ## RUN GLM
      for(i in 1:length(IDD)){
        data1 <- dat <- d[d$Study_year==IDD[i],] # Select one territory
        used <- data1$used
        
        # SELECT DAT TO USE IN THE MODEL 
        data1 <- data1[,3:8] 
        #IF EMPTY VARIABLES==> REMOVE (NOT NECESSARY)
        variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE) in the territory
        variablestrue <- names(variables[variables==FALSE]) # Names variables without NA in the territory
        df <- data1[,variables!=TRUE] # Data frame variables without NA
        
        #DEFINE GLM FORMULA
        form <- as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA
        glm.list[[i]] <- glm (form, # Run model
                              family = binomial (link = "logit"),
                              data =df )
        
        # STORE RESULTS FROM THE GLM 
        glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
        m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
        m[i, "moose"] <- dat$moose[1]
        
      } 
      glm.m <- m
      glm.m$Study_year <- rownames(glm.m)
      
      ## CHECK AND SUBSET TERRITORIES 
      # t <- d[which(d$territory == "Tandsjon_2012_s"), ]
      # t_random <- t[which(t$used == 0), ]
      # t_used <- t[which(t$used == 1), ]
      # 
      # m_tand <- glm(used ~ forest_pro + tri5 + clip_dem + main25m + X2nd25m  + closest2,
      #               family = binomial (link = "logit"),
      #               data = t)
      # 
      
      ### ---- 5. PCA TO CHARACTERISE NATAL TERRITORY  ====
      setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/")
      v <- read.csv("natal_values_complete.csv", header = TRUE) #Load extracted natal values
      v$ID_pair <- paste(v$ID_F, v$ID_M, sep = "_") # Create ID_pair column to add it as random in the model
      
      ### ----  5.1  JOIN FEMALES-MALES (Same variable names) ----
      # Sort out females
      f <- v[ , which(colnames(v) %in% c("Territory_antonio","ID_F", "F_human_1", "F_humanlands_1", "F_agri_1", "F_forest_1",
                                         "F_roadens_sec1", "F_mainroad_1", "F_bear_1", "F_roadbuild_1",
                                         "F_build_1", "ID_pair"))]
      colnames(f)[2:11] <- c("ID", "human_1", "humanlands_1", "agri_1", "forest_1",
                             "roadens_sec1", "mainroad_1", "bear_1", "roadbuild_1",
                             "build_1")
      f$Sex <- "F"
      
      # Sort out males
      m <- v[ , which(colnames(v) %in% c("Territory_antonio","ID_M", "M_human_1", "M_humanlands_1", "M_agri_1", "M_forest_1",
                                         "M_roadens_sec1", "M_mainroad_1", "M_bear_1", "M_roadbuild_1",
                                         "M_build_1", "ID_pair"))]
      colnames(m)[2:11] <- c("ID", "human_1", "humanlands_1", "agri_1", "forest_1",
                             "roadens_sec1", "mainroad_1", "bear_1", "roadbuild_1",
                             "build_1")
      m$Sex <- "M"
      
      #Join
      natal <- bind_rows(f,m) # Contains natal values of all territories
      # natal <- cbind(f,m) # Contains natal values of all territories
      
      #Delete NA inmigrant males
      natal <- natal[complete.cases(natal), ]
      
      ### ----  5.2  PCA to characterize natal territories ----
      # Create column with PC scores of the natal territory (characterized by human variables)
      # to link it later with the selection coefficients of each territories
      
      library(factoextra)
      n <- natal[ ,colnames(natal) %in% c("Territory_antonio", "ID", "human_1", "humanlands_1", "agri_1", "mainroad_1", #"roadbuild_1",
                                          "roadens_sec1", "build_1")] # Only human-related variables
      
      sd_n <- as.data.frame(scale(n[3:8]))
      pc <- prcomp(sd_n)
      fviz_pca_biplot(pc, label="var",col.var = "black") +
        theme(text = element_text(size = 15),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))
      pc$x
      pc$sdev/sum(pc$sdev)
      pc$x[ ,1] #PC1 explains 72% of the variance and separates human (+) vS non-human (-) 
      # characterized territories
      # However, PC1 doesnt capture the difference in sec. roads, which is more captured by PC2
      natal$PC <- pc$x[ ,1] 
      natal$PC2 <- pc$x[ ,2]
      58.6+26.2
      ################### Check natal territory of wolves with high natal values ###
      natal[which(natal$PC > 5), ] # Kloten and Stadra
      natal[which(natal$PC2 < -4), ] # Stadra
      
      # Kloten_2009_s and Kloten_2010_w (Individual M0918, natal territory = Krp2) have high values of human density and roadbuild
      # Stadra_2003_w (ID M0314, natal territory = Mos) have high values of human density
      # These territories fall in high human-dominated areas (south), so there is nothing wrong, they are real outliers.
      # They give us variation in human variables, so for the moment we keep them in the models.
      
      ### ---- 6. LINK PCA SCORES WITH NATAL TERRITORY  ====
      colnames(glm.m)[1] <- "Territory_antonio"
      e <- left_join(natal, glm.m, by = "Territory_antonio") 
      
      e <- e[which(!is.na(e$forest_pro)),]
      as.factor(e$ID_pair)
      e$Season <- "W" # Add season (Summer/Winter = 1/0)
      e$Season[grep("_S", e$Territory_antonio, ignore.case = TRUE)] <- "S"
      
      
      # reorganise /F and M
      prov_f <- e[which(e$Sex == "F"), ]
      prov_m <- e[which(e$Sex == "M"), ] 
      
      colnames(prov_f) <- paste(colnames(prov_f),"_F",sep="")
      colnames(prov_m) <- paste(colnames(prov_m),"_M",sep="")
      colnames(prov_f)[1] <- "Territory_antonio"
      colnames(prov_m)[1] <- "Territory_antonio"
      
      prov <- left_join(prov_m, prov_f, by = "Territory_antonio") 
      
      # III. ---- Link with coefficients ----
      #setwd("~/Norway/NHBD_humans/Antonio")
      
      d_used <- d1[which(d1$used == 1), ] # Select used (gps positions)
      tapply(d_used$Study_year, d_used$Study_year, length) # nº positions/territory
      
      positions <- as.data.frame(tapply(d_used$Study_year, d_used$Study_year, length)) # Data frame with nº of positions/territory
      remove <- rownames(positions)[which(positions$`tapply(d_used$Study_year, d_used$Study_year, length)` < thresholdNBGPS[FF])] # Remove the ones < 250
      if(length(remove)>0){
        prov <- prov[-which(prov$Territory_antonio %in% remove),]
      }
      # ---- IV. PLOTS ----
      # ==== 1. MALES ====
      
      # plot(prov[,Variable[M]] ~ prov$PC_M, pch = 16) # Overall trend not significant for all sexes together
      # abline(lm(prov[,Variable[M]] ~ prov$PC_M)) 
      # summary(lm(prov[,Variable[M]] ~ prov$PC_M))
      # 
      # #moose
      # plot(prov[,Variable[M]] ~ prov$moose_M, pch = 16) # Overall trend not significant for all sexes together
      # abline(lm(prov[,Variable[M]] ~ prov$moose_M)) 
      # summary(lm(prov[,Variable[M]] ~ prov$moose_M))
      # 
      
      # ==== 1.1 SEASON ====
      Variable.axis <- c("Beta distance humans","Beta distance all humans")
      
      # ==== 1.1.1 PC1 ====
      # setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
      # pdf(file=paste("Males_SeasonPC1", files[FF], Variable[M],".pdf",sep="_"))
      # 
      # # plot points
      # col_season <- c("red","blue")[as.factor(prov$Season_M)]
      # plot(prov$PC_M, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC1", ylab=Variable.axis[M])
      # 
      # # subset season to plot it
      # prov_s_M <- prov[which(prov$Season_M == "W"), ] # Create 1 dataset for each season
      # prov_w_M <- prov[which(prov$Season_M == "S"), ]
      # 
      # # plot lines
      # abline(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC_M), col = "red") #Summer
      # summary(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC_M))
      # 
      # abline(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC_M), col = "blue") #Winter
      # summary(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC_M))
      # #ADD LEGEND 
      # legend("topright", col=c("blue","red"), pch=16, legend = c("winter","summer"))
      # dev.off()
      # 
      # ==== 1.1.2 PC2 ====
      # setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
      # pdf(file=paste("Males_SeasonPC2", files[FF], Variable[M],".pdf",sep="_"))
      # 
      # # plot points
      # col_season <- c("red","blue")[as.factor(prov$Season_M)]
      # plot(prov$PC2_M, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC2", ylab=Variable.axis[M])
      # 
      # # subset season to plot it
      # prov_s_M <- prov[which(prov$Season_M == "S"), ] # Create 1 dataset for each season
      # prov_w_M <- prov[which(prov$Season_M == "W"), ]
      # 
      # # plot lines
      # abline(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC2_M), col = "red") #Summer
      # summary(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC2_M))
      # 
      # abline(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC2_M), col = "blue") #Winter
      # summary(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC2_M))
      # #ADD LEGEND 
      # legend("bottomleft", col=c("blue","red"), pch=16, legend = c("winter","summer"))
      # dev.off()
      
      
      # ==== 2. FEMALES ====
      
      # plot(prov[,Variable[M]] ~ prov$PC_F, pch = 16) # Overall trend not significant for all sexes together
      # abline(lm(prov[,Variable[M]] ~ prov$PC_F)) 
      # summary(lm(prov[,Variable[M]] ~ prov$PC_F))
      # 
      # # ==== 2.1 SEASON ====
      # Variable.axis <- c("Beta distance humans","Beta distance all humans")
      # 
      # # ==== 2.1.1 PC1 ====
      # setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
      # pdf(file=paste("Females_SeasonPC1", files[FF], Variable[M],".pdf",sep="_"))
      # 
      # # plot points
      # col_season <- c("red","blue")[as.numeric(as.factor(prov$Season_M))]
      # plot(prov$PC_F, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC1", ylab=Variable.axis[M])
      # 
      # # subset season to plot it
      # prov_w_F <- prov[which(prov$Season_F == "W"), ] # Create 1 dataset for each season
      # prov_s_F <- prov[which(prov$Season_F == "S"), ]
      # 
      # # plot lines
      # abline(lm(prov_s_F[,Variable[M]] ~ prov_s_F$PC_F), col = "red") #Summer
      # summary(lm(prov_s_F[,Variable[M]] ~ prov_s_F$PC_F))
      # 
      # abline(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC_F), col = "blue") #Winter
      # summary(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC_F))
      # #ADD LEGEND 
      # legend("bottomright", col=c("blue","red"), pch=16, legend = c("winter","summer"))
      # dev.off()
      # 
      # ### NOT SEASON
      # pdf(file=paste("Females_PC1", files[FF], Variable[M],".pdf",sep="_"))
      # # plot points
      # plot(prov$PC_F, prov[,Variable[M]], pch = 16, xlab = "PC1", ylab=Variable.axis[M])
      # # plot lines
      # abline(lm(prov[,Variable[M]] ~ prov$PC_F), col = "black") #Summer
      # dev.off()
      # 
      # 
      # # ==== 2.1.2 PC2 ====
      # setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
      # pdf(file=paste("Females_SeasonPC2", files[FF], Variable[M],".pdf",sep="_"))
      # 
      # # plot points
      # col_season <- c("red","blue")[as.factor(prov$Season_M)]
      # plot(prov$PC2_F, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC2", ylab=Variable.axis[M])
      # 
      # # subset season to plot it
      # prov_s_F <- prov[which(prov$Season_F == "S"), ] # Create 1 dataset for each season
      # prov_w_F <- prov[which(prov$Season_F == "W"), ]
      # 
      # # plot lines
      # abline(lm(prov_s_F[,Variable[M]] ~ prov_s_M$PC2_F), col = "red") #Summer
      # summary(lm(prov_s_F[,Variable[M]] ~ prov_s_M$PC2_F))
      # 
      # abline(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC2_F), col = "blue") #Winter
      # summary(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC2_F))
      # #ADD LEGEND 
      # legend("topleft", col=c("blue","red"), pch=16, legend = c("winter","summer"))
      # dev.off()
      
      # ---- IV. TABLES AND MODELS ----
      # ==== 1. PC1
      # ==== 1.1 AIC TABLE  ====
      
      # RUN MODELS 
      mods <- list()
      mods[["m_null"]] <- summary(lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov,REML = F))
      mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_F + (1|ID_pair_F), data = prov, REML = F))
      mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_F*Season_F + (1|ID_pair_F), data = prov, REML = F))
      mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M + (1|ID_pair_F), data = prov, REML = F))
      mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M*Season_M + (1|ID_pair_F), data = prov, REML = F))
      mods[["m_5"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M+PC_F + (1|ID_pair_F), data = prov, REML = F))
      mods[["m_6"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M*PC_F + (1|ID_pair_F), data = prov, REML = F))
      
      
      
      aictab <- matrix(NA, nrow=length(mods), ncol= 4 )
      colnames(aictab) <- c("AIC", "LL", "df", "deltaAIC" )
      row.names(aictab) <- c("Null", "Natal_F", "Natal_F * Season",
                             "Natal_M", "Natal_M * Season","Natal_M+Natal_F", "Natal_M*Natal_F" )
      for(mo in 1:length(mods)){
        aictab[mo,1] <- mods[[mo]]$AIC[1] # 
        aictab[mo,2] <- mods[[mo]]$AIC[3] # LL
        aictab[mo,3] <- mods[[mo]]$AIC[5]# df
      }
      aictab[,4] <- aictab[,1]-min(aictab[,1])
      
      VARIABLE
      aictab <- aictab[order(aictab[,4]),]
      
      AIC_TAB[[HR[FF]]][[VARIABLE[[M]]]] <- aictab
      
      
      # ==== 1.2 COEFS TABLE  ====
      mods <- list()
      mods[["m_null"]] <- summary(lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_F + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_F*Season_F + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M*Season_M + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_5"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M + PC_F + (1|ID_pair_F), data = prov, REML = T))
      mods[["m_6"]] <- summary(lmer(prov[,Variable[M]] ~ moose_M + PC_M * PC_F + (1|ID_pair_F), data = prov, REML = T))
      
      modSS <- list()
      modSS[["m_null"]] <- lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T)
      modSS[["m_1"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_F + (1|ID_pair_F), data = prov, REML = T)
      modSS[["m_2"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_F*Season_F + (1|ID_pair_F), data = prov, REML = T)
      modSS[["m_3"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_M + (1|ID_pair_F), data = prov, REML = T)
      modSS[["m_4"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_M*Season_M + (1|ID_pair_F), data = prov, REML = T)
      modSS[["m_5"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_M + PC_F +(1|ID_pair_F), data = prov, REML = T)
      modSS[["m_6"]] <- lmer(prov[,Variable[M]] ~ moose_M + PC_M * PC_F + (1|ID_pair_F), data = prov, REML = T)
      
      
      mod.names <- c("Null", "Natal_F", "Natal_F * Season",
                     "Natal_M", "Natal_M * Season","Natal_M+Natal_F", "Natal_M*Natal_F" )
      coeffs_list <- list()
      for(mo in 1:length(mods)){
        
        coeffstab <- matrix(NA, nrow=nrow(mods[[mo]]$coefficients)+1, ncol= 4 )
        colnames(coeffstab) <- c("Beta", "SE", "CI Low", "CI high" )
        row.names(coeffstab) <- c(row.names(mods[[mo]]$coefficients), mod.names[mo])
        
        coeffstab[1:nrow(mods[[mo]]$coefficients),1:2] <-  mods[[mo]]$coefficients[,1:2] # 
        if(mo==1){coeffstab[1:nrow(mods[[mo]]$coefficients),3:4]  <- confint(modSS[[mo]])[1,]}else{
          coeffstab[1:nrow(mods[[mo]]$coefficients),3:4]  <- confint(modSS[[mo]])[3:nrow(confint(modSS[[mo]])),] 
        }
        coeffs_list[[mo]] <- round(coeffstab, digit=2)
      }
      
      coeftab <- do.call(rbind, coeffs_list)
      COEFF_TAB[[HR[FF]]][[VARIABLE[[M]]]] <- coeffs_list
      
    }
    
    
    
  }
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/OutFiles/")
  save(COEFF_TAB, file=paste("PC1_COEFF", loop,".RData",sep="_"))
  save(AIC_TAB, file=paste("PC1_AIC", loop,".RData",sep="_"))
  
  filesList <- list.files("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/New/InFiles/")
  
  print(paste("loop NR", loop))
}#loop

#### PROCESS RESULTS 

loop=1
 mod.names <- c("Null", "Natal_F", "Natal_M+Natal_F","Natal_M","Natal_F * Season",
  "Natal_M*Natal_F",  "Natal_M * Season")
 
rank.MCP.not.moved.closest  <- rank.MCP.not.moved.closest2 <-   array(NA, c(nrow=100, ncol=length(mod.names),2)) 
rank.KERN.not.moved.closest2 <- rank.KERN.not.moved.closest <-  array(NA, c(nrow=100, ncol=length(mod.names),2)) 
rank.MCP.moved.closest  <- rank.MCP.moved.closest2 <-  array(NA, c(nrow=100, ncol=length(mod.names),2)) 
rank.KERN.moved.closest2 <- rank.KERN.moved.closest <- array(NA, c(nrow=100, ncol=length(mod.names),2)) 

  
dimnames(rank.KERN.moved.closest2)[[2]] <- dimnames(rank.MCP.not.moved.closest2)[[2]] <- mod.names
dimnames(rank.MCP.moved.closest)[[2]] <- dimnames(rank.KERN.not.moved.closest)[[2]] <- mod.names
dimnames(rank.KERN.not.moved.closest2)[[2]] <- dimnames(rank.MCP.moved.closest2)[[2]] <- mod.names
dimnames(rank.MCP.not.moved.closest)[[2]] <- dimnames(rank.KERN.moved.closest)[[2]] <- mod.names


id <- c(1:82,84:100)
for(loop in id){
load(paste("PC1_COEFF", loop,".RData",sep="_"))
load(paste("PC1_AIC", loop,".RData",sep="_"))
rank.MCP.not.moved.closest[loop,,1] <-   match(mod.names , row.names(AIC_TAB$MCP.not.moved$closest))
rank.MCP.not.moved.closest2[loop,,1] <-   match(mod.names , row.names(AIC_TAB$MCP.not.moved$closest2))
rank.KERN.not.moved.closest[loop,,1] <-   match(mod.names ,row.names(AIC_TAB$KERN.not.moved$closest))
rank.KERN.not.moved.closest2[loop,,1] <-  match(mod.names , row.names(AIC_TAB$KERN.not.moved$closest2))

rank.MCP.moved.closest[loop,,1] <-   match(mod.names , row.names(AIC_TAB$MCP.moved$closest))
rank.MCP.moved.closest2[loop,,1] <-  match(mod.names , row.names(AIC_TAB$MCP.moved$closest2))
rank.KERN.moved.closest[loop,,1] <-  match(mod.names , row.names(AIC_TAB$KERN.moved$closest))
rank.KERN.moved.closest2[loop,,1] <- match(mod.names , row.names(AIC_TAB$KERN.moved$closest2))


rank.MCP.not.moved.closest[loop,,2] <-   AIC_TAB$MCP.not.moved$closest[rank.MCP.not.moved.closest[loop,,1] ,4]
rank.MCP.not.moved.closest2[loop,,2] <-   AIC_TAB$MCP.not.moved$closest[rank.MCP.not.moved.closest2[loop,,1] ,4]
rank.KERN.not.moved.closest[loop,,2] <-   AIC_TAB$MCP.not.moved$closest[rank.KERN.not.moved.closest[loop,,1] ,4]
rank.KERN.not.moved.closest2[loop,,2] <-  AIC_TAB$MCP.not.moved$closest[rank.KERN.not.moved.closest2[loop,,1] ,4]

rank.MCP.moved.closest[loop,,2] <-   AIC_TAB$MCP.not.moved$closest[rank.MCP.moved.closest[loop,,1] ,4]
rank.MCP.moved.closest2[loop,,2] <-  AIC_TAB$MCP.not.moved$closest[rank.MCP.moved.closest2[loop,,1] ,4]
rank.KERN.moved.closest[loop,,2] <-  AIC_TAB$MCP.not.moved$closest[rank.KERN.moved.closest[loop,,1] ,4]
rank.KERN.moved.closest2[loop,,2] <- AIC_TAB$MCP.not.moved$closest[rank.KERN.moved.closest2[loop,,1] ,4]
}


#########
# Calculate the ranking 
ranking.KERN.moved.closest2 <- ranking.KERN.moved.closest <-  matrix(0, ncol=length(mod.names), nrow=length(mod.names) )
ranking.MCP.moved.closest2 <- ranking.MCP.moved.closest <-  matrix(0, ncol=length(mod.names), nrow=length(mod.names) )

ranking.KERN.not.moved.closest2 <- ranking.KERN.not.moved.closest<-  matrix(0, ncol=length(mod.names), nrow=length(mod.names) )
ranking.MCP.not.moved.closest2 <- ranking.MCP.not.moved.closest <-  matrix(0, ncol=length(mod.names), nrow=length(mod.names) )

dimnames(ranking.KERN.moved.closest2)[[1]] <- dimnames(ranking.MCP.not.moved.closest2)[[1]] <- mod.names
dimnames(ranking.MCP.moved.closest)[[1]] <- dimnames(ranking.KERN.not.moved.closest)[[1]] <- mod.names
dimnames(ranking.KERN.not.moved.closest2)[[1]] <- dimnames(ranking.MCP.moved.closest2)[[1]] <- mod.names
dimnames(ranking.MCP.not.moved.closest)[[1]] <- dimnames(ranking.KERN.moved.closest)[[1]] <- mod.names

dimnames(ranking.KERN.moved.closest2)[[2]] <- dimnames(ranking.MCP.not.moved.closest2)[[2]] <- mod.names
dimnames(ranking.MCP.moved.closest)[[2]] <- dimnames(ranking.KERN.not.moved.closest)[[2]] <- mod.names
dimnames(ranking.KERN.not.moved.closest2)[[2]] <- dimnames(ranking.MCP.moved.closest2)[[2]] <- mod.names
dimnames(ranking.MCP.not.moved.closest)[[2]] <- dimnames(ranking.KERN.moved.closest)[[2]] <- mod.names

for(i in 1:length(mod.names)){
# MOVED
tb <- table(rank.MCP.moved.closest[,i,1])/sum(table(rank.MCP.moved.closest[,i,1]))
ranking.MCP.moved.closest[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.MCP.moved.closest2[,i,1])/sum(table(rank.MCP.moved.closest2[,i,1]))
ranking.MCP.moved.closest2[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.KERN.moved.closest[,i,1])/sum(table(rank.KERN.moved.closest[,i,1]))
ranking.KERN.moved.closest[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.KERN.moved.closest2[,i,1])/sum(table(rank.KERN.moved.closest2[,i,1]))
ranking.KERN.moved.closest2[i,as.numeric(names(tb))] <- round(tb,digits=2)

# NOT MOVED
tb <- table(rank.MCP.not.moved.closest[,i,1])/sum(table(rank.MCP.not.moved.closest[,i,1]))
ranking.MCP.not.moved.closest[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.MCP.not.moved.closest2[,i,1])/sum(table(rank.MCP.not.moved.closest2[,i,1]))
ranking.MCP.not.moved.closest2[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.KERN.not.moved.closest[,i,1])/sum(table(rank.KERN.not.moved.closest[,i,1]))
ranking.KERN.not.moved.closest[i,as.numeric(names(tb))] <- round(tb,digits=2)

tb <- table(rank.KERN.not.moved.closest2[,i,1])/sum(table(rank.KERN.not.moved.closest2[,i,1]))
ranking.KERN.not.moved.closest2[i,as.numeric(names(tb))] <- round(tb,digits=2)
}



