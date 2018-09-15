library(sp)
library(rgdal)
library(rgeos)
# ==== I. LOAD DATA ====
#setwd("~/Norway/NHBD_humans/Antonio")
rm(list=ls())
# ==== 1. AIMEE CYRIL DATA ====

setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
gps1 <- read.csv("Final_GPS_Data_Aimee.csv", header = TRUE, sep = ",")
gps2 <- read.csv("Final_GPS_Data_Cyril.csv", header = TRUE, sep = ",")

# convert time 
gps1$Date_time <- as.POSIXct(paste(gps1$Date,gps1$Time), format = "%m/%d/%Y %H:%M",tz = "GMT") 
gps2$Date_time <- as.POSIXct(paste(gps2$DATE, gps2$UTC_TIME), format = "%m/%d/%Y %H:%M",tz = "GMT")


## merge 
colnames(gps1)
colnames(gps2)

colnames(gps2)[5:6] <- c("X","Y")
gps <- rbind(gps1[,c("X","Y","Date_time","study","Study_Year","Study_Start", "Study_End")],
      gps2[,c("X","Y","Date_time","study","Study_Year","Study_Start", "Study_End")]
      )
# delete na coords
gps<- gps[!is.na(gps$X),]
cooords <- data.frame(gps$X, gps$Y)
coordinates(cooords) <- cooords
proj4string(cooords) <-  CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")
cooords <- spTransform(cooords, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# points(coords)

gps$X <- coordinates(cooords)[,1]
gps$Y <- coordinates(cooords)[,2]

# ==== 2. NORWEGIAN DATA  ====

gps3 <- read.csv("Copy of Norwegian wolf data 2015 for Cyril.csv", header = TRUE, sep = ",")
gps3$Wolf_ID_doublecheck <- as.character(gps3$Wolf_ID_doublecheck)

# just keep one ID (the one with hourly gps lcoations
gps3 <- gps3[gps3$Wolf_ID_doublecheck %in% "M1409" ,]
# check date_time
gps3$Date_time <- as.POSIXct(paste(gps3$UTC_date, gps3$UTC_time), format = "%m/%d/%Y %H:%M:%S",tz = "GMT")  
## keep hourly positions 
gps3 <- gps3[420:1329,]
diff(gps3$Date_time)

#update the coordinates 
coords <- data.frame(gps3$Longitude,gps3$Latitude)
coordinates(coords) <- coords
proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##
scan <- readOGR("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/Scandinavia_border_33N.shp")
# scan <- spTransform(scan, CRS("+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs "))


# proj4string(scan) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

plot(scan)
coords <- spTransform(coords, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# points(coords)

gps3$X <- coordinates(coords)[,1]
gps3$Y <- coordinates(coords)[,2]
#update fields 
gps3$study <- gps3$Wolf_ID
gps3$Study_Year <- paste(gps3$Wolf_ID, format(gps3$Date_time,"%Y"), sep="_")
gps3$Study_Start <- range(format(gps3$Date_time,"%d-%m-%Y"))[1]
gps3$Study_End <- range(format(gps3$Date_time,"%d-%m-%Y"))[1]

#join GPS
gps <- rbind(gps,gps3[,c("X","Y","Date_time","study","Study_Year","Study_Start", "Study_End")])
points(gps$Y~gps$X, col="red", pch=16, cex=0.1)
points(gps3$Y~gps3$X, col="black", pch=16, cex=0.1)

# ==== 3. SWEDISH DATA  ====
# ==== 3.1. ASPAFALLET  ====
gps5 <- read.csv("GPS_Collar15766.csv", header = TRUE, sep = ",")# Aspafallet - 15766 (female, 15-01),captured 2015-01-27
gps5$Date_time <- as.POSIXct(paste(gps5$UTC_DATE, gps5$UTC_TIME), format = "%m/%d/%Y %H:%M:%S",tz = "GMT")  
#date capture
gps5 <- gps5[gps5$Date_time >  as.POSIXct("2015-01-31", format = "%Y-%m-%d",tz = "GMT"),]

gps4 <- read.csv("GSM15762.csv", header = TRUE, sep = ",")# Aspafallet - 15762 (male, 15-02), captured 2015-01-27
gps4$Date_time <- as.POSIXct(paste(gps4$UTC_DATE, gps4$UTC_TIME), format = "%m/%d/%Y %H:%M:%S",tz = "GMT")  
gps4 <- gps4[gps4$Date_time >  as.POSIXct("2015-01-31", format = "%Y-%m-%d",tz = "GMT"),]

# keep the gps 4 because having high interval but remove na coords
gps4 <- gps4[!is.na(gps4$LATITUDE),] 

coords <- data.frame(gps4$LONGITUDE,gps4$LATITUDE)
coordinates(coords) <- coords
proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


plot(scan)
coords <- spTransform(coords, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(coords)
# points(coords)

gps4$X <- coordinates(coords)[,1]
gps4$Y <- coordinates(coords)[,2]
#update fields 
gps4$study <- "Aspafallet"
gps4$Study_Year <- paste("Aspafallet", format(gps4$Date_time,"%Y"), sep="_")
gps4$Study_Start <- range(format(gps4$Date_time,"%d-%m-%Y"))[1]
gps4$Study_End <- range(format(gps4$Date_time,"%d-%m-%Y"))[1]

#join GPS
gps <- rbind(gps,gps4[,c("X","Y","Date_time","study","Study_Year","Study_Start", "Study_End")])
plot(scan)
points(gps$Y~gps$X, col="red", pch=16, cex=0.1)

# ==== 3.2. KUKUMAKI ==== 
gps6 <- read.csv("GSM15761.csv", header = TRUE, sep = ",")# 15761 (female, 13-01), captured 2015-01-29
gps6$Date_time <- as.POSIXct(paste(gps6$UTC_DATE, gps6$UTC_TIME), format = "%m/%d/%Y %H:%M:%S",tz = "GMT")  
#date capture
gps6 <- gps6[gps6$Date_time >  as.POSIXct("2015-02-03", format = "%Y-%m-%d",tz = "GMT"),]
gps6 <- gps6[which(diff(gps6$Date_time)<70),]
range(gps6$Date_time)
# keep the gps 4 because having high interval but remove na coords
gps6 <- gps6[!is.na(gps6$LATITUDE),] 

coords <- data.frame(gps6$LONGITUDE, gps6$LATITUDE)
coordinates(coords) <- coords
proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


coords <- spTransform(coords, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(coords)
# points(coords)
gps6$X <- coordinates(coords)[,1]
gps6$Y <- coordinates(coords)[,2]
#update fields 
gps6$study <- "Kukumaki"
gps6$Study_Year <- paste("Kukumaki", format(gps6$Date_time,"%Y"), sep="_")
gps6$Study_Start <- range(format(gps6$Date_time, "%d-%m-%Y"))[1]
gps6$Study_End <- range(format(gps6$Date_time, "%d-%m-%Y"))[1]

#join GPS
gps <- rbind(gps, gps6[,c("X", "Y", "Date_time", "study", "Study_Year", "Study_Start", "Study_End")])
plot(scan)
points(gps$Y~gps$X, col="red", pch=16, cex=0.1)

# ==== 4. SEASON  ====
gps$Month <- as.numeric(format(gps$Date_time, "%m"))
gps$Year <- as.numeric(format(gps$Date_time, "%Y"))

gps$Season <- "W"
gps$Season[gps$Month %in% c(5:7)] <- "S"
gps <- gps[-which(gps$Month %in% c(8:11)), ] 

gps$Study_year <- unlist(lapply(strsplit(as.character(gps$study), '_'),function(x) x[1]))
gps$Study_year <- paste(gps$Study_year, gps$Year, gps$Season ,sep="_" )

# ==== 5. REMOVE DUPLICATES  ====
#REMOVE GPS LOCATIONS HAVING SIMILAR x y and date time  
length1 <- tapply(gps$Study_year, gps$Study_year,length)
gps <- gps[!duplicated(paste(gps[,c("X")], gps[,c("Y")], gps[,c("Date_time")])),]

# ==== 6. IDENTIFY MOVING GPS LOCATIONS  ====
#buffer size 
BufferWidth <- 100
speed <- 500 # (200m per hour)
moving_used <- list()

#
gps$unique.id <- 1:nrow(gps)
gps$move <- 0

ID <- unique(gps$Study_year)

for (i in 1:length(ID)){
  tmp <- gps[gps$Study_year==ID[i], ]
  tmp <- tmp[order(tmp$Date_time),]
  
  # get X and Y coordinates to caclulate step length
  X <- tmp$X[1:(nrow(tmp)-1)]
  Y <- tmp$Y[1:(nrow(tmp)-1)]
  X1 <- tmp$X[2:(nrow(tmp))]
  Y1 <- tmp$Y[2:(nrow(tmp))]
  
  # get distance and time 
  dist <- sqrt((X-X1)^2 + (Y-Y1)^2)
  time <- diff(tmp$Date_time)
  
  # identify moving locations
  tmp$speed_m_H[2:nrow(tmp)] <- dist/as.numeric(time, units="hours")
  id.move <- tmp$unique.id[tmp$speed_m_H>speed]
  gps$move[gps$unique.id %in% id.move] <- 1
  
  coordinates(tmp) <- data.frame(tmp$X, tmp$Y)
  #create buffer
  buffer <- gBuffer(tmp, width = BufferWidth, byid =F)
  buffer <- disaggregate(buffer)
  
  # plot check
  plot(buffer)  
  points(tmp[tmp$unique.id%in%id.move, ], col="red")
  points(tmp[which(!tmp$unique.id %in% id.move), ], col="blue")
  
  }  



# During the late-winter period (1 March - 30 April) male bears start to
# The spring period (1 May - 30 June)  
# ==== II. MAKE A SUMMARY====
ID <- unique(gps$Study_year)
date.summary <- matrix(NA, nrow=length(ID), ncol=9)
# check the time of the predation study
# d$date <- as.POSIXct(strptime(d$date, "%d/%m/%Y"))
for (i in 1:length(ID)){
  fc <- gps[which(gps$Study_year == ID[i]), ]
  date.summary[i,1] <- as.character(ID[i])
  date.summary[i,2] <- as.character(as.Date(min(fc$Date_time)))
  date.summary[i,3] <- as.character(as.Date(max(fc$Date_time)))
  date.summary[i,4] <- as.character(diff(range(fc$Date_time)))
  date.summary[i,5] <- length(fc$Date_time)
  date.summary[i,6] <- sum(fc$move==1)
  date.summary[i,7] <- NA
  date.summary[i,8] <- as.character(fc$Study_Start[1])
  date.summary[i,9] <- as.character(fc$Study_End[1])
}

colnames(date.summary) <- c("Territory_year", "Start", "End","Range","Nblocations", "nblocsmoving", "Season","Start", "End" )

#  
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data/new")
write.csv(gps, file="gps.dataCM.csv")

gps[which(gps$Study_year == "Glaskogen_2002_S"), ]
