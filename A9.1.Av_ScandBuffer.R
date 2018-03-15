


library(raster)
library(sp)
library("rgdal", lib.loc="~/R/win-library/3.1")
library(dplyr)
library(tidyr)

#Data wolves natal territories
e<-read.csv("Data_NHBD_id_wolf_density.csv")
e <- e[e$Category == "Natal", c(3,6:9)]

# Data buffers occupied
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("Buffer.RData") #b
proj4string(b)

#Study area
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS")
study_area <- readOGR(".", "hand_study_area1")
study_area <- study_area[which(study_area$KUSTX_ %in% c(35)),]##keep only sweden and norway  and small lakes.
plot(study_area)
proj4string(b) <- CRS(proj4string(study_area))

g<-list()
for (j in 1:271){
  o<- list()
  for (i in 1:11){
    repeat{
      rdm_sp <- spsample(study_area, 1,type="random") # sample 1 random point in line
      ovr <- is.na(over(rdm_sp, b[b$year==(e$Year[j]-1), 1])) 
      #1. Select buffers of the year before the year of establishment (dispering)
      #2. Over: NA -> Doesnt overlap
      if (ovr==TRUE) break
    }
    o[[i]] <- rdm_sp
  }
  print(j)
  g[[j]]<-o
}

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
save(g,file = "available_points_studyarea.RData")

plot(study_area)
plot(b[b$year==(e$Year[7]-1), 1],add = T)
plot(g[[7]][[10]], add = T)


# Extract values AVAILABLE
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("stack.RData")
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
load ("available_points_studyarea.RData")

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

save(u,file = "available_study_area.RData")

#Sort extracted values available

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
load("available_study_area.RData")

e<-read.csv("Data_NHBD_id_wolf_density.csv")
e <- e[e$Category == "Natal", c(4,7:11)]

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

#Add point coordinates (of available territories) to the matrix of
#extracted values (hey).
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
load("available_points_studyarea.RData")
#Points: List with the points created for the available territories.
#Transform the spatial points of the list on a data frame.
#Then, this data frame will be joined to the one with the extracted values
#of the available territories

yey <- g[[1]][[1]]
yey<- as.data.frame((yey))
yey[1,] <- rep(NA, ncol(yey))
yey$ID <- 0
yey$ID_rand <- 0

row.names(yey)<- 1

for ( i in 1:length(u)){
  
  for ( j in 1:length(g[[i]])){
    tmp <- as.data.frame(g[[i]][[j]])[1,]
    row.names(tmp)  <- as.numeric(row.names(yey)[nrow(yey)])+1
    yey[(nrow(yey)+1) , 1: (ncol(yey)-2) ]  <- tmp
    
    yey$ID[(nrow(yey))] <- i
    yey$ID_rand[(nrow(yey))] <- j
  }
}
head(yey)
yey<-yey[-c(1),]

#Add yey (coordinates) to hey (extracted values)
hey<-bind_cols(hey,yey)

library(splitstackshape)
o<-expandRows(e,11,count.is.col = FALSE)

hey$ID_individual<-o$ID_individual
hey$Year<-o$Year
hey$Sex<-o$Sex


#Join extracted values for established and natal territories with
#sex, year and coordinates
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")
#NATAL:
n<-read.csv(file="natal_values_final",header=TRUE)
Category <- vector(mode='numeric', length=271)
n <- data.frame(n, Category)
n$Category<-factor(n$Category,labels = "Natal")
n$ID_individual<-disper$ID
n$Year<-disper$Year.establishment
n$Sex<-disper$MALES
n$x<-disper$X_birth
n$y<-disper$Y_birth
#ESTABLISHED:
e<-read.csv(file="established_values_final",header=TRUE)
Category <- vector(mode='numeric', length=271)
e <- data.frame(e, Category)
e$Category<-factor(e$Category,labels = "Established")
e$ID_individual<-disper$ID
e$Year<-disper$Year.establishment
e$Sex<-disper$MALES
e$x<-disper$X_Established
e$y<-disper$Y_Established

#Join natal and established
library(dplyr)
ne<-bind_rows(n,e)
ne<-arrange(ne,ID_individual)

values <- bind_rows(ne,hey)
?arrange
values<- arrange(values,ID_individual,ID_rand,Category)
values<-values[,-c(1)]

values<-values[,c(32,1,37,33,34,35,36,31,2:30)]

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
write.csv(values,file="extracted_values_study_area")

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
e<-read.csv(file="extracted_values_study_area",header=TRUE)

#Make a column with all moose densities by year
moose<-e[ ,c(10:23)]
length((moose))

e$moose_dens<-0

YEAR <- c(1998:2011)

for (i in 1:3523){
  
  e$moose_dens[i]<- moose[i, which(YEAR==e$Year[i])]
}

View(e)
e<-e[,-c(1,10:23)]

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")
write.csv(e,file="extracted_values_study_area")
