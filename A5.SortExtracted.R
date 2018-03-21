

rm(list=ls())
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/Multivariate")
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")

#Join extracted values for established and natal territories with
#sex, year and coordinates
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
    #SAVE NATAL_ESTABLISHED IN ORDER FOR ANALYSIS NEW APPROACH
    #ny <- arrange(ne, ID, Category)
    #setwd("C:/Users/ana.sanz/Documents/MASTER THESIS/Data")
    #write.csv(ny,file="extracted_values_NatEst")
ne<-arrange(ne,ID_individual)




#List of available extracted values converted to matrix.
load("available_values_final.RData")

hey <- g[[1]][[1]]
hey<- as.data.frame((hey))
hey[1,] <- rep(NA, ncol(hey))
hey$ID <- 0
hey$ID_rand <- 0

row.names(hey)<- 1

for ( i in 1:length(g)){
  
  for ( j in 1:length(g[[i]])){
    tmp <- as.data.frame(g[[i]][[j]])[1,]
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
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("points.RData")
#Points: List with the points created for the available territories.
#Transform the spatial points of the list on a data frame.
#Then, this data frame will be joined to the one with the extracted values
#of the available territories

yey <- points[[1]][[1]]
yey<- as.data.frame((yey))
yey[1,] <- rep(NA, ncol(yey))
yey$ID <- 0
yey$ID_rand <- 0

row.names(yey)<- 1

for ( i in 1:length(g)){
  
  for ( j in 1:length(points[[i]])){
    tmp <- as.data.frame(points[[i]][[j]])[1,]
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

#Add IDs available and year to the matrix
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("Short_ID.RData")
load("medlong_ID.RData")

ord_av<-bind_rows(short,ml)
library(splitstackshape)
o<-expandRows(ord_av,11,count.is.col = FALSE)

hey$ID_individual<-o$ID
hey$Year<-o$Year.establishment
hey$Sex<-o$MALES

#Join extracted natal, established and available (and order them by ID)
values <- bind_rows(ne,hey)
?arrange
values<- arrange(values,ID_individual,ID_rand,Category)
values<-values[,-c(1)]

values<-values[,c(32,1,37,33,34,35,36,31,2:30)]

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
write.csv(values,file="extracted_values")

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="extracted_values",header=TRUE)

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
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
write.csv(e,file="extracted_values")

#Since from the available points, ID 34 ID_rand 3 ID G309 generates
#NA, I created another point in  script7 and extracted values 
#again in 8. Now I will substitute the observation
rm(list=ls())
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
e<-read.csv(file="extracted_values",header=TRUE)

e[1589, ]$x<- 374803.2
e[1589, ]$y<- 6412572
e[1589, ]$human_1 <- 70.85308
e[1589, ]$humanlands_1 <- 0.05666588
e[1589, ]$agri_1 <- 0.128913
e[1589, ]$forest_1 <- 0.7334658
e[1589, ]$mires_1 <- 0.03195783
e[1589, ]$water_1 <- 0.04886811
e[1589, ]$mountains_1 <- 0
e[1589, ]$roadens_sec1 <- 1.396675
e[1589, ]$mainroad_1 <- 0.4908958
e[1589, ]$bear_1 <- 0.0003171177
e[1589, ]$dem_1 <- 200.1468
e[1589, ]$slope_1 <- 2.067674
e[1589, ]$roughness_1 <- 20.96051
e[1589, ]$roadbuild_1 <- 150.7421
e[1589, ]$build_1 <- 2.474432
e[1589, ]$moose_dens <- 0.2607319

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
write.csv(e,file="extracted_values")

