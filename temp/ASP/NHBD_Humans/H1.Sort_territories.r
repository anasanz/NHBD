

rm(list=ls())
library(dplyr)

# Sort data NHBD-HUMANS ----

setwd("~/Norway/NHBD_humans")
p <- read.csv("Pred_studies.csv", sep = ";")

  # 1. --- Track partner ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
m <- read.csv("natal_established_territories.csv", sep = ";")

#For each established territory, one column for each sex (ID_M/ID_F)
p$ID_M <- NA
p$ID_F <- NA
p$ID_individual <- as.character(p$ID_individual)

for (i in 1:nrow(p)){ 
  if (p$Sex[i] == "M") {p$ID_M[i] = p$ID_individual[i]} 
  else {p$ID_F[i] = p$ID_individual[i]}
}

colnames(p)[1] <- "ID_individual_GPS" # To know which individual of the pair was the GPS_collared
colnames(p)[2] <- "Sex_GPS" 

#Find ID mate: The ones missing are from 2012
m$ID_M <- as.character(m$ID_M)
m$ID_F <- as.character(m$ID_F)

for (i in 1:nrow(p)) 
  if (!is.na(p$ID_M[i])) {
      p$ID_F[i] <- m$ID_F[which(p$ID_M[i] == m$ID_M)][1]
  } else {
    p$ID_M[i] <- m$ID_M[which(p$ID_F[i] == m$ID_F)][1]
}


  # 2. ---- Get birth territory ----

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
d <- read.delim("dispersal_p.txt")

# Join info males
d_m <- d[which(d$MALES == "M"), c(1:9)]
colnames(d_m)[c(2:9)] <- c("ID_M","Birth_territory_M", "Y_birth_M",  "X_birth_M", "Established_territory_M", "Y_Established_M",
                         "X_Established_M", "Year.establishment_M")
j <- left_join(p,d_m, by = "ID_M")

# Join info females
d_f <- d[which(d$MALES == "F"), c(1:9)]
colnames(d_f)[c(2:9)] <- c("ID_F","Birth_territory_F", "Y_birth_F",  "X_birth_F", "Established_territory_F", "Y_Established_F",
                         "X_Established_F", "Year.establishment_F")
j <- left_join(j,d_f, by = "ID_F")
j <- j[which(complete.cases(j[,c(11:25)])), ]
write.csv(j,"data_pairs_human.csv")


# Find missing IDs
g <- read.csv("GPSdata180207.csv", sep = ";")
aspa_2015 <- g[which(g$Marking_territory == "Aspafallet" ) & which(g$UTC_date ==), ]
unique(aspa$Wolf_ID)
unique(g$UTC_date)
