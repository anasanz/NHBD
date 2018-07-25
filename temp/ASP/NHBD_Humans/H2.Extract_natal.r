
library(dplyr)
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/GIS/Clips")
load("stack.RData") # Load layers

setwd("~/Norway/NHBD_humans")
d <- read.csv("data_pairs_human.csv", header = TRUE) #Load territory coordinates

# Extract female birth territory values
disper<-d[ ,c("X_birth_F","Y_birth_F")] 
natal_F <- extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
natal_FID <- bind_cols(d[ , which(colnames(d) %in% c("ID_F", "Sex_GPS", "Territory_antonio"))], natal_F) #Join IDs and whether if was the female
                                                                                    #the one with the GPS collar
colnames(natal_FID)[4:33] <- paste("F",colnames(natal_FID)[4:33], sep = "_")
natal_FID <- natal_FID[ ,which(colnames(natal_FID) %in% c("Territory_antonio","Sex_GPS","ID_F", "F_human_1", "F_humanlands_1", "F_agri_1", "F_forest_1",
                                                        "F_roadens_sec1", "F_mainroad_1", "F_bear_1", "F_roadbuild_1",
                                                        "F_build_1"))] # Select human & interesting variables

# Extract male birth territory values
disper<-d[ ,c("X_birth_M","Y_birth_M")] 
natal_M <- extract(stack,disper,method='simple',buffer=17841,small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,sp=TRUE)
natal_MID <- bind_cols(d[ , which(colnames(d) %in% c("ID_M", "Sex_GPS"))], natal_M)
colnames(natal_MID)[4:32] <- paste("M",colnames(natal_MID)[4:32], sep = "_")
natal_MID <- natal_MID[ ,which(colnames(natal_MID) %in% c("ID_M", "M_human_1", "M_humanlands_1", "M_agri_1", "M_forest_1",
                                                          "M_roadens_sec1", "M_mainroad_1", "M_bear_1", "M_roadbuild_1",
                                                          "M_build_1"))] # Select human & interesting variables

values <- bind_cols(natal_FID,natal_MID)

setwd("~/Norway/NHBD_humans")

write.csv(values,file="natal_values.csv")


