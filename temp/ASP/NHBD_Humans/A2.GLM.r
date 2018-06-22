

rm(list=ls())
library(dplyr) 
library(broom)

# ---- 1. Repeating model with variables Antonio ----
# The variables selected are the ones that have values for all territories, in order to be
# able to compare the selection coefficients on the PCA

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

#Select relevant variables for analysis
# Remove hdl and include distance to buildings
# Remove mountains and include elevation
# Remove agriculture (territories sympatric dont have agriculture)

d <- d[ , which(colnames(d) %in% c("territory", "used", "forest_pro", "clip_dem",
                                   "tri5", "main25m", "X2nd25m", "cov_build"))]

#.Scale by territory
terr <- unique(d$territory) 

for(i in 1:length(terr)){
  d[d$territory==terr[i],c(3:8)] <- scale(d[d$territory==terr[i],c(3:8)])
}

# Run one model for each territory

IDD <- unique(d$territory)

m <- matrix(NA, ncol=8, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "forest_pro","tri5", 
                 "clip_dem", "main25m", "X2nd25m", "cov_build")
rownames(m) <- IDD

glm.list <-list()

for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
  used <- data1$used
  data1 <- data1[,3:8] # Select only variables to put in the model
variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE)
variablestrue <- names(variables[variables==FALSE]) # Names variables without NA
df <- data1[,variables!=TRUE] # Data frame variables without NA

form <-as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA

glm.list[[i]] <- glm (form, # Run model
     family = binomial (link = "logit"),
     data =df )

glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
  
} 

m$territory <- rownames(m)

setwd("~/Norway/NHBD_humans/Antonio")
write.csv(m,"coef_Antonio_new.csv")

########################################################################################3
##########################################################
# Example: Fulufjallet_2010_w
ful <- d[which(d$territory == "Fulufjallet_2010_w"), ]

ful[ ,c(7:16)] <- scale(ful[,c(7:16)])

m1 <- glm (used ~ humland_pro + agri_pro + forest_pro + water_pro + mountains_pro + 
             tri5 + clip_dem + main25m + X2nd25m,
           family = binomial (link = "logit"),
           data = ful) #Removing humans it works

ful_used <- ful[which(ful$used == 1), ]
ful_random <- ful[which(ful$used == 0), ]

ful$humland_pro[which(ful$humland_pro > 0)] <- 1

ful_random_human <- ful[which(ful$used == 0 & ful$humland_pro > 0), ]
points(ful_random_human$x_UTM, ful_random_human$y_UTM, col = "blue")

unique(ful_used$humland_pro) # Only one value in the used of human
unique(ful_random$humland_pro) 

setwd("~/Norway/NHBD_humans/Antonio/GIS/Analysis")
humanlands <- raster("humland_pro.tif")

plot(humanlands, xlim = c(380249,407676), ylim = c(6798476,6837217))
points(ful_used$x_UTM, ful_used$y_UTM)
points(ful_random$x_UTM, ful_random$y_UTM, col = "red")

unique(ful_used$agri_pro)
#################Try with his data set
setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$territory_ == "Fulufjallet_2010_w"),  ]
unique(c$hdl5x5) # Only one value also!

c_used <- c[which(c$Used == 1), ]
unique(c_used$hdl5x5) 
c_random <- c[which(c$Used == 0), ]
unique(c_random$hdl5x5) 
c_random[which(c_random$hdl5x5 == 1), ]
#Plot the obs of Antonio that have a 1
plot(humanlands, xlim = c(380249,407676), ylim = c(6798476,6837217))
points(388747,6807313) # Plot random obs 64055 Antonio that has a 1 in humans
points(388468,6807024)
points(397303,6809662)
points(396169,6804159)

setwd("~/Norway/NHBD_humans/Antonio/GIS/Analysis")
humanlands <- raster("humland_pro.tif")

plot(humanlands, xlim = c(380249,407676), ylim = c(6798476,6837217))
points(c_used$x_UTM, c_used$y_UTM)
points(c_random$x_UTM, c_random$y_UTM, col = "red")


#################Remove human from whole data set and run the model
setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")
colnames(d)[3] <- "territory"
d <- d[ ,-7]
#.Scale by territory
terr <- unique(d$territory) 
for(i in 1:length(terr)){
  d[d$territory==terr[i],c(7:15)] <- scale(d[d$territory==terr[i],c(7:15)])
}
# Run one model for each territory
IDD <- unique(d$territory)
m <- matrix(NA, ncol=10, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "agri_pro", "forest_pro", "water_pro", "mountains_pro", 
                 "tri5", "clip_dem", "main25m", "X2nd25m")
rownames(m) <- IDD

glm.list <-list()

for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
  used <- data1$used
  data1 <- data1[,7:(ncol(data1)-1)] # Select only variables to put in the model
  data1 <- data1[,colnames(data1)!= "mires_pro"]
  variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE)
  variablestrue <- names(variables[variables==FALSE]) # Names variables without NA
  df <- data1[,variables!=TRUE] # Data frame variables without NA
  
  form <-as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA
  
  glm.list[[i]] <- glm (form, # Run model
                        family = binomial (link = "logit"),
                        data =df )
  
  glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
  m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
  
} 

###############################################################################################


########################################################################################3
##########################################################
# Example: Djurskog_2004_w
dj <- d[which(d$territory == "Djurskog_2004_w"), ]
dj[ ,c(7:16)] <- scale(dj[,c(7:16)])
m1 <- glm (used ~ agri_pro + forest_pro + water_pro + 
             tri5 + clip_dem + main25m + X2nd25m,
           family = binomial (link = "logit"),
           data = dj) #Removing humans it works
ful_used <- ful[which(ful$used == 1), ]
unique(ful_used$humland_pro) # Only one value in the used of human
unique(ful_used$agri_pro)
#################Try with his data set
setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("FINAL_dataset_v2_season+numeric.csv", header = TRUE, sep = ";")
c <- d[which(d$Used == 1 & d$territory_ == "Fulufjallet_2010_w"),  ] 
unique(c$hdl5x5) # Only one value also!
#################Remove human from whole data set and run the model
setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")
colnames(d)[3] <- "territory"
d <- d[ ,-7]
#.Scale by territory
terr <- unique(d$territory) 
for(i in 1:length(terr)){
  d[d$territory==terr[i],c(7:15)] <- scale(d[d$territory==terr[i],c(7:15)])
}
# Run one model for each territory
IDD <- unique(d$territory)
m <- matrix(NA, ncol=10, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "agri_pro", "forest_pro", "water_pro", "mountains_pro", 
                 "tri5", "clip_dem", "main25m", "X2nd25m")
rownames(m) <- IDD

glm.list <-list()

for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
  used <- data1$used
  data1 <- data1[,7:(ncol(data1)-1)] # Select only variables to put in the model
  data1 <- data1[,colnames(data1)!= "mires_pro"]
  variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE)
  variablestrue <- names(variables[variables==FALSE]) # Names variables without NA
  df <- data1[,variables!=TRUE] # Data frame variables without NA
  
  form <-as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA
  
  glm.list[[i]] <- glm (form, # Run model
                        family = binomial (link = "logit"),
                        data =df )
  
  glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
  m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
  
} 

###############################################################################################











# 2. ---- PCA ----

library(factoextra)

setwd("~/Norway/NHBD_humans/Antonio")
coef <- read.csv("coef_Antonio_new.csv")

coef <- coef[ ,-c(1)] 

pc <- prcomp(coef[, c(3:8)])

fviz_pca_biplot(pc, label="var",col.var = "black") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

# Analyze bear presence

setwd("~/Norway/NHBD_humans/Antonio")
bear <- read.csv("coef_maximal_functional_response_1.csv", sep = ";")
bear <- bear[ ,colnames(bear) %in% c("territory_", "bears")] # Data bear presence/territory (1/0)
colnames(bear)[1] <- "territory"

data <- left_join(coef,bear)

# 1. Plot

fviz_pca_biplot(pc, label="var",col.var = "black", habillage = data$bears, addEllipses = TRUE) +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(2,3), label="var",col.var = "black", habillage = data$bears, addEllipses = TRUE) +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(3,4), label="var",col.var = "black", habillage = data$bears, addEllipses = TRUE) +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pc, axes = c(2,4), label="var",col.var = "black", habillage = data$bears, addEllipses = TRUE) +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

# Regression with PCA scores (PC ~ Bear presence)

lm1 <- lm (pc$x[,1]~data$bears)
summary(lm1) 
      # Antonio: territories with bear tend to have lower scores on the first axis (no significant)
lm2 <- lm (pc$x[,2]~data$bears)
summary(lm2) 

lm3 <- lm (pc$x[,3]~data$bears)
summary(lm3) 

lm4 <- lm (pc$x[,4]~data$bears)
summary(lm4) 
plot(lm4)
