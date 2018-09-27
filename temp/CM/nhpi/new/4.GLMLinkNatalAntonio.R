rm(list=ls())
library(dplyr) 
library(lme4) 
### ==== I. CHECK CORRELATION BETWEEN VARIABLES ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
files <- c("all_points.not.moved_MCP", "all_points.not.moved_KERN",
             "all_points.move_MCP", "all_points.move_KERN")


f=2
for(f in 1:length(files)){
  d1 <- read.csv(paste("covariates_",files[f], ".csv", sep=""), sep = ",")
  d1[is.na(d1)] <- 0
  d2 <- d1[ ,which(colnames(d1) %in% c("forest_pro", "clip_dem","mires_pro","clip_dem",
                                        "tri5", "main25m", "X2nd25m", "cov_build","closest","closest2"))]
  print(cor(d2))
  
}#f

#forest_pro vs mires_pro == -0.64
#X2nd25m vs closest == 0.8302396
#closest2 vs cov_build ==0.86309757

### ==== II. PERFORM THE ANALYSIS ====
Model <- list(closest = c("Study_year", "used", "forest_pro", "clip_dem",
              "tri5", "main25m", "X2nd25m","closest2"),
              closest_all = c("Study_year", "used", "forest_pro", "clip_dem",
              "tri5", "main25m", "cov_build","closest"))
Variable <- c("closest2_M","closest_M")



thresholdNBGPS <- c(250,250,50,55)
# thresholdNBGPS <- c(0,0,0,0)

for(FF in 1:length(files)){
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
  d1 <- read.csv(paste("covariates_", files[FF], ".csv", sep=""), sep = ",")

## modify territory names
levels(unique(d1$Study_year))

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

write.csv(glm.m, file=paste("GLM", files[FF], Variable[M],".pdf",sep="_"))

### here an example of the relationshipt between the coefficient of selection towards forest and moose density. 
### this where you could do the same for bears :)
plot(glm.m$forest_pro~glm.m$moose)

}
}
