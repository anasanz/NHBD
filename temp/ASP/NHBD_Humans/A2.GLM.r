

rm(list=ls())
library(dplyr) 

# ---- 1. Repeating model Antonio ----

# This model varies from the previous one in:
#   - Availability sampled within 100 % MCP
#   - The variables selected are the ones that have values for all territories, in order to be
#     able to compare the selection coefficients on the PCA
#     Variables included:
#               -> Remove hdl and include distance to buildings
#               -> Remove mountains and include elevation
#               -> Remove agriculture (territories sympatric dont have agriculture)

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

d <- d[ , which(colnames(d) %in% c("territory", "used", "forest_pro", "clip_dem",
                                   "tri5", "main25m", "X2nd25m", "cov_build"))]

# Scale by territory
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
variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE) in the territory
variablestrue <- names(variables[variables==FALSE]) # Names variables without NA in the territory
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



# ---- 2. PCA ----

library(factoextra)

setwd("~/Norway/NHBD_humans/Antonio")
coef <- read.csv("coef_Antonio_new.csv", sep = ";")

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

    # ---- 2.1. Plot ----

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

    # ---- 2.2. Regression with PCA scores (PC ~ Bear presence) ----

lm1 <- lm (pc$x[,1]~data$bears)
summary(lm1) 

lm2 <- lm (pc$x[,2]~data$bears)
summary(lm2) 

lm3 <- lm (pc$x[,3]~data$bears)
summary(lm3) 

lm4 <- lm (pc$x[,4]~data$bears)
summary(lm4) 
plot(lm4)


# ---- 3. New coefficients including distance to closest human feature ----
    # ---- 3.1. The variable "closest" is distance to closest human feature including main roads, buildings and sec. roads ----
setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

d <- d[ , which(colnames(d) %in% c("territory", "used", "forest_pro", "clip_dem",
                                   "tri5", "main25m", "X2nd25m", "cov_build", "closest"))]

# Scale by territory
terr <- unique(d$territory) 

for(i in 1:length(terr)){
  d[d$territory==terr[i],c(3:9)] <- scale(d[d$territory==terr[i],c(3:9)])
}

# Run one model for each territory

IDD <- unique(d$territory)

m <- matrix(NA, ncol=9, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "forest_pro","tri5", 
                 "clip_dem", "main25m", "X2nd25m", "cov_build", "closest")
rownames(m) <- IDD

glm.list <-list()

for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
  used <- data1$used
  data1 <- data1[,3:9] # Select only variables to put in the model
  variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE) in the territory
  variablestrue <- names(variables[variables==FALSE]) # Names variables without NA in the territory
  df <- data1[,variables!=TRUE] # Data frame variables without NA
  
  form <-as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA
  
  glm.list[[i]] <- glm (form, # Run model
                        family = binomial (link = "logit"),
                        data =df )
  
  glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
  m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
  
} 

m$territory <- rownames(m)

#Seems like Tandsjon_2012_s is the one that doesnt converge with closest variable (all converge with closest2)

d_used <- d[which(d$used == 1), ]
tapply(d_used$territory, d_used$territory, length)
tapply(d_used$tri5, d_used$territory, mean)




t <- d[which(d$territory == "Tandsjon_2012_s"), ]
t_random <- t[which(t$used == 0), ]
t_used <- t[which(t$used == 1), ]

m_tand <- glm(used ~ forest_pro + tri5 + clip_dem + main25m + X2nd25m + cov_build + closest,
    family = binomial (link = "logit"),
    data = t)

# The problem is the closest variable indeed

length(t_random$closest)
unique(t_random$closest)
hist(t_random$closest)
unique(t_used$closest)
hist(t_used$closest)

unique(t_random$main25m)
hist(t_random$main25m)
unique(t_used$main25m)
hist(t_used$main25m)

setwd("~/Norway/NHBD_humans")
#write.csv(m,"coef_human.csv")


    # ----- 3.2. The variable "closest2" is distance to closest human feature including only main roads and buildings ----

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

d <- d[ , which(colnames(d) %in% c("territory", "used", "forest_pro", "clip_dem",
                                   "tri5", "main25m", "X2nd25m", "cov_build", "closest2"))]

# Scale by territory
terr <- unique(d$territory) 

for(i in 1:length(terr)){
  d[d$territory==terr[i],c(3:9)] <- scale(d[d$territory==terr[i],c(3:9)])
}

# Run one model for each territory

IDD <- unique(d$territory)

m <- matrix(NA, ncol=9, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "forest_pro","tri5", 
                 "clip_dem", "main25m", "X2nd25m", "cov_build", "closest2")
rownames(m) <- IDD

glm.list <-list()

for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
  used <- data1$used
  data1 <- data1[,3:9] # Select only variables to put in the model
  variables <- apply(data1, 2, function(x) sum(is.na(x))==length(x) ) # Select variables without NA (FALSE) in the territory
  variablestrue <- names(variables[variables==FALSE]) # Names variables without NA in the territory
  df <- data1[,variables!=TRUE] # Data frame variables without NA
  
  form <-as.formula(paste("used ~ ", paste(variablestrue, collapse= "+"))) # Create formula with variables without NA
  
  glm.list[[i]] <- glm (form, # Run model
                        family = binomial (link = "logit"),
                        data =df )
  
  glm.list.coef <- as.data.frame(glm.list[[i]]$coefficients) # Store it
  m[i, as.character(rownames(glm.list.coef))] <- glm.list[[i]]$coefficients
  
} 

m$territory <- rownames(m)


setwd("~/Norway/NHBD_humans")
write.csv(m,"coef_human2.csv")
