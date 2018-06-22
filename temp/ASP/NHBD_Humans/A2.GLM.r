

rm(list=ls())
library(dplyr) 
library(broom)

# ---- 1. Repeating model with variables Antonio ----

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")
colnames(d)[3] <- "territory"

#.Scale by territory
terr <- unique(d$territory) 

for(i in 1:length(terr)){
  d[d$territory==terr[i],c(7:16)] <- scale(d[d$territory==terr[i],c(7:16)])
}

# Run one model for each territory

IDD <- unique(d$territory)

m <- matrix(NA, ncol=11, nrow=length(unique(d$territory)))
m <-data.frame(m)
colnames(m) <- c("territory","(Intercept)", "humland_pro", "agri_pro", "forest_pro", "water_pro", "mountains_pro", 
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

m$territory <- rownames(m)

setwd("~/Norway/NHBD_humans/Antonio")
write.csv(m,"coef_Antonio_new.csv")

# 2. ---- PCA ----

library(factoextra)

setwd("~/Norway/NHBD_humans/Antonio")
coef <- read.csv("coef_Antonio_new.csv")


coef <- coef[ ,-c(1,8)] # Delete mountains (many NA) and first column
coef <- coef[complete.cases(coef), ] # Delete NA

pc <- prcomp(coef[, c(3:10)], center = TRUE)

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
