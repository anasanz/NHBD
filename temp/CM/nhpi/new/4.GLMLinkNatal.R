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

plot(prov[,Variable[M]] ~ prov$PC_M, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov[,Variable[M]] ~ prov$PC_M)) 
summary(lm(prov[,Variable[M]] ~ prov$PC_M))

#moose
plot(prov[,Variable[M]] ~ prov$moose_M, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov[,Variable[M]] ~ prov$moose_M)) 
summary(lm(prov[,Variable[M]] ~ prov$moose_M))


# ==== 1.1 SEASON ====
Variable.axis <- c("Beta distance humans","Beta distance all humans")

# ==== 1.1.1 PC1 ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf(file=paste("Males_SeasonPC1", files[FF], Variable[M],".pdf",sep="_"))

# plot points
col_season <- c("red","blue")[as.factor(prov$Season_M)]
plot(prov$PC_M, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC1", ylab=Variable.axis[M])

# subset season to plot it
prov_s_M <- prov[which(prov$Season_M == "W"), ] # Create 1 dataset for each season
prov_w_M <- prov[which(prov$Season_M == "S"), ]

# plot lines
abline(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC_M), col = "red") #Summer
summary(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC_M))

abline(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC_M), col = "blue") #Winter
summary(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC_M))
#ADD LEGEND 
legend("topright", col=c("blue","red"), pch=16, legend = c("winter","summer"))
dev.off()

# ==== 1.1.2 PC2 ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf(file=paste("Males_SeasonPC2", files[FF], Variable[M],".pdf",sep="_"))

# plot points
col_season <- c("red","blue")[as.factor(prov$Season_M)]
plot(prov$PC2_M, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC2", ylab=Variable.axis[M])

# subset season to plot it
prov_s_M <- prov[which(prov$Season_M == "S"), ] # Create 1 dataset for each season
prov_w_M <- prov[which(prov$Season_M == "W"), ]

# plot lines
abline(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC2_M), col = "red") #Summer
summary(lm(prov_s_M[,Variable[M]] ~ prov_s_M$PC2_M))

abline(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC2_M), col = "blue") #Winter
summary(lm(prov_w_M[,Variable[M]] ~ prov_w_M$PC2_M))
#ADD LEGEND 
legend("bottomleft", col=c("blue","red"), pch=16, legend = c("winter","summer"))
dev.off()


# ==== 2. FEMALES ====

plot(prov[,Variable[M]] ~ prov$PC_F, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov[,Variable[M]] ~ prov$PC_F)) 
summary(lm(prov[,Variable[M]] ~ prov$PC_F))

# ==== 2.1 SEASON ====
Variable.axis <- c("Beta distance humans","Beta distance all humans")

# ==== 2.1.1 PC1 ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf(file=paste("Females_SeasonPC1", files[FF], Variable[M],".pdf",sep="_"))

# plot points
col_season <- c("red","blue")[as.numeric(as.factor(prov$Season_M))]
plot(prov$PC_F, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC1", ylab=Variable.axis[M])

# subset season to plot it
prov_w_F <- prov[which(prov$Season_F == "W"), ] # Create 1 dataset for each season
prov_s_F <- prov[which(prov$Season_F == "S"), ]

# plot lines
abline(lm(prov_s_F[,Variable[M]] ~ prov_s_F$PC_F), col = "red") #Summer
summary(lm(prov_s_F[,Variable[M]] ~ prov_s_F$PC_F))

abline(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC_F), col = "blue") #Winter
summary(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC_F))
#ADD LEGEND 
legend("bottomright", col=c("blue","red"), pch=16, legend = c("winter","summer"))
dev.off()

### NOT SEASON
pdf(file=paste("Females_PC1", files[FF], Variable[M],".pdf",sep="_"))
# plot points
plot(prov$PC_F, prov[,Variable[M]], pch = 16, xlab = "PC1", ylab=Variable.axis[M])
# plot lines
abline(lm(prov[,Variable[M]] ~ prov$PC_F), col = "black") #Summer
dev.off()


# ==== 2.1.2 PC2 ====
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
pdf(file=paste("Females_SeasonPC2", files[FF], Variable[M],".pdf",sep="_"))

# plot points
col_season <- c("red","blue")[as.factor(prov$Season_M)]
plot(prov$PC2_F, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC2", ylab=Variable.axis[M])

# subset season to plot it
prov_s_F <- prov[which(prov$Season_F == "S"), ] # Create 1 dataset for each season
prov_w_F <- prov[which(prov$Season_F == "W"), ]

# plot lines
abline(lm(prov_s_F[,Variable[M]] ~ prov_s_M$PC2_F), col = "red") #Summer
summary(lm(prov_s_F[,Variable[M]] ~ prov_s_M$PC2_F))

abline(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC2_F), col = "blue") #Winter
summary(lm(prov_w_F[,Variable[M]] ~ prov_w_F$PC2_F))
#ADD LEGEND 
legend("topleft", col=c("blue","red"), pch=16, legend = c("winter","summer"))
dev.off()

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

aictab <- aictab[order(aictab[,4]),]
write.csv(aictab, file=paste("PC1_AIC", files[FF], Variable[M],".csv",sep="_"))


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

write.csv(coeftab, file=paste("PC1_coeffs", files[FF], Variable[M],".csv",sep="_"))

# ==== 2. PC2
# ==== 2.1. AIC TABLE  ====

# RUN MODELS 
mods <- list()
mods[["m_null"]] <- summary(lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov,REML = F))
mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_F + (1|ID_pair_F), data = prov, REML = F))
mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_F*Season_F + (1|ID_pair_F), data = prov, REML = F))
mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_M + (1|ID_pair_F), data = prov, REML = F))
mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_M*Season_M + (1|ID_pair_F), data = prov, REML = F))



aictab <- matrix(NA, nrow=length(mods), ncol= 4 )
colnames(aictab) <- c("AIC", "LL", "df", "deltaAIC" )
row.names(aictab) <- c("Null", "Natal_F", "Natal_F * Season",
                       "Natal_M", "Natal_M * Season")
for(mo in 1:length(mods)){
  aictab[mo,1] <- mods[[mo]]$AIC[1] # 
  aictab[mo,2] <- mods[[mo]]$AIC[3] # LL
  aictab[mo,3] <- mods[[mo]]$AIC[5]# df
}
aictab[,4] <- aictab[,1]-min(aictab[,1])

aictab <- aictab[order(aictab[,4]),]
write.csv(aictab, file=paste("PC2_AIC", files[FF], Variable[M],".csv",sep="_"))


# ==== 2.2. COEFS TABLE  ====
mods <- list()
mods[["m_null"]] <- summary(lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T))
mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_F + (1|ID_pair_F), data = prov, REML = T))
mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_F*Season_F + (1|ID_pair_F), data = prov, REML = T))
mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_M + (1|ID_pair_F), data = prov, REML = T))
mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ PC2_M*Season_M + (1|ID_pair_F), data = prov, REML = T))

modSS <- list()
modSS[["m_null"]] <- lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_1"]] <- lmer(prov[,Variable[M]] ~ PC2_F + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_2"]] <- lmer(prov[,Variable[M]] ~ PC2_F*Season_F + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_3"]] <- lmer(prov[,Variable[M]] ~ PC2_M + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_4"]] <- lmer(prov[,Variable[M]] ~ PC2_M*Season_M + (1|ID_pair_F), data = prov, REML = T)


mod.names <- c("Null", "Natal_F", "Natal_F * Season",
               "Natal_M", "Natal_M * Season")
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

write.csv(coeftab, file=paste("PC2_coeffs", files[FF], Variable[M],".csv",sep="_"))

}
}


