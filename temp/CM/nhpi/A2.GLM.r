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
Model <- list(closest = c("territory", "used", "forest_pro", "clip_dem",
              "tri5", "main25m", "X2nd25m","closest2"),
              closest_all = c("territory", "used", "forest_pro", "clip_dem",
              "tri5", "main25m", "cov_build","closest"))
Variable <- c("closest2_M","closest_M")



thresholdNBGPS <- c(250,250,20,20)

for(FF in 1:length(files)){
  setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/nhbd_2/data")
  d1 <- read.csv(paste("covariates_", files[FF], ".csv", sep=""), sep = ",")

## modify territory names
unique(d1$territory)
d1$territory <- as.character(d1$territory)
d1$territory[which(d1$territory == "Forshyttan_2005s")] <- "Forshyttan_2005_s"
d1$territory[which(d1$territory == "Jangen_2004w")] <- "Jangen_2004_w"
d1$territory[which(d1$territory == "Kloten_2008w")] <- "Kloten_2008_w"
d1$territory[which(d1$territory == "Kloten_2010w")] <- "Kloten_2010_w"
d1$territory[which(d1$territory == "Kukumaki_2014s")] <- "Kukumaki_2014_s"
d1$territory[which(d1$territory == "Nyskoga_2003s")] <- "Nyskoga_2003_s"
d1$territory[which(d1$territory == "Nyskoga_2004w")] <- "Nyskoga_2004_w"
d1$territory[which(d1$territory == "Riala_2010w")] <- "Riala_2010_w"
d1$territory[which(d1$territory == "Stadra_2003w")] <- "Stadra_2003_w"
d1$territory[which(d1$territory == "Tandsjon_2014s")] <- "Tandsjon_2014_s"
d1$territory[which(d1$territory == "Tandsjon_2014w")] <- "Tandsjon_2014_w"
d1$territory[which(d1$territory == "Tenskog_2010w")] <- "Tenskog_2010_w"
d1$territory[which(d1$territory == "Ulriksberg_2006w")] <- "Ulriksberg_2006_w"
d1$territory[which(d1$territory == "Kukumaki_2014w")] <- "Kukumaki_2014_w"
d1$territory[which(d1$territory == "Bograngen_2003s")] <- "Bograngen_2003_s"
d1$territory[which(d1$territory == "Fulufjallet_2009w")] <- "Fulufjallet_2009_w"
d1$territory[which(d1$territory == "Glaskogen_2002s1")] <- "Glaskogen_2002_s1"
d1$territory[which(d1$territory == "Glaskogen_2002s2")] <- "Glaskogen_2002_s2"
d1$territory[which(d1$territory == "Glaskogen_2002s3")] <- "Glaskogen_2002_s3"
d1$territory[which(d1$territory == "Kukumaki_2015w")] <- "Kukumaki_2015_w"
d1$territory[which(d1$territory == "Kukumaki_2015s")] <- "Kukumaki_2015_s"

### ---- 1. SELECT THE VARIABLES FOR THE MODEL ====
M=2
for(M in 1:length(Model)){
  
d <- d1[ , which(colnames(d1) %in% Model[[M]])]

### ---- 2. SCALE BY TERRYTORY ====
terr <- unique(d$territory) 
for(i in 1:length(terr)){
    d[d$territory==terr[i],c(3:8)] <- scale(d[d$territory==terr[i],c(3:8)])
}

### ---- 3. RUN SEPARATE GLM PER TERRITORY ====
# CREATE NECESSARY OBJECT 
IDD <- unique(d$territory)
m <- matrix(NA, ncol=8, nrow=length(unique(d$territory)))
m <- data.frame(m)
colnames(m) <- c("territory","(Intercept)",Model[[M]][3:8])
rownames(m) <- IDD
glm.list <- list()

## RUN GLM
for(i in 1:length(IDD)){
  data1 <- d[d$territory==IDD[i],] # Select one territory
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
} 
glm.m <- m
glm.m$territory <- rownames(glm.m)

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
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/natal_habitat_biased_dispersal/nhbd/NHBD/temp/ASP/NHBD_Humans/Data")
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
as.factor(e$ID_pair)
e$Season <- "W" # Add season (Summer/Winter = 1/0)
e$Season[grep("_s", e$Territory_antonio, ignore.case = TRUE)] <- "S"


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
tapply(d_used$territory, d_used$territory, length) # n� positions/territory

positions <- as.data.frame(tapply(d_used$territory, d_used$territory, length)) # Data frame with n� of positions/territory
remove <- rownames(positions)[which(positions$`tapply(d_used$territory, d_used$territory, length)` < thresholdNBGPS[FF])] # Remove the ones < 250
prov <- prov[-which(prov$Territory_antonio %in% remove),]
# ---- IV. PLOTS ----
# ==== 1. MALES ====

plot(prov[,Variable[M]] ~ prov$PC_M, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov[,Variable[M]] ~ prov$PC_M)) 
summary(lm(prov[,Variable[M]] ~ prov$PC_M))

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
prov_s_M <- prov[which(prov$Season_M == "W"), ] # Create 1 dataset for each season
prov_w_M <- prov[which(prov$Season_M == "S"), ]

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
col_season <- c("red","blue")[as.factor(prov$Season_M)]
plot(prov$PC_F, prov[,Variable[M]], pch = 16, col = col_season, xlab = "PC1", ylab=Variable.axis[M])

# subset season to plot it
prov_s_F <- prov[which(prov$Season_F == "W"), ] # Create 1 dataset for each season
prov_w_F <- prov[which(prov$Season_F == "S"), ]

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
prov_s_F <- prov[which(prov$Season_F == "W"), ] # Create 1 dataset for each season
prov_w_F <- prov[which(prov$Season_F == "S"), ]

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
mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ PC_F + (1|ID_pair_F), data = prov, REML = F))
mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ PC_F*Season_F + (1|ID_pair_F), data = prov, REML = F))
mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ PC_M + (1|ID_pair_F), data = prov, REML = F))
mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ PC_M*Season_M + (1|ID_pair_F), data = prov, REML = F))



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
write.csv(aictab, file=paste("PC1_AIC", files[FF], Variable[M],".csv",sep="_"))


# ==== 1.2 COEFS TABLE  ====
mods <- list()
mods[["m_null"]] <- summary(lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T))
mods[["m_1"]] <- summary(lmer(prov[,Variable[M]] ~ PC_F + (1|ID_pair_F), data = prov, REML = T))
mods[["m_2"]] <- summary(lmer(prov[,Variable[M]] ~ PC_F*Season_F + (1|ID_pair_F), data = prov, REML = T))
mods[["m_3"]] <- summary(lmer(prov[,Variable[M]] ~ PC_M + (1|ID_pair_F), data = prov, REML = T))
mods[["m_4"]] <- summary(lmer(prov[,Variable[M]] ~ PC_M*Season_M + (1|ID_pair_F), data = prov, REML = T))

modSS <- list()
modSS[["m_null"]] <- lmer(prov[,Variable[M]] ~ 1 + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_1"]] <- lmer(prov[,Variable[M]] ~ PC_F + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_2"]] <- lmer(prov[,Variable[M]] ~ PC_F*Season_F + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_3"]] <- lmer(prov[,Variable[M]] ~ PC_M + (1|ID_pair_F), data = prov, REML = T)
modSS[["m_4"]] <- lmer(prov[,Variable[M]] ~ PC_M*Season_M + (1|ID_pair_F), data = prov, REML = T)


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








confint(m_2)

#summary(m_1)
m_2 <- lmer(closest2 ~ PC*Season + (1|ID), data = prov_f,REML = F)
AIC(m_2)
confint(m_2)

#summary(m_1)
m_2 <- lmer(closest2 ~ PC*Season*Sex + (1|ID), data = prov, REML = F)
AIC(m_2)
confint(m_2)

df <- expand.grid(list(PC=seq(range(prov$PC)[1],range(prov$PC)[2] ,by=0.2), 
                       Sex= c("M","F"),
                       Season = c(0,1)))

# library(merTools)
# preds <- predictInterval(m_2, newdata = df, n.sims = 999)

df$pred <- predict(m_2,newdata=df,re.form=NA)
plot(-5, xlim=c(range(prov$PC)),ylim=c(0,2))
fw <- df[ which(df$Sex=="F" & df$Season==0) ,]
points(fw$pred~fw$PC, col="blue", type="l")

fs <- df[ which(df$Sex=="F" & df$Season==1) ,]
points(fs$pred~fs$PC, col="red", type="l")

mw <- df[ which(df$Sex=="M" & df$Season==0) ,]
plot(mw$pred~mw$PC)

































# setwd("~/Norway/NHBD_humans/Antonio")
write.csv(m,"coef_Antonio_new_closest2.csv")
write.csv(m,"coef_Antonio_new_closest2_kern.csv")

write.csv(m,"coef_Antonio_new_closest2_mcp.csv")
write.csv(m,"coef_Antonio_new_closest2_mcpnot.moved.csv")

 # ---- 1.2 Repeating model Antonio closest  ----
## closest 1  
 d <- d1[ , which(colnames(d1) %in% c("territory", "used", "forest_pro", "clip_dem",
                                      "tri5", "main25m", "closest","cov_build"))]
 # FIRST: RUN THIS TO FIX TERRITORY NAMES
 
 # Scale by territory
 terr <- unique(d$territory) 
 
 for(i in 1:length(terr)){
   d[d$territory==terr[i],c(2:7)] <- scale(d[d$territory==terr[i],c(2:7)])
 }
 
 # Run one model for each territory
 
 IDD <- unique(d$territory)
 
 m <- matrix(NA, ncol=8, nrow=length(unique(d$territory)))
 m <-data.frame(m)
 colnames(m) <- c("territory","(Intercept)", "forest_pro", "clip_dem",
                  "tri5", "main25m", "cov_build","closest")
 rownames(m) <- IDD
 
 glm.list <- list()
 
 for(i in 1:length(IDD)){
   data1 <- d[d$territory==IDD[i],] # Select one territory
   used <- data1$used
   data1 <- data1[,2:7] # Select only variables to put in the model
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
 
 ## CHECK AND SUBSET TERRITORIES 
 # t <- d[which(d$territory == "Tandsjon_2012_s"), ]
 # t_random <- t[which(t$used == 0), ]
 # t_used <- t[which(t$used == 1), ]
 # 
 # m_tand <- glm(used ~ forest_pro + tri5 + clip_dem + main25m + X2nd25m  + closest2,
 #               family = binomial (link = "logit"),
 #               data = t)
 # 
 d_used <- d[which(d$used == 1), ]
 tapply(d_used$territory, d_used$territory, length) # There are territories with very few positions, thats why Tandsjon doesnt converge
 # We will remove the territories with less of 250 positions from the next analyses (in script H3)
 
 # setwd("~/Norway/NHBD_humans/Antonio")
 write.csv(m,"coef_Antonio_new_closest.csv")
 write.csv(m,"coef_Antonio_new_closest_kern.csv")
 
 

# ---- 2. PCA ----
library(factoextra)

# setwd("~/Norway/NHBD_humans/Antonio")
 coef <- read.csv("coef_Antonio_new_closest2.csv", sep = ",")

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

#setwd("~/Norway/NHBD_humans/Antonio")
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

t <- d[which(d$territory == "Tandsjon_2012_s"), ]
t_random <- t[which(t$used == 0), ]
t_used <- t[which(t$used == 1), ]

m_tand <- glm(used ~ forest_pro + tri5 + clip_dem + main25m + X2nd25m + cov_build + closest,
    family = binomial (link = "logit"),
    data = t)

d_used <- d[which(d$used == 1), ]
tapply(d_used$territory, d_used$territory, length) # There are territories with very few positions, thats why Tandsjon doesnt converge
# We will remove the territories with less of 250 positions from the next analyses (in script H3)


setwd("~/Norway/NHBD_humans")
write.csv(m,"coef_human.csv")


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
