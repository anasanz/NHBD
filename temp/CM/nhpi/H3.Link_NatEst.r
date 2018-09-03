
rm(list=ls())
library(dplyr)
library(lme4)

# 0. ---- Exploratory selection coefficients ----

# Is there a difference in the way they select main and secondary roads?
# Check if the direction of selecting main and secondary roads is the same for all territories

#setwd("~/Norway/NHBD_humans/Antonio")
setwd("~/Norway/NHBD_humans")
c <- read.csv("coef_human.csv", sep = ";")
c <- c[ ,-c(1)]
colnames(c)[1] <- "Territory_antonio"

plot(c$main25m, c$X2nd25m, col = as.factor(c$Territory_antonio), pch = 16)
abline(h=0)
abline(v=0)

sum(length(which(c$main25m > 0 & c$X2nd25m > 0)) + length(which(c$main25m < 0 & c$X2nd25m < 0))) 

# Conclusion: 26/47 territories select main and sec roads in the same direction 
# In PCA of natal territories, they seem to have different proportions of main and secondary roads

# 1. ---- Characterize natal territories ----

setwd("~/Norway/NHBD_humans")
v <- read.csv("natal_values_complete.csv", header = TRUE) #Load extracted natal values
v$ID_pair <- paste(v$ID_F,v$ID_M,sep = "_") # Create ID_pair column to add it as random in the model


  # A. ---- JOIN FEMALES-MALES (Same variable names) ----

  # Shape data in long format to analyze it

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
#Delete NA inmigrant males
natal <- natal[complete.cases(natal), ]

  # B. ---- PCA to characterize natal territories ----

# Create column with PC scores of the natal territory (characterized by human variables)
# to link it later with the selection coefficients of each territories

library(factoextra)

n <- natal[ ,colnames(natal) %in% c("Territory_antonio", "ID", "human_1", "humanlands_1", "agri_1", "mainroad_1", #"roadbuild_1",
                                    "roadens_sec1", "build_1")] # Only human-related variables

sd_n<- as.data.frame(scale(n[3:8]))
pc <- prcomp(sd_n)
fviz_pca_biplot(pc, label="var",col.var = "black") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))
pc$x

pc$x[ ,1] #PC1 explains 72% of the variance and separates human (+) vS non-human (-) 
          # characterized territories
          # However, PC1 doesnt capture the difference in sec. roads, which is more captured by PC2
natal$PC <- pc$x[ ,1] 
natal$PC2 <- pc$x[ ,2]

################### Check natal territory of wolves with high natal values ###

natal[which(natal$PC > 5), ] # Kloten and Stadra
setwd("~/Norway/NHBD_humans")
hum <- read.csv("data_pairs_human_complete.csv", sep = ";")

natal[which(natal$PC2 < -4), ] # Stadra

# Kloten_2009_s and Kloten_2010_w (Individual M0918, natal territory = Krp2) have high values of human density and roadbuild
# Stadra_2003_w (ID M0314, natal territory = Mos) have high values of human density
# These territories fall in high human-dominated areas (south), so there is nothing wrong, they are real outliers.
# They give us variation in human variables, so for the moment we keep them in the models.

# 2. ---- Link with coefficients ----

# There are 2 datasets:

#2.1. coef_human.csv contains the variable "closest" (minimun distance to mainroads,buildings,secroads)
#2.2. coef_human2.csv contains the variable "closest2" (minimun distance to mainroads,buildings)

# In both of them, we remove the territories that had <250 positions for estimating the selection coefficients, which are:

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

d_used <- d[which(d$used == 1), ] # Select used (gps positions)
tapply(d_used$territory, d_used$territory, length) # nº positions/territory

positions <- as.data.frame(tapply(d_used$territory, d_used$territory, length)) # Data frame with nº of positions/territory
remove <- rownames(positions)[which(positions$`tapply(d_used$territory, d_used$territory, length)` < 250)] # Remove the ones < 250

# 2.1. ----- Dataset with "closest" variable ----

setwd("~/Norway/NHBD_humans")
c <- read.csv("coef_human.csv", sep = ",")
c <- c[ ,-c(1)]
colnames(c)[1] <- "Territory_antonio"

e <- left_join(natal,c, by = "Territory_antonio") 

e$Season <- 0 # Add season (Summer/Winter = 1/0)
e$Season[grep("_s", e$Territory_antonio, ignore.case = TRUE)] <- 1

prov <- e[-which(e$Territory_antonio %in% remove), ] # Remove territories with < 250 positions to estimate selection coefficients


## Make one dataset for each sex for analyses

prov_f <- prov[which(prov$Sex == "F"), ]
prov_m <- prov[which(prov$Sex == "M"), ] 

### Exploratory plots with both sexes together

# Main roads

plot(prov$PC,prov$main25m, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov$main25m ~ prov$PC)) 
summary(lm(prov$main25m ~ prov$PC))

prov_s <- prov[which(prov$Season == 1), ] # Create 1 dataset for each season
prov_w <- prov[which(prov$Season == 0), ]
col_season <- c("blue","red")[as.factor(prov$Season)]
plot(prov$PC,prov$main25m, pch = 16, col = col_season)

abline(lm(prov_s$main25m ~ prov_s$PC), col = "red") #Summer
summary(lm(prov_s$main25m ~ prov_s$PC))

abline(lm(prov_w$main25m ~ prov_w$PC), col = "blue") #Winter
summary(lm(prov_w$main25m ~ prov_w$PC))

# Distance to buildings

plot(prov$PC,prov$cov_build, pch = 16)
abline(lm(prov$cov_build ~ prov$PC)) # Overall trend not significant for all together
summary(lm(prov$cov_build ~ prov$PC))

# Sec roads

  #With PC1

plot(prov$PC,prov$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov$X2nd25m ~ prov$PC)) 
summary(lm(prov$X2nd25m ~ prov$PC))

  #With PC2 (Characterize better sec.roads)

plot(prov$PC2,prov$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov$X2nd25m ~ prov$PC2)) 
summary(lm(prov$X2nd25m ~ prov$PC2))

# Closest cosa (minimun distance to mainroads,buildings,secroads)

#With PC1

plot(prov$PC,prov$closest, pch = 16) # Overall trend not significant for all together
abline(lm(prov$closest ~ prov$PC)) 
summary(lm(prov$closest ~ prov$PC))

#With PC2 (Characterize better sec.roads)

plot(prov$PC2,prov$closest, pch = 16) # Overall trend not significant for all together
abline(lm(prov$closest ~ prov$PC2)) 
summary(lm(prov$closest ~ prov$PC2))

  # A. ---- SIMPLE MODEL (Not very interesting, we need random effects) ----
      # ---- 1. RESPONSE: MAIN ROADS COEFFICIENT ----

m1 <- lm(main25m ~ PC + forest_1 + bear_1 + Sex + Season, prov)
summary(m1)

#Females 

m_f <- lm(main25m ~ PC + forest_1 + bear_1 + Season, prov_f)
summary(m_f)

col_season <- c("blue","red")[as.factor(prov_f$Season)]
plot(prov_f$PC,prov_f$main25m, pch = 16, col = col_season)
abline(lm(prov_f$main25m ~ prov_f$PC))

#Males

m_m <- lm(main25m ~ PC + forest_1 + bear_1 + Season, prov_m)
summary(m_m)

col_season <- c("blue","red")[as.factor(prov_m$Season)]
plot(prov_m$PC,prov_m$main25m, pch = 16, col = col_season)
abline(lm(prov_m$main25m ~ prov_m$PC))


  # B. ---- INCLUDE PAIR AS RANDOM ----
      # ---- 1. RESPONSE: MAIN ROADS COEFFICIENT ----
### ALL

m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # Better without interaction (m3)

m4 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) # Overlap 0

m5 <- lmer(main25m ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(main25m ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex
summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Better without season and sex (m6)


### BY SEX

m_f2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) 
warnings()


m_m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) 

#Try with nlme to see if the warning is fixed: WARNINGS FIXED, USE THIS ONES

library(nlme)

fm1 <- lme(main25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) # FEMALES: SLIGHT OVERLAP 0; NO WARNINGS


mm1 <- lme(main25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_m)
summary(mm1)
intervals(mm1, level = 0.95) # MALES: OVERLAP 0; NO WARNINGS


      # ---- 2. RESPONSE: DISTANCE TO BUILDINGS COEFFICIENT ----

### ALL

m2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2) 
confint(m2) 

m3 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # Better without

m4 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(cov_build ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(cov_build ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Best m6


### BY SEX

#Females

m_f2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; NO WARNINGS

#Males
 
m_m2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed
library(nlme)

fm1 <- lme(cov_build ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) 

mm1 <- lme(cov_build ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_m)
summary(mm1)
intervals(mm1, level = 0.95) # Doesnt work

      # ---- 3. RESPONSE: CLOSEST COSA - PC ----

### ALL

m2 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2) 
confint(m2) 

m3 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3)

AIC(m2,m3) 

m4 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(closest ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(closest ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

AIC(m3, m4, m5, m6) # Best m6
summary(m6)
confint(m6) # WARNINGS, Overlap


# BY SEX

#Females

m_f2 <- lmer(closest ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; WARNINGS

#Males

m_m2 <- lmer(closest ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed
library(nlme)

fm1 <- lme(closest ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) 

mm1 <- lme(closest ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_m)
summary(mm1)
intervals(mm1, level = 0.95) # Doesnt work


      # ---- 4. RESPONSE: SEC ROADS COEFFICIENT ----
### ALL

m2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov2) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov2) #Without interaction
summary(m3)
confint(m3) 
 
AIC(m2,m3)#m3

m4 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov2) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov2) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(X2nd25m ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex
summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Best 4 & 5


### BY SEX

m_f2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: SLIGHT OVERLAP 0; WARNINGs
warnings()


m_m2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed: ITS FIXED

fm1 <- lme(X2nd25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) # FEMALES: SLIGHT OVERLAP 0; NO WARNINGS

mm1 <- lme(X2nd25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(mm1)
intervals(mm1, level = 0.95) # MALES: SLIGHT OVERLAP 0; NO WARNINGS


# 2.2. ---- Dataset with "closest2" variable ----

setwd("~/Norway/NHBD_humans")
c2 <- read.csv("coef_human2.csv")
c2 <- c2[ ,-c(1)]
colnames(c2)[1] <- "Territory_antonio"

e2 <- left_join(natal,c2, by = "Territory_antonio")

e2$Season <- 0 # Add season (Summer/Winter = 1/0)
e2$Season[grep("_s", e2$Territory_antonio, ignore.case = TRUE)] <- 1

prov2 <- e2[-which(e2$Territory_antonio %in% remove), ] # Remove territories with < 250 positions to estimate selection coefficients


### Check outliers


## Make one dataset for each sex for analyses

prov2_f <- prov2[which(prov2$Sex == "F"), ]
prov2_m <- prov2[which(prov2$Sex == "M"), ] 

### Exploratory plots with both sexes together

# Main roads

plot(prov2$PC,prov2$main25m, pch = 16) # Overall trend not significant for all sexes together
abline(lm(prov2$main25m ~ prov2$PC)) 
summary(lm(prov2$main25m ~ prov2$PC))

prov2_s <- prov2[which(prov2$Season == 1), ] # Create 1 dataset for each season
prov2_w <- prov2[which(prov2$Season == 0), ]
col_season <- c("blue","red")[as.factor(prov2$Season)]
plot(prov2$PC,prov2$main25m, pch = 16, col = col_season)

abline(lm(prov2_s$main25m ~ prov2_s$PC), col = "red") #Summer
summary(lm(prov2_s$main25m ~ prov2_s$PC))

abline(lm(prov2_w$main25m ~ prov2_w$PC), col = "blue") #Winter
summary(lm(prov2_w$main25m ~ prov2_w$PC))

# Distance to buildings

plot(prov2$PC,prov2$cov_build, pch = 16)
abline(lm(prov2$cov_build ~ prov2$PC)) # Overall trend not significant for all together
summary(lm(prov2$cov_build ~ prov2$PC))

# Sec roads

#With PC1

plot(prov2$PC,prov2$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov2$X2nd25m ~ prov2$PC)) 
summary(lm(prov2$X2nd25m ~ prov2$PC))

#With PC2 (Characterize better sec.roads)

plot(prov2$PC2,prov2$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov2$X2nd25m ~ prov2$PC2)) 
summary(lm(prov2$X2nd25m ~ prov2$PC2))


# Closest cosa2 (minimun distance to mainroads,buildings)

#With PC1

plot(prov2$PC,prov2$closest2, pch = 16) # Overall trend not significant for all together
abline(lm(prov2$closest2 ~ prov2$PC)) 
summary(lm(prov2$closest2 ~ prov2$PC))

  # B. ---- INCLUDE PAIR AS RANDOM ----
      # ---- 1. RESPONSE: MAIN ROADS COEFFICIENT ----
### ALL

m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov2) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov2) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # Better without interaction (m3)

m4 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov2) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(main25m ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov2) 
summary(m5)
confint(m5) 

m6 <- lmer(main25m ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov2) #No season no sex
summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Better without season and sex (m6)


### BY SEX

m_f2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; WARNINGS


m_m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_m)
summary(m_m2)
confint(m_m2) # MALES: slight OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed

library(nlme)

fm1 <- lme(main25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_f)
summary(fm1)
intervals(fm1, level = 0.95) # FEMALES: SLIGHT OVERLAP 0; NO WARNINGS

mm1 <- lme(main25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_m)
summary(mm1)
intervals(mm1, level = 0.95) # Doesnt work


      # ---- 2. RESPONSE: DISTANCE TO BUILDINGS COEFFICIENT ----

### ALL

m2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov2) # Sex, With interaction, season
summary(m2) 
confint(m2) 

m3 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov2) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # bEST M3

m4 <- lmer(cov_build ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov2) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(cov_build ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov2) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(cov_build ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov2) #No season no sex

summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Better without Sex but with Season (m5)


### BY SEX

#Females

m_f2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; WARNINGS

#Males

m_m2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_m)
summary(m_m2)
confint(m_m2) # MALES: sLIGHT OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed
library(nlme)

fm1 <- lme(cov_build ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_f)
summary(fm1)
intervals(fm1, level = 0.95) # FEMALES: OVERLAP 0

mm1 <- lme(cov_build ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_m)
summary(mm1)
intervals(mm1, level = 0.95) # Doesnt work

      # ---- 3. RESPONSE: CLOSEST COSA 2 - PC ----

#ALL

m2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov2) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov2) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # Better without

m4 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov2) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(closest2 ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov2) # No sex, Si season
summary(m5)
confint(m5) 

m6 <- lmer(closest2 ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov2) #No season no sex

AIC(m3, m4, m5, m6) # bEST M5 WITHOUT SEX


# BY SEX

#Females

m_f2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_f) # SI SEASON
summary(m_f2)
confint(m_f2) # FEMALES: WARNINGS, OVERLAP


#Males

m_m2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_m) # SI SEASON
summary(m_m2)
confint(m_m2) # MALES: NO OVERLAP; WARNING


#Try with nlme to see if the warning is fixed: FIXED

#Females

fm1 <- lme(closest2 ~ PC + forest_1 + bear_1 + Season, 
           random = ~ 1|ID_pair, data = prov2_f)
summary(fm1)
intervals(fm1, level = 0.95) 


#Males 
mm1 <- lme(closest2 ~ PC + forest_1 + bear_1 + Season, 
           random = ~ 1|ID_pair, data = prov2_m)
summary(mm1)
intervals(mm1, level = 0.95) # VERY SLIGHT OVERLAP 0, NO WARNINGS


      # ---- 4. RESPONSE: SEC ROADS COEFFICIENT ----
### ALL

m2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov2) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov2) #Without interaction
summary(m3)
confint(m3) 

AIC(m2,m3) # Best m3

m4 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov2) # Si sex, No season
summary(m4)
confint(m4) 

m5 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov2) # No sex
summary(m5)
confint(m5) 

m6 <- lmer(X2nd25m ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov2) #No season no sex
summary(m6)
confint(m6) 

AIC(m3, m4, m5, m6) # Best m4 (m6 doesnt converge)


### BY SEX

m_f2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_f)
summary(m_f2)
confint(m_f2) # FEMALES: SLIGHT OVERLAP 0; WARNING


m_m2 <- lmer(X2nd25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov2_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP; WARNINGS

#Try with nlme to see if the warning is fixed: NOT FIXED

fm1 <- lme(X2nd25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_f)
summary(fm1)
intervals(fm1, level = 0.95) # FEMALES: Doesnt work

mm1 <- lme(X2nd25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov2_m)
summary(mm1)
intervals(mm1, level = 0.95) # MALES: doesnt work


