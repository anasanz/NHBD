
rm(list=ls())
library(dplyr)
library(lme4)

# 0. ---- Exploratory selection coefficients ----
# Is there a difference in the way they select main and secondary roads?
#setwd("~/Norway/NHBD_humans/Antonio")
setwd("~/Norway/NHBD_humans")
c <- read.csv("coef_Antonio_new.csv", sep = ",")
c <- c[ ,-c(1)]
colnames(c)[1] <- "Territory_antonio"

plot(c$main25m, c$X2nd25m, col = as.factor(c$Territory_antonio), pch = 16)
abline(h=0)
abline(v=0)

sum(length(which(c$main25m > 0 & c$X2nd25m > 0)) + length(which(c$main25m < 0 & c$X2nd25m < 0))) # 26/47 in the same direction

# In PCA of natal territories, they seem to have different proportions of main and secondary roads

# 1. ---- Characterize natal territories ----

setwd("~/Norway/NHBD_humans")
v <- read.csv("natal_values.csv", header = TRUE) #Load extracted natal values
v$ID_pair <- paste(v$ID_F,v$ID_M,sep = "_") # ID_pair column to add it as random in the model


  # A. ---- JOIN FEMALES-MALES (Same variable names) ----
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
natal <- bind_rows(f,m)

  # B. ---- PCA to characterize natal territories ----

library(factoextra)

n <- natal[ ,colnames(natal) %in% c("Territory_antonio", "ID", "human_1", "humanlands_1", "agri_1", "mainroad_1", "roadbuild_1",
                                    "roadens_sec1", "build_1")] # Only human-related variables
sd_n<- as.data.frame(scale(n[3:9]))
pc <- prcomp(sd_n)
fviz_pca_biplot(pc, label="var",col.var = "black") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

pc$x[ ,1] #PC1 explains 72% of the variance and separates human (+) vS non-human (-) 
          # characterized territories
          # However, PC1 doesnt capture the difference in sec. roads, which is more captured by PC2
natal$PC <- pc$x[ ,1] 
natal$PC2 <- pc$x[ ,2]


# 2. ---- Link with coefficients ----

#coef_human.csv contains the variable "closest" (minimun distance to mainroads,buildings,secroads)
#coef_human2.csv contains the variable "closest2" (minimun distance to mainroads,buildings)

setwd("~/Norway/NHBD_humans")
c <- read.csv("coef_human.csv", sep = ";")
c <- c[ ,-c(1)]
colnames(c)[1] <- "Territory_antonio"


e <- left_join(natal,c, by = "Territory_antonio")


setwd("~/Norway/NHBD_humans")
c2 <- read.csv("coef_human2.csv", sep = ";")
c2 <- c2[ ,-c(1)]
colnames(c2)[1] <- "Territory_antonio"


e <- left_join(natal,c, by = "Territory_antonio")


e$Season <- 0 # Add season (Summer/Winter = 1/0)
e$Season[grep("_s", e$Territory_antonio, ignore.case = TRUE)] <- 1

#Plot

# Main roads
plot(e$PC,e$main25m, pch = 16)
abline(lm(e$main25m ~ e$PC))

hist(e$PC)
prov <- e[-which(e$PC > 4), ] #Removing outliers

plot(prov$PC,prov$main25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov$main25m ~ prov$PC)) 
summary(lm(prov$main25m ~ prov$PC))

prov_s <- prov[which(prov$Season == 1), ] # By Seasons
prov_w <- prov[which(prov$Season == 0), ]
col_season <- c("blue","red")[as.factor(prov$Season)]
plot(prov$PC,prov$main25m, pch = 16, col = col_season)

abline(lm(prov_s$main25m ~ prov_s$PC), col = "red") #Summer
summary(lm(prov_s$main25m ~ prov_s$PC))

abline(lm(prov_w$main25m ~ prov_w$PC), col = "blue") #Winter
summary(lm(prov_w$main25m ~ prov_w$PC))

# Distance to buildings
plot(e$PC,e$cov_build , pch = 16)
abline(lm(e$cov_build ~ e$PC))

hist(e$PC)
prov <- e[-which(e$PC > 4), ] #Removing outliers

plot(prov$PC,prov$cov_build, pch = 16)
abline(lm(prov$cov_build ~ prov$PC)) # Overall trend not significant for all together
summary(lm(prov$cov_build ~ prov$PC))

# Sec roads

#With PC1
plot(e$PC,e$X2nd25m, pch = 16)
abline(lm(e$X2nd25m ~ e$PC))

hist(e$PC)
prov <- e[-which(e$PC > 4), ] #Removing outliers

plot(prov$PC,prov$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov$X2nd25m ~ prov$PC)) 
summary(lm(prov$X2nd25m ~ prov$PC))

#With PC2 (Characterize better sec.roads)
plot(e$PC2,e$X2nd25m, pch = 16)
abline(lm(e$X2nd25m ~ e$PC2))

hist(e$PC2)
prov <- e[-which(e$PC2 < -1), ] #Removing outliers (< -1 or-2?)

plot(prov$PC2,prov$X2nd25m, pch = 16) # Overall trend not significant for all together
abline(lm(prov$X2nd25m ~ prov$PC2)) 
summary(lm(prov$X2nd25m ~ prov$PC2))

# Closest cosa (minimun distance to mainroads,buildings,secroads)

#With PC1

hist(e$closest)
which(e$closest>1)
prov <- e[-which(e$PC > 6), ] #Removing outliers PC
prov <- prov[-which(prov$closest > 200), ]

plot(prov$PC,prov$closest, pch = 16) # Overall trend not significant for all together
abline(lm(prov$closest ~ prov$PC)) 
summary(lm(prov$closest ~ prov$PC))

#With PC2 (Characterize better sec.roads)

prov <- e[-which(e$PC2 < -3), ] #Removing outliers (< -1 or-2?)
prov <- prov[-which(prov$closest > 200), ]

plot(prov$PC2,prov$closest, pch = 16) # Overall trend not significant for all together
abline(lm(prov$closest ~ prov$PC2)) 
summary(lm(prov$closest ~ prov$PC2))

# Closest cosa2 (minimun distance to mainroads,buildings)

#With PC1

hist(e$closest2)
hist(e$PC)
prov <- e[-which(e$PC > 6), ] #Removing outliers PC
prov <- prov[-which(prov$closest2 > 2), ]

plot(prov$PC,prov$closest2, pch = 16) # Overall trend not significant for all together
abline(lm(prov$closest2 ~ prov$PC)) 
summary(lm(prov$closest2 ~ prov$PC))


  # A. ---- SIMPLE MODEL (Not very interesting, we need random effects) ----
    # 1. ---- RESPONSE: MAIN ROADS COEFFICIENT ----

m1 <- lm(main25m ~ PC + forest_1 + bear_1 + Sex + Season, prov)
summary(m1)

#Females 

prov_f <- prov[which(prov$Sex == "F"), ] 

m_f <- lm(main25m ~ PC + forest_1 + bear_1 + Season, prov_f)
summary(m_f)

col_season <- c("blue","red")[as.factor(prov_f$Season)]
plot(prov_f$PC,prov_f$main25m, pch = 16, col = col_season)
abline(lm(prov_f$main25m ~ prov_f$PC))

#Males

prov_m <- prov[which(prov$Sex == "M"), ] 

m_m <- lm(main25m ~ PC + forest_1 + bear_1 + Season, prov_m)
summary(m_m)

col_season <- c("blue","red")[as.factor(prov_m$Season)]
plot(prov_m$PC,prov_m$main25m, pch = 16, col = col_season)
abline(lm(prov_m$main25m ~ prov_m$PC))

  # 2. RESPONSE: DISTANCE TO BUILDINGS COEFFICIENT





  # B. ---- INCLUDE PAIR AS RANDOM ----
    # ---- 1. RESPONSE: MAIN ROADS COEFFICIENT ----

prov <- e[-which(e$PC > 4), ] 
prov_f <- prov[which(prov$Sex == "F"), ] 
prov_m <- prov[which(prov$Sex == "M"), ] 


# ALL

m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
              Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2)
confint(m2) #Slight overlap

m3 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3) #Slight overlap

AIC(m2,m3) # Better without

m4 <- lmer(main25m ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) # Overlap

m5 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) #Slight overlap

m6 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

AIC(m3, m4, m5, m6) # Better without
summary(m6)
confint(m6) # Overlap


# BY SEX

m_f2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: DOESN'T OVERLAP 0; WARNINGS
warnings()


m_m2 <- lmer(main25m ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: DOESN'T OVERLAP 0; WARNINGS

#Try with nlme to see if the warning is fixed

library(nlme)

fm1 <- lme(main25m ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) # NO WARNINGS; Very slight overlap



        
    # ---- 2. RESPONSE: DISTANCE TO BUILDINGS COEFFICIENT ----

#ALL

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
confint(m4) # Overlap

m5 <- lmer(cov_build ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) # Overlap

m6 <- lmer(cov_build ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

AIC(m3, m4, m5, m6) # Better without
summary(m6)
confint(m6) # Overlap


# BY SEX

#Females
prov_f <- prov[which(prov$Sex == "F"), ] 
m_f2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; Convergence problems?
warnings()

#Males
prov_m <- prov[which(prov$Sex == "M"), ] 
m_m2 <- lmer(cov_build ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; Convergence problems?

#Try with nlme to see if the warning is fixed
library(nlme)

fm1 <- lme(cov_build ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) 

    # ---- 3. RESPONSE: CLOSEST COSA - PC ----

#ALL

m2 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2)
confint(m2) 

m3 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3) # WARNINGS

AIC(m2,m3) # Better without

m4 <- lmer(closest ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) # WARNINGS, Overlap

m5 <- lmer(closest ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex
summary(m5)
confint(m5) # WARNINGS, Overlap

m6 <- lmer(closest ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

AIC(m3, m4, m5, m6) # Better without
summary(m6)
confint(m6) # WARNINGS, Overlap


# BY SEX

#Females
prov_f <- prov[which(prov$Sex == "F"), ] 
m_f2 <- lmer(closest ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: OVERLAP 0; Convergence problems?
warnings()

#Males
prov_m <- prov[which(prov$Sex == "M"), ] 
m_m2 <- lmer(closest ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; Convergence problems?

#Try with nlme to see if the warning is fixed
library(nlme)

fm1 <- lme(closest ~ PC + forest_1 + bear_1 + Season,
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) # Cant get them...

    # ---- 4. RESPONSE: CLOSEST COSA 2 - PC ----

#ALL

m2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex + 
             Season + PC*Sex + (1|ID_pair), data = prov) # Sex, With interaction, season
summary(m2)
confint(m2) # 1 WARNING, OVERLAP

m3 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov) #Without interaction
summary(m3)
confint(m3) # OVERLAP

AIC(m2,m3) # Better without

m4 <- lmer(closest2 ~ PC + forest_1 + bear_1 + Sex
           + (1|ID_pair), data = prov) # Si sex, No season
summary(m4)
confint(m4) # NO WARNINGS!! Overlap

m5 <- lmer(closest2 ~ PC + forest_1 + bear_1 +
             Season + (1|ID_pair), data = prov) # No sex, Si season
summary(m5)
confint(m5) # NO WARNINGS!! Overlap

m6 <- lmer(closest2 ~ PC + forest_1 + bear_1
           + (1|ID_pair), data = prov) #No season no sex

AIC(m3, m4, m5, m6) # Same with and without season (m5 and m6)
summary(m6)
confint(m6) # NO WARNINGS!! Overlap


# BY SEX

#Females
prov_f <- prov[which(prov$Sex == "F"), ] 
m_f2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f) # SI SEASON
summary(m_f2)
confint(m_f2) # FEMALES: WARNINGS, OVERLAP
warnings()

m_f3 <- lmer(closest2 ~ PC + forest_1 + bear_1 + (1|ID_pair), data = prov_f) # NO SEASON
summary(m_f3)
confint(m_f3) # FEMALES: NO WARNINGS!!, OVERLAP (Is season giving problems?)

#Males
prov_m <- prov[which(prov$Sex == "M"), ] 
m_m2 <- lmer(closest2 ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m) # SI SEASON
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; Convergence problems?

m_m3 <- lmer(closest2 ~ PC + forest_1 + bear_1 + (1|ID_pair), data = prov_m) # NO SEASON
summary(m_m3)
confint(m_m3) # MALES: OVERLAP 0; Convergence problems?

#Try with nlme to see if the warning is fixed
library(nlme)

#Females: No effect
fm1 <- lme(closest2 ~ PC + forest_1 + bear_1 + Season, # SI SEASON
           random = ~ 1|ID_pair, data = prov_f)
summary(fm1)
intervals(fm1, level = 0.95) 

fm2 <- lme(closest2 ~ PC + forest_1 + bear_1,# NO SEASON 
           random = ~ 1|ID_pair, data = prov_f)
summary(fm2)
intervals(fm2, level = 0.95)  # NO WARNINGS

#Males: Almost significant :O
mm1 <- lme(closest2 ~ PC + forest_1 + bear_1 + Season, # SI SEASON
           random = ~ 1|ID_pair, data = prov_m)
summary(mm1)
intervals(mm1, level = 0.95) 

mm2 <- lme(closest2 ~ PC + forest_1 + bear_1,# NO SEASON 
           random = ~ 1|ID_pair, data = prov_m)
summary(mm2)
intervals(mm2, level = 0.95)  # NO WARNINGS

