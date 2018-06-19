
library(dplyr)

# ---- 1. Link ----
setwd("~/Norway/NHBD_humans")
v <- read.csv("natal_values.csv", header = TRUE) #Load extracted natal values
c <- read.csv("coef_maximal_functional_response.csv", header = TRUE, sep = ";") # Load established selection coef.
c <- c[ , which(colnames(c) %in% c("territory_", "agri5x5.estimate", "altitude.estimate", "forest5x5.estimate",
                                  "hdl5x5.estimate", "main_roads.estimate", "sec_roads.estimate",  "sec_roads.estimate.1"))]
colnames(c)[1] <- "Territory_antonio"
e <- left_join(v,c)


# ---- 2. Explore RAW data ----

    # ---- FEMALES ----

#Selection of main roads
plot(e$F_mainroad_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$F_mainroad_1))

plot(e$F_human_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$F_human_1))

plot(e$F_roadbuild_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$F_roadbuild_1))

#Selection of humanlands

plot(e$F_humanlands_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$F_humanlands_1))

plot(e$F_human_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$F_human_1))

plot(e$F_roadbuild_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$F_roadbuild_1))

# Selection secondary roads

plot(e$F_roadens_sec1,e$sec_roads.estimate, pch = 16)
abline(lm(e$sec_roads.estimate ~ e$F_roadens_sec1)) # Est 1

plot(e$F_roadens_sec1,e$sec_roads.estimate.1, pch = 16)
abline(lm(e$sec_roads.estimate.1 ~ e$F_roadens_sec1)) # Est 2
                                                      # Same value of road sec in natal territory?
# Selection agricultural lands

plot(e$F_agri_1,e$agri5x5.estimate, pch = 16)
abline(lm(e$agri5x5.estimate ~ e$F_agri_1))

# Selection forest

plot(e$F_forest_1,e$forest5x5.estimate, pch = 16)
abline(lm(e$forest5x5.estimate ~ e$F_forest_1))

    # ---- MALES ----

#Selection of main roads

plot(e$M_mainroad_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$M_mainroad_1))

plot(e$M_human_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$M_human_1))

plot(e$M_roadbuild_1,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$M_roadbuild_1))

#Selection of humanlands

plot(e$M_humanlands_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$M_humanlands_1))

plot(e$M_human_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$M_human_1))

plot(e$M_roadbuild_1,e$hdl5x5.estimate, pch = 16)
abline(lm(e$hdl5x5.estimate ~ e$M_roadbuild_1))

# Selection secondary roads

plot(e$M_roadens_sec1,e$sec_roads.estimate, pch = 16)
abline(lm(e$sec_roads.estimate ~ e$M_roadens_sec1)) # Est 1

plot(e$M_roadens_sec1,e$sec_roads.estimate.1, pch = 16)
abline(lm(e$sec_roads.estimate.1 ~ e$M_roadens_sec1)) # Est 2
# Same value of road sec in natal territory?
# Selection agricultural lands

plot(e$M_agri_1,e$agri5x5.estimate, pch = 16)
abline(lm(e$agri5x5.estimate ~ e$M_agri_1))

# Selection forest

plot(e$M_forest_1,e$forest5x5.estimate, pch = 16)
abline(lm(e$forest5x5.estimate ~ e$M_forest_1))






# ---- 3. Explore characterized territories

    # A. ---- Characterize natal territories ----
v <- read.csv("natal_values.csv", header = TRUE) #Load extracted natal values


  # 1. JOIN FEMALES-MALES (Same variable names)

    # First create ID_pair column to add it as random in the model
v$ID_pair <- paste(v$ID_F,v$ID_M,sep = "_")
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

  # 2. PCA

library(factoextra)

n <- natal[ ,colnames(natal) %in% c("Territory_antonio", "ID", "human_1", "humanlands_1", "agri_1", "mainroad_1", "roadbuild_1",
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

pc$x[ ,1] #PC1 explains 72% of the variance and separates human (+) vS non-human (-) 
          # characterized territories
natal$PC <- pc$x[ ,1]

    # B. ---- Graphs ----

c <- read.csv("coef_maximal_functional_response.csv", header = TRUE, sep = ";") # Load established selection coef.
c <- c[ , which(colnames(c) %in% c("territory_", "agri5x5.estimate", "altitude.estimate", "forest5x5.estimate",
                                   "hdl5x5.estimate", "main_roads.estimate", "sec_roads.estimate"))]
colnames(c)[1] <- "Territory_antonio"

e <- left_join(natal,c, by = "Territory_antonio")

e$Season <- 0 # Add season (Summer/Winter = 1/0)
e$Season[grep("_s", e$Territory_antonio, ignore.case = TRUE)] <- 1

#Plot all
plot(e$PC,e$main_roads.estimate, pch = 16)
abline(lm(e$main_roads.estimate ~ e$PC))

hist(e$PC)
prov <- e[-which(e$PC > 4), ] #Removing outliers
prov_s <- prov[which(prov$Season == 1), ] #Seasons
prov_w <- prov[which(prov$Season == 0), ]

col_season <- c("blue","red")[as.factor(prov$Season)]
plot(prov$PC,prov$main_roads.estimate, pch = 16, col = col_season)

abline(lm(prov$main_roads.estimate ~ prov$PC)) # Overall trend
summary(lm(prov$main_roads.estimate ~ prov$PC))

abline(lm(prov_s$main_roads.estimate ~ prov_s$PC), col = "red") #Summer
summary(lm(prov_s$main_roads.estimate ~ prov_s$PC))

abline(lm(prov_w$main_roads.estimate ~ prov_w$PC), col = "blue") #Winter
summary(lm(prov_w$main_roads.estimate ~ prov_w$PC))

#SIMPLE MODEL

m1 <- lm(main_roads.estimate ~ PC + forest_1 + bear_1 + Sex + Season, prov)
summary(m1)

#Females 

prov_f <- prov[which(prov$Sex == "F"), ] 

m_f <- lm(main_roads.estimate ~ PC + forest_1 + bear_1 + Season, prov_f)
summary(m_f)

col_season <- c("blue","red")[as.factor(prov_f$Season)]
plot(prov_f$PC,prov_f$main_roads.estimate, pch = 16, col = col_season)
abline(lm(prov_f$main_roads.estimate ~ prov_f$PC))

#Males

prov_m <- prov[which(prov$Sex == "M"), ] 

m_m <- lm(main_roads.estimate ~ PC + forest_1 + bear_1 + Season, prov_m)
summary(m_m)

col_season <- c("blue","red")[as.factor(prov_m$Season)]
plot(prov_m$PC,prov_m$main_roads.estimate, pch = 16, col = col_season)
abline(lm(prov_m$main_roads.estimate ~ prov_m$PC))


# INCLUDE PAIR AS RANDOM

library(lme4)

m2 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1 + Sex + 
              Season + PC*Sex + (1|ID_pair), data = prov)
summary(m2)
confint(m2)

m3 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1 + Sex + 
             Season + (1|ID_pair), data = prov)
summary(m3)
confint(m3) # 

# By sex
m_f2 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_f)
summary(m_f2)
confint(m_f2) # FEMALES: DOESN'T OVERLAP 0; Convergence problems?
warnings()


m_m2 <- lmer(main_roads.estimate ~ PC + forest_1 + bear_1 + 
               Season + (1|ID_pair), data = prov_m)
summary(m_m2)
confint(m_m2) # MALES: OVERLAP 0; Convergence problems?


        