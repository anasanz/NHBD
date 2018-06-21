
library(dplyr) 
library(broom)

# ---- 1. Repeating with variables Antonio

setwd("~/Norway/NHBD_humans/Antonio")
d <- read.csv("covariates_Antonio.csv")

by_territory <- group_by (d, territory) # Grouping variable data by territory

by_territory[ ,c(7:16)] <- scale(by_territory[7:16], scale=TRUE) # Standardized variables 

summary_maximal <- do (by_territory, # Maximal model For each territory_year
                      glance (glm (used ~ as.numeric (Season) +
                                     humland_pro + agri_pro + forest_pro + 
                                     water_pro + mountains_pro+
                                     tri5 + clip_dem +
                                     main25m + X2nd25m,
                                   family = binomial (link = "logit"),
                                   data = .)))

setwd("~/Norway/NHBD_humans/Antonio")
write.csv(summary_maximal,"summary_maximal.csv")

coef_maximal <- do (by_territory,
                    tidy (glm (used ~ as.numeric (Season) +
                                   humland_pro + agri_pro + forest_pro + 
                                   water_pro + mountains_pro+
                                   tri5 + clip_dem +
                                   main25m + X2nd25m,
                                 family = binomial (link = "logit"),
                                 data = .)))

setwd("~/Norway/NHBD_humans/Antonio")
write.csv(coef_maximal,"coef_maximal.csv")

# PCA

library(tidyr)
library(factoextra)

estimates <- coef_maximal[ ,c(1:3)]
coef <- spread(estimates, key = "term", value = "estimate")
coef <- coef[ ,-c(8)] # Delete mountains, many NA

coef <- coef[complete.cases(coef), ]

pc <- prcomp(coef[, c(3:10)])

fviz_pca_biplot(pc, label="var",col.var = "black") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

