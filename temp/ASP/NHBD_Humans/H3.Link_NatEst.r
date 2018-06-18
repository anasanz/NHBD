
library(dplyr)

# ---- Link ----
setwd("~/Norway/NHBD_humans")
v <- read.csv("natal_values.csv", header = TRUE) #Load extracted natal values
c <- read.csv("coef_maximal_functional_response.csv", header = TRUE, sep = ";") # Load established selection coef.
c <- c[ , which(colnames(c) %in% c("territory_", "agri5x5.estimate", "altitude.estimate", "forest5x5.estimate",
                                  "hdl5x5.estimate", "main_roads.estimate", "sec_roads.estimate",  "sec_roads.estimate.1"))]
colnames(c)[1] <- "Territory_antonio"
e <- left_join(v,c)


# ---- Explore data ----

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




