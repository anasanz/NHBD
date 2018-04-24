
library(rgdal)
library(rgeos)

## ---1. SORT OUT TRAJECTORIES ----

M0909<- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0909") # Short
M0907<- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0907") # Short
M0302 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0302") # Medium
M0609 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0609") # Medium
M0902 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0902") # Medium
M0908 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0908") # Medium
M0912 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0912") # Medium
M1104 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1104") # Medium
M1112 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1112") # Medium
M1406 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1406$") # Medium
M1408 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1408$") # Medium and Long
M0301 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0301") # Medium and Long
M1105 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1105") # Medium and Long

# CREATE DF

dfM0909 <- as.data.frame(M0909)
M0909 <- M0909[, c(6,7)]

dfM0907 <- as.data.frame(M0907)
M0907 <- M0907[, c(3,4)]

M0302 <- as.data.frame(M0302)
M0302 <- M0302[, c(4,5)]

M0609 <- as.data.frame(M0609)
M0609 <- M0609[, c(6,7)]

M0902 <- as.data.frame(M0902)
M0902 <- M0902[, c(6,7)]

M0908 <- as.data.frame(M0908)
M0908 <- M0908[, c(4,5)]

M0912 <- as.data.frame(M0912)
M0912 <- M0912[, c(6,7)]

M1104 <- as.data.frame(M1104)
M1104 <- M1104[, c(4,5)]

M1112 <- as.data.frame(M1112)
M1112 <- M1112[, c(4,5)]

M1406 <- as.data.frame(M1406)
M1406 <- M1406[, c(6,7)]

M1408 <- as.data.frame(M1408)
M1408 <- M1408[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M1105 <- as.data.frame(M1105)
M1105 <- M1105[, c(4,5)]


j <- list(M0909, M0907, M0302, M0609, M0902, M0908, M0912, M1104, M1112, M1406, 
          M1408, M0301, M1105)


## ---2. CREATE SPLINES ----


l <- vector("list", length(j))

for (i in seq_along(l)) {

l[[i]] <- Lines(list(Line(rbind(j[[i]][1, ], j[[i]][nrow(j[[i]]), ]))), as.character(i))
}

spl <- SpatialLines(l)
# PLOT TO CHECK

# LOAD SWEDEN AND NORWAY 
COUNTRIES <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Scripts/NHBD/Data", "countries.removed.islands")       
COUNTRIES <- COUNTRIES[which(COUNTRIES$ISO %in% c("NOR","SWE")),]           

# PLOT TO CHECK 
plot(COUNTRIES)
# jus select a few of IDS
ID <- sample(1:length(spl), 13)
col <- rainbow(length(ID))

for(i in 1:length(ID)){
  plot(spl[ID[i]], add = TRUE, col=col[i])
}

## ---3. CHECK DISTANCES

# Distance: Points trajectory -> Straight line

M0909<- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0909") # Short
M0907<- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0907") # Short
M0302 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0302") # Medium
M0609 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0609") # Medium
M0902 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0902") # Medium
M0908 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0908") # Medium
M0912 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0912") # Medium
M1104 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1104") # Medium
M1112 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1112") # Medium
M1406 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1406$") # Medium
M1408 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1408$") # Medium and Long
M0301 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0301") # Medium and Long
M1105 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1105") # Medium and Long

t <- list(M0909, M0907, M0302, M0609, M0902, M0908, M0912, M1104, M1112, M1406, 
          M1408, M0301, M1105)

proj4string(spl) <- proj4string(M0301)
proj4string(t) <- proj4string(M0301)

md <- list()

for (i in 1:13){
d <- as.vector(gDistance(t[[i]], spl[i], byid = TRUE))
md [i] <- max(d) 
}


md[[1]][2] <- "Short"
md[[2]][2] <- "Short"
md[[3]][2] <- "Medium"
md[[4]][2] <- "Medium"
md[[5]][2] <- "Medium"
md[[6]][2] <- "Medium"
md[[7]][2] <- "Medium"
md[[8]][2] <- "Medium"
md[[9]][2] <- "Medium" 
md[[10]][2] <- "Medium"
md[[11]][2] <- "Medium and Long" 
md[[12]][2] <- "Medium and Long"
md[[13]][2] <- "Medium and Long"


save(md,file = "buf_dist_good.RData")



