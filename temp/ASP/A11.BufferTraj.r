
library(rgdal)
library(rgeos)

## ---1. SORT OUT TRAJECTORIES ----

M0301 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0301")
M0302 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0302")
M0609 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0609")
M0902 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0902")
M0908 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0908")
M0912 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0912")
M1104 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1104")
M1105 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1105")
M1108 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1108")
M1112 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1112")
M1206 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1206$")
M1406 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1406$")
M1408 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1408$")

# CREATE DF

dfM0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

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

M1105 <- as.data.frame(M1105)
M1105 <- M1105[, c(4,5)]

M1108 <- as.data.frame(M1108)
M1108 <- M1108[, c(4,5)]

M1112 <- as.data.frame(M1112)
M1112 <- M1112[, c(4,5)]

M1206 <- as.data.frame(M1206)
M1206 <- M1206[, c(6,7)]

M1406 <- as.data.frame(M1406)
M1406 <- M1406[, c(6,7)]

M1408 <- as.data.frame(M1408)
M1408 <- M1408[, c(6,7)]

j <- list(M0301, M0302, M0609, M0902, M0908, M0912, M1104, M1105, M1108, M1112, 
          M1206, M1406, M1408)


## ---2. CREATE SPLINES ----


l <- vector("list", length(j))

for (i in seq_along(l)) {

l[[i]] <- Lines(list(Line(rbind(j[[i]][1, ], j[[i]][nrow(j[[i]]), ]))), as.character(i))
}

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

## ---3. CREATE BUFFERS ----

# Distance: Points trajectory -> Straight line


M0301 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0301")
M0302 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0302")
M0609 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0609")
M0902 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0902")
M0908 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0908")
M0912 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M0912")
M1104 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1104")
M1105 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1105")
M1108 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1108")
M1112 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "M1112")
M1206 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1206$")
M1406 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1406$")
M1408 <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Data/GPS positions", "XYM1408$")

t <- list(M0301, M0302, M0609, M0902, M0908, M0912, M1104, M1105, M1108, M1112, 
          M1206, M1406, M1408)

proj4string(spl) <- proj4string(M0301)
proj4string(t) <- proj4string(M0301)

md <- list()

for (i in 1:13){
d <- as.vector(gDistance(t[[i]], spl[i], byid = TRUE))
md [i] <- max(d) 
}

save(md,file = "buf_dist.RData")

