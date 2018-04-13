
library(rgdal)

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

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0302 <- as.data.frame(M0302)
M0302 <- M0302[, c(4,5)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]

M0301 <- as.data.frame(M0301)
M0301 <- M0301[, c(6,7)]





# LOAD SWEDEN AND NORWAY 
COUNTRIES <- readOGR("C:/Users/ana.sanz/Documents/MASTER THESIS/Scripts/NHBD/Data", "countries.removed.islands")       
COUNTRIES <- COUNTRIES[which(COUNTRIES$ISO %in% c("NOR","SWE")),]           
plot(COUNTRIES)
plot(M0301, add = TRUE)

M0301<-as.data.frame(M0301)
M0301 <- M0301[c(1,1987), c(6,7)]

Lines(list(Line(rbind(nat[i, ], est[i,]))), as.character(i))

l <- Lines(list(Line(M0301)), ID = "a")
SpatialLines(l)

l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

Sl1 <- Line(l1)
Sl2 <- Line(l2)

S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")


, 
Lines(posM0301[1, ], posM0301[1987, ])

