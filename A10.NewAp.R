
#New Approach to define availability
  # 1 - Straight line from natal to established
  # 2 - Buffer of different categories (50 km - 100 km - 150 km)
  # 3 - Create random points within the buffer
  # 4 - Extract habitat characteristics
  # 5 - Clustering 6C and/OR Distance metric
  # 6 - CLR

library(sp)
library(rgdal)
library(rgeos)

# --------------- 1 Straight line from natal to established -------------------------- #

setwd("~/MASTER THESIS/Data")

disper <- read.delim("~/MASTER THESIS/Data/dispersal_p.txt")

nat<-disper[ ,c("X_birth","Y_birth")]
est<-disper[ ,c("X_Established","Y_Established")]

colnames(nat)[1] <- "X"
colnames(nat)[2] <- "Y"
colnames(est)[1] <- "X"
colnames(est)[2] <- "Y"

l <- vector("list", nrow(nat))

for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(nat[i, ], est[i,]))), as.character(i))
}

summary(l)

    # Plot 

    setwd("~/MASTER THESIS/Data/GIS")
    study_area <- readOGR(".", "hand_study_area1")
    plot(study_area)
    
    spl <- SpatialLines(l[1])
    summary(spl)
    plot(spl, add = TRUE)
    points(nat[1, ])
    points(est[1, ])
    
    spl <- SpatialLines(l[2])
    summary(spl)
    plot(spl, add = TRUE)
    points(nat[2, ])
    points(est[2, ])

spl <- SpatialLines(l)


# --------------- 2 Buffer of different categories (50 km - 100 km - 150 km) -------------------------- #

b <- gBuffer(spl,byid = TRUE, width = 25000)

  plot(study_area)
  plot(spl[1], add = TRUE)
  plot(b[1], add = TRUE)
  
  plot(study_area)
  
  for (i in seq_along(spl)) {
    plot(spl[i], add = TRUE)
    plot(b[i], add = TRUE)
  }

# --------------- 3 Create random points within the buffer -------------------------- #
