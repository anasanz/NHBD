rm(list=ls())


#try distance 
library(survival)
setwd("C:/My_documents/PhD/natal_habitat_biased_dispersal/predict_interactions")
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/Datos")

e<-read.csv("Data_NHBD_id_wolf_density.csv")
e<-e[,-c(1,2)]


head(e)
e1 <- as.data.frame(e[,c("Year","human_1", "humanlands_1", "agri_1", "forest_1", "mires_1",
                         "water_1", "mountains_1", "roadens_sec1", "mainroad_1","bear_1", "dem_1","slope_1",
                         "roughness_1", "roadbuild_1", "moose_dens")])

row.names(e1) <- e$X

d <- dist(e1,method = "euclidean")
df <- data.frame(X=e$x,Y=e$y)
coordinates(df)<-df 
library(rgeos)
real.dist <- rgeos::gDistance(df,byid = T )


class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]
d2 <- as.vector(d1)
real.dist1 <- as.vector(real.dist)
plot(d2[1:5000]~real.dist1[1:5000])
plot(d2[1:50000]~real.dist1[1:50000])
plot(d2[10000:50000]~real.dist1[10000:50000], cex = 0.1)
plot(d2[1000000:1500000]~real.dist1[1000000:1500000], cex = 0.1)

ID <- sample(length(d2), 100000)
plot(d2[ID]~real.dist1[ID], cex = 0.1)
abline(h = 115, col = "red") # Short
abline(v = 40000, col = "red")
abline(h = 98, col = "blue") # Medium
abline(v = 200000, col = "blue")
abline(h = 187, col = "orange") #Long
abline(h = 119, col = "green") #medLong

length(d2)

head(real.dist1)

#####
ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])

}

e$Category <- as.character(e$Category)

################################
#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
#p <- read.csv("ManyClusters")
#Save for submission
#p$ID_individual <- as.numeric(as.factor(p$ID_individual))
#p <- p[, c(5,8,11,30:35)]
#p$wolf_density <- e$wolf_density
#p$distance <- e$distance

#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
#f <- read.csv("ManyClusterMethods")
#p$Clusters_hier <- f$Clusters_hier
#p$Clusters_pam <- f$Clusters_pam

#p <- p[, c(1:3,10,4:9,12,13,11)]

#setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 2/Last version/Submitted")
#write.csv(p,"Habitat_definition.csv")

################################

#Add dispersal distances
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data")
disper <- read.delim("C:/Users/Ana/Desktop/MASTER THESIS/Data/dispersal_p.txt")
disperB<-disper[ ,c("X_birth","Y_birth")]
disperE<-disper[ ,c("X_Established","Y_Established")]
x1<-as.matrix(disperB)
x2<-as.matrix(disperE)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
dist <- NULL
for(i in 1:nrow(x1)) dist[i] <- euc.dist(x1[i,],x2[i,])
dist #Vector with distances between points
d<-as.data.frame(dist)
disper$dist <- d$dist
colnames(disper)[2] <- "ID_individual"
disper <- disper[ ,c("ID_individual","dist")]

library(dplyr)
e <- left_join(e,disper)


cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + strata(ID_individual), cd)

c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)
#When the distance increases, the probability of having the event decreases: NHBD

short <- subset(cd,dist <= 40000)
mean(short$distance)
c <- clogit(Category ~ Clusters   + wolf_density * distance  + Sex * distance + strata(ID_individual), short)
summary(c)
confint(c,level = 0.95)

medium <- subset(cd,40000 < dist & dist <= 200000)
mean(medium$distance)
c <- clogit(Category ~ Clusters   + wolf_density * distance  + Sex * distance + strata(ID_individual), medium)
summary(c)
confint(c,level = 0.95)

long <- subset(cd,dist > 200000)
mean(long$distance)
c <- clogit(Category ~ Clusters   + wolf_density * distance  + Sex * distance + strata(ID_individual), long)
summary(c)
confint(c,level = 0.95)

medlong <- subset(cd,dist > 40000)
mean(medlong$distance)
c <- clogit(Category ~ Clusters   + wolf_density * E  + Sex * E + strata(ID_individual), medlong)
summary(c)
confint(c,level = 0.95)

plot(cd$distance ~ cd$dist, cex = 0.1)
abline(h = 115, col = "red") # Short
abline(v = 40000, col = "red")
abline(h = 98, col = "blue") # Medium
abline(v = 200000, col = "blue")
abline(h = 187, col = "orange") #Long
abline(h = 119, col = "green") #medLong

short.strange <- short[short$ID_individual %in% short[which(short$distance>500),]$ID_individual,]
short.strange$ID_individual<-as.character(short.strange$ID_individual)
for(i in 1:length(unique(short.strange$ID_individual))){
 tmp <-  short.strange[short.strange$ID_individual %in% as.character(unique(short.strange$ID_individual)[i]), ]
 plot(tmp$y~tmp$x)
 tmp1 <- tmp[tmp$Category %in% 1, ]
 points(tmp1$y~tmp1$x, pch=16)
  
}

#Adding the dispersal distance:
c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + dist * distance + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)

########################
#### PREDICTIONS #######
########################
df <- list(distance= seq( min(cd$distance),600, by=10 )
                 , Sex=c("M","F")
                 , wolf_density= mean(e$wolf_density))
new.df <- expand.grid(df)
new.df$pred <- predict(c,newdata=new.df, type="risk", reference=c("sample") )


pred.M <- new.df[new.df$Sex=="M",]
pred.F <- new.df[new.df$Sex=="F",]

plot(pred.F$pred~pred.F$distance, col="red", xlab="distance", ylab="Linear predictor", ylim=c(0,10))
abline(h=1, lty=2)
points(pred.M$pred~pred.M$distance, col="blue")
legend("topleft", legend=c("female", "male"), col=c("red", "blue"), pch=c(16,16))


########################
#### PREDICTIONS #######
########################
df <- list(distance= mean(e$distance)
           , Sex=c("M","F")
           , wolf_density= seq(0,10, by=0.5))
new.df <- expand.grid(df)
new.df$pred <- predict(c,newdata=new.df, type="risk", reference=c("sample") )


pred.M <- new.df[new.df$Sex=="M",]
pred.F <- new.df[new.df$Sex=="F",]

plot(pred.F$pred~pred.F$wolf_density, col="red", xlab="wolf density", ylab="Linear predictor", ylim=c(0,10))
abline(h=1, lty=2)
points(pred.M$pred~pred.M$wolf_density, col="blue")
legend("topleft", legend=c("female", "male"), col=c("red", "blue"), pch=c(16,16))


