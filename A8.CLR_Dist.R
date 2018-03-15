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
class(d)
d1 <- as.matrix(d)
d1[1:10,1:10]

#####
ID <- unique(e$ID_individual)
e$distance <- 0
for(i in 1:length(ID)){
  IDD <- which(e$ID_individual==ID[i])
  
  e$distance[IDD]  <- c(d1[IDD[13], IDD[1:13]])

}

e$Category <- as.character(e$Category)

################################
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
p <- read.csv("ManyClusters")
#Save for submission
p$ID_individual <- as.numeric(as.factor(p$ID_individual))
p <- p[, c(5,8,11,30:35)]
p$wolf_density <- e$wolf_density
p$distance <- e$distance

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Multivariate")
f <- read.csv("ManyClusterMethods")
p$Clusters_hier <- f$Clusters_hier
p$Clusters_pam <- f$Clusters_pam

p <- p[, c(1:3,10,4:9,12,13,11)]

setwd("C:/Users/Ana/Desktop/MASTER THESIS/Publication/SUBMISSION 2/Last version/Submitted")
write.csv(p,"Habitat_definition.csv")

################################


cd <- e[e$Category!="Natal",]

cd$Category[cd$Category == "Established"] <- 1
cd$Category[cd$Category == "Available"] <- 0
cd$Category <-as.numeric(cd$Category)


c <- clogit(Category ~ distance + strata(ID_individual), cd)

c <- clogit(Category ~ distance  + distance * Sex + distance * wolf_density + strata(ID_individual), cd)
summary(c)
confint(c,level = 0.95)
#When the distance increases, the probability of having the event decreases: NHBD

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


