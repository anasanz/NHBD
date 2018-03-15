
setwd("C:/Users/Ana/Desktop/MASTER THESIS/Data/Random walks")
load("medlong_points.RData")
ml<-g
load("short_points.RData")
s<-g

points<-c(s,ml)
save(points,file = "points.RData")
load("stack.RData")

p<-list()
for(j in 1:271){
  o<-list()
  for (i in 1:11){
    a_v<-extract(stack,g[[j]][[i]],method='simple',buffer=17841,
                  small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,
                  sp=TRUE)
    o[[i]]<-a_v
  }
  print(j)
  p[[j]]<-o
}
coordinates(points[[1]][[1]])


#Extract observation number 1589

id<-extract(stack,c,method='simple',buffer=17841,
             small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,
             sp=TRUE)

c<-matrix(c(349109.6,6375946),ncol = 2)
c<-SpatialPoints(c,proj4string = CRS(proj4string(stack)))
proj4string(c) <- CRS(proj4string(stack))

#It falls out of the study area. Repeated the creation of the point
# and now extract the values for that point (rdm_sp from script 7)

coordinates(rdm_sp)
c<-matrix(c(374803.2,6412572),ncol = 2)
id<-extract(stack,c,method='simple',buffer=17841,
            small=TRUE,fun=mean,na.rm=TRUE,df=TRUE,factors=TRUE,
            sp=TRUE)

c<-matrix(c(374803.2,6412572),ncol = 2)
c<-SpatialPoints(c,proj4string = CRS(proj4string(stack)))
proj4string(c) <- CRS(proj4string(stack))


str(id)
