getwd()
setwd("C:/Users/HS/Documents/GitHub/leaf_multivariate")
leaf<-read.csv("./data/leaf.csv")
head(leaf)
colnames(leaf)<-c("X1","X2",
                  "X3","X4","X5","X6","X7","X8","X9",
                  "X10","X11","X12","X13","X14","X15","X16")

#data division : shape / texture 
leaf_shape<-leaf[,c(3:9)]
leaf_texture<-leaf[,c(10:16)]
head(leaf_shape)
head(leaf_texture)

#data scoping
colMeans(leaf_shape)
var(leaf_shape)
round(var(leaf_shape),6)
round(cor(leaf_shape),6)

leaff_shape<-scale(leaf_shape, center=T, scale=T) #data 표준화
colMeans(leaff_shape) #평균이 0 에 가까워짐 
var(leaff_shape) 
round(var(leaff_shape),6)
round(cor(leaff_shape),6)

#factor anaylsis
summary(princomp(leaff_shape)) #2 or 3
plot(princomp(leaff_shape))
pairs(leaff_shape)
(factanal(leaff_shape,factors=1))
(factanal(leaff_shape,factors=2)) #2
(factanal(leaff_shape,factors=3))

#rotation
(factanal(leaff_shape,factors=2,rotation="none"))





#clustering
install.packages("NbClust")
library(NbClust)

nc <- NbClust(leaf_shape, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
#3개 군집
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(leaf_shape)

#K-Means Clustering
dist.leaff_shape<-dist(leaff_shape,method="euclidean")
h.complete<-hclust(dist.leaff_shape,method="complete")
plot(h.complete,hang=-1,main="complete linkage")
cutree(h.complete,h=7000)

h.single<-hclust(dist.leaff_shape,method="single")
plot(h.single,hang=-1,main="single linkage")
cutree(h.single,h=1900)

h.average<-hclust(dist.leaff_shape,method="average")
plot(h.average,hang=-1,main="average linkage")
cutree(h.average,h=4000)

kmeans.util<-kmeans(leaff_shape,3)
kmeans.util
pairs(leaff_shape,col=kmeans.util$cluster+1,pch=16)

#

