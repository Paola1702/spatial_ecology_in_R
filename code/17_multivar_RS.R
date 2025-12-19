#related to multivariate analysis 
library(imageRy)
library(terra)
library(ggplot2)
library(patchwork) 
library(viridis)

#using the sentinel data 
sent<-im.import("sentinel.png")

#we can plot every single band
#first band
p1<-im.ggplot(sent[[1]])
#second band
p2<-im.ggplot(sent[[2]])
#third band
p3<-im.ggplot(sent[[3]])

p1 + p2 + p3

pairs(sent)

#names the bands
names(sent)<-c("b01_nir", "b02_red", "b03_green")
pairs(sent)

#lets do multivariate analysis
sentpc<-im.pca(sent) #ho grafici diversi da quelli del prof

pcsd3<-focal(sentpc[[1]], w=3, fun="sd")
plot(pcsd3)
sd3<-focal(sent[[1]], w=3, fun="sd")

p0<-im.ggplot(sd3)
p1<-im.ggplot(pcsd3)

p0 + p1

