library(terra)
library(imageRy)
im.list()
#using data from sentinelle programme to see concentration of NO2 during covid 

EN01 <- im.import("EN_01.png")
EN01<-flip(EN01)
plot(EN01)

EN01 #the radiometric resolution of EN01 is 8 bit (it is important to normalize the data to compare them)

EN13 <- im.import("EN_13.png")

difEN = EN01[[1]] - EN13[[1]]
plot(difEN) #in most of eu countries the values were higher in January, bc the difference is a high value (giaallo) 


install.package(ggridges)
library (ggplot2)

# import NDVI data
ndvi<-im.import("NDVI_2020")

im.ridgeline(ndvi,scale=1) #si vede un grafico solo e non quatrro perche si chiamano tutti e 4 NDI quindi uno viene scritto sopra l'altro
#change the names of the dataset 

names(ndvi)=c("feb","may","aug","nov"))
im.ridgeline(ndvi,scale=1) #a questo punto si vedono tutti e quattro

plot (ndvi)
im.ridgeline(ndvi,scale=2)

#ice melting in Greenland
gr<-im.import("greenland")

#why are we using stereografich instead of N, preche sta al èpolo nord qundi sarebbe in molti spicchi diversi. 

