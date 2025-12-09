#this code can be used to classify data
library (terra)
library(imageRy)

im.list()

m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")

#rast function from terra
#layers: 1=NIR, 2=red 3=green

plot(m1992) #rgb 123

#Ex: import the image from 2006
m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")
plot(m2006)

sun<- im.import("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
plot (sun)

sunc <- im.classify(sun,num_clusters=3) #unsupervised classification

par(mfrow=c(2,1))
plot(sun)
plot(sunc)
