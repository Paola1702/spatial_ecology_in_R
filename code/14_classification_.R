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
m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")

m1992c <- im.classify(m1992, num_clusters=2)
# class 1 = human related areas and water
# class 2 = rain forest

m2006c <- im.classify(m2006, num_clusters=2)
# class 1 = forest
# class 2 = human related areas and water

# calculating frequencies
f1992 <- freq(m1992c)

# total number with ncell function
tot1992c <- ncell(m1992c)
tot1992 
#calculate proportions
prop1992=f1992$count/tot1992c

#%
perc1992=prop1992*100


#mi sono persa
f2006 <- freq(m2006c)
tot2006 <- ncell(m2006c)

perc2006 = f2006 * 100 / tot2006
# class 1 = forest = 45
# class 2 = human related areas and water = 55

# building a dataframe
class <- c("Forest","Human")
perc1992 <- c(83, 17)
perc2006 <- c(45, 55)

tabout <- data.frame(class, perc1992, perc2006)
tabout

#use ggplot2
library(ggplot2)
# final graph using gggplot package
ggplot(tabout, aes(x=class, y=perc1992, color=class)) + geom_bar(stat="identity", fill="white")
ggplot(tabout, aes(x=class, y=perc2006, color=class)) + geom_bar(stat="identity", fill="white")

#package to use is patchwork to use to avoid parfrow
install.packages("patchwork")
library(patchwork)
p1<- ggplot(tabout, aes(x=class, y=perc1992, color=class)) + geom_bar(stat="identity", fill="white")+ ylim(c(0,100))
p2<-ggplot(tabout, aes(x=class, y=perc2006, color=class)) + geom_bar(stat="identity", fill="white")+ ylim(c(0,100))
p1  + p2 #one next to the other
p1 / p2 #one on top to the other
