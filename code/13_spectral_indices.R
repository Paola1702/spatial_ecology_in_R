#spectral indices from satellite images
library(terra)
library(imageRy)

im.list()

#importing data:
m1992<- im.import("matogrosso_l5_1992219_lrg.jpg")

im.plotRGB(m1992,r=1,g=2,b=3)

#all the vegertation is blue and the bair soil is yellow
im.plotRGB(m1992,r=2,g=3,b=1)

#exercise import the 2006 image 

m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")

# band 1 = NIR
# band 2 = red
# band 3 = green

im.plotRGB(m2006, r=1, g=2, b=3)
im.plotRGB(m2006, r=3, g=2, b=1)
im.plotRGB(m2006, r=3, g=1, b=2)
