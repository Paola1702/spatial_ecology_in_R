## how to import external data in R 
In order to import data in R we should set the working directory: 
```r
install.packagings("terra")
library(terra) #managing raster and vector data
library(imageRy) #analysis of RS data
setwd(C:\Users\17020\Downloads)
setwd(choose.dir()) #ho usato questa perchè l'altra non funziona
getwd()
```
to check the folder you can make use of: 
```r
group<- rast("image.JPG")
group
```


```r
im.plotRGB(group,r=1, g=2, b=3)

```
the image is flip so I can solve the problem with the flip funtion

```r
group<-flip(group)
```

```r

```
