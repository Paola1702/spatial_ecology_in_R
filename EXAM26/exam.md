# IMPACT OF THE VAIA STORM (30 OCTOBER 2018) ON THE VEGETATION OF THE PARCO NATURALE PANEVEGGIO - PALE DI SAN MARTINO
## Analysis of the Sentinel-2 images (pre-Vaia 2018, post-vaia same season 2019)

## LIBRARY AND WORKING DIRECTORY
```
library(terra) #for spatial data analysis with vector and raster data
library(viridis) 
library(imageRy)
library(ggplot2) #graphs 

setwd("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology")
```

## Selection of the Parco Paneveggio area from the shapefile
```
parchi_tn <- vect("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/z307_p_pup.shp")   #directory of the shapefile
names(parchi_tn) # check on the different fields in the file
head(as.data.frame(parchi_tn))
paneveggio <- parchi_tn[grepl("PANEVEGGIO", parchi_tn$descr, ignore.case = TRUE), ] #recall of the area of the Paneveggio area

plot(paneveggio, main = "Parco Naturale Paneveggio - Pale di San Martino") #plot of the area boundaries
```
<img width="458" height="330" alt="paneveggioboundaries_Rplot" src="https://github.com/user-attachments/assets/bf639fa9-7326-4cdf-8df7-b69228ca1a12" />

## Upload of the raster files of the bands needed from the pre Vaia Sentinel-2 pictures
```
B02_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B02_10m.jp2")
B03_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B03_10m.jp2")
B04_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B04_10m.jp2")
B08_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B08_10m.jp2")
SCL_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_SCL_20m.jp2")
```

## Upload of the raster files of the bands needed from the post Vaia Sentinel-2 pictures
```
B02_2019 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190916T101029_B02_10m.jp2")
B03_2019 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190916T101029_B03_10m.jp2")
B04_2019 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190916T101029_B04_10m.jp2")
B08_2019 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190916T101029_B08_10m.jp2")
SCL_2019 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190916T101029_SCL_20m.jp2")
```

## plot the maps from 2018 satellites pictures 
```
par(mfrow = c(2, 3)) 
plot(B02_2018, main = "B2")
plot(B03_2018, main = "B3")
plot(B04_2018, main = "B4")
plot(B08_2018, main = "B8")
plot(SCL_2018, main = "SCL")
dev.off()
```
![bands2018](https://github.com/user-attachments/assets/1dcd3ea7-5301-47e3-98f5-4a854b11122e)
## plot the maps from 2019 satellites pictures 
```
par(mfrow = c(2, 3))
plot(B02_2019, main = "B2")
plot(B03_2019, main = "B3")
plot(B04_2019, main = "B4")
plot(B08_2019, main = "B8")
plot(SCL_2019, main = "SCL")
```
![bands2019](https://github.com/user-attachments/assets/ee4623ed-d385-4d3f-bb2b-b604816b6230)

## crop only the park area
```
paneveggio_utm <- project(paneveggio, crs(B02_2018))
B02_2018_crop <- mask(crop(B02_2018, paneveggio_utm), paneveggio_utm)
B03_2018_crop <- mask(crop(B03_2018, paneveggio_utm), paneveggio_utm)
B04_2018_crop <- mask(crop(B04_2018, paneveggio_utm), paneveggio_utm)
B08_2018_crop <- mask(crop(B08_2018, paneveggio_utm), paneveggio_utm)
SCL_2018_crop <- mask(crop(SCL_2018, paneveggio_utm), paneveggio_utm)

B02_2019_crop <- mask(crop(B02_2019, paneveggio_utm), paneveggio_utm)
B03_2019_crop <- mask(crop(B03_2019, paneveggio_utm), paneveggio_utm)
B04_2019_crop <- mask(crop(B04_2019, paneveggio_utm), paneveggio_utm)
B08_2019_crop <- mask(crop(B08_2019, paneveggio_utm), paneveggio_utm)
SCL_2019_crop <- mask(crop(SCL_2019, paneveggio_utm), paneveggio_utm)
```
## SCL crop
The pixel without data and with snow and clouds will be erased to precisly calculate the vegetation indices. The codes are from the [Copernicus website](https://sentiwiki.copernicus.eu/web/s2-processing)

* 0- WITHOUT DATA
* 1-SATURATED_DEFEC
* 3-CLOUD_SHADOW
* 7-CLOUD_LOW_PROBA / UNCLASSIFIED
* 8-CLOUD_MEDIUM_PROBA
* 9-CLOUD_HIGH_PROBA
* 10-THIN_CIRRUS
* 11-SNOW or ICE

```
mask_scl <- function(x, scl) {
#the SCL band is at 20m so an adaptation to 10m is needed
  scl_10m <- resample(scl, x, method = "near")

#TRUE when the pixel is not in the codes written
codici_da_escludere <- c(0, 1, 3, 8, 9, 10, 11)
valido <- !(scl_10m %in% codici_da_escludere)
  
  #all the non valid pixel will be substituted with NA
  mask(x, valido, maskvalues = FALSE)
}
#apply mask SCL to each layer 2018
B02_2018_crop <- mask_scl(B02_2018_crop, SCL_2018_crop)
B03_2018_crop <- mask_scl(B03_2018_crop, SCL_2018_crop)
B04_2018_crop <- mask_scl(B04_2018_crop, SCL_2018_crop)
B08_2018_crop <- mask_scl(B08_2018_crop, SCL_2018_crop)

B02_2019_crop <- mask_scl(B02_2019_crop, SCL_2019_crop)
B03_2019_crop <- mask_scl(B03_2019_crop, SCL_2019_crop)
B04_2019_crop <- mask_scl(B04_2019_crop, SCL_2019_crop)
B08_2019_crop <- mask_scl(B08_2019_crop, SCL_2019_crop)
```

## RGB visualization of the cropped images 
```
layout(
  matrix(c(1, 0, 2), nrow = 1),
  widths = c(1, 0.20, 1)
)
par(cex.main = 0.8)

plotRGB(c(B04_2018_crop, B03_2018_crop, B02_2018_crop), 
        r = 1, g = 2, b = 3,
        stretch = "lin",
        main = "Sentinel-2 RGB - Pre-Vaia (27/08/2018)",
        axes=TRUE)

plotRGB(c(B04_2019_crop, B03_2019_crop, B02_2019_crop), 
        r = 1, g = 2, b = 3,
        stretch = "lin",
        main = "Sentinel-2 RGB - Post-Vaia (16/09/2019)",
        axes=TRUE)
dev.off()
```
![RGB](https://github.com/user-attachments/assets/7181f615-302b-4511-b696-609afce1d726)

# Calcolo DVI (Difference Vegetation Index) and dDVI
This index is calculated with the difference between the reflectance values of the **near-infrared(NIR)** and **red spectral** bands. It is a simple index and it tells us about the density and health of the vegetation because when the plants are healthy they reflect more NIR light while absorbing red light.
```
dvi_2018 = B08_2018_crop - B04_2018_crop # Calculation DVI pre-Vaia
dvi_2019 = B08_2019_crop - B04_2019_crop # Calculation DVI post-Vaia

# Pixel valid in both the dates
validi <- !is.na(dvi_2018) & !is.na(dvi_2019)
dDVI <- ifel(
  validi,
  dvi_2019 - dvi_2018,
  NA
)
dDVI_real <- dDVI / 10000
plot(dDVI_real,col=viridis::viridis(100), main="ΔDVI")
```
![deltaDVI](https://github.com/user-attachments/assets/cbaa81d7-2196-433c-ba30-f3847021429c)
```
hist(
  x,
  breaks = 100,
  main = "Distribuzione di ΔDVI (2018-2019)",
  col = "palegreen",
  xlab=("ΔDVI")
)

abline(v = 0, col = "darkgreen", lwd = 1.5)
```
<img width="611" height="330" alt="dDVI_hist_Rplot" src="https://github.com/user-attachments/assets/77f7031b-9f67-4c84-8171-0a596eac827b" />

# ΔNDVI calculation
```
ndvi_2018 <- (B08_2018_crop - B04_2018_crop) /
  (B08_2018_crop + B04_2018_crop)

ndvi_2019 <- (B08_2019_crop - B04_2019_crop) /
  (B08_2019_crop + B04_2019_crop)

#common mask
validi <- !is.na(ndvi_2018) & !is.na(ndvi_2019)

#subtraction NDVI
dNDVI <- ifel(
  validi,
  ndvi_2019 - ndvi_2018,
  NA
)

#visualization
par(mfrow = c(1, 3))
breaks_ndvi <- seq(-1, 1, by = 0.2)
col_ndvi <- viridis::viridis(100)

plot(ndvi_2018,
     main = "NDVI - Pre-Vaia",
     xlab = "Easting (m)",
     ylab = "Northing (m)",
     breaks = breaks_ndvi,
     col = col_ndvi)

plot(ndvi_2019,
     main = "NDVI - Post-Vaia",
     xlab = "Easting (m)",
     ylab = "Northing (m)",
     breaks = breaks_ndvi,
     col = col_ndvi)

plot(dNDVI,
  main = "ΔNDVI - Post-Vaia vs Pre-Vaia",
  xlab = "Easting (m)",
  ylab = "Northing (m)",
  breaks = breaks_ndvi,
  col=viridis::inferno(100),
)
```
![NDVIplots](https://github.com/user-attachments/assets/601df027-db5f-443e-87be-44eb4948109a)


# ISTOGRAMMI NDVI

```
breaks_ndvi_h <- seq(-1, 1, length.out = 21)
par(mfrow = c(1, 2))

hist(
  ndvi_2018,
  breaks = breaks_ndvi_h,
  main = "NDVI PRE-STORM",
  col = "green"
)

hist(
  ndvi_2019,
  breaks = breaks_ndvi_h,
  main = "NDVI POST-STORM",
  col = "blue"
)

```
<img width="580" height="330" alt="NDVI_hist_Rplot" src="https://github.com/user-attachments/assets/ade26475-a4e1-4f71-9c2c-1262de5352f5" />

# dNDVI vegetation cover classification based on literature 
* NDVI < 0.2      = Very low vegetation cover
* 0.2 ≤ NDVI < 0.4 = Low vegetation cover
* 0.4 ≤ NDVI < 0.6 = Moderate vegetation cover
* 0.6 ≤ NDVI < 0.8 = High vegetation cover
* NDVI ≥ 0.8       = Very high vegetation cover
> Considering that in the 2019 there are no pixel in the 5 class and in the 2018 image there are only 2 I have decided to not consider it to avoid complication in the calculations and in the graphs representation. 
```
# NDVI classification
class_matrix <- matrix(c(
  -Inf, 0.2, 1,
  0.2, 0.4, 2,
  0.4, 0.6, 3,
  0.6, 0.8, 4
), ncol = 3, byrow = TRUE)

class_matrix

ndvi_2018_cl <- classify(ndvi_2018, class_matrix)
ndvi_2019_cl <- classify(ndvi_2019, class_matrix)

par(mfrow = c(1, 2))

col_ndvi <- c(
  "orange",
  "gold",
  "yellowgreen",
  "darkgreen"
)

plot(
  ndvi_2018_cl,
  col = col_ndvi,
  main = "NDVI Classification - 2018"
)

plot(
  ndvi_2019_cl,
  col = col_ndvi,
main = "NDVI Classification - 2019"
)

```
<img width="536" height="330" alt="NDVI_classification_Rplot" src="https://github.com/user-attachments/assets/528b835d-c716-44da-9638-685063931171" />

# Pixels frequences and percentages calculation
```
#Pixel frequencies
freq_2018 <- freq(ndvi_2018_cl)
freq_2019 <- freq(ndvi_2019_cl)

#Percentages
perc_2018 <- freq_2018$count * 100 / sum(freq_2018$count)
perc_2019 <- freq_2019$count * 100 / sum(freq_2019$count)

#Summary table
tab <- data.frame(
  Class = c(
    "Very low vegetation cover",
    "Low vegetation cover",
    "Moderate vegetation cover",
    "High vegetation cover"
  ),
  NDVI_2018 = round(perc_2018, 2),
  NDVI_2019 = round(perc_2019, 2)
)
print(tab)
```
Class | NDVI_2018 | NDVI_2019
------------ | ------------- | -------------
Very low vegetation cover |  15.78  |   19.33
Low vegetation cover |  25.02  |   36.99
Moderate vegetation cover |   58.15   |  43.32
High vegetation cover |     1.05    |  0.36

# Bibliography 
Wang B, Li H, Xia W, Wang J, Cai C and Chen J (2026) Response of NDVI spatiotemporal variation to meteorological factors in arid areas. Front. Environ. Sci. 14:1856508. doi: 10.3389/fenvs.2026.1856508
