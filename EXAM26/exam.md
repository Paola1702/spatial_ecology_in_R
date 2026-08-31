# IMPATTO DELLA TEMPESTA VAIA (30 OTTOBRE 2018) SULLA VEGETAZIONE DEL PARCO NATURALE PANEVEGGIO - PALE DI SAN MARTINO
## Analisi multitemporale con immagini Sentinel-2 (due date: pre-Vaia,post-schianto)

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
The pixel with the snow and the clouds will be erased to precisly calculate the vegetation indices. The codes are from the [Copernicus website](https://sentiwiki.copernicus.eu/web/s2-processing)

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

# ---------------------------------------------------------------------------
# 4. RGB visualization 
# ---------------------------------------------------------------------------
par(mfrow = c(1, 2))
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

# ---------------------------------------------------------------------------
# Calcolo DVI 
# ---------------------------------------------------------------------------

dvi_2018 = B08_2018_crop - B04_2018_crop # Calcolo DVI pre-vaia
dvi_2019 = B08_2019_crop - B04_2019_crop # Calcolo DVI post-vaia


# Pixel validi in entrambe le date
validi <- !is.na(dvi_2018) & !is.na(dvi_2019)
dDVI <- ifel(
  validi,
  dvi_2019 - dvi_2018,
  NA
)
plot(dDVI)

# 1. Controllo distribuzione
#quantile(dDVI, probs = seq(0, 1, 0.1), na.rm = TRUE)

# 2. Calcolo ΔNDVI
ndvi_2018 <- (B08_2018_crop - B04_2018_crop) /
  (B08_2018_crop + B04_2018_crop)

ndvi_2019 <- (B08_2019_crop - B04_2019_crop) /
  (B08_2019_crop + B04_2019_crop)

# 3. Maschera comune
validi <- !is.na(ndvi_2018) & !is.na(ndvi_2019)

# 4. Differenza
dNDVI <- ifel(
  validi,
  ndvi_2019 - ndvi_2018,
  NA
)

# 5. Visualizzazione
par(mfrow = c(1, 3))
breaks_ndvi <- seq(-1, 1, length.out = 6)
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


plot(
  dNDVI,
  main = "ΔNDVI - Post-Vaia vs Pre-Vaia",
  xlab = "Easting (m)",
  ylab = "Northing (m)",
  breaks = breaks_ndvi,
  col=viridis::inferno(100),
)

# ============================================================
# 14. ISTOGRAMMI NDVI
# ============================================================

# INVARIATO
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

class_matrix <- matrix(c(-Inf, 0.2, 1, 
                         0.2, 0.4, 2, 
                         0.4, Inf, 3), 
                       ncol = 3, byrow = TRUE)
class_matrix
# Se NDVI < 0.2 allora si associa una classe di tipo 1 (Suolo nudo)
# Se 0.2 ≤ NDVI < 0.4 allora si associa una classe di tipo 2 (Vegetazione media)
# Se NDVI ≥ 0.4 allora si associa una classe di tipo 3 (Vegetazione sana)  

ndvi_2018_cl <- classify(ndvi_2018, class_matrix)  
ndvi_2019_cl <- classify(ndvi_2019, class_matrix)  
# Verifica visuale   
plot(ndvi_2018_cl, col = c("orange", "yellow", "darkgreen"), main = "NDVI class. 2018")  
plot(ndvi_2019_cl, col = c("orange", "yellow", "darkgreen"), main = "NDVI class. 2019")  
# Frequenze
freq_2018 <- freq(ndvi_2018_cl)
freq_2019 <- freq(ndvi_2019_cl)
# Percentuali
perc_2018 <- freq_2018$count * 100 / sum(freq_2018$count)
perc_2019 <- freq_2019$count * 100 / sum(freq_2019$count)
# Tabella
tab <- data.frame(
  classi = c("Suolo nudo", "Vegetazione media", "Vegetazione sana"),
  a2018 = round(perc_2018, 2),
  a2019 = round(perc_2019, 2)
)
print(tab)

# ---------------------------------------------------------------------------
# CLASSIFICAZIONE DEL dNDVI IN CLASSI DI CAMBIAMENTO
#   1 = Perdita di vegetazione  (dNDVI < -0.2)
#   2 = Stabile                 (-0.2 <= dNDVI < 0.2)
#   3 = Guadagno di vegetazione (dNDVI >= 0.2)
# ---------------------------------------------------------------------------
class_dNDVI <- matrix(c(-Inf, -0.2, 1,
                        -0.2,  0.2, 2,
                        0.2,  Inf, 3),
                      ncol = 3, byrow = TRUE)

dNDVI_cl <- classify(dNDVI, class_dNDVI)

plot(dNDVI_cl,
     col = c("red", "khaki", "darkgreen"),
     main = "Classi di variazione NDVI - Post-Vaia vs Pre-Vaia")

# frequenze per classe
freq_dNDVI <- freq(dNDVI_cl)

# percentuale sul totale dei pixel VALIDI (non NA) - non su ncell(),
# altrimenti sottostimi le percentuali come nel bug delle classi assolute
freq_dNDVI$percentuale <- freq_dNDVI$count / sum(freq_dNDVI$count) * 100

# risoluzione 10 m -> ogni pixel = 100 m^2 = 0.01 ha
freq_dNDVI$ettari <- freq_dNDVI$count * 0.01

tab_variazione <- data.frame(
  Classe      = c("Perdita di vegetazione", "Stabile", "Guadagno di vegetazione"),
  Percentuale = round(freq_dNDVI$percentuale, 2),
  Ettari      = round(freq_dNDVI$ettari, 1)
)
print(tab_variazione)
 
# ---------------------------------------------------------------------------
# GRAFICO DI CONFRONTO DELLE PERCENTUALI PER CLASSE (2018 vs 2019)
# ---------------------------------------------------------------------------
# reshape() trasforma "tab" da formato wide (una colonna per anno) a
# formato long (una riga per ogni combinazione classe x anno), che e'
# quello che ggplot si aspetta per raggruppare le barre per colore
tab_long <- reshape(
  tab,
  varying   = c("a2018", "a2019"),
  v.names   = "percentuale",
  timevar   = "anno",
  times     = c("2018", "2019"),
  direction = "long"
)

ggplot(tab_long, aes(x = classi, y = percentuale, fill = anno)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2018" = "forestgreen", "2019" = "firebrick")) +
  labs(title = "Classes NDVI - Parco Paneveggio (pre vs post Vaia)",
       y = "% area", x = NULL, fill = "Anno") +
  theme_minimal()
