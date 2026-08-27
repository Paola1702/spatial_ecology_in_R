Pre-Vaia: estate 2018 (prima del 30 ottobre) — bosco integro
Post-Vaia, pre-bostrico: estate 2019 — danno da schianto già visibile (chiome a terra, radure nette)
Fase epidemica bostrico: estate 2022 o 2023 — ulteriore perdita di chioma nelle piante rimaste in piedi, prima dei tagli sanitari
##############################################################################
# IMPATTO DELLA TEMPESTA VAIA (30 OTTOBRE 2018) E DELL'EPIDEMIA DI BOSTRICO
# SULLA VEGETAZIONE DEL PARCO NATURALE PANEVEGGIO - PALE DI SAN MARTINO
# Analisi multitemporale con immagini Sentinel-2 (tre date: pre-Vaia,
# post-schianto, fase epidemica bostrico)
##############################################################################

# ---------------------------------------------------------------------------
# 1. LIBRERIE E WORKING DIRECTORY
# ---------------------------------------------------------------------------
# terra    -> gestione dati raster/vettoriali (lettura .jp2, crop, mask, ecc.)
# viridis  -> palette di colori percettivamente uniformi per le mappe
# imageRy  -> funzioni rapide di visualizzazione usate a lezione (im.multiframe...)
# ggplot2  -> grafici a barre per il confronto delle percentuali per classe

library(terra)
library(viridis)
library(imageRy)
library(ggplot2)

setwd("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology")

# ---------------------------------------------------------------------------
# 2. AREA DI STUDIO: selezione del Parco Paneveggio dallo shapefile
#    provinciale dei parchi del Trentino (campo attributo = "descr")
# ---------------------------------------------------------------------------
parchi_tn <- vect("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/z307_p_pup.shp")   # <-- adatta il path/nome file
names(parchi_tn)                                        # controllo dei campi disponibili
head(as.data.frame(parchi_tn))
paneveggio <- parchi_tn[grepl("PANEVEGGIO", parchi_tn$descr, ignore.case = TRUE), ]

plot(paneveggio, main = "Parco Naturale Paneveggio - Pale di San Martino")

#upload of the bands we need from the pre Vaia 
B02_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B02_10m.jp2")
B03_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B03_10m.jp2")
B04_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B04_10m.jp2")
B08_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_B08_10m.jp2")
SCL_2018 <- rast("C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/pre_vaia_2018/T32TQS_20180827T101021_SCL_20m.jp2")

B02_2019 <- rast(C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190827T101021_B02_10m.jp2)
B03_2019 <- rast(C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190827T101021_B03_10m.jp2)
B04_2019 <- rast(C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20190827T101021_B04_10m.jp2)
B08_2019 <- rast(C:/Users/17020/Documents/UNI Magistrale/spatial_ecology/vaia_2019/T32TQS_20180827T101021_B08_10m.jp2)

#crop only the park zone
paneveggio_utm <- project(paneveggio, crs(B02_2018))
B02_2018_crop <- mask(crop(B02_2018, paneveggio_utm), paneveggio_utm)
B03_2018_crop <- mask(crop(B03_2018, paneveggio_utm), paneveggio_utm)
B04_2018_crop <- mask(crop(B04_2018, paneveggio_utm), paneveggio_utm)
B08_2018_crop <- mask(crop(B08_2018, paneveggio_utm), paneveggio_utm)
SCL_2018_crop <- mask(crop(SCL_2018, paneveggio_utm), paneveggio_utm)

# resample(): la banda SCL nasce a 20 m, le bande B02/B03/B04/B08 sono a
# 10 m. Per poterle sovrapporre pixel-per-pixel dobbiamo riportare la SCL
# alla stessa griglia/risoluzione delle altre bande. method="near" perche'
# la SCL contiene codici di classe (dati categoriali): un metodo di
# interpolazione continuo (es. bilineare) non avrebbe senso qui, creerebbe
# codici di classe inventati e intermedi che non esistono.
mask_scl <- function(x, scl) {

  scl_10m <- resample(scl, x, method = "near")
  
  # costruiamo la maschera booleana: TRUE dove il pixel NON e' tra i codici
  # da escludere (quindi e' un pixel valido da mantenere)
  codici_da_escludere <- c(0, 1, 3, 8, 9, 10, 11)
  valido <- !(scl_10m %in% codici_da_escludere)
  
  # mask() sostituisce con NA tutti i pixel di x dove "valido" e' FALSO
  mask(x, valido, maskvalues = FALSE)
}
#apply mask SCL to each layer 2018
B02_2018_crop <- mask_scl(B02_2018_crop, SCL_2018_crop)
B03_2018_crop <- mask_scl(B03_2018_crop, SCL_2018_crop)
B04_2018_crop <- mask_scl(B04_2018_crop, SCL_2018_crop)
B08_2018_crop <- mask_scl(B08_2018_crop, SCL_2018_crop)




