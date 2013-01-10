library(raster)

workspace <- "E:/Data/Metsakeskukset/Etela-Savo/Zonation/ESMK/data/mlvmi"
# Oletus: rasterit ovat yllä mainitussa kansiossa
org.raster.file <- "Päätyypit_kangas_turve.img"
agg.raster.file <- "Päätyypit_kangas_turve_60.img"

setwd(workspace)

custom.modal <- function(x, ...) {
  return(modal(x, ties="highest", ...))
}

org.raster <- raster(org.raster.file)
(aggregate(org.raster, fact=3, fun=custom.modal, 
           filename=agg.raster.file, option=c("COMPRESS=YES")))