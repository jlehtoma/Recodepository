library(raster)

workspace <- "C:\\Data\\ESMK\\data\\maski\\CorrectExtent"
# Oletus: rasterit ovat yllä mainitussa kansiossa
org.raster.file <- "kasvupaikka_5cl.img"
agg.raster.file <- "kasvupaikka_5cl_60.img"

setwd(workspace)

custom.modal <- function(x, ...) {
  return(modal(x, ties="highest", ...))
}

org.raster <- raster(org.raster.file)
(aggregate(org.raster, fact=3, fun=custom.modal, 
           filename=agg.raster.file, option=c("COMPRESS=YES")))