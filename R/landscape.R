library(raster)
library(rgdal)
library(plyr)

buffer.zonal <- function(point.shp, id.field, target.raster, buffer.dist) {
  
  # Read in the point shapefile
  shp <- shapefile(point.shp)
  # Buffer the points. Note that gBuffer will return a SpatialPolygons object.
  # We'll need to unite that with the original attribute data
  #shp.buffer.poly <- gBuffer(shp, byid=TRUE, width=buffer.dist)
  #shp.buffer <- SpatialPolygonsDataFrame(shp.buffer.poly, shp@data)
  #browser()
  # Extract the zonal data from the target raster
  shp.buffer.extract = extract(raster(target.raster), shp, buffer=2000)
  
  # Calculate the amount of pixels of each class in each polygon
  pixel.table <- lapply(shp.buffer.extract, table)
  # Convert the pixel counts into fractions
  pixel.table.frac <- lapply(pixel.table, function(x) {sapply(x, function(y, z=sum(x)) {y/z} )})
  # Let's return a data frame. ID are replaced with the original shp ids
  pixel.df.frac <- as.data.frame(do.call("rbind.fill", pixel.table.frac))
  pixel.df.frac <- cbind(shp@data[id.field], pixel.df.frac)
  
  return(pixel.df.frac)
}

point.shp <- "H:/Data/People/Ilkka/FINRISK/Shapefiles/FINRISK_kohteet_sample.shp"
target.raster <- "H:/Data/People/Ilkka/FINRISK/Rasters/clc_fi25_paaluokat_sample.img"

result <- buffer.zonal(point.shp, "ID", target.raster, 2000)