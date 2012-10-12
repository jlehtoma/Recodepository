library(raster)
library(rgdal)
library(plyr)

buffer.zonal <- function(point.shp, id.field, target.raster, buffer.dist) {
  
  # Read in the point shapefile
  shp <- shapefile(point.shp)
  # Extract the zonal data from the target raster
  shp.buffer.extract = extract(raster(target.raster), shp, buffer=2000)
  
  # Calculate the amount of pixels of each class in each polygon
  pixel.table <- lapply(shp.buffer.extract, table)
  # Convert the pixel counts into fractions
  pixel.table.frac <- lapply(pixel.table, function(x) {sapply(x, function(y, z=sum(x)) {y/z} )})
  # Convert the list of table matrices into a list of data frames so that 
  # rbind.fill can be used. Note that some transposing must be done.
  for (i in 1:length(pixel.table.frac)) {
    header <- names(pixel.table.frac[[i]])
    dat <- as.data.frame(t(pixel.table.frac[[i]]))
    colnames(dat) <- header
    pixel.table.frac[[i]] <- dat
  }
  # Join the list into a single data frame with missing counts (for classes) as
  # NAs
  pixel.table.frac.df <- do.call("rbind.fill", pixel.table.frac)
  # Re-order the dataframe by column names
  pixel.table.frac.df <- pixel.table.frac.df[,order(names(pixel.table.frac.df))]
  # Replace the missing classes (NAs) with zeros (0)
  pixel.table.frac.df[is.na(pixel.table.frac.df)] <- 0
  # Add in the original ID numbers
  result.df <- cbind(shp@data[id.field], pixel.table.frac.df)

  return(result.df)
}

point.shp <- "H:/Data/People/Ilkka/FINRISK/Shapefiles/FINRISK_kohteet_sample.shp"
target.raster <- "H:/Data/People/Ilkka/FINRISK/Rasters/clc_fi25_paaluokat_sample.img"

result <- buffer.zonal(point.shp, "ID", target.raster, 2000)