#' Function to convert atlas XYZ data into a raster
#'
#' atlas2raster takes a data frame as an input and converts it into a raster
#' based on x and y columns specified. Pixel value is defined by z column. 
#' 
#' The provided coordinate values can be offset with arguments. This means that 
#' specified offset values can either be added ("additive") to the coordinates
#' or the coordinates can be multiplied ("multiply").
#'
#' @param data a data frame holding the data 
#' @param filename string holding the filepath and name for the output raster
#' @param x.col string column name for x coordinate
#' @param y.col string column name for y coordinate
#' @param z.col string column name for z values
#' @param x.offset numerical value defining the offset added to x coordinates
#' @param y.offset numerical value defining the offset added to y coordinates
#' @param type.offset a string defining the type of the offset ("additive", "multiply")
#' @param proj4.string string defining the CRS used (default: Finnish YKJ)

atlas2raster <- function(data, filename, x.col, y.col, z.col,  x.offset=0, 
                           y.offset=0, type.offset="additive", 
                           proj4.string="+init=epsg:2393") {
  
  col.names <- c(x.col, y.col, z.col)
  # Check the provided column names
  if (any(!sapply(col.names, function(x) (x %in% names(data))))) {
    stop(paste("Error: column name", 
               col.names[!sapply(col.names, function(x) (x %in% names(data)))],
               "not found in data provided"))
  }
  
  # Offsetting function to handle different types of offsetting the coordinates
  offset <- function(x, offset, type) {
    if (type == "additive") {
      x <- x + offset
    } else if (type == "multiply") {
      x <-x * offset
    } else {
      warning(paste("Offset type", offset.type, "not valid"))
    }
    return(x)
  }
  
  # Add the offsets if provided
  if (x.offset != 0) {
    data[x.col] <- offset(data[x.col], x.offset, type.offset)
  }
  
  if (y.offset != 0) {
    data[y.col] <- offset(data[y.col], y.offset, type.offset)
  }
  
  # Convert to a raster and write to a file
  data.raster <- rasterFromXYZ(data[c(x.col, y.col, z.col)], 
                               crs=CRS(proj4.string))
  # Note that the output raster type is infered from the raser file name
  writeRaster(data.raster, filename)
}
