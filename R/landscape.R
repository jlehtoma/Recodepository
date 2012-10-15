library(raster)
library(rgdal)
library(plyr)
library(ggmap)

# Map KKJ zones into appropriate EPSG codes
KKJ.CODES <- list("0" = "+init=epsg:2391", "1" = "+init=epsg:2391", 
                  "2" = "+init=epsg:2392", "3" = "+init=epsg:2393", 
                  "4" = "+init=epsg:2394", 
                  "5" = "+init=epsg:3387")

address2df <- function(df, address.field, ...) {
  
  coords <- .geocode(unlist(df[address.field]), ...)
  coords <- do.call("rbind", coords)
  return(cbind(df, coords))
  
}

address2sppoints <- function(df, address.field, proj4.string=NULL) {
  
  coords <- .geocode(df[address.field])
  
  # Google will return coordinates WGS84
  sp.data <- SpatialPointsDataFrame(coords, df, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Is projection needed
  if (proj4.string) {
    library("rgdal")
    message(paste("Projecting from WGS84 to", proj4.string))
    sp.data <- spTransform(sp.data, CRS(proj4.string))
  }
  
  return(sp.data)
}

buffer.zonal <- function(point.shp, id.fields, target.raster, buffer.dist) {
  
  if (class(target.raster) == "character") {
    target.raster <- raster(target.raster)
  }
  
  if (class(point.shp) == "character") {
    # Read in the point shapefile
    shp <- shapefile(point.shp)
  } else if (class(point.shp) == "SpatialPointsDataFrame") {
    shp <- point.shp
  } else {
    stop(paste("Object type must be a path to a shapefile or a SpatialPointsDataFrame object, not", class(point.shp)))
  }
  
  # Extract the zonal data from the target raster
  shp.buffer.extract = extract(target.raster, shp, buffer=buffer.dist)
  
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
  # Add in the original ID fields and rownumbers
  result.df <- cbind(shp@data[id.fields], pixel.table.frac.df)
  rownames(result.df) <- rownames(shp@data)
  
  return(result.df)
}

KKJ2YKJ <- function(sp.obj, from=NA) {
  
  # Check if the object is spatial
  if (!any(is(sp.obj) == "Spatial")) {
    stop("Provided object is not Spatial")
  }
  
  # Check if the sp object has a CRS defined and if it corresponds to what the 
  # user is claiming
  proj4.string.from <- proj4string(sp.obj) 
  if (is.na(proj4.string.from) && is.na(from)) {
    stop("sp object has no projection defined and no KKJ zone is provided by user")
  } else if (is.na(proj4.string.from) && from) {
    proj4.string.from <- KKJ.CODES[[as.character(from)]]
    proj4string(sp.obj) <- proj4.string.from
  } 
  
  # We're always projecting to KKJ3 (=YKJ)
  projected.sp <- spTransform(sp.obj, 
                              CRS(KKJ.CODES[["3"]]))
  return(projected.sp)
}

# Private functions -------------------------------------------------------

.geocode <- function(addresses, cache=TRUE) {
  
  # Check the inputs
  if (typeof(addresses) != "character") {
    stop("Addresses must be provided as a character vector")
  }
  
  cache.dir <- ".cache"
  cache.file <- file.path(cache.dir, "coords.RData")
  
  if (cache && !file.exists(cache.dir)) {
    dir.create(cache.dir)
  }
  
  if (cache) { 
    if (file.exists(cache.file)) {
      
      message("Trying cached coordinates")
      load(cache.file)
      
      # Check whether the addresses are in the cache
      cached.coords <- coords[addresses]
      notin.cache.addresses <- addresses[!addresses %in% names(coords)]
      
      if (length(notin.cache.addresses) > 0) {
        message("Not all requested addresses found in the cache, getting the rest")
        addresses <- notin.cache.addresses
      } else {
        return(cached.coords)
      }
    }
  }
  
  queries <- geocodeQueryCheck()
  
  if (queries < length(addresses)) {
    warning(paste("Daily Google API query quota", queries, 
                  "will be exceeded."))
  }
  
  coords <- list()
  
  for (i in 1:length(addresses)) {
    
    coords[[addresses[[i]][1]]] <- geocode(addresses[[i]][1])
  }
  
  if (cache) {
    save(coords, file=cache.file)
  }
  
  return(coords)
}

#point.shp <- "H:/Data/People/Ilkka/FINRISK/Shapefiles/FINRISK_kohteet_sample.shp"
#target.raster <- "H:/Data/People/Ilkka/FINRISK/Rasters/clc_fi25_paaluokat_sample.img"

#result <- buffer.zonal(point.shp, "ID", target.raster, 2000)
