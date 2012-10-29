if (!require('raster')) {
  install.packages('raster')
}

if (!require('rgdal')) {
  install.packages('rgdal')
}

korvaa.skandit <- function (s) {
  
  s <- gsub("\\xe4", "a", s)
  s <- gsub("\\xC4", "A", s)
  s <- gsub("\\xD6", "O", s)
  s <- gsub("\\xf6", "o", s)
  s <- gsub("\\xE5", "a", s)
  s <- gsub("\\U3e34633c", "A", s)
  
  return(s)
  
}

source("R/atlas.R")

setwd("C:/suoZ")

# Read in the raw data
atlas.data <- read.csv("C:/SuoZ/data/Lintuatlas3_12lajia/Lintuatlas3_12lajia.csv", 
                       header=TRUE, sep=",")

str(atlas.data)

# Reclass the observation class
atlas.breaks <- c(0, 10, 30, 70, 82)
atlas.labels <- c(-1, 1, 2, 3)
atlas.data$class <- cut(atlas.data$atlas_max, atlas.breaks, atlas.labels)

# Get all the species names (in Finnish)
species <- levels(atlas.data$Finnish)

bog.mask <- raster("C:/SuoZ/data/rasteri_100/z2_laik100_bin.img")
bog.extent <- extent(bog.mask)

for (spp in species) {
  # subset the atlas.data for the current species
  spp.data <- subset(atlas.data, atlas.data$Finnish == spp)
  spp <- korvaa.skandit(spp)
  # Construct the suitable raster filename
  filename <- file.path("data", "rasteri_100", "lintuatlas",
                        paste(spp, "_atlas3.img", sep=""))
  
  # Atlas data coordinates give the raster cell center point, rasterFromXYZ
  # will use it as upper right corner -> add 5000 to N and E 
  spp.data$gri_n10 <- spp.data$gri_n10 + 0.5
  spp.data$gri_e10 <- spp.data$gri_e10 + 0.5
  
  # Convert to a raster. Coordinates in the Finnish bird atlas data are given
  # as the 3 first digits of the National Uniform Coordinate system (YKJ)
  # because of the data resoluton (10x10km grid).  In order to get correct
  # coordinate values the x and y coordinates need to offset (multiplied) with 
  # 10 000
  spp.raster  <- atlas2raster(spp.data, 
                              x.col="gri_e10", 
                              y.col="gri_n10", 
                              z.col="class",
                              levels=c(2, 3),
                              x.offset=10000,
                              y.offset=10000,
                              type.offset="multiply")
  
  # Upscale the resolution to 100 meters
  message(paste("Dissaggregating raster", spp, "..."))
  spp.raster.100 <- disaggregate(spp.raster, 100)
  #browser()
  # Expand the spp raster so that it has the same extent as the bog mask  
  message(paste("Expanding raster", spp, "..."))
  spp.raster.100 <- expand(spp.raster.100, bog.extent)
  # Mask the spp raster with the bog mask
  paste("Masking raster", spp, "...")
  spp.raster.100 <- mask(spp.raster.100, bog.mask)
  
  message(paste("Writing raster", spp, "..."))
  # Note that the output raster type is infered from the raser file name
  writeRaster(spp.raster.100, filename, option=c("COMPRESS=YES"))
}