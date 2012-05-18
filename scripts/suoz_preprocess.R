if (!require('raster')) {
  install.packages('raster')
}

if (!require('rgdal')) {
  install.packages('rgdal')
}

setwd("C:/suoZ")

source("atlas.R")

# Read in the raw data
atlas.data <- read.csv("data/Lintuatlas3_12lajia/Lintuatlas3_12lajia.csv", 
                       header=TRUE, sep=",")

str(atlas.data)

# Get all the species names (in Finnish)
species <- levels(atlas.data$Finnish)

for (spp in species) {
  # Construct the suitable raster filename
  filename <- file.path("data", "Lintuatlas3_12lajia", 
                        paste(spp, "_atlas3.img", sep=""))
  # subset the atlas.data for the current species
  spp.data <- subset(atlas.data, atlas.data$Finnish == spp)
  # Convert to a raster. Coordinates in the Finnish bird atlas data are given
  # as the 3 first digits of the National Uniform Coordinate system (YKJ)
  # because of the data resoluton (10x10km grid).  In order to get correct
  # coordinate values the x and y coordinates need to offset (multiplied) with 
  # 10 000
  m  <- convert2raster(spp.data, 
                       filename=filename, 
                       x.col="gri_e10", 
                       y.col="gri_n10", 
                       z.col="atlas_max",  
                       x.offset=10000, 
                       y.offset=10000,
                       type.offset="multiply")
  print(paste("Wrote", spp, "to file:", filename))
}