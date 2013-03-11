library(raster)

source("R/zonation.R")

result.folder <- "C:/Data/Tord/Zonation/setup"

setwd(result.folder)

variants <- c("1_abf",
	            "2_abf_wnspp",  
	            "3_abf_wroc")

# Read in the rasters and create a RasterStack - FOR IMG
results <- read.result.rasters(variants, result.folder, format=".rank.img")

# Calculate the Jaccard coefficient
cut.off.levels <- c(0.8, 0.9, 0.95, 0.99)

j.099 <- jaccard(results[[1]], results[[2]], 0.99)

jaccards <- lapply(cut.off.levels, function(x) {return(cross.jaccard(results, x))})

 row.names(jaccards) <- variants[1:6]

write.csv(jaccards, file=file.path(result.folder.new.6cl, "jaccard_new_old.csv"))
