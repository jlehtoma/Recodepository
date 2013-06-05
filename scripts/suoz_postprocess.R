library(zonator)
library(rgdal)

# SuoZ --------------------------------------------------------------------
# Which variants are to be included
variants <- paste0("^", c(36:43, 45:47))
# Where to look for the variant result folders
root.path <- "Z:/JOONA/suoZ/suoajot"
setwd(root.path)

# Reference PPA unit shapefile to which PPA information is to be joined
# Whole area
PPA.units.file <- "Z:/JOONA/suoZ/data/Z4_dissolved.shp"
PPA.units.sp <- readOGR(PPA.units.file, ogrListLayers(PPA.units.file))
# Northern parts
PPA.units.north.file <- "Z:/JOONA/suoZ/data/Z4_dissolved_pohjoinen.shp"
PPA.units.north.sp <- readOGR(PPA.units.north.file, 
                              ogrListLayers(PPA.units.north.file))
# Southern parts
PPA.units.south.file <- "Z:/JOONA/suoZ/data/Z4_dissolved_etela.shp"
PPA.units.south.sp <- readOGR(PPA.units.south.file, 
                              ogrListLayers(PPA.units.south.file))

# ESMK --------------------------------------------------------------------
variants <- paste0("^", c(21:27))
root.path <- "C:/Data/ESMK/analyysi"
setwd(root.path)

# Penalize for scientific notation
options(scipen=500)

for (variant in variants) {
  variant.folder <- list.files(pattern=variant)
  output.folder <- file.path(variant.folder, "output")
  if (file.exists(output.folder)) {
    nwout.file <- file.path(getwd(),
                            output.folder, 
                            paste0("result_", variant.folder, 
                                   ".nwout.1.spp_data.txt"))
    dat <- read.ppa.lsm(nwout.file)
    output1 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout1.csv"))
    output2 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout2.csv"))
    output3 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout3.csv"))
    
    # Output the tables as CSV files
    write.table(dat[[1]], file=output1, sep=";", row.names=FALSE)
    write.table(dat[[2]], file=output2, sep=";", row.names=FALSE)
    write.table(dat[[3]], file=output3, sep=";", row.names=FALSE)
    
    # Join the tables to existing reference spatial data
    if (grepl("etela", nwout.file)) {
      reference.data.sp <- PPA.units.south.sp
      grid.code <- "SGRIDCODE"
    } else if (grepl("pohjoinen", nwout.file)) {
      reference.data.sp <- PPA.units.north.sp
      grid.code <- "NGRIDCODE"
    } else {
      reference.data.sp <- PPA.units.sp
      grid.code <- "GRIDCODE"
    }
    
    # Make copies of the reference data as the same shapefile needs to be joined
    # twice. Numbering corresponds to dat list indexes.
    reference.data1.sp <- reference.data.sp
    reference.data3.sp <- reference.data.sp
    
    reference.data1.sp@data <- merge(reference.data1.sp@data, dat[[1]],
                                    by.x=grid.code, by.y="Unit")
    reference.data3.sp@data <- merge(reference.data3.sp@data, dat[[3]],
                                     by.x=grid.code, by.y="Unit_number")
    
    output1.sp <- gsub(".csv", ".shp", output1)
    output3.sp <- gsub(".csv", ".shp", output3)
    
    if (file.exists(output1.sp)) {
      file.remove(output1.sp)
    }
    if (file.exists(output3.sp)) {
      file.remove(output3.sp)
    }
    
    writeOGR(reference.data1.sp, output1.sp, layer=gsub(".shp", "", output1.sp),
             driver="ESRI Shapefile")
    message(paste("Wrote shapefile", output1.sp))
    writeOGR(reference.data3.sp, output3.sp, layer=gsub(".shp", "", output3.sp),
             driver="ESRI Shapefile")
    message(paste("Wrote shapefile", output3.sp))
  }
}

library(gridExtra)

project.suoz <- new("Zproject", root=root.path)

variant.36 <- getVariant(project.suoz, 1)
variant.39 <- getVariant(project.suoz, 4)
variant.48 <- getVariant(project.suoz, 13)

labels <- c("Peruspiirteet kytk", "Peruspiirteet", "Lisäpiirteet", "Corine",
            "Linnut", "Vaikea suo")

p1 <- plot(variant.36, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="36_abf_extra_w_cond_cmat_birds")
p1  <- p1 + theme(legend.position=c(.25, .2))

p2 <- plot(variant.39, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")
p2  <- p2 + theme(legend.position=c(.25, .2))

p3 <- plot(variant.48, monochrome=FALSE, group=TRUE, labels=labels[2:6],
          statistic="mean", invert.x=TRUE, main="48_abf_extra_w_cond_birds")
p3  <- p3 + theme(legend.position=c(.25, .2))

grid.arrange(p1, p2, p3, nrow=1, ncol=3)

p4 <- plot(variant.39, monochrome=FALSE, labels=labels, features=1:8,
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")

p5 <- plot(variant.39, monochrome=FALSE, group=TRUE, groups=1:2, 
           labels=c("CONN", "LHQ"),
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")
p5