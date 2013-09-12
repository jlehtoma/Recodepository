library(zonator)
library(rgdal)
library(XLConnect)

# SuoZ --------------------------------------------------------------------
# Which variants are to be included
variants <- paste0("^", c(36:43, 45:47))
# Where to look for the variant result folders
root.path <- "/var/run/media/jlehtoma/OSDisk/Data/suoajot36_47/suoajot/"
setwd(root.path)

data.folder <- "/var/run/media/jlehtoma/OSDisk/Data/suoajot36_47/data"

# Reference PPA unit shapefile to which PPA information is to be joined
# Whole area

PPA.units.file <- file.path(data.folder, "z4_dissolved.shp")
PPA.units.sp <- readOGR(PPA.units.file, ogrListLayers(PPA.units.file))
# Northern parts
PPA.units.north.file <- file.path(data.folder, 
                                  "z4_dissolved_pohjoinen.shp")
PPA.units.north.sp <- readOGR(PPA.units.north.file, 
                              ogrListLayers(PPA.units.north.file))
# Southern parts
PPA.units.south.file <- file.path(data.folder, "z4_dissolved_etela.shp")
PPA.units.south.sp <- readOGR(PPA.units.south.file, 
                              ogrListLayers(PPA.units.south.file))

# An extra Excel-file is also needed, it maps the different ID numbers used in 
# different unit files
ID.map.file <- file.path(data.folder, "laikku_id.xlsx")

ID.map.wb <- loadWorkbook(ID.map.file)
ID.map.data <- readWorksheet(ID.map.wb, sheet = "z5_laikut")
# Clean the header
names(ID.map.data) <- c("IDz5", "LaikkuID", "IDz4", "Nimi", 
                        "SKoodi")
# Factorize suojelukoodi
ID.map.data$Suojelukoodi <- factor(ID.map.data$Suojelukoodi, 
                                   labels=c("suojeltu", "suojelematon", 
                                            "suojeltu osa", "suojelematon osa"))

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
    reference.data.sp@data <- merge(reference.data.sp@data, ID.map.data, 
                                    by.x="GRIDCODE", by.y="IDz5", all=TRUE)
    
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

project.suoz <- create_zproject(root=root.path)

variant.36 <- getvariant(project.suoz, 1)
variant.37 <- getvariant(project.suoz, 2)
variant.38 <- getvariant(project.suoz, 3)
variant.39 <- getvariant(project.suoz, 4)
variant.45 <- getvariant(project.suoz, 10)
variant.46 <- getvariant(project.suoz, 11)
variant.47 <- getvariant(project.suoz, 12)
variant.48 <- getvariant(project.suoz, 13)

labels <- c("Peruspiirteet kytk", "Peruspiirteet", "Lisäpiirteet", "Corine",
            "Linnut", "Vaikea suo")

p1 <- plot(variant.45, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="45 - koko maa")
p1  <- p1 + theme(legend.position=c(.25, .2))

p2 <- plot(variant.46, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="46 - etelä")
p2  <- p2 + theme(legend.position=c(.25, .2))

p3 <- plot(variant.47, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="47 - pohjoinen")
p3  <- p3 + theme(legend.position=c(.25, .2))

p4 <- plot(variant.38, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="38- pohjoinen pikseli")
p4  <- p4 + theme(legend.position=c(.25, .2))

#p3 <- plot(variant.48, monochrome=FALSE, group=TRUE, labels=labels[2:6],
#          statistic="mean", invert.x=TRUE, main="48_abf_extra_w_cond_birds")
#p3  <- p3 + theme(legend.position=c(.25, .2))

grid.arrange(p1, p2, p3, nrow=1, ncol=3)
grid.arrange(p3, p4, nrow=1, ncol=2)

p4 <- plot(variant.46, monochrome=FALSE, labels=labels, features=1:8,
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")

p5 <- plot(variant.57, monochrome=FALSE, group=TRUE, groups=1:2, 
           labels=c("CONN", "LHQ"),
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")

p6 <- plot(variant.46, monochrome=FALSE, group=FALSE, features=14:18,
           statistic="mean", invert.x=TRUE, main="46")
p6  <- p6 + theme(legend.position=c(.25, .2))
p6