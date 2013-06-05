if (!require("ggplot2")) {
  install.packages("ggplot2")
}

if (!require("rgdal")) {
  install.packages("rgdal")
}

if (!require("plyr")) {
  install.packages("plyr")
}

# 1. Read in the shapefile data --------------------------------------------

# Set up the data source holding the Metsähallitus vector data
dsn.folder <- "H:/Data/Metsahallitus/ArcGIS/Shapefiles/2010"
dsn <- file.path(dsn.folder, "Biotooppi_p.shp") 

# Get the available layer
layer <- ogrListLayers(dsn)

# Read the shapefile, WARNING: may take a while, shapefile is quite large
sp.mh <- readOGR(dsn, layer)

# NOTE: Converting Biotooppi_p to shape will mangle the field names. Fix them.
field.names <- c("OBJECTID", "IDEAPERKUV", "INVENT_LK", 
                 "LISAMAARE1", "LISAMAARE2", "LISAMAARE3", "RAVINTEISUUS",
                 "SUOYHD_TYYPPI", "RANTA_TYYPPPI", "VESI_KASVPEITE", 
                 "VESI_SYVYYS", "VESI_TYYPP", "RINNE_SUUNTA", "JYRKANNE_KORK", 
                 "KALTEVUUS", "GEOMOR", "KASV_TYYPPI", "PL_VALT", 
                 "PENSAS_KOKPEITTO", "LATVUS_KOKPEITTO", "BIOT_LK", 
                 "NATURA_TYYPPI1", "NATURA_TYYPPI2", "NATURA_EDUST1",
                 "NATURA_EDUST2", "NATURA_ARVTAPA1", "NATURA_ARVTAPA2",
                 "PAIV_AIKA", "ARV_AIKA", "ARV_TAPA", "Shape_Length",
                 "Shape_Area", "INVENT_LK_INT", "KASV_TYYPPI_INT", 
                 "NAT_TYYPPI2_INT", "NAT_EDUST1_INT","NAT_EDUST2_INT", 
                 "NAT_TYYPPI1_INT")

names(sp.mh@data) <- field.names

# Save the spatial object in RData for quicker reading later on
output.sp.file <- file.path(dsn.folder, "sp_biotooppi_p.RData")
save(sp.mh, file=output.sp.file)

# Get just the attribute table
df.mh <- sp.mh@data

# 2. Read in the Natura2000 codes -----------------------------------------

n2000.codes <- read.csv("data_public/Metsahallitus/n2000_codes.csv", sep=";")

# 3. Natura2000 types -----------------------------------------------------

# Metsahallitus data has two fields that mark a Natura2000 type for a polygon.
# These fields are "NATURA_TYYPPI1" and "NATURA_TYYPPI2". One polygon can have
# both in which case both are considered equal. If a polygon has 
# "NATURA_TYYPPI2" it will always have "NATURA_TYYPPI1" too.

# How many non-NAs do we actually we have for "NATURA_TYYPPI1" and
# "NATURA_TYYPPI2"

na.perc1 <- sum(is.na(df.mh$NATURA_TYYPPI1)) / length(df.mh$NATURA_TYYPPI1) 
print(paste("Percentage of NAs in NATURA_TYYPPI1:", 100 * round(na.perc1, 3), "%"))

na.perc2 <- sum(is.na(df.mh$NATURA_TYYPPI2)) / length(df.mh$NATURA_TYYPPI2) 
print(paste("Percentage of NAs in NATURA_TYYPPI2:", 100 * round(na.perc2, 3), "%"))

# Merge the actual Natura2000 types based on the code
df.mh$NATURA_TYYPPI1_DESC <- as.character(n2000.codes[df.mh$NATURA_TYYPPI1,]$Selitys)
df.mh$NATURA_TYYPPI2_DESC <- as.character(n2000.codes[df.mh$NATURA_TYYPPI2,]$Selitys)

# Save that too
output.file <- file.path(dsn.folder, "biotooppi_p.RData")
save(df.mh, file=output.file)

# Tabulate the count of each type 1 and 2 occurrences and areas
n2000.types1 <- ddply(df.mh, .(NATURA_TYYPPI1_DESC), summarise,
                      count=length(NATURA_TYYPPI1_DESC),
                      area=sum(Shape_Area))
n2000.types1$NTYPE <- "1"

n2000.types2 <- ddply(df.mh, .(NATURA_TYYPPI2_DESC), summarise,
                      count=length(NATURA_TYYPPI2_DESC),
                      area=sum(Shape_Area))
n2000.types2$NTYPE <- "2"

names(n2000.types1) <- c("Description", "Count", "Area", "Type")
names(n2000.types2) <- c("Description", "Count", "Area", "Type")

n2000.types <- rbind(n2000.types1, n2000.types2)

# Do we have any cases where we have type 2 but not type 1?
sum(!is.na(df.mh$NATURA_TYYPPI1) & is.na(df.mh$NATURA_TYYPPI2))

# Remove the NA rows
n2000.types <- n2000.types[!is.na(n2000.types$Description),]
# Order by count
n2000.types <- n2000.types[with(n2000.types, order(Count, decreasing=TRUE)), ]

# Over all count and summed areas (use only type 1 since there are only 7
# cases where we have type 2 but not type 1)
total.count <- sum(subset(n2000.types, Type == "1")$Count)
# Hectares
total.area <- sum(subset(n2000.types, Type == "1")$Area) / 10000

# Plot the types
p <- ggplot(n2000.types, aes(x=Description, y=log(Count), fill=Type)) +
  geom_bar(stat="identity", position="dodge") + coord_flip()
p

# Order by area
n2000.types <- n2000.types[with(n2000.types, order(Area, decreasing=TRUE)), ]
p <- ggplot(n2000.types, aes(x=Description, y=log(Area), fill=Type)) +
  geom_bar(stat="identity", position="dodge") + coord_flip()
p

# Plot count against area
p <- ggplot(n2000.types, aes(x=log(Count), y=log(Area), color=Type)) + 
  geom_point(size=3)
p