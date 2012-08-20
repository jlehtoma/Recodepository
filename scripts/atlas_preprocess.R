atlas.data <- read.csv("C:/SuoZ/data/Lintuatlas3_12lajia/Lintuatlas3_12lajia.csv", 
                       header=TRUE, sep=",")

atlas.data.coord <- ddply(atlas.data, .(Finnish), summarise, 
                          wmn_x = weighted.mean(gri_e10, atlas_max),
                          wmn_y = weighted.mean(gri_n10, atlas_max))

atlas.points <- atlas2points(atlas.data.coord, 
                             x.col="wmn_x", 
                             y.col="wmn_y", 
                             z.col="Finnish",
                             x.offset=10000,
                             y.offset=10000,
                             type.offset="multiply")

finland <- readShapeSpatial("H:/Data/Finland/Finland_borders.shp",
                            proj4string=CRS("+init=epsg:2393"))

p.pch <- c(2:length(atlas.points$Finnish))

spl <- list('sp.points', atlas.points, pch=p.pch[atlas.points$Finnish], 
            cex=0.75, col='black')

spplot(finland, "Suojelualu", sp.layout=spl, colorkey=FALSE)