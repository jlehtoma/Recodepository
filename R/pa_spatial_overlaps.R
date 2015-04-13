library(dplyr)
library(magrittr)

source("R/config.R")

rli_db <- src_postgres(dbname = DBNAME, host = HOST, port = PORT, user = USER,
                       password = PASSWORD)

q <- "SELECT lso.subtype AS programme,
             SUM(ST_AREA(lsa.geom)) / 10000 AS PA_area_ha, 
             lso_tot.Prog_area_ha AS Prog_area_ha, 
             SUM(ST_AREA(ST_INTERSECTION(lsa.geom, lso.geom))) / 10000 AS overlap_ha
      FROM oiva.lsalue lsa, oiva.lsoalue lso, 
           (SELECT subtype, SUM(ST_AREA(lsoalue.geom)) / 10000 AS Prog_area_ha
            FROM oiva.lsoalue
            GROUP BY subtype) AS lso_tot
      WHERE st_intersects(lsa.geom, lso.geom) AND
            lso.subtype = lso_tot.subtype
      GROUP BY lso.subtype, lso_tot.Prog_area_ha
      ORDER BY lso.subtype"

qtbl <- collect(tbl(rli_db, sql(q)))
qtbl %<>% mutate(overlap_perc = round(overlap_ha / prog_area_ha * 100, 2)) %>% 
  arrange(programme) %>% 
  select(programme, prog_area_ha, overlap_ha, overlap_perc)


# Test the numbers from intersected shapefiles ----------------------------

library(maptools)

dsn <- "/home/jlehtoma/Data/SYKE/oiva/luonnonsuojeluohjelmat/LsOAlue_lsavaltio_intersect.shp"

shape_lso_lsa_intersect <- readShapePoly(dsn)
lso_lsa_intersect <- shape_lso_lsa_intersect@data

lso_lsa_summary <- lso_lsa_intersect %>% 
  group_by(Subtype_2) %>% 
  summarise(
    overlap_ha = sum(as.numeric(Area), na.rm = TRUE) / 10000
  ) %>% 
  select(programme = Subtype_2, overlap_ha)
