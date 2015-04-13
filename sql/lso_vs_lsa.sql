SELECT lso.subtype AS programme,
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
ORDER BY lso.subtype