library(raster)

# Define the data set location
ds <- "/media/DataVault/Data/Finland/Corine/clc_fi25m_paaluokat.tif"

# Read in the data for the whole country
corine.fin <- raster(ds)
hist.values <- data.frame(Freq=c(82652718, 4423681, 136868474, 36234351, 27331533, 4862941, 
                 27976268, 1235334))
rownames <- c("", "Wetlands", "Aquatic", "Agriculture", "Forests", "Built",
              "Sparse", "Sparse_forest")
row.names(hist.values) <- rownames
barplot(hist.values$Freq)