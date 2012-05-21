#### Modelled distribution data in conservation planning
### Spring school 2012
### 15th of March 
### Laura Meller & Laure Zupan 

library(BIOMOD)
library(raster)
library(rgdal)

##############################################
### 1) Model the distribution of the species
## Example with Gam
##############################################

### Set the director, you need to change the 

setwd("/Users/zupanl/Documents/THESE/Spring_summer_school_2012/SpringSchool2012_Practical_Conservation")

###############
# a) Load the data
###############


load("DATA/sp.env")
load("DATA/VarEnvCurProj")

#check what is in your workspace
ls()


###############
# b) Calibrate the model
###############


# b1) Initial state 


XY= sp.env[,c(1,2)]
Expl.Var= sp.env[,c(3:6)]
Resp.Var = sp.env[,c(7:13)]

Initial.State(Response = Resp.Var, Explanatory = Expl.Var)




# b2) Run Biomod



Models(GLM = F, TypeGLM = "poly", Test = "AIC",
GBM = F, No.trees = 2000, GAM = T, Spline = 3, CTA = F, CV.tree = 50, ANN = F, CV.ann = 2, SRE = F, quant=0.025, FDA = F, MARS = F, RF = F, NbRunEval = 3, DataSplit = 80, Yweights=NULL, Roc = T, Optimized.Threshold.Roc = T, Kappa = T, TSS=T, KeepPredIndependent = T, VarImport=5, NbRepPA=1, strategy="circles", coor=XY, distance=2, nb.absences=1000)



###############
# c) Project your prediction with the current variables (e.g: with one transformation method: TSS)
###############

Projection(Proj = as.data.frame(VarEnvCurProj[,c(3:6)]), Proj.name =  "Current_GAM", GLM = FALSE, GBM = FALSE, GAM = TRUE, CTA = FALSE, ANN = FALSE, SRE = FALSE, quant=0.025, FDA = FALSE, MARS = FALSE, RF = FALSE, BinRoc = FALSE, BinKappa = FALSE, BinTSS = TRUE, FiltRoc = FALSE, FiltKappa = FALSE, FiltTSS = FALSE, repetition.models=FALSE, compress="xz")



##############################################
### 2) The gap analysis 
## Example with Presence/absence data
##############################################

###############
# a) Load the protected area information
###############



load("DATA/PA.coverage")

ls()

head(PA.coverage) ## 3 columns dataframe: X and Y are the coordinates, "coverage" is the fraction of each pixel considered as protected (vary betw. 0 and 1)



###############
# b) Create a table with the predicted distribution of our species 
###############



coor = VarEnvCurProj[,c(1,2)] # coordinates of the study area

sp = colnames(Resp.Var) # Names of the species 



# b1) we first create an ampty table (NA values) where we will put the predicted distribution of our species:

occ = matrix(nrow = nrow(coor), ncol = length(sp)) 
colnames(occ) = sp # We assign the column names 
occ = cbind(coor, occ) # we add the coordinates 



# b2) We use a loop to fill the table "occ" that we just created with the predicted distribution of the species 

for (i in 1:length(sp))

	{
	
	# we load the projection 
	
	proj = get(load(paste("proj.Current_GAM/Proj_Current_GAM_", 	sp[i], "_BinTSS", sep = ""))) # get() permit to consider 		proj as an object and not as a character chain.
	
	
	# We fil our table: 
	occ[,i+2] = proj[,"GAM",1,1] ## i+2 because the 2 first 		column are the coordinates 
		
	}


###############
# c) Perform the GAP ANALYSIS
###############



# c1) Calculate of the proportion of occupied cell covered by protected area:

occ.PA = occ[,-c(1:2)]*PA.coverage$coverage


# c2) Calculate the sum of predicted presence for each species

Nocc = (colSums(occ[,-c(1:2)])) 


# c3) Calculate the sum of the predicted presence that are protected

Ncellprot = (colSums(occ.PA))


# c4)  Calculate the percentage of representation of each species in the PA network
representation = Ncellprot/Nocc 

representation


##############################################
### 3) Prepare the files for Zonation
##############################################



# If you are mac or linux user and have a virtual machine on your computer, the easiest is to prepare all the files on your Mac 1st and then copy them in windows. BUT you have to be carefull when compiling the splist.spp file(see b), below), that contain information on the directory that Zonation have to go for the species rasters, the path you specify should be the windows one.


###############
# a) Prepare the raster files for each of the species 
###############




# a1)Load the mask. This is a raster of the study area with the same extent of our projection. We will use it as a calc to build the raster of the species

load("DATA/mask") # call mask in the R workspace


# Now we create a raster of the modelled distribution for each of the species 

for (i in 1: length(sp))

	{
		
	# We use the table we produced in the loop before:

  	proj = occ[,sp[i]]
	
	ras = rasterize(coor, mask, field = proj, background = NA)
	
	# Write the raster (Gtiff format)
	writeRaster(ras, filename = paste("rasters/raster_", sp[i], ".tif", sep = ""), format = "GTiff", NAflag = -999, overwrite = T)
	
	}



###############
#b) Prepare the splist.spp file. 
###############

## NOTE: In the loop, you need to change the path to the rasters to match your directory. Please specify the complete path!

splist = 1:length(sp)

for (i in 1:length(sp))
{
  splist[i] <- paste("1.0 1 1 1 1.0 C:/Users/Administrateur/Documents/ZONATION/rasters/raster_", sp[i], ".tif", sep="") 
  
}


## Save the data in text file format

write.table(splist, file ="ZONATION/splist.spp.txt", quote = F, row.names= F, col.names = F) 

# for mac user: 

write.table(splist, file = "ZONATION/splist.spp.txt", quote = F, row.names= F, col.names = F, eol = "\r\n") ## you need to put this "eol = "\r\n"" that make your text file readable by windows...


#### And now you are ready to run Zonation!!!

