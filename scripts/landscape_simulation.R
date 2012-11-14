# TODO: Add comment
#
# Author: jlehtoma
###############################################################################

## Case 1: Create 3 simple feature landscapes corresponding to forest features
##   	   and create heterogeneity index with a suitable function form.

###############################################################################

rm( list = ls( all=TRUE ))

r.home <- "C:/Users/jlehtoma/Documents/EclipseWorkspace/Framework/trunk/framework/R"
z.home <- "C:/Users/jlehtoma/Documents/EclipseWorkspace/Framework/trunk/framework/zonation"
setwd(r.home)
source("hg.landscape.R")
source("hg.function.forms.R")
source("hg.zonation.R")
source("hg.solution.comparison.R")
source("w.R")

setup.real.landscapes <- function (features, hetgens) {

}

setup.simulated.landscapes <- function (input='input', output='output', type,
					landscapes) {

################################################################################
# Step 1: Create the landscapes and heterogeneity indices
	# Set the dimensions
	x <- y <- seq(1, 100, step=1.0)
	cat(">>> Simulating landscapes with Gaussian random fields...")
	#browser()
	# Create independent feature distributions (landscapes)
	data.list <- batch.create.landscape(x=x, y=y, type=type, mean=0,
					    variance=10, nugget=1, scale=10,
					    alpha=1, n=landscapes)
	cat(" done.")

	cat("\n>>> Calculating heterogeneity transformations...")

	# Create two composite heterogeneity features
	data.list[["hetgen1"]] <- hgen(data.list, form="sqrt.multiply")
	# TODO: focal only accepts one layer for now
	data.list[["hetgen2"]] <- hgen(data.list, form="focal", layer=1, ngb=5,
								   fun=sd)

	# The number of heterogeneity layers
	hetgens <- length(data.list) - landscapes

	cat(" done.")

	return(data.list)

}

setup.real.zonation <- function(data, input, output, landscapes, run=FALSE) {

	# Create the input files: spp files
	# Spp files in set1
	# Descend into the input file directory
	#browser()

	# Just the plain landscape features
	spp1 <- "set1.spp"
	create.spp.file(filename=spp1, sppfiles=data[['features']])
	# Landscape features + hg_qmultiply + hq_avghgh_blstd
	spp2 <- "set2.spp"
	combination <- append(data[['features']], data[['hetgens']][c(1, 2)])
	#browser()
	create.spp.file(filename=spp2, sppfiles=combination)

	# Landscape features + hetgen2
	#spp3 <- "set3.spp"
	#create.spp.file(filename=spp3, sppfiles=spp.files[c(1:landscapes,
	#						(landscapes+2))])
	# Just the heterogeneity features
	spp3 <- "set3.spp"
	create.spp.file(filename=spp3, sppfiles=data[['hetgens']])

	# Crawl out of the input directory
	setwd(z.home)
	# Create the input files: bat files
	bat <-  paste("run_", strsplit(input, "/")[[1]][-1], '.bat', sep="")
	dat <-  "settings.dat"
	op1 <- "op1"
	op2 <- "op2"
	op3 <- "op3"
	#browser()
	create.batch.file(filename = bat, dat=dat,
			spp=paste(input, "/", spp1, sep=""),
			output=paste(output, "/", op1, sep=""))
	create.batch.file(filename = bat, dat=dat,
			spp=paste(input, "/", spp2, sep=""),
			output=paste(output, "/", op2, sep=""),
			append=TRUE)
	create.batch.file(filename = bat, dat=dat,
			spp=paste(input, "/", spp3, sep=""),
			output=paste(output, "/", op3, sep=""),
			append=TRUE)

	if (run) {
		cat("\n>>> Running Zonation...")
		# Run Zonation bats
		setwd(z.home)

		system(bat)
	}
	cat(" done.")
}

setup.sim.zonation <- function(data, input, output, landscapes, run=FALSE) {
################################################################################
# Step 2: Run Zonation analysis

	cat("\n>>> Writing ASCII files...")

	# Ouput ascii grids for Zonation
	## TODO: fix this to handle dirnames automatically
	setwd(z.home)
	# Write files and get file paths in return
	spp.files <- hg.write.multiple.asc(data, use.header=TRUE,
									   prefix=paste(input, "/", sep=""))

	cat("\ndone.")

	# Create the input files: spp files
	# Spp files in set1
	# Descend into the input file directory
	setwd(input)

	# Just the plain landscape features
	spp1 <- "set1.spp"
	create.spp.file(filename=spp1, sppfiles=spp.files[1:landscapes])
	# Landscape features + hetgen1
	spp2 <- "set2.spp"
	create.spp.file(filename=spp2, sppfiles=spp.files[1:(landscapes+1)])
	# Landscape features + hetgen2
	spp3 <- "set3.spp"
	create.spp.file(filename=spp3, sppfiles=spp.files[c(1:landscapes,
															(landscapes+2))])
	# Just the heterogeneity features
	spp4 <- "set4.spp"
	create.spp.file(filename=spp4,
							sppfiles=spp.files[(landscapes+1):length(data)])

	# Crawl out of the input directory
	setwd(z.home)
	# Create the input files: bat files
	bat <-  paste("run_", strsplit(input, "/")[[1]][-1], ".bat", sep="")
	dat <-  "settings.dat"
	op1 <- "op1"
	op2 <- "op2"
	op3 <- "op3"
	op4 <- "op4"
	#browser()
	create.batch.file(filename = bat, dat=dat,
					  spp=paste(input, "/", spp1, sep=""),
					  output=paste(output, "/", op1, sep=""))
	create.batch.file(filename = bat, dat=dat,
					  spp=paste(input, "/", spp2, sep=""),
					  output=paste(output, "/", op2, sep=""),
					  append=TRUE)
	create.batch.file(filename = bat, dat=dat,
					  spp=paste(input,"/", spp3, sep=""),
					  output=paste(output, "/", op3, sep=""),
					  append=TRUE)
	create.batch.file(filename = bat, dat=dat,
					  spp=paste(input,"/", spp4, sep=""),
					  output=paste(output, "/", op4, sep=""),
					  append=TRUE)

	if (run) {
		cat("\n>>> Running Zonation...")
		# Run Zonation bats
		setwd(z.home)

		system(bat)
	}
	cat(" done.")
}

################################################################################
# Step 3: Compare solutions

run.real.compare.solutions <- function(output) {

	op1 <- "op1"
	op2 <- "op2"
	op3 <- "op3"

	cat("\n>>> Starting solution comparison.")

	# Compare solutions
	setwd(output)
	comps <- list(A=c(op1, op2), B=c(op1, op3))
	comp.suite(comps, input="")

	cat(" done.")

	cat("\n\nSimulation done.\n")
}


run.sim.compare.solutions <- function(output) {

	op1 <- "op1"
	op2 <- "op2"
	op3 <- "op3"
	op4 <- "op4"

	cat("\n>>> Starting solution comparison.")

	# Compare solutions
	setwd(output)
	comps <- list(A=c(op1, op2), B=c(op1, op3), C=c(op1, op4))
	comp.suite(comps, input="")

	cat(" done.")

	cat("\n\nSimulation done.\n")
}

run.real <- function(input, output) {
	setwd(input)
	features <- list.files(pattern='^ft_*')
	for (i in 1:length(features)) {
		features[i] <- paste(input, '/', features[i], sep="")
	}
	hetgens <- list.files(pattern='^hg_*')
	for (i in 1:length(hetgens)) {
		hetgens[i] <- paste(input, '/', hetgens[i], sep="")
	}
	#browser()
	data <- list(features=features, hetgens=hetgens)
	setup.real.zonation(data, input, output, run=TRUE)

	run.real.compare.solutions(output)
}

run.simulation <- function (input, output, type, landscapes) {

	data <- setup.simulated.landscapes(input, output, type, landscapes)
	setup.sim.zonation(data, input, output, landscapes, run=TRUE)
	run.sim.compare.solutions(output)
}

landscapes <- c(3, 5, 10)

#for (i in 1:length(landscapes)) {
#	#browser()
#	input <- paste("correct_input/", landscapes[i], sep="")
#	output <- paste("correct_output/", landscapes[i], sep="")
#	run.simulation(input, output, type="GaussRF", landscapes = landscapes[i])
#}
setwd(z.home)
input <- "real_input/ASCII"
output <- "real_output"

run.real(input, output)