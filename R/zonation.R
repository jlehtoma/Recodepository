create.batch.file <- function(filename = "run_multiple.bat", exe="zig2c.exe",
							  param="-r", dat, spp, output, uc=0.0, ds=0,
							  am=1.0, win=1, append=FALSE) {
	line <- paste("call", exe, param, dat, spp, output, uc, ds, am, win, "\n")

	task <- ifelse(append, "edited", "created")
	cat(line, file=filename, append=append)

	cat(paste("\n<<<", task, "batch file"), filename)
}

create.spp.file <- function(filename="filelist.spp", weight=1.0, alpha=1.0,
							bqp=1, bqp.p=1, cellrem=1, sppfiles) {
	for (i in 1:length(sppfiles)) {
		append <- ifelse (i == 1, FALSE, TRUE)
		line <- paste(weight, alpha, bqp, bqp.p, cellrem, sppfiles[i], "\n")
		cat(line, file=filename, append=append)
	}
	cat(paste("\n<<< Created spp file"), filename)
}

read.curves <- function(infile) {

	# Read in all the lines from curves input file
	# TODO: this is really slow
	lines <- readLines(infile)
	# Scan the lines until the header line is found
	for (i in 1:length(lines)) {
		if (grepl("Prop_landscape_lost", lines[i])) {
			# Mark the header line index
			lines <- i
		}
	}

	# Read in the curves file skipping the beginning lines
	dat <- data.frame(read.table(infile, as.is=TRUE, header=FALSE,
					             skip=lines))
	# Standard header entries
	header <- c("Prop_landscape_lost", "cost_needed_for_top_fraction",
			"min_prop_rem", "ave_prop_rem", "W_prop_rem", "ext-1", "ext-2")
	# Populate the rest of the header lines with sp headers.
	for (i in 1:(ncol(dat) - length(header))) {
		header <- append(header, paste("sp", i, sep=""))
	}
	colnames(dat) <- header
	# Assign S3 type class
	class(dat) <- "z.curve.plot"
	return(dat)
}

read.stats <- function(wildcard=".cmp$") {

	data <- list()

	# Get all the comparisons (.cmp) files
	# TODO: fix the wildcard so that it's strict about the extension
	files <- list.files(pattern=wildcard)

	# Loop over the comparison files

	for (i in 1:length(files)) {

		thresh <- read.table(files[i], nrows=10, as.is=TRUE, header=TRUE)
		lines <- readLines(files[i])
		tot <- grep("Total correlation", lines, value=TRUE)
		tot <- as.numeric(tail(strsplit(tot, ":")[[1]], 1))
		data[[files[i]]] <- list(thresh=thresh, total=tot)
	}
	class(data) <- "z.comp.plot"
	return(data)

}

plot.z.comp.plot <- function(x, y, show=TRUE, ...) {

	xrange <- seq(0.1, 1, .1)
	yrange <- seq(0.1, 1, .1)
	#browser()
	# Data structure of x:
	# list
	#	-comparison (list)
	#		-thresh (data frame)
	#		-total	(num)

	# Correlations
	windows()
	plot(xrange, yrange, type="n",  xlab="Features alone",
			ylab="Correlation", ylim=c(-0.1, 1.0))

	abline(h=0, col="grey")
	comparisons <- length(x)
	colors <- rainbow(comparisons)
	linetype <- c(1:comparisons)

	for (i in 1:comparisons){
		data <- x[[i]]$thresh
		#browser()
		lines(xrange, data$correlation, type="l", lwd=1.5,
			  lty=linetype[i], col=colors[i])
	}
	legend("topright", legend=names(x), col=colors,
			lty = linetype)
	savePlot("comparisons_correlation.png", type="png")
	if (!show) {
		dev.off()
	}

	#browser()
	windows()
	# Coverages
	plot(xrange, yrange, type="n",  xlab="Features alone",
			ylab="Coverage proportion" )
	abline(h=1, col="grey")
	for (i in 1:comparisons){
		data <- x[[i]]$thresh
		#browser()
		lines(xrange, data$cover, type="l", lwd=1.5,
				lty=linetype[i], col=colors[i])
	}
	legend("bottomleft", legend=names(x), col=colors,
			lty = linetype)
	text(10, 20, "foo")
	savePlot("comparisons_coverage.png", type="png")
	if (!show) {
		dev.off()
	}
}

plot.z.curve.plot <- function(x, y, ...) {
	nlines <- length(x) - 7
	nelems <- length(x[[8]])
	p <- palette(rainbow(nlines))
	plot(x[[8]], type="l", col=p[1], main="Performance curves",
			ylab="Proportion of ditribution lost",
			xlab="Fraction of landscape lost",
			xaxt="n")
	for (i in 9:length(x)) {
		points(x[[i]], type="l", col=p[length(x) + 2 - i])
	}
	axis(1, at=seq(0, nelems, nelems/10), labels=seq(0.0, 1, 0.1))
	legend("topright", legend=names(x[8:length(x)]), col=p,
			lty = rep(1, nlines))
}

#path <- "C:/Users/jlehtoma/Documents/EclipseWorkspace/Framework/trunk/framework/zonation/correct_output/3/"
#file <- "op1.curves.txt"
#df <- read.curves(paste(path, file, sep=""))
#plot(df)
#setwd(path)
#data <- read.stats()
#plot(data)

read.curves <- function(file) {
  
  # 1st rows header is always incomplete in the file -> header is hard coded
  # here
  header <- c("Prop_landscape_lost", "cost_needed_for_top_fraction", 
              "min_prop_rem", "ave_prop_rem", "W_prop_rem", "ext-1", "ext-2")
  
  # Read in the actual data, skipping the first (header) row
  dat <- read.table(file, header=FALSE, skip=1)
  
  # Fill in the needed amount of columnd headers
  header <- c(header, paste("feature", 1:(ncol(dat) - length(header)), sep="-"))
  colnames(dat) <- header
  return(dat)
}

read.admu.curves <- function(file) {
  
  # First row is (again) malformatted
  
}

# Function for reading in Zonation result rasters in various formats

read.result.rasters <- function(rasters, path=NULL, format=NULL) {
  if (is.null(format)) {
    ext <- ".rank.asc"
  } else {
    ext <- format
  }
  
  results <- stack(sapply(rasters, function(x){named.raster(filepath=file.path(path,
                                                                               paste("result_", x, ext, sep="")),
                                                            name=x) }, 
                          USE.NAMES=F))
  return(results)
}



# Solution comparison -----------------------------------------------------

## A function to compare to to matrices in different ways, for example when
## comparing solutions created bu Zonation
## Parameters:
## x - matrix of values
## y - matrix of values
## fun - function that is used for comparison

comp <- function(x, y, fun="correlation", ...) {
  
  # Check the data
  if (!is.matrix(x) | !is.matrix(y)) {
    stop("Both inputs must be matrices.")
  }
  if (dim(x)[1] != dim(y)[1] | dim(x)[2] != dim(y)[2]) {
    stop("Matrix dimensions do not match.")
  }
  
  switch(fun,
         correlation = correlation(x, y, ...),
         substraction = substraction(x, y),
         frequency = selection.frequency(x, y, ...),
         coverage = selection.coverage(x, y, ...))
}

compare.solutions <- function(file1, file2, ...) {
  # Read in the solutions
  sol1 <- hg.read.asc.file(file1, rm.nodata=-1)
  sol2 <- hg.read.asc.file(file2, rm.nodata=-1)
  subs <- comp(sol1, sol2, fun="substraction")
  
  tsh <-  seq(0, 0.9, 0.1)
  corr <- comp(sol1, sol2, fun="correlation", thresholds=tsh)
  cover <- comp(sol1, sol2, fun="coverage", thresholds=tsh)
  return(list(thresholds=cbind(corr$classes, cover), totalcor=corr$total,
              subs=subs))
}

comp.suite <- function(x, input) {
  for (item in x){
    
    res <- compare.solutions(paste(item[1], ".rank.asc", sep=""),
                             paste(item[2], ".rank.asc", sep=""))
    
    filename <- paste(input, "comparisons_", item[1], "_", item[2],
                      ".cmp", sep="")
    
    write.table(res$thresholds, filename, col.names = TRUE, row.names = TRUE,
                quote=FALSE)
    
    cat(file=filename, paste("Total correlation:", res$totalcor, "\n"),
        append=TRUE)
    
    write.asc.file(res$subs, filename, nrow(res$subs),
                   ncol(res$subs))
    
    plot(read.stats(), show=FALSE)
    
  }
}

correlation <- function(x, y, method="spearman", thresholds=c(0)) {
  
  res <- c()
  for (i in 1:length(thresholds)) {
    x.sel <- as.vector(x[which(x > thresholds[i])])
    y.sel <- as.vector(y[which(y > thresholds[i])])
    res <- append(res, cor(x.sel, y.sel, method=method))
  }
  
  res <- data.frame(res, row.names=thresholds)
  colnames(res) <- "correlation"
  # Returns a list [1] threshold class correlations (data frame), [2] total
  # correlation
  return(list(classes=res, total=cor(as.vector(x), as.vector(y),
                                     method=method)))
}

selection.coverage <- function(x, y, thresholds) {
  
  covs <- c()
  total <- c()
  for (thresh in thresholds) {
    sel1 <- which(x >= thresh)
    sel2 <- which(y >= thresh)
    
    # All produce the same indices -> is this real or not?
    total <- append(total, length(sel1) / length(x))
    covs <- append(covs, sum(sel1 %in% sel2) / length(sel1))
  }
  
  res <- data.frame(total=total, cover=covs, row.names=thresholds)
  return(res)
  plot(read.stats(), show=FALSE)
}

substraction <- function(x, y) {
  return(x - y)
}


# From: http://r-sig-geo.2731867.n2.nabble.com/questions-on-RasterStack-Brick-td5553580.html

stackcor <- function(s1, s2, method='spearman') {
  mycor <- function(v) {
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    cor(x, y, method=method)
  }
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  calc(s, fun=mycor )
}

# The Jaccard coefficient measures similarity between sample sets, and is 
# defined as the size of the intersection divided by the size of the union of 
# the sample sets

jaccard <- function(raster1, raster2, threshhold, warn.uneven=FALSE) {
  
  # Get the values above the threshhold
  raster1.bin <- raster1 > threshhold
  raster2.bin <- raster2 > threshhold
  
  if (warn.uneven) {
    raster1.size <- count(raster1.bin, 1)
    raster2.size <- count(raster2.bin, 1)
    # Sort from smaller to larger
    sizes <- sort(c(raster1.size, raster2.size))
    if (sizes[2] / sizes[1] > 20) {
      warning("The extents of raster values above the threshhold differ more than 20-fold: Jaccard coefficient may not be informative.")
    }
  }
  
  # Calculate the intersection of the two rasters, this is given by adding 
  # the binary rasters together -> 2 indicates intersection
  combination <- raster1.bin + raster2.bin
  intersection <- combination == 2
  
  # Union is all the area covered by the both rasters
  union <- combination >= 1
  
  return(count(intersection, 1) / count(union, 1))
}