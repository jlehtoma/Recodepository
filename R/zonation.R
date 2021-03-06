if (!require('ggplot2')) {
  install.packages('ggplot2')
}
if (!require('reshape2')) {
  install.packages('reshape2')
}

# ggplot2 themes ----------------------------------------------------------

curve.theme <- theme(plot.title=element_text(face="bold", size=20),
                     axis.title.x=element_text(size=24),
                     axis.title.y=element_text(size=24),
                     axis.text.x=element_text(size=20),
                     axis.text.y=element_text(size=20),
                     axis.ticks = element_line(size = 2),
                     legend.text=element_text(size=20),
                     legend.title=element_text(size=20),
                     panel.border = element_rect(size=2, colour="black"))

# Globals -----------------------------------------------------------------

curve.x.title <- "\nProp. of landscape lost"
curve.x.title.invert <- "\nProportion of landscape\n under conservation"
curve.y.title <- "Prop. of ditributions remaining\n"
curve.legend.title <- "Features"
grp.curve.legend.title <- "Feature groups"

red <- "#FF0000"

# File creation -----------------------------------------------------------

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


# Plotting ----------------------------------------------------------------

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

plot.z.curves <- function(x, statistic=NULL, features=NULL, monochrome=FALSE, 
                          invert.x=FALSE, labels=NULL,  ...) {
  
  #browser()
  
  if (is.null(statistic)) {
    index <- NULL
  } else if (statistic == "min") {
    index <- 3
  } else if (statistic == "mean") {
    index <- 4
  }
  
  col.ids <- 8:length(x)
  # Which features are actually included
  if (!is.null(features)) {
    col.ids <- col.ids[features]
  }
  col.ids <- c(1, index, col.ids)
  
  # Subset only the needed columns
  x <- x[col.ids]
  
  # List -> DataFrame
  x <- as.data.frame(x)
  
  # Reshape the DataFrame
  x.melt <- melt(data = x, id.vars=c(1), measure.vars=2:length(col.ids))
  
  # Create the necessary widths
  #color.scale <- c(red, gray.colors(length(col.ids) - 2))
  size.scale <- c(2, rep(1, length(col.ids) - 2))
  
  p <- ggplot(x.melt, aes(x=Prop_landscape_lost, y=value, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.5)
  
  if (monochrome) {
    p <- p + theme_bw() + scale_colour_grey(name=curve.legend.title)
    
  } else {
    p <- p + scale_color_discrete(name=curve.legend.title)
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.25)
    p + xlab(curve.x.title.invert) + ylab(curve.y.title) + curve.theme +
      scale_x_continuous(breaks=x.scale, labels=1-x.scale)
  } else {
    p + xlab(curve.x.title) + ylab(curve.y.title) + curve.theme
  }
}

plot.z.grp.curves <- function(x, statistic="mean", groups=NULL, 
                              monochrome=FALSE, invert.x=FALSE, 
                              labels=NULL, ...) {
  
  # Set the statistics indeces
  index <- list("min"=3, "mean"=4, "max"=5, "w.mean"=6, "ext2"=7)
  
  if (statistic %in% names(index)) {
    # Starting from nth column, every 5th column is the same statistic
    col.ids <- seq(index[[statistic]], length(x), 5)
    # Keep also F.lost
    x <- x[c(1, col.ids)]
  } else {
    stop(paste("Unkown statistic type:", statistic))
  }
  
  # Which groups are actually included
  grps <- 2:ncol(x)
  if (!is.null(groups)) {
    grps <- grps[groups]
  }
  
  # Reshape the DataFrame
  x.melt <- melt(data = x, id.vars=c(1), measure.vars=grps)
  
  p <- ggplot(x.melt, aes(x=value, y=F.lost, group=variable))
  p <- p + geom_line(aes(colour = variable))
  
  
  if (is.null(labels)) {
    if (monochrome) {
      p <- p + scale_colour_grey(name=grp.curve.legend.title) + theme_bw() 
    } else {
      p <- p + scale_colour_brewer(name=grp.curve.legend.title) 
    }
  } else {
    if (monochrome) {
      p <- p + scale_colour_grey(name=grp.curve.legend.title, labels=labels) +
        theme_bw() 
    } else {
      p <- p + scale_colour_brewer(name=grp.curve.legend.title, labels=labels,
                                   type = "qual", palette=2)
      theme_update(curve.theme)
    }
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.25)
    p <- p + scale_x_continuous(breaks=x.scale, labels=1-x.scale)
  } else {
    
  }
  p + xlab(curve.x.title) + ylab(curve.y.title) + curve.theme + 
    ggtitle(statistic)
}


# Reading -----------------------------------------------------------------

read.curves <- function(infile) {

	# Read in the curves file skipping the header line, we'll construct this 
  # later on
	curves <- read.table(infile, as.is=TRUE, header=FALSE, skip=1)
	# Standard header entries
  
  # The header has a set of standard components + proportion for each species
  # remaining at level of removal (created dynamically)
	header <- c("Prop_landscape_lost",           # 1
              "cost_needed_for_top_fraction",  # 2
			        "min_prop_rem",                  # 3
              "ave_prop_rem",                  # 4
              "W_prop_rem",                    # 5
              "ext-1",                         # 6
              "ext-2")                         # 7
  
	# Populate the rest of the header lines with sp headers and assign it
	header <- c(header, paste("F", 1:(ncol(curves) - length(header)), sep=""))
	colnames(curves) <- header
	return(curves)
}

read.admu.curves <- function(file) {
  
  # First row is (again) malformatted 
  
}
  
read.grp.curves <- function(file) {

  grp.curves <- read.table(file, header=TRUE)
  
  # standard part of the header
  header <- c("F.lost", "TF_cost")
  
  # Repeating parts of the group curves header
  rep.header <- c("min", "mean", "max", "w.mean", "ext2")
  times <- (ncol(grp.curves) - length(header)) / length(rep.header)
  rep.header <- paste(rep(rep.header, times), rep(1:times, each=length(rep.header)), sep="-")
  header <- c(header, rep.header)
  colnames(grp.curves) <- header
  
  return(grp.curves)
  
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


#path <- "C:/Users/jlehtoma/Documents/EclipseWorkspace/Framework/trunk/framework/zonation/correct_output/3/"
#file <- "op1.curves.txt"
#df <- read.curves(paste(path, file, sep=""))
#plot(df)
#setwd(path)
#data <- read.stats()
#plot(data)

# Function for reading in Zonation result rasters in various formats

read.result.rasters <- function(rasters, path=NULL, format=NULL) {
  if (is.null(format)) {
    ext <- ".rank.asc"
  } else {
    ext <- format
  }
  
  results <- raster::stack(lapply(rasters, function(x){raster(file.path(path, x, "output",
                                                                paste("result_", x, ext, sep=""))) }))
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

cross.jaccard <- function(results, cut.off) {

  jaccards <- matrix(nrow=nlayers(results), ncol=nlayers(results))
  
  for (i in 1:nrow(jaccards)) {
    for (j in 1:ncol(jaccards)) {
      if (i == j) {
        jaccards[i, j] <- 1
      } else {
        # See the complement, if it's not NA then the pair has already been
        # compared
        if (is.na(jaccards[j, i])) {
          jaccards[i, j] <- jaccard(results[[i]], results[[2]], cut.off)
        } else {
          jaccards[i, j]  <- jaccards[j, i]
        }
      }
    }
  }
  return(jaccards)
}