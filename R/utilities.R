require.package <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}

require.package("XLConnect")
require.package("plyr")

barplot.site.summaries <- function(x, main, rowlab, collab, ...) {
  
  cols <- brewer.pal(nrow(x), "Greens")
  
  rownames(x) <- rowlab
  colnames(x)  <- collab
  
  barplot(x[, 2:ncol(x)], beside=TRUE, col=cols,
          legend.text=source.names, xlab="Kasvupaikka", ylab="Indeksi", 
          main=main)
}

# Function replaces 1+ whitespaces with only 1 whitespace and removes leading
# and trailing whitespaces

clean.str <- function(str) {
  
  str <- gsub("\\s+", " ", str)
  
  # returns string w/o leading or trailing whitespace
  str <- gsub("^\\s+|\\s+$", "", str)
  return(str)
}

hierarchy <- function(x) {
  
   
}

library(raster)

named.raster <- function(filepath, name) {
  raster.obj <- raster(filepath)
  slot(raster.obj, "title")  <- name
  return(raster.obj)
}

histPlot <- function(x, mask.obj=NULL, add.mean=FALSE, add.median=FALSE, 
                     show=TRUE, save.dir="", binwidth=0.05) {
  if (class(x) != "RasterLayer") {
    stop("x must be an object of class 'RasterLayer'!")
  } 
  if (!is.null(mask.obj)) {
    if (class(mask.obj) == "RasterLayer") {
      raster.obj <- mask(x, mask.obj)
    } else {
      print("Mask provided is not a RasterLayer, skipping")
    }
  } else {
    raster.obj <- x
  }
  
  if (!is.null(mask.obj)) {
    name.body <- paste(slot(x, "title"), "_", slot(mask.obj, "title"), sep="")
  } else {
    name.body <- slot(x, "title")
  }
  
  raster.values <- values(raster.obj)
  temp.df <- data.frame(data=raster.values)
  m <- ggplot(temp.df, aes(x = data)) + geom_histogram(colour = "white", 
                                                       binwidth=binwidth) + 
    ggtitle(name.body)
  
  if (add.median) {
    m <- m + geom_vline(xintercept = median(raster.values, na.rm=T), 
                        colour = "red") 
  }
  
  if (add.mean) {
    m <- m + geom_vline(xintercept = mean(raster.values, na.rm=T), 
                        colour = "blue") 
  }
  
  if (show) {
    show(m)
  }
  if (save.dir != "") {
    file.path <- file.path(save.dir, paste("hist_", name.body, ".png", sep=""))
    print(paste("Saving plot to:", file.path))
    ggsave(filename = file.path, plot = m)
  }
}

# Function for reading in Zonation result rasters in various formats

read.results <- function(rasters, path=NULL, format=NULL) {
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

readWorksheet.disjoint <- function(wb, sheet, regions, ...) {
  
  regions <- unlist(strsplit(regions, ";"))
  
  data.regions <- data.frame()
  
  for (i in 1:length(regions)) {
    if (i == 1) {
      data.regions <- readWorksheet(wb, sheet, region = regions[i], 
                                    header=TRUE)
    } else {
      temp <- readWorksheet(wb, sheet, region = regions[i], header=FALSE)
      # Use colnames fromt the first read
      colnames(temp) <- colnames(data.regions)
      data.regions <- rbind(data.regions, temp)
    }
  }
  
  return(data.regions)
}

stack.boxplot <- function(x, mask.obj=NULL, show=TRUE, save.dir="") {
  if (class(x) != "RasterStack") {
    stop("x must be an object of class 'RasterStack'!")
  } 
  if (!is.null(mask.obj)) {s
                           if (class(mask.obj) != "Raster") {
                             
                             raster.obj <- mask(x@raster, mask.obj@raster)
                           } else {
                             print("Mask provided is not a named.raster, skipping")
                           }
  } else {
    raster.obj <- x@raster
  }
  
  if (!is.null(mask.obj)) {
    name.body <- paste(x@name, "_", mask.obj@name, sep="")
  } else {
    name.body <- x@name
  }
}