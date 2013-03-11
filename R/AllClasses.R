setGeneric("getVariant", function(object, index) standardGeneric("getVariant"))
setGeneric("plot.curves", function(x, ...) standardGeneric("plot.curves"))

# Zonation project ------------------------------------------------------------

setClass("Zproject", representation(root = "character",
                                    variants = "list"))

setMethod("initialize", "Zproject", function(.Object, root) {

  .Object@root <- root

  variants <- list()
  
  # All the folders that 1) start with a number and 2) have a subfolder 
  # "output" are cosidered to be variants
  folders <- list.dirs(root, recursive=FALSE)
  
  for (folder in folders) {
    # Get the leaf folder
    variant.folder <- tail(unlist(strsplit(folder, "/")), n=1)
    # A variant folder must be a directory starting with numbers and it
    # must have a subfolder named "output"
    if (grepl("^[0-9]+", variant.folder) & file.exists(file.path(folder, 
                                                                 "output"))) {
      variants[variant.folder] <- new("Zvariant", name=variant.folder,
                                      root=folder)
    } else {
      next
    } 
  }
  .Object@variants <- variants 
  
  .Object
})

setMethod("getVariant", c("Zproject", "ANY"), function(object, index) {
    return(object@variants[[index]])
  }
)

setMethod("names", "Zproject", function(x) {
  return(names(x@variants))
})


# Zonation variant --------------------------------------------------------

setClass("Zvariant", representation(name = "character",
                                    root = "character",
                                    results = "list"))

setMethod("initialize", "Zvariant", function(.Object, name, root) {

  .Object@name <- name
  .Object@root <- root
  
  results <- list()
  
  if (file.exists(root)) {
    # Try if there is a subfolder named "output", if not, let's try the root
    output.folder <- file.path(root, "output") 
    if (!file.exists(output.folder)) {
      output.folder <- root
    }
    
    .get.file <- function(output.folder, x) {
      target <- list.files(output.folder, pattern=x, full.names=TRUE)
      if (length(target) == 0) {
        return(NA)
      } else if (length(target) == 1) {
        return(target)
      } else {
        warning(paste("More matches than 1 found for", x, "using only the first"))
        return(target[1])
      }
    }
    
    # Curves file is named *.curves.txt
    results[["curves"]] <- read.curves(.get.file(output.folder, 
                                                 "\\.curves\\.txt"))
    
    # Group curves file is named *.grp_curves.txt
    results[["grp.curves"]] <- read.grp.curves(.get.file(output.folder, 
                                                        "\\.grp_curves\\.txt"))
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- .get.file(output.folder, "\\.rank\\.")
    
  } else {
    stop(paste0("Cannot create variant "), name, ": path ", root, " does not exist")
  }
  
  .Object@results <- results
  
  .Object
})

setMethod("plot.curves", "Zvariant", function(x, groups=FALSE, ...) {
  if (groups) {
    plot.z.grp.curves(x@results[["grp.curves"]], ...)
  } else {
    plot.z.curves(x@results[["curves"]], ...)
  }
})