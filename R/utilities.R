# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' clean.str
#' Clean leading and trailing whitespaces from a given string. Additionally,
#' all occurrences of multiple whitespaces are replaced with a single 
#' whitespace.
#'
#' @param x character string
#'
#' @return A cleaned character string
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

clean.str <- function(x) {
  
  x <- gsub("\\s+", " ", x)
  
  # returns string w/o leading or trailing whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste("dolphin", dir, "&"))
  }
}

require.package <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
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
      
      if (ncol(temp) != ncol(data.regions)) {
      
        # If the whole reqion is NA, then populate 
        nas <- data.frame(matrix(NA, nrow(temp), ncol(data.regions) - ncol(temp)))
        temp <- cbind(temp, nas)
      }
      # Use colnames from the first read
      colnames(temp) <- colnames(data.regions)
      data.regions <- rbind(data.regions, temp)
    }
  }
  
  return(data.regions)
}